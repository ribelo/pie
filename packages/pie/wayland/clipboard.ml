exception Retired
exception Bundle of exn * exn
exception Clipboard_failure of string

type helper = {
  token : int;
  pid : int;
  cancel : unit -> unit;
  settled : unit Eio.Promise.t;
  ready : (int, string) result Eio.Promise.t;
}

type packed_mgr = Mgr : _ Eio.Process.mgr -> packed_mgr

type t = {
  owner_sw : Eio.Switch.t;
  process_mgr : packed_mgr;
  copy_program : string;
  slots : Eio.Semaphore.t;
  current : helper option ref;
  next_token : int Atomic.t;
}

let combine primary cleanup = Bundle (primary, cleanup)

let resolve_once promise value =
  try Eio.Promise.resolve promise value with Invalid_argument _ -> ()

let known_message exn = Sh.failure_message ~prog:"wl-copy" exn

let kill_await helper =
  Eio.Cancel.protect (fun () ->
      helper.cancel ();
      Eio.Promise.await helper.settled)

let retire_if_current t token =
  match !(t.current) with
  | Some helper when helper.token = token ->
      t.current := None;
      kill_await helper
  | Some _ | None -> ()

let retire_current t =
  match !(t.current) with
  | None -> ()
  | Some helper -> retire_if_current t helper.token

let with_slot t f =
  Eio.Semaphore.acquire t.slots;
  match try Ok (f ()) with exn -> Error exn with
  | Ok value ->
      Eio.Semaphore.release t.slots;
      value
  | Error primary -> (
      match
        try
          Eio.Semaphore.release t.slots;
          None
        with cleanup -> Some cleanup
      with
      | None -> raise primary
      | Some cleanup -> raise (combine primary cleanup))

let spawn t text =
  let token = Atomic.fetch_and_add t.next_token 1 in
  let settled, resolve_settled = Eio.Promise.create () in
  let ready, resolve_ready = Eio.Promise.create () in
  let sw_r = ref None in
  Eio.Fiber.fork_daemon ~sw:t.owner_sw (fun () ->
      Fun.protect
        ~finally:(fun () ->
          sw_r := None;
          resolve_once resolve_settled ())
        (fun () ->
          try
            Eio.Switch.run @@ fun helper_sw ->
            sw_r := Some helper_sw;
            let (Mgr process_mgr) = t.process_mgr in
            let stdin_source, stdin_sink =
              Eio.Process.pipe ~sw:helper_sw process_mgr
            in
            let discard_out = Sh.discarded_stderr () in
            let discard_err = Sh.discarded_stderr () in
            (* No [--type]: a lone text/plain;charset=utf-8 offer is not a Paste
               target for Wayland Chrome. wl-copy's text default also offers
               text/plain, TEXT, STRING and UTF8_STRING. *)
            let process =
              Eio.Process.spawn ~sw:helper_sw process_mgr ~stdin:stdin_source
                ~stdout:discard_out ~stderr:discard_err
                ~executable:t.copy_program
                [ t.copy_program; "--foreground" ]
            in
            Eio.Flow.close stdin_source;
            match
              Eio.Fiber.first
                (fun () ->
                  Eio.Flow.copy_string text stdin_sink;
                  Eio.Flow.close stdin_sink;
                  `Copied)
                (fun () -> `Dead (Eio.Process.await process))
            with
            | `Dead _ ->
                resolve_once resolve_ready (Error "clipboard helper failed")
            | `Copied ->
                resolve_once resolve_ready (Ok (Eio.Process.pid process));
                ignore (Eio.Process.await process)
          with
          | Retired ->
              resolve_once resolve_ready (Error "clipboard helper failed")
          | Eio.Cancel.Cancelled _ as exn ->
              resolve_once resolve_ready (Error "clipboard helper failed");
              raise exn
          | Eio.Exn.Multiple _ as exn -> raise exn
          | exn -> resolve_once resolve_ready (Error (known_message exn)));
      `Stop_daemon);
  {
    token;
    pid = 0;
    cancel =
      (fun () ->
        match !sw_r with None -> () | Some sw -> Eio.Switch.fail sw Retired);
    settled;
    ready;
  }

let offer_sync t text =
  with_slot t @@ fun () ->
  let helper = spawn t text in
  let committed = ref false in
  try
    Eio.Switch.run @@ fun wait_sw ->
    Eio.Switch.on_release wait_sw (fun () ->
        if not !committed then kill_await helper);
    match Eio.Promise.await helper.ready with
    | Error message -> raise (Clipboard_failure message)
    | Ok pid -> (
        if pid = 0 then raise (Clipboard_failure "clipboard helper failed");
        (* Retire the previous source only after this helper is ready, and not
           via [retire_current]: [current] already names the new helper. *)
        let previous = !(t.current) in
        t.current := Some { helper with pid };
        committed := true;
        match previous with None -> () | Some old -> kill_await old)
  with
  | Clipboard_failure _ as exn -> raise exn
  | exn when !committed -> raise exn
  | exn -> (
      match
        try
          kill_await helper;
          None
        with cleanup -> Some cleanup
      with
      | None -> raise exn
      | Some cleanup -> raise (combine exn cleanup))

let create ~caps ~sw ~process_mgr ?(copy_program = "wl-copy") () =
  let copy_program =
    if Filename.is_relative copy_program then
      Option.value ~default:copy_program (Sh.find_executable ~caps copy_program)
    else copy_program
  in
  let t =
    {
      owner_sw = sw;
      process_mgr = Mgr process_mgr;
      copy_program;
      slots = Eio.Semaphore.make 1;
      current = ref None;
      next_token = Atomic.make 1;
    }
  in
  Eio.Switch.on_release sw (fun () ->
      Eio.Cancel.protect (fun () -> retire_current t));
  t

let of_sync f =
  try Ok (f ()) with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Eio.Exn.Multiple _ as exn -> raise exn
  | Bundle _ as exn -> raise exn
  | Clipboard_failure message -> Error (`Clipboard_error message)
  | exn -> Error (`Clipboard_error (known_message exn))

let offer t text = of_sync (fun () -> offer_sync t text)
let shutdown t = of_sync (fun () -> with_slot t (fun () -> retire_current t))
