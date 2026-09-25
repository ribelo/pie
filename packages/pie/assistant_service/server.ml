let mkdir_p = Fs.mkdir_p
let max_handlers = 16
let listen_backlog = 16

type budget = { peer_timeout_s : float }

let default_budget = { peer_timeout_s = 1.0 }

exception Error of string
exception Bundle of exn * exn

let err_endpoint_conflict = "endpoint-conflict"
let err_unsafe_runtime_dir = "unsafe-runtime-dir"
let err_inaccessible_runtime_dir = "inaccessible-runtime-dir"
let err_active_listener = "active-listener"
let err_stale_probe_failed = "stale-probe-failed"
let err_lock_conflict = "lock-conflict"
let err_lock_identity = "lock-identity"
let err_malformed_request = "malformed-request"
let combine_exn primary cleanup = Bundle (primary, cleanup)

let raise_primary primary cleanup_opt =
  match cleanup_opt with
  | None -> raise primary
  | Some cleanup -> raise (combine_exn primary cleanup)

type cleanup_acc = { mutable primary : exn option }

let acc_note acc exn =
  match acc.primary with
  | None -> acc.primary <- Some exn
  | Some previous -> acc.primary <- Some (combine_exn previous exn)

let acc_run acc f = try f () with exn -> acc_note acc exn
let acc_finish acc = match acc.primary with None -> () | Some exn -> raise exn

let snapshot_of_coordinator coordinator =
  let snap = Recording.Coordinator.snapshot coordinator in
  {
    Contract.phase = snap.phase;
    active = snap.active;
    mode = snap.mode;
    enabled = snap.enabled;
    started_at = snap.started_at;
    updated_at = Some snap.updated_at;
    transcript_path = snap.transcript_path;
    last_error = snap.last_error;
  }

let rejected_action coordinator action message =
  Contract.Action_resp
    {
      Contract.action;
      accepted = false;
      message = Some message;
      snapshot = snapshot_of_coordinator coordinator;
    }

let handle_request ?action_handler coordinator = function
  | Contract.Status ->
      Contract.Status_resp (snapshot_of_coordinator coordinator)
  | Contract.Pause ->
      Recording.Coordinator.set_enabled coordinator false;
      Contract.Pause_resp
  | Contract.Resume ->
      Recording.Coordinator.set_enabled coordinator true;
      Contract.Resume_resp
  | Contract.Toggle ->
      Contract.Toggle_resp (Recording.Coordinator.toggle_enabled coordinator)
  | Contract.Action action -> (
      match action_handler with
      | Some handler -> Contract.Action_resp (handler action)
      | None ->
          rejected_action coordinator action "daemon actions are unavailable")

let write_error_category flow category =
  let payload =
    Yojson.Safe.to_string (`Assoc [ ("error", `String category) ]) ^ "\n"
  in
  Eio.Flow.write flow [ Cstruct.of_string payload ]

let handle_peer ~timeout ?action_handler coordinator flow _addr =
  try
    Eio.Time.Timeout.run_exn timeout (fun () ->
        match
          try
            let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
            let line = Eio.Buf_read.line reader in
            match
              Yojson.Safe.from_string (String.trim line)
              |> Contract.request_of_json
            with
            | Ok request ->
                Ok
                  (Contract.response_to_json
                     (handle_request ?action_handler coordinator request))
            | Error _ -> Error err_malformed_request
          with
          | End_of_file -> Error "eof"
          | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> Error "eof"
          | Unix.Unix_error
              ((Unix.EPIPE | Unix.ECONNRESET | Unix.ECONNABORTED), _, _) ->
              Error "eof"
          | Eio.Buf_read.Buffer_limit_exceeded -> Error err_malformed_request
          | Yojson.Json_error _ -> Error err_malformed_request
        with
        | Error "eof" -> ()
        | Error category -> (
            try write_error_category flow category with
            | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> ()
            | Unix.Unix_error
                ((Unix.EPIPE | Unix.ECONNRESET | Unix.ECONNABORTED), _, _) ->
                ())
        | Ok json -> (
            try
              let payload = Yojson.Safe.to_string json ^ "\n" in
              Eio.Flow.write flow [ Cstruct.of_string payload ]
            with
            | Eio.Io (Eio.Net.E (Eio.Net.Connection_reset _), _) -> ()
            | Unix.Unix_error
                ((Unix.EPIPE | Unix.ECONNRESET | Unix.ECONNABORTED), _, _) ->
                ()))
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Eio.Time.Timeout -> ()

module Key = struct
  type t = { parent_dev : int; parent_ino : int; basename : string }

  let equal a b =
    a.parent_dev = b.parent_dev
    && a.parent_ino = b.parent_ino
    && String.equal a.basename b.basename

  let hash k = Hashtbl.hash (k.parent_dev, k.parent_ino, k.basename)
end

module Registry = struct
  module Table = Hashtbl.Make (Key)

  let mutex = Mutex.create ()
  let table : int Table.t = Table.create 8
  let next = Atomic.make 1

  let claim key =
    Mutex.lock mutex;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock mutex)
      (fun () ->
        if Table.mem table key then Result.Error err_lock_conflict
        else
          let token = Atomic.fetch_and_add next 1 in
          Table.add table key token;
          Result.Ok token)

  let release key token =
    Mutex.lock mutex;
    Fun.protect
      ~finally:(fun () -> Mutex.unlock mutex)
      (fun () ->
        match Table.find_opt table key with
        | Some current when current = token -> Table.remove table key
        | Some _ | None -> ())
end

let same_identity a b = a.Unix.st_dev = b.Unix.st_dev && a.st_ino = b.st_ino
let uid_is_ours stats = stats.Unix.st_uid = Unix.getuid ()
let group_or_other_writable stats = stats.Unix.st_perm land 0o022 <> 0

let rec ensure_private_dir dir =
  match Unix.lstat dir with
  | stats ->
      if stats.st_kind <> Unix.S_DIR then
        raise (Error err_inaccessible_runtime_dir);
      if (not (uid_is_ours stats)) || group_or_other_writable stats then
        raise (Error err_unsafe_runtime_dir)
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
      try Unix.mkdir dir 0o700 with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> ensure_private_dir dir
      | Unix.Unix_error ((Unix.EACCES | Unix.EPERM | Unix.ENOTDIR), _, _) ->
          raise (Error err_inaccessible_runtime_dir))
  | exception Unix.Unix_error ((Unix.EACCES | Unix.EPERM), _, _) ->
      raise (Error err_inaccessible_runtime_dir)

let endpoint_key socket_path =
  let parent = Filename.dirname socket_path in
  let stats = Unix.lstat parent in
  if stats.Unix.st_kind <> Unix.S_DIR then
    raise (Error err_inaccessible_runtime_dir);
  {
    Key.parent_dev = stats.st_dev;
    parent_ino = stats.st_ino;
    basename = Filename.basename socket_path;
  }

let lock_path socket_path = socket_path ^ ".lock"

let close_fd_preserving primary fd =
  match Unix.close fd with
  | () -> ( match primary with None -> () | Some exn -> raise exn)
  | exception close_exn -> (
      match primary with
      | None -> raise close_exn
      | Some exn -> raise (combine_exn exn close_exn))

let close_and_reraise exn fd =
  match Unix.close fd with
  | () -> raise exn
  | exception close_exn -> raise (combine_exn exn close_exn)

let acquire_lock socket_path =
  (* No O_NOFOLLOW in this Unix.open_flag set. lstat/fstat identity rejects a
     raced replacement before lock/use; it is not a kernel nofollow guarantee
     against hostile same-uid namespace mutation. Assumes an owned
     non-group-writable directory and cooperating processes. Existing
     symlink/non-regular/foreign-uid/insecure-mode/hardlinked lockfiles are
     rejected and left in place. The lockfile is never unlinked. *)
  let path = lock_path socket_path in
  let own_fd fd =
    let opened = Unix.fstat fd in
    if
      opened.st_kind <> Unix.S_REG
      || (not (uid_is_ours opened))
      || group_or_other_writable opened
      || opened.st_nlink <> 1
    then raise (Error err_lock_identity);
    opened
  in
  let open_existing stats =
    if stats.Unix.st_kind = Unix.S_LNK then raise (Error err_lock_identity);
    if stats.st_kind <> Unix.S_REG then raise (Error err_lock_identity);
    if not (uid_is_ours stats) then raise (Error err_lock_identity);
    if group_or_other_writable stats then raise (Error err_lock_identity);
    if stats.st_nlink <> 1 then raise (Error err_lock_identity);
    let fd = Unix.openfile path [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0o600 in
    match own_fd fd with
    | opened when same_identity stats opened -> fd
    | _ -> close_and_reraise (Error err_lock_identity) fd
    | exception exn -> close_and_reraise exn fd
  in
  let fd =
    match Unix.lstat path with
    | stats -> open_existing stats
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
        try
          let fd =
            Unix.openfile path
              [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ]
              0o600
          in
          match own_fd fd with
          | _ -> fd
          | exception exn -> close_and_reraise exn fd
        with Unix.Unix_error (Unix.EEXIST, _, _) -> (
          match Unix.lstat path with
          | stats -> open_existing stats
          | exception exn -> raise exn))
  in
  match Unix.lockf fd Unix.F_TLOCK 0 with
  | () -> fd
  | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES), _, _) ->
      close_and_reraise (Error err_lock_conflict) fd
  | exception exn -> close_and_reraise exn fd

type probe = Live | Stale | Missing

let probe_listener socket_path =
  let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  match
    try
      Unix.set_nonblock fd;
      Unix.connect fd (Unix.ADDR_UNIX socket_path);
      Live
    with
    | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> Stale
    | Unix.Unix_error (Unix.ENOENT, _, _) -> Missing
    | Unix.Unix_error ((Unix.EAGAIN | Unix.EINPROGRESS), _, _) ->
        raise (Error err_stale_probe_failed)
    | Unix.Unix_error _ -> raise (Error err_stale_probe_failed)
  with
  | result ->
      close_fd_preserving None fd;
      result
  | exception exn -> close_and_reraise exn fd

let unlink_if_owned socket_path dev ino =
  match Unix.lstat socket_path with
  | { st_kind = Unix.S_REG | Unix.S_LNK; _ } -> ()
  | stats when stats.st_dev = dev && stats.st_ino = ino && uid_is_ours stats ->
      Unix.unlink socket_path
  | _ -> ()
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let recover_stale socket_path =
  match Unix.lstat socket_path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | stats -> (
      if stats.st_kind <> Unix.S_SOCK then raise (Error err_endpoint_conflict);
      if not (uid_is_ours stats) then raise (Error err_endpoint_conflict);
      match probe_listener socket_path with
      | Live -> raise (Error err_active_listener)
      | Missing -> ()
      | Stale -> (
          match Unix.lstat socket_path with
          | again
            when again.st_kind = Unix.S_SOCK
                 && same_identity stats again && uid_is_ours again ->
              Unix.unlink socket_path
          | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
          | _ -> raise (Error err_endpoint_conflict)))

let start ~sw ~net:_ ~mono_clock ?(socket_path = Paths.daemon_socket_path)
    ?(budget = default_budget) ?action_handler coordinator =
  ensure_private_dir (Filename.dirname socket_path);
  let key = endpoint_key socket_path in
  let token =
    match Registry.claim key with
    | Result.Ok token -> token
    | Result.Error category -> raise (Error category)
  in
  let lock_fd_opt = ref None in
  let socket_id = ref None in
  let fd_owned = ref None in
  let released = ref false in
  let retire () =
    if not !released then (
      released := true;
      let acc = { primary = None } in
      acc_run acc (fun () ->
          match !fd_owned with
          | Some fd ->
              fd_owned := None;
              Unix.close fd
          | None -> ());
      acc_run acc (fun () ->
          match !socket_id with
          | Some (dev, ino) -> unlink_if_owned socket_path dev ino
          | None -> ());
      acc_run acc (fun () ->
          match !lock_fd_opt with
          | Some fd ->
              lock_fd_opt := None;
              Unix.close fd
          | None -> ());
      acc_run acc (fun () -> Registry.release key token);
      acc_finish acc)
  in
  (try Eio.Switch.on_release sw retire
   with exn ->
     raise_primary exn
       (try
          retire ();
          None
        with c -> Some c));
  try
    let lock_fd = acquire_lock socket_path in
    lock_fd_opt := Some lock_fd;
    recover_stale socket_path;
    let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    fd_owned := Some fd;
    Unix.bind fd (Unix.ADDR_UNIX socket_path);
    let stats = Unix.lstat socket_path in
    if stats.st_kind <> Unix.S_SOCK || not (uid_is_ours stats) then
      raise (Error err_endpoint_conflict);
    socket_id := Some (stats.st_dev, stats.st_ino);
    Unix.listen fd listen_backlog;
    Unix.chmod socket_path 0o600;
    Eio.Switch.check sw;
    (* Backend (eio_linux/eio_posix): Import_socket_listening calls Fd.of_unix
       first, which wraps [fd] and registers Switch close. Relinquish the raw
       owner at that consumption boundary so retire will not close it again. *)
    fd_owned := None;
    let listening =
      Eio_unix.Net.import_socket_listening ~sw ~close_unix:true fd
    in
    let slots = Eio.Semaphore.make max_handlers in
    let timeout = Eio.Time.Timeout.seconds mono_clock budget.peer_timeout_s in
    Eio.Fiber.fork_daemon ~sw (fun () ->
        try
          while true do
            Eio.Semaphore.acquire slots;
            let flow, addr =
              try Eio.Net.accept ~sw listening
              with exn ->
                Eio.Semaphore.release slots;
                raise exn
            in
            let () =
              Eio.Fiber.fork ~sw (fun () ->
                  let work_exn = ref None in
                  (try
                     handle_peer ~timeout ?action_handler coordinator flow addr
                   with exn -> work_exn := Some exn);
                  Eio.Cancel.protect (fun () ->
                      let close_exn = ref None in
                      (try Eio.Flow.close flow
                       with exn -> close_exn := Some exn);
                      Eio.Semaphore.release slots;
                      match (!work_exn, !close_exn) with
                      | None, None -> ()
                      | Some w, None -> raise w
                      | None, Some c -> raise c
                      | Some w, Some c -> raise (combine_exn w c)))
            in
            ()
          done
        with
        | Eio.Cancel.Cancelled _ -> `Stop_daemon
        | exn ->
            Eio.Switch.fail sw exn;
            `Stop_daemon)
  with exn -> (
    match
      try
        retire ();
        None
      with c -> Some c
    with
    | None -> raise exn
    | Some cleanup -> raise_primary exn (Some cleanup))
