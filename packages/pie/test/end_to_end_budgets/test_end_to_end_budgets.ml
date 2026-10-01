let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

(* PIE013 private e2e: one long-lived Service.run_with_inputs workflow over a
   private socket, retry store and synthetic audio; no provider, no keyboard, no
   microphone, no live control socket.
   Capture overflow accepted in assistant_owner test_capture_budget_sets_last_error.
   Direct style: [Ox.Scope.run] owns the boundary, the workflow is the background
   of [Ox.Concurrent.with_background], the audio fixture is a private
   [Eio.Stream] behind the service's pull, and every wait is bounded.
   ^pienext-4ps6 ^pienext-bee9 ^pienext-b3x8 *)

let transcribe_keysym = 65478

let base_config () : Pie.Config.Stt.t =
  {
    provider = Pie.Config.Stt.Openrouter;
    transcription_model = "openrouter/e2e";
    translation_model = "openrouter/e2e";
    transcription_language = "English";
    translation_source_language = "English";
    translation_target_language = "English";
    ptt_release_grace_ms = 0;
    ptt_min_capture_rms = 0.0;
    ptt_min_capture_peak = 0.0;
    ptt_transcribe_keysym = transcribe_keysym;
    ptt_translate_keysym = 65479;
    ptt_transcribe_shift = false;
    ptt_translate_shift = false;
    thinking_effort = None;
    newline_rules = [];
    text_entry_rules = [];
    prompt_rules = [];
    pipeline = Two_pass;
    openrouter_provider_order = None;
    one_pass_timeout_seconds = 30.;
    streaming = false;
    correction = None;
    (* This plateau counts fds at attempt idle. Archive encode runs after
       settlement and holds ffmpeg pipes and a pidfd only while it is in
       flight; those close when the encode finishes. Retention 0 keeps that
       background work out of the attempt budget. *)
    audio_retention_hours = 0.;
    bindings =
      [
        {
          keysym = transcribe_keysym;
          shift = false;
          mode = Pie.Config.Stt.Transcribe;
          correct = false;
          prompt_named = false;
          prompt_file = "correction.md";
          prompt = "{{glossary}}";
        };
        {
          keysym = 65479;
          shift = false;
          mode = Pie.Config.Stt.Translate;
          correct = false;
          prompt_named = false;
          prompt_file = "correction-translate.md";
          prompt = "{{glossary}} {{source_language}} {{target_language}}";
        };
      ];
  }

let loud_pcm =
  let b = Bytes.create 640 in
  for i = 0 to 319 do
    Bytes.set_int16_le b (i * 2) 20000
  done;
  b

let key ~released : Pie.Keyboard.Monitor.key_event =
  { released; state = 0; keysym = transcribe_keysym; unichar = 0; keycode = 75 }

let wait_until ?(steps = 80) ~clock pred =
  let rec loop n =
    if pred () then ()
    else if n <= 0 then Alcotest.fail "e2e-deadline"
    else (
      Eio.Time.sleep clock 0.05;
      loop (n - 1))
  in
  loop steps

let pp_run_error fmt : Pie.Ptt_loop.run_error -> unit = function
  | `Pulse_error error ->
      Format.pp_print_string fmt (Pie.Pulse.Client.error_message error)
  | `Ptt_error msg -> Format.pp_print_string fmt msg
  | `Ptt_cause cause ->
      Ox.Cause.pp
        (fun fmt (`Ptt_error msg) -> Format.pp_print_string fmt msg)
        fmt cause

let fail_cause cause =
  Alcotest.failf "workflow failed: %a" (Ox.Cause.pp pp_run_error) cause

let fail_error error = Alcotest.failf "workflow failed: %a" pp_run_error error

let test_logger env =
  Ox_observability.Logger.create ~service:"pie-e2e-test"
    ~clock:(Eio.Stdenv.clock env)
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~reporter:(Ox_observability.Logger.Reporter.records (fun _ -> ()))
    ()

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      Array.iter
        (fun n -> if n <> "." && n <> ".." then rm_rf (Filename.concat path n))
        (try Sys.readdir path with Sys_error _ -> [||]);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let with_dir f =
  let dir = Printf.sprintf "/tmp/pie-e2e-%d" (Unix.getpid ()) in
  rm_rf dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

type audio_cmd = Chunk of bytes | Fail of Pie.Pulse.Client.error

(* The service's live pull over the fixture stream: [Ok (Some bytes)] and [Error]
   stay distinct, and the fixture never closes the queue. *)
let make_audio () =
  let cmd = Eio.Stream.create 16 in
  let source () =
    match Eio.Stream.take cmd with
    | Chunk bytes -> Ok (Some bytes)
    | Fail error -> Error (`Pulse_error error)
  in
  (cmd, source)

let offer_pcm cmd = Eio.Stream.add cmd (Chunk loud_pcm)

let fd_count () =
  let dir = Unix.opendir "/proc/self/fd" in
  Fun.protect
    ~finally:(fun () -> Unix.closedir dir)
    (fun () ->
      let rec loop n =
        match Unix.readdir dir with
        | exception End_of_file -> n
        | "." | ".." -> loop n
        | _ -> loop (n + 1)
      in
      loop 0)

let slot_names retry_dir =
  let active = Filename.concat retry_dir "active" in
  try
    Sys.readdir active |> Array.to_list
    |> List.filter (fun n -> String.length n >= 5 && String.sub n 0 5 = "slot-")
  with Sys_error _ -> []

let draft_count retry_dir =
  slot_names retry_dir
  |> List.filter (fun n ->
      let slot = Filename.concat (Filename.concat retry_dir "active") n in
      Sys.file_exists (Filename.concat slot "draft.pcm"))
  |> List.length

let peek_meta retry_dir =
  match Pie.Recording.Retry_store.Active.open_store ~caps ~root:retry_dir with
  | Error _ -> None
  | Ok store -> (
      match Pie.Recording.Retry_store.Active.peek store with
      | Ok v -> v
      | Error _ -> None)

let peek_kind retry_dir =
  match peek_meta retry_dir with
  | Some (Pie.Recording.Retry_store.Active.Audio _) -> Some "audio"
  | Some (Pie.Recording.Retry_store.Active.Text { dispatch_uncertain; _ }) ->
      Some (if dispatch_uncertain then "text-uncertain" else "text")
  | None -> None

let payload_bytes retry_dir =
  match peek_meta retry_dir with
  | Some (Pie.Recording.Retry_store.Active.Audio { pcm_bytes; _ }) -> pcm_bytes
  | Some (Pie.Recording.Retry_store.Active.Text { utf8_bytes; _ }) -> utf8_bytes
  | None -> 0

let rpc ~net ~sw socket_path body =
  let socket = Eio.Net.connect ~sw net (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> Eio.Flow.close socket)
    (fun () ->
      Eio.Flow.copy_string (body ^ "\n") socket;
      Eio.Buf_read.of_flow ~max_size:65536 socket |> Eio.Buf_read.line)

let status_json = {|{"method":"Status"}|}
let action_pause = {|{"method":"Action","action":"pause"}|}
let action_resume = {|{"method":"Action","action":"resume"}|}
let action_retry = {|{"method":"Action","action":"retry"}|}

let status_active ~net ~sw socket_path =
  let line = rpc ~net ~sw socket_path status_json in
  match Yojson.Safe.from_string line with
  | `Assoc fields -> (
      match List.assoc_opt "active" fields with
      | Some (`Bool b) -> b
      | _ -> false)
  | _ -> false

let idle ~net ~sw socket_path =
  match Yojson.Safe.from_string (rpc ~net ~sw socket_path status_json) with
  | `Assoc fields ->
      List.assoc_opt "active" fields = Some (`Bool false)
      && List.assoc_opt "processing" fields = Some (`Int 0)
  | _ -> false

let owner_workflow ~logger ~env ~sw ~net ~process_mgr ~mono_clock ~dir
    ~config_control ~stt ~deliver ~keyboard ~audio =
  let socket_path = Filename.concat dir "c.sock" in
  let retry_dir = Filename.concat dir "r" in
  (try Unix.mkdir retry_dir 0o700
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let inputs =
    { Pie.Assistant.Service.keyboard; audio; source_name = Some "e2e" }
  in
  let workflow () =
    Pie_test_runtime.Service.run_with_inputs ~caps ~sw ~net
      ~fs:(Eio.Stdenv.fs env) ~clock:(Eio.Stdenv.clock env) ~logger ~process_mgr
      ~mono_clock ~inputs ~config_control ~stt ~deliver ~socket_path ~retry_dir
      ~min_duration_ms:0 ()
  in
  (socket_path, retry_dir, workflow)

exception Workflow_failed of Pie.Ptt_loop.run_error Ox.Cause.t

let register_printer = Printexc.register_printer [@alert "-unsafe_multidomain"]

let () =
  register_printer (function
    | Workflow_failed cause ->
        Some
          (Format.asprintf "Workflow_failed: %a" (Ox.Cause.pp pp_run_error)
             cause)
    | _ -> None)

(* The scope boundary: an interrupt-only or otherwise failed scope is not success. *)
let require_success (exit : (unit, Pie.Ptt_loop.run_error) Ox.Exit.t) =
  match exit with
  | Ox.Exit.Success () -> ()
  | Ox.Exit.Failure cause -> raise (Workflow_failed cause)

(* [Ox.Concurrent.with_background]: the long-lived workflow is the background, the
   assertions are the body, and the first side to finish stops the other. *)
let with_workflow_background ~sw workflow assertions =
  match
    Ox.Concurrent.with_background ~sw workflow (fun () ->
        assertions ();
        Ok ())
  with
  | Ok () -> ()
  | Error error -> fail_error error

let assert_attempts_complete ~completed ~finished =
  let n = Atomic.get completed in
  let done_ = Atomic.get finished in
  if n <> 100 || not done_ then
    raise
      (Workflow_failed
         (Ox.Cause.Fail
            (`Ptt_error
               (Printf.sprintf "incomplete attempts completed=%d finished=%b" n
                  done_))))

let test_interrupt_only_rejected_by_require_success () =
  match require_success (Ox.Exit.Failure (Ox.Cause.Interrupt None)) with
  | () -> Alcotest.fail "require_success accepted interrupt-only"
  | exception Workflow_failed cause ->
      if not (Ox.Cause.is_interrupt_only cause) then
        Alcotest.failf "expected interrupt-only cause, got %a"
          (Ox.Cause.pp pp_run_error) cause

let test_ok_with_completed_lt_100_fails () =
  let completed = Atomic.make 0 in
  let finished = Atomic.make false in
  require_success (Ox.Exit.Success ());
  match assert_attempts_complete ~completed ~finished with
  | () -> Alcotest.fail "Ok with completed<100 was accepted"
  | exception Workflow_failed (Ox.Cause.Fail (`Ptt_error _)) -> ()
  | exception Workflow_failed cause ->
      Alcotest.failf "expected Fail incomplete attempts, got %a"
        (Ox.Cause.pp pp_run_error) cause

type stt_mode = Stt_ok | Stt_fail
type deliver_mode = Deliver_ok | Deliver_fail

let assert_settled ~label ~retry_dir ~baseline_fd ~clock ~net ~sw ~socket_path =
  wait_until ~clock (fun () -> idle ~net ~sw socket_path);
  let slots = slot_names retry_dir in
  if List.length slots > 1 then
    Alcotest.failf "%s more than one slot: %d" label (List.length slots);
  if draft_count retry_dir <> 0 then
    Alcotest.failf "%s unpublished draft remains" label;
  let bytes = payload_bytes retry_dir in
  (match peek_kind retry_dir with
  | Some "audio" when bytes > Pie.Dictation_limits.capture_pcm_bytes ->
      Alcotest.failf "%s pcm payload %d exceeds capture cap" label bytes
  | (Some "text" | Some "text-uncertain")
    when bytes > Pie.Dictation_limits.result_utf8_bytes ->
      Alcotest.failf "%s text payload %d exceeds result cap" label bytes
  | _ -> ());
  let now = fd_count () in
  if now <> baseline_fd then
    Alcotest.failf "%s fd drifted baseline=%d now=%d" label baseline_fd now

let press_release ~keyboard ~audio_q =
  Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
  offer_pcm audio_q;
  Pie.Keyboard.Monitor.emit keyboard (key ~released:true)

let test_100_settled_attempts () =
  Pie_test_runtime.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  with_dir (fun dir ->
      let completed = Atomic.make 0 in
      let finished = Atomic.make false in
      require_success
        (match
           Ox.Timeout.timeout ~clock (Ox.Duration.seconds 600) (fun () ->
               Ok
                 (Ox.Scope.run (fun sw ->
                      let mono_clock = Eio.Stdenv.mono_clock env in
                      let net = Eio.Stdenv.net env in
                      let process_mgr = Eio.Stdenv.process_mgr env in
                      let keyboard = Pie.Keyboard.Monitor.create () in
                      let audio_q, audio = make_audio () in
                      let stamp () = [ Some (1, 1, 0, 0.) ] in
                      let load _ = (base_config (), stamp ()) in
                      let stt_mode = Atomic.make Stt_ok in
                      let deliver_mode = Atomic.make Deliver_ok in
                      let stt_n = Atomic.make 0 in
                      let deliver_n = Atomic.make 0 in
                      let stt _ _ _ =
                        Atomic.incr stt_n;
                        match Atomic.get stt_mode with
                        | Stt_ok -> Ok "e2e-text"
                        | Stt_fail -> Error (`Ptt_error "stt:unavailable")
                      in
                      let deliver _ _ _target _ =
                        Atomic.incr deliver_n;
                        match Atomic.get deliver_mode with
                        | Deliver_ok -> Ok Pie.History.Pasted
                        | Deliver_fail -> Error (`Ptt_error "inject:failed")
                      in
                      let socket_path, retry_dir, workflow =
                        owner_workflow ~logger:(test_logger env) ~env ~sw ~net
                          ~process_mgr ~mono_clock ~dir
                          ~config_control:{ stamp; load } ~stt ~deliver
                          ~keyboard ~audio
                      in
                      with_workflow_background ~sw workflow (fun () ->
                          wait_until ~clock (fun () ->
                              Sys.file_exists socket_path);
                          wait_until ~clock (fun () ->
                              try
                                ignore (rpc ~net ~sw socket_path status_json);
                                true
                              with _ -> false);
                          let baseline_fd = fd_count () in
                          let settle label =
                            assert_settled ~label ~retry_dir ~baseline_fd ~clock
                              ~net ~sw ~socket_path
                          in
                          for i = 1 to 100 do
                            let label = Printf.sprintf "op-%d" i in
                            (match i mod 5 with
                            | 1 ->
                                Atomic.set stt_mode Stt_fail;
                                Atomic.set deliver_mode Deliver_ok;
                                let n = Atomic.get stt_n in
                                press_release ~keyboard ~audio_q;
                                wait_until ~clock (fun () ->
                                    Atomic.get stt_n > n
                                    && peek_kind retry_dir = Some "audio");
                                settle label;
                                Alcotest.(check (option string))
                                  (label ^ " audio candidate")
                                  (Some "audio") (peek_kind retry_dir)
                            | 2 ->
                                Atomic.set stt_mode Stt_ok;
                                Atomic.set deliver_mode Deliver_fail;
                                let n = Atomic.get deliver_n in
                                press_release ~keyboard ~audio_q;
                                wait_until ~clock (fun () ->
                                    Atomic.get deliver_n > n
                                    &&
                                    match peek_kind retry_dir with
                                    | Some "text" | Some "text-uncertain" ->
                                        true
                                    | _ -> false);
                                settle label;
                                Alcotest.(check bool)
                                  (label ^ " one text candidate")
                                  true
                                  (match peek_kind retry_dir with
                                  | Some "text" | Some "text-uncertain" -> true
                                  | _ -> false)
                            | 3 ->
                                Atomic.set stt_mode Stt_ok;
                                Atomic.set deliver_mode Deliver_ok;
                                let n = Atomic.get deliver_n in
                                let before = peek_kind retry_dir in
                                press_release ~keyboard ~audio_q;
                                wait_until ~clock (fun () ->
                                    Atomic.get deliver_n > n);
                                settle label;
                                Alcotest.(check (option string))
                                  (label ^ " success does not clear older")
                                  before (peek_kind retry_dir)
                            | 4 ->
                                let before = peek_kind retry_dir in
                                Pie.Keyboard.Monitor.emit keyboard
                                  (key ~released:false);
                                offer_pcm audio_q;
                                ignore (rpc ~net ~sw socket_path action_pause);
                                wait_until ~clock (fun () ->
                                    idle ~net ~sw socket_path);
                                Pie.Keyboard.Monitor.emit keyboard
                                  (key ~released:true);
                                ignore (rpc ~net ~sw socket_path action_resume);
                                wait_until ~clock (fun () ->
                                    idle ~net ~sw socket_path);
                                settle label;
                                Alcotest.(check (option string))
                                  (label ^ " abort leaves older")
                                  before (peek_kind retry_dir)
                            | 0 ->
                                let cycle = i / 5 in
                                if cycle mod 2 = 0 then (
                                  Atomic.set deliver_mode Deliver_fail;
                                  let n = Atomic.get deliver_n in
                                  ignore (rpc ~net ~sw socket_path action_retry);
                                  wait_until ~clock (fun () ->
                                      Atomic.get deliver_n > n);
                                  settle label;
                                  Alcotest.(check bool)
                                    (label ^ " retry-fail keeps candidate")
                                    true
                                    (peek_kind retry_dir <> None))
                                else (
                                  Atomic.set deliver_mode Deliver_ok;
                                  Atomic.set stt_mode Stt_ok;
                                  ignore (rpc ~net ~sw socket_path action_retry);
                                  wait_until ~clock (fun () ->
                                      peek_kind retry_dir = None);
                                  settle label;
                                  Alcotest.(check (option string))
                                    (label ^ " retry-success clears selected")
                                    None (peek_kind retry_dir))
                            | _ -> Alcotest.failf "%s unreachable mod" label);
                            Atomic.incr completed
                          done;
                          Atomic.set finished true;
                          Printf.printf
                            "e2e_budgets 100 settled baseline_fd=%d end_fd=%d \
                             completed=%d\n\
                             %!"
                            baseline_fd (fd_count ()) (Atomic.get completed));
                      Ok ())))
         with
        | Ok exit -> exit
        | Error `Timeout -> Alcotest.fail "e2e plateau deadline");
      assert_attempts_complete ~completed ~finished)

module Active = Pie.Recording.Retry_store.Active

let action_ptt_start = {|{"method":"Action","action":"ptt-start"}|}

let assoc_of_json line =
  match Yojson.Safe.from_string line with `Assoc fields -> fields | _ -> []

let bool_field name fields =
  match List.assoc_opt name fields with Some (`Bool b) -> b | _ -> false

let snapshot_fields fields =
  match List.assoc_opt "snapshot" fields with
  | Some (`Assoc snap) -> snap
  | _ -> []

let status_last_error ~net ~sw socket_path =
  let fields = assoc_of_json (rpc ~net ~sw socket_path status_json) in
  match List.assoc_opt "lastError" fields with
  | Some (`String s) -> Some s
  | _ -> None

let peek_generation retry_dir =
  match peek_meta retry_dir with
  | Some (Active.Audio { generation; _ }) | Some (Active.Text { generation; _ })
    ->
      Some generation
  | None -> None

let seed_audio retry_dir =
  (try Unix.mkdir retry_dir 0o700
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  match Active.open_store ~caps ~root:retry_dir with
  | Error (`Retry_store msg) -> Alcotest.fail msg
  | Ok store -> (
      match
        Active.open_audio_writer store
          ~mode:Pie.Recording.Retry_store.Transcribe
      with
      | Error (`Retry_store msg) -> Alcotest.fail msg
      | Ok writer -> (
          (match Active.append_audio writer loud_pcm with
          | Error (`Retry_store msg) -> Alcotest.fail msg
          | Ok () -> ());
          (match Active.close_writer writer with
          | Error (`Retry_store msg) -> Alcotest.fail msg
          | Ok () -> ());
          match Active.publish_audio store writer with
          | Error (`Retry_store msg) -> Alcotest.fail msg
          | Ok generation -> generation))

let seed_text retry_dir text =
  (try Unix.mkdir retry_dir 0o700
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  match Active.open_store ~caps ~root:retry_dir with
  | Error (`Retry_store msg) -> Alcotest.fail msg
  | Ok store -> (
      match
        Active.publish_text store ~mode:Pie.Recording.Retry_store.Transcribe
          ~text ~dispatch_uncertain:false
      with
      | Error (`Retry_store msg) -> Alcotest.fail msg
      | Ok generation -> generation)

let publish_lock_path retry_dir =
  Filename.concat (Filename.concat retry_dir "active") "publish.lock"

type lock_save = Created_dir | Replaced of string

let install_lock_dir retry_dir =
  let path = publish_lock_path retry_dir in
  let saved = path ^ ".e2e-saved" in
  (try Unix.unlink saved with Unix.Unix_error _ -> ());
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
      Unix.mkdir path 0o700;
      Created_dir
  | { Unix.st_kind = Unix.S_REG; _ } ->
      Unix.rename path saved;
      Unix.mkdir path 0o700;
      Replaced saved
  | { Unix.st_kind = Unix.S_DIR; _ } -> Created_dir
  | _ ->
      Unix.unlink path;
      Unix.mkdir path 0o700;
      Created_dir

let restore_lock retry_dir save =
  let path = publish_lock_path retry_dir in
  (try Unix.rmdir path with Unix.Unix_error _ -> ());
  match save with
  | Created_dir -> ()
  | Replaced saved -> (
      try Unix.rename saved path with Unix.Unix_error _ -> ())

(* One workflow lifetime on its own scope: the assertions run while the workflow
   stays live on the scope's switch. Bounded, so a regression that never settles
   fails on the deadline instead of hanging the suite. *)
let run_e2e ?(on_stopped = fun () -> ()) ~dir ~stt ~deliver assertions =
  Pie_test_runtime.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  match
    Ox.Timeout.timeout ~clock (Ox.Duration.seconds 60) (fun () ->
        Ok
          (Ox.Scope.run (fun sw ->
               let mono_clock = Eio.Stdenv.mono_clock env in
               let net = Eio.Stdenv.net env in
               let process_mgr = Eio.Stdenv.process_mgr env in
               let keyboard = Pie.Keyboard.Monitor.create () in
               let _audio_q, audio = make_audio () in
               let stamp () = [ Some (1, 1, 0, 0.) ] in
               let load _ = (base_config (), stamp ()) in
               let socket_path, retry_dir, workflow =
                 owner_workflow ~logger:(test_logger env) ~env ~sw ~net
                   ~process_mgr ~mono_clock ~dir ~config_control:{ stamp; load }
                   ~stt ~deliver ~keyboard ~audio
               in
               with_workflow_background ~sw workflow (fun () ->
                   wait_until ~clock (fun () -> Sys.file_exists socket_path);
                   wait_until ~clock (fun () ->
                       try
                         ignore (rpc ~net ~sw socket_path status_json);
                         true
                       with _ -> false);
                   assertions ~clock ~net ~sw ~socket_path ~retry_dir);
               on_stopped ();
               Ok ())))
  with
  | Ok exit -> require_success exit
  | Error `Timeout -> Alcotest.fail "e2e workflow deadline"

(* Hold processing, not capture. Pause must leave this callback alive. A guard
   in each assertion releases the hold even if an assertion fails. *)
let held_callback cleanup_hold entered_u body =
  ignore (Eio.Promise.try_resolve entered_u ());
  Fun.protect
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () -> Eio.Promise.await cleanup_hold))
    (fun () ->
      Eio.Promise.await cleanup_hold;
      body ())

let test_audio_retry_held_cleanup_pause_busy () =
  with_dir (fun dir ->
      let retry_dir = Filename.concat dir "r" in
      ignore (seed_audio retry_dir);
      let stt_n = Atomic.make 0 and deliver_n = Atomic.make 0 in
      let cleanup_hold, cleanup_u = Eio.Promise.create () in
      let entered, entered_u = Eio.Promise.create () in
      let stt _ _ _ =
        Atomic.incr stt_n;
        held_callback cleanup_hold entered_u (fun () -> Ok "held")
      in
      let deliver _ _ target _ =
        Atomic.incr deliver_n;
        Alcotest.(check bool)
          "retained audio retry clipboard-only" true (target = None);
        Ok Pie.History.Clipboard
      in
      run_e2e ~dir ~stt ~deliver (fun ~clock ~net ~sw ~socket_path ~retry_dir ->
          Fun.protect
            ~finally:(fun () -> ignore (Eio.Promise.try_resolve cleanup_u ()))
            (fun () ->
              ignore (rpc ~net ~sw socket_path action_retry);
              wait_until ~clock (fun () ->
                  Atomic.get stt_n = 1 && Eio.Promise.is_resolved entered);
              ignore (rpc ~net ~sw socket_path action_pause);
              Alcotest.(check bool)
                "pause does not own retry processing" false
                (status_active ~net ~sw socket_path);
              ignore (rpc ~net ~sw socket_path action_resume);
              let start =
                assoc_of_json (rpc ~net ~sw socket_path action_ptt_start)
              in
              Alcotest.(check bool)
                "fresh capture admitted while retry processes" true
                (bool_field "accepted" start);
              ignore (rpc ~net ~sw socket_path action_pause);
              wait_until ~clock (fun () ->
                  not (status_active ~net ~sw socket_path));
              Alcotest.(check bool)
                "retry still processing" false
                (idle ~net ~sw socket_path);
              Alcotest.(check int)
                "retry delivery still held" 0 (Atomic.get deliver_n);
              ignore (Eio.Promise.try_resolve cleanup_u ());
              wait_until ~clock (fun () -> idle ~net ~sw socket_path);
              Alcotest.(check int)
                "pause did not cancel retry delivery" 1 (Atomic.get deliver_n);
              Alcotest.(check bool)
                "successful retry clears selected candidate" true
                (peek_generation retry_dir = None))))

let test_text_retry_held_delivery_cleanup_stt_zero () =
  with_dir (fun dir ->
      let retry_dir = Filename.concat dir "r" and text0 = "e2e-retained-text" in
      ignore (seed_text retry_dir text0);
      let stt_n = Atomic.make 0 and deliver_n = Atomic.make 0 in
      let cleanup_hold, cleanup_u = Eio.Promise.create () in
      let entered, entered_u = Eio.Promise.create () in
      let stt _ _ _ =
        Atomic.incr stt_n;
        Ok "should-not-run"
      in
      let deliver _ _ target text =
        Atomic.incr deliver_n;
        Alcotest.(check bool)
          "retained text retry clipboard-only" true (target = None);
        Alcotest.(check string) "exact retained text" text0 text;
        held_callback cleanup_hold entered_u (fun () ->
            Ok Pie.History.Clipboard)
      in
      run_e2e ~dir ~stt ~deliver (fun ~clock ~net ~sw ~socket_path ~retry_dir ->
          Fun.protect
            ~finally:(fun () -> ignore (Eio.Promise.try_resolve cleanup_u ()))
            (fun () ->
              ignore (rpc ~net ~sw socket_path action_retry);
              wait_until ~clock (fun () ->
                  Atomic.get deliver_n = 1 && Eio.Promise.is_resolved entered);
              Alcotest.(check int) "text retry skips STT" 0 (Atomic.get stt_n);
              ignore (rpc ~net ~sw socket_path action_pause);
              ignore (rpc ~net ~sw socket_path action_resume);
              let start =
                assoc_of_json (rpc ~net ~sw socket_path action_ptt_start)
              in
              Alcotest.(check bool)
                "fresh capture admitted while delivery held" true
                (bool_field "accepted" start);
              ignore (rpc ~net ~sw socket_path action_pause);
              wait_until ~clock (fun () ->
                  not (status_active ~net ~sw socket_path));
              Alcotest.(check bool)
                "delivery still processing" false
                (idle ~net ~sw socket_path);
              ignore (Eio.Promise.try_resolve cleanup_u ());
              wait_until ~clock (fun () -> idle ~net ~sw socket_path);
              Alcotest.(check int)
                "new capture cancelled before STT" 0 (Atomic.get stt_n);
              Alcotest.(check bool)
                "successful text retry clears candidate" true
                (peek_meta retry_dir = None))))

let test_queued_retry_processing_only check_capture =
  with_dir (fun dir ->
      ignore (seed_audio (Filename.concat dir "r"));
      let entered, entered_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let calls = Atomic.make 0 in
      let stt _ _ _ =
        Atomic.incr calls;
        ignore (Eio.Promise.try_resolve entered_u ());
        Eio.Promise.await never;
        Ok "unused"
      in
      let on_stopped () =
        let state =
          Yojson.Safe.from_file (Pie.Paths.recording_state_path ~caps)
          |> Yojson.Safe.Util.to_assoc
        in
        Alcotest.(check bool)
          "shutdown released all capture ownership" false
          (bool_field "active" state);
        Alcotest.(check bool)
          "shutdown released queued processing admission" true
          (List.assoc_opt "processing" state = Some (`Int 0))
      in
      run_e2e ~dir ~stt
        ~deliver:(fun _ _ _ _ -> Alcotest.fail "held retry must not deliver")
        ~on_stopped
        (fun ~clock ~net ~sw ~socket_path ~retry_dir:_ ->
          let first = assoc_of_json (rpc ~net ~sw socket_path action_retry) in
          Alcotest.(check bool)
            "first retry admitted" true
            (bool_field "accepted" first);
          wait_until ~clock (fun () -> Eio.Promise.is_resolved entered);
          let second = assoc_of_json (rpc ~net ~sw socket_path action_retry) in
          Alcotest.(check bool)
            "second retry admitted while first held" true
            (bool_field "accepted" second);
          Alcotest.(check int)
            "second retry remains queued" 1 (Atomic.get calls);
          if check_capture then (
            Alcotest.(check bool)
              "queued retry never owns capture" false
              (status_active ~net ~sw socket_path);
            let state = assoc_of_json (rpc ~net ~sw socket_path status_json) in
            Alcotest.(check bool)
              "both retries count as processing" true
              (List.assoc_opt "processing" state = Some (`Int 2));
            let start =
              assoc_of_json (rpc ~net ~sw socket_path action_ptt_start)
            in
            Alcotest.(check bool)
              "fresh microphone press admitted" true
              (bool_field "accepted" start);
            wait_until ~clock (fun () -> status_active ~net ~sw socket_path);
            ignore (rpc ~net ~sw socket_path action_pause);
            wait_until ~clock (fun () ->
                not (status_active ~net ~sw socket_path));
            Alcotest.(check int)
              "pause does not dequeue or cancel retries" 1 (Atomic.get calls);
            ignore (rpc ~net ~sw socket_path action_resume);
            for _ = 1 to 2 do
              let retry =
                assoc_of_json (rpc ~net ~sw socket_path action_retry)
              in
              Alcotest.(check bool)
                "queued retry reserves processing capacity" true
                (bool_field "accepted" retry)
            done;
            let full = assoc_of_json (rpc ~net ~sw socket_path status_json) in
            Alcotest.(check bool)
              "four queued/active retries fill the cap" true
              (List.assoc_opt "processing" full = Some (`Int 4));
            let fifth = assoc_of_json (rpc ~net ~sw socket_path action_retry) in
            Alcotest.(check bool)
              "fifth retry refused" false
              (bool_field "accepted" fifth);
            ignore (rpc ~net ~sw socket_path action_ptt_start);
            Eio.Fiber.yield ();
            Alcotest.(check bool)
              "full processing cap refuses microphone capture" false
              (status_active ~net ~sw socket_path))))

(* Both failures must survive: the delivery failure stays primary and the failed
   publication stays retained as the finalizer, neither replacing the other. *)
let rec suppressed_inject_and_publish
    (cause : [ `Ptt_error of string ] Ox.Cause.t) =
  match cause with
  | Ox.Cause.Suppressed
      { primary = Ox.Cause.Fail (`Ptt_error "inject:failed"); finalizer } ->
      Ox.Cause.failures finalizer <> []
  | Ox.Cause.Suppressed { primary; finalizer } ->
      suppressed_inject_and_publish primary
      || suppressed_inject_and_publish finalizer
  | Ox.Cause.Concurrent xs -> List.exists suppressed_inject_and_publish xs
  | Ox.Cause.Finalizer cause -> suppressed_inject_and_publish cause
  | Ox.Cause.Fail _ | Ox.Cause.Die _ | Ox.Cause.Interrupt _ -> false

let test_audio_retry_delivery_and_text_publish_both_causes () =
  with_dir (fun dir ->
      let retry_dir = Filename.concat dir "r" in
      let gen0 = seed_audio retry_dir in
      let stt_n = Atomic.make 0 in
      let deliver_n = Atomic.make 0 in
      let lock_save = ref Created_dir in
      let stt _ _ _ =
        Atomic.incr stt_n;
        Ok "recognized"
      in
      let deliver _ _ _target _ =
        Atomic.incr deliver_n;
        lock_save := install_lock_dir retry_dir;
        Error (`Ptt_error "inject:failed")
      in
      Fun.protect
        ~finally:(fun () -> restore_lock retry_dir !lock_save)
        (fun () ->
          Pie_test_runtime.run @@ fun env ->
          let clock = Eio.Stdenv.clock env in
          (* The workflow must report the compound failure: in v1 only an exact single
             [`Ptt_error] was a recoverable retry failure, while a [`Ptt_cause] had to
             reach this boundary. A deadline that fails the scope switch keeps a
             regression to a bounded test failure instead of a hang. *)
          let kept_running = ref false in
          let observed = ref "<no status>" in
          let exit =
            match
              Ox.Timeout.timeout ~clock (Ox.Duration.seconds 30) (fun () ->
                  Ok
                    (Ox.Scope.run (fun sw ->
                         let mono_clock = Eio.Stdenv.mono_clock env in
                         let net = Eio.Stdenv.net env in
                         let process_mgr = Eio.Stdenv.process_mgr env in
                         let keyboard = Pie.Keyboard.Monitor.create () in
                         let _audio_q, audio = make_audio () in
                         let stamp () = [ Some (1, 1, 0, 0.) ] in
                         let load _ = (base_config (), stamp ()) in
                         let memory =
                           Ox_observability.Logger.Memory.create ()
                         in
                         let logger =
                           Ox_observability.Logger.create
                             ~service:"pie-e2e-test" ~clock ~mono_clock
                             ~reporter:
                               (Ox_observability.Logger.Memory.reporter memory)
                             ()
                         in
                         let socket_path, retry_dir, workflow =
                           owner_workflow ~logger ~env ~sw ~net ~process_mgr
                             ~mono_clock ~dir ~config_control:{ stamp; load }
                             ~stt ~deliver ~keyboard ~audio
                         in
                         let got : Pie.Ptt_loop.run_error option Atomic.t =
                           Atomic.make None
                         in
                         Eio.Fiber.fork ~sw (fun () ->
                             match workflow () with
                             | Ok () -> ()
                             | Error error -> Atomic.set got (Some error));
                         wait_until ~clock (fun () ->
                             Sys.file_exists socket_path);
                         ignore (rpc ~net ~sw socket_path action_retry);
                         wait_until ~clock (fun () -> Atomic.get deliver_n >= 1);
                         let rec await_workflow n =
                           if Atomic.get got <> None then ()
                           else if n <= 0 then (
                             kept_running := true;
                             observed :=
                               String.concat "; "
                                 (List.map
                                    (fun (r : Ox_observability.Logger.record) ->
                                      r.msg)
                                    (Ox_observability.Logger.Memory.records
                                       memory));
                             Eio.Cancel.protect (fun () ->
                                 Eio.Switch.fail sw Exit))
                           else (
                             Eio.Time.sleep clock 0.05;
                             await_workflow (n - 1))
                         in
                         await_workflow 80;
                         (match Atomic.get got with
                         | None ->
                             Alcotest.fail "expected compound workflow failure"
                         | Some (`Ptt_cause cause)
                           when suppressed_inject_and_publish cause ->
                             ()
                         | Some error ->
                             Alcotest.failf
                               "expected inject:failed plus publish Fail, got \
                                %a"
                               pp_run_error error);
                         Alcotest.(check int) "stt ran" 1 (Atomic.get stt_n);
                         (match peek_kind retry_dir with
                         | Some "audio" -> ()
                         | other ->
                             Alcotest.failf
                               "expected audio retained after publish fail, \
                                got %s"
                               (Option.value ~default:"none" other));
                         (match peek_generation retry_dir with
                         | Some g when Active.generation_equal g gen0 -> ()
                         | _ -> Alcotest.fail "audio generation replaced");
                         Ok ())))
            with
            | Ok exit -> exit
            | Error `Timeout -> Alcotest.fail "e2e workflow deadline"
          in
          if !kept_running then
            Alcotest.failf
              "workflow kept running after the compound retry failure \
               (status=%s): the retry loop did not return the compound \
               Ptt_cause"
              !observed
          else require_success exit))

let test_delivery_success_clear_fail_preserves_candidate () =
  with_dir (fun dir ->
      let retry_dir = Filename.concat dir "r" in
      let gen0 = seed_audio retry_dir in
      let deliver_n = Atomic.make 0 in
      let lock_save = ref Created_dir in
      let stt _ _ _ = Ok "recognized" in
      let deliver _ _ _target _ =
        Atomic.incr deliver_n;
        lock_save := install_lock_dir retry_dir;
        Ok Pie.History.Pasted
      in
      Fun.protect
        ~finally:(fun () -> restore_lock retry_dir !lock_save)
        (fun () ->
          run_e2e ~dir ~stt ~deliver
            (fun ~clock ~net ~sw ~socket_path ~retry_dir ->
              ignore (rpc ~net ~sw socket_path action_retry);
              wait_until ~clock (fun () ->
                  Atomic.get deliver_n >= 1 && idle ~net ~sw socket_path);
              Alcotest.(check int) "one delivery" 1 (Atomic.get deliver_n);
              (match peek_generation retry_dir with
              | Some g when Active.generation_equal g gen0 -> ()
              | None -> Alcotest.fail "candidate cleared despite clear fail"
              | Some _ -> Alcotest.fail "candidate generation changed");
              match status_last_error ~net ~sw socket_path with
              | None -> Alcotest.fail "expected lastError after clear fail"
              | Some "" -> Alcotest.fail "empty lastError"
              | Some _ ->
                  Alcotest.(check int) "no auto-repeat" 1 (Atomic.get deliver_n))))

let () =
  Alcotest.run "end_to_end_budgets"
    [
      ( "strict",
        [
          Alcotest.test_case "require_success rejects interrupt-only" `Quick
            test_interrupt_only_rejected_by_require_success;
          Alcotest.test_case "Ok with completed<100 fails after helper" `Quick
            test_ok_with_completed_lt_100_fails;
        ] );
      ( "edges",
        [
          Alcotest.test_case
            "dict-q7we audio retry survives pause; fresh capture admitted"
            `Quick test_audio_retry_held_cleanup_pause_busy;
          Alcotest.test_case "text retry held delivery STT stays 0" `Quick
            test_text_retry_held_delivery_cleanup_stt_zero;
          Alcotest.test_case "audio retry delivery+publish fail keeps audio"
            `Quick test_audio_retry_delivery_and_text_publish_both_causes;
          Alcotest.test_case "dict-q7we queued retry never owns capture" `Quick
            (fun () -> test_queued_retry_processing_only true);
          Alcotest.test_case "dict-q7we shutdown releases queued retry" `Quick
            (fun () -> test_queued_retry_processing_only false);
          Alcotest.test_case "delivery success clear fail preserves candidate"
            `Quick test_delivery_success_clear_fail_preserves_candidate;
        ] );
      ( "plateau",
        [
          Alcotest.test_case
            "100 settled attempts one workflow retained-store+fd" `Slow
            test_100_settled_attempts;
        ] );
    ]
