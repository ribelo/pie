(* PIE013 private e2e: one long-lived Service.run_with_inputs workflow. *)
(* Capture overflow accepted in assistant_owner test_capture_budget_sets_last_error. *)

let transcribe_keysym = 65478

let base_config () : Pie.Config.Stt.t =
  {
    provider = Pie.Config.Stt.Openrouter;
    transcription_model = "openrouter/e2e";
    translation_model = "openrouter/e2e";
    transcription_language = "English";
    translation_source_language = "English";
    translation_target_language = "English";
    ptt_min_capture_rms = 0.0;
    ptt_min_capture_peak = 0.0;
    ptt_transcribe_keysym = transcribe_keysym;
    ptt_translate_keysym = 65479;
    ptt_transcribe_shift = false;
    ptt_translate_shift = false;
    thinking_effort = None;
    transcription_prompt = "Transcribe in {{language}}.";
    translation_prompt =
      "Translate from {{source_language}} to {{target_language}}.";
    newline_rules = [];
    text_entry_rules = [];
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

let pp_run_error fmt = function
  | `Pulse_error error ->
      Format.pp_print_string fmt (Pie.Pulse.Client.error_message error)
  | `Ptt_error msg -> Format.pp_print_string fmt msg

let fail_cause cause =
  Alcotest.failf "workflow failed: %a" (Eta.Cause.pp pp_run_error) cause

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      Array.iter
        (fun n -> if n <> "." && n <> ".." then rm_rf (Filename.concat path n))
        (try Sys.readdir path with Sys_error _ -> [||]);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> (try Unix.unlink path with Unix.Unix_error _ -> ())

let with_dir f =
  let dir = Printf.sprintf "/tmp/pie-e2e-%d" (Unix.getpid ()) in
  rm_rf dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

type audio_cmd = Chunk of bytes | Fail of Pie.Pulse.Client.error

let make_audio () =
  let cmd = Eio.Stream.create 16 in
  let rec source () =
    Eta_stream.Stream.from_effect
      (Eta.Effect.sync (fun () -> Eio.Stream.take cmd)
      |> Eta.Effect.bind (function
           | Chunk bytes -> Eta.Effect.pure bytes
           | Fail error -> Eta.Effect.fail (`Pulse_error error)))
    |> Eta_stream.Stream.flat_map (fun chunk ->
           Eta_stream.Stream.concat (Eta_stream.Stream.succeed chunk) (source ()))
  in
  (cmd, source ())

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
  match Pie.Recording.Retry_store.Active.open_store ~root:retry_dir with
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

let idle ~net ~sw socket_path = not (status_active ~net ~sw socket_path)

let owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir ~config_control ~stt
    ~deliver ~keyboard ~audio =
  let socket_path = Filename.concat dir "c.sock" in
  let retry_dir = Filename.concat dir "r" in
  (try Unix.mkdir retry_dir 0o700
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let inputs =
    { Pie.Assistant.Service.keyboard; audio; source_name = Some "e2e" }
  in
  let workflow =
    Pie.Assistant.Service.run_with_inputs ~sw ~net ~process_mgr ~mono_clock
      ~inputs ~config_control ~stt ~deliver ~socket_path ~retry_dir
      ~min_duration_ms:0 ()
  in
  (socket_path, retry_dir, workflow)

type e2e_error =
  [ `Pulse_error of Pie.Pulse.Client.error | `Ptt_error of string ]

exception Workflow_failed of e2e_error Eta.Cause.t

let register_printer = (Printexc.register_printer [@alert "-unsafe_multidomain"])

let () =
  register_printer (function
    | Workflow_failed cause ->
        Some
          (Format.asprintf "Workflow_failed: %a" (Eta.Cause.pp pp_run_error)
             cause)
    | _ -> None)

let require_success (exit : (unit, e2e_error) Eta.Exit.t) =
  match exit with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error cause -> raise (Workflow_failed cause)

let run_workflow_background ~sw ~clock workflow assertions =
  Eta_eio.run_once ~sw ~clock
    (Eta.Effect.with_background ~name:"pie.e2e.workflow" workflow (fun () ->
         Eta.Effect.sync assertions))

let with_workflow_background ~sw ~clock workflow assertions =
  require_success (run_workflow_background ~sw ~clock workflow assertions)

let assert_attempts_complete ~completed ~finished =
  let n = Atomic.get completed in
  let done_ = Atomic.get finished in
  if n <> 100 || not done_ then
    raise
      (Workflow_failed
         (Eta.Cause.fail
            (`Ptt_error
               (Printf.sprintf "incomplete attempts completed=%d finished=%b" n
                  done_))))

let test_interrupt_only_rejected_by_require_success () =
  match require_success (Eta.Exit.Error Eta.Cause.interrupt) with
  | () -> Alcotest.fail "require_success accepted interrupt-only"
  | exception Workflow_failed cause ->
      if not (Eta.Cause.is_interrupt_only cause) then
        Alcotest.failf "expected interrupt-only cause, got %a"
          (Eta.Cause.pp pp_run_error) cause

let test_ok_with_completed_lt_100_fails () =
  let completed = Atomic.make 0 in
  let finished = Atomic.make false in
  require_success (Eta.Exit.Ok ());
  match assert_attempts_complete ~completed ~finished with
  | () -> Alcotest.fail "Ok with completed<100 was accepted"
  | exception Workflow_failed (Eta.Cause.Fail (`Ptt_error _)) -> ()
  | exception Workflow_failed cause ->
      Alcotest.failf "expected Fail incomplete attempts, got %a"
        (Eta.Cause.pp pp_run_error) cause

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
  | Some "text" | Some "text-uncertain"
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
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
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
        | Stt_ok -> Eta.Effect.pure "e2e-text"
        | Stt_fail -> Eta.Effect.fail (`Ptt_error "stt:unavailable")
      in
      let deliver _ _ _ =
        Atomic.incr deliver_n;
        match Atomic.get deliver_mode with
        | Deliver_ok -> Eta.Effect.unit
        | Deliver_fail -> Eta.Effect.fail (`Ptt_error "inject:failed")
      in
      let socket_path, retry_dir, workflow =
        owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
          ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
      in
      let completed = Atomic.make 0 in
      let finished = Atomic.make false in
      with_workflow_background ~sw ~clock workflow (fun () ->
          wait_until ~clock (fun () -> Sys.file_exists socket_path);
          wait_until ~clock (fun () ->
              try
                ignore (rpc ~net ~sw socket_path status_json);
                true
              with _ -> false);
          let baseline_fd = fd_count () in
          let settle label =
            assert_settled ~label ~retry_dir ~baseline_fd ~clock ~net ~sw
              ~socket_path
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
                    Atomic.get stt_n > n && peek_kind retry_dir = Some "audio");
                settle label;
                Alcotest.(check (option string))
                  (label ^ " audio candidate") (Some "audio")
                  (peek_kind retry_dir)
            | 2 ->
                Atomic.set stt_mode Stt_ok;
                Atomic.set deliver_mode Deliver_fail;
                let n = Atomic.get deliver_n in
                press_release ~keyboard ~audio_q;
                wait_until ~clock (fun () ->
                    Atomic.get deliver_n > n
                    &&
                    match peek_kind retry_dir with
                    | Some "text" | Some "text-uncertain" -> true
                    | _ -> false);
                settle label;
                Alcotest.(check bool)
                  (label ^ " one text candidate") true
                  (match peek_kind retry_dir with
                  | Some "text" | Some "text-uncertain" -> true
                  | _ -> false)
            | 3 ->
                Atomic.set stt_mode Stt_ok;
                Atomic.set deliver_mode Deliver_ok;
                let n = Atomic.get deliver_n in
                let before = peek_kind retry_dir in
                press_release ~keyboard ~audio_q;
                wait_until ~clock (fun () -> Atomic.get deliver_n > n);
                settle label;
                Alcotest.(check (option string))
                  (label ^ " success does not clear older") before
                  (peek_kind retry_dir)
            | 4 ->
                let before = peek_kind retry_dir in
                Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
                offer_pcm audio_q;
                ignore (rpc ~net ~sw socket_path action_pause);
                wait_until ~clock (fun () -> idle ~net ~sw socket_path);
                Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
                ignore (rpc ~net ~sw socket_path action_resume);
                wait_until ~clock (fun () -> idle ~net ~sw socket_path);
                settle label;
                Alcotest.(check (option string))
                  (label ^ " abort leaves older") before (peek_kind retry_dir)
            | 0 ->
                let cycle = i / 5 in
                if cycle mod 2 = 0 then (
                  Atomic.set deliver_mode Deliver_fail;
                  let n = Atomic.get deliver_n in
                  ignore (rpc ~net ~sw socket_path action_retry);
                  wait_until ~clock (fun () -> Atomic.get deliver_n > n);
                  settle label;
                  Alcotest.(check bool)
                    (label ^ " retry-fail keeps candidate") true
                    (peek_kind retry_dir <> None))
                else (
                  Atomic.set deliver_mode Deliver_ok;
                  Atomic.set stt_mode Stt_ok;
                  ignore (rpc ~net ~sw socket_path action_retry);
                  wait_until ~clock (fun () -> peek_kind retry_dir = None);
                  settle label;
                  Alcotest.(check (option string))
                    (label ^ " retry-success clears selected") None
                    (peek_kind retry_dir))
            | _ -> Alcotest.failf "%s unreachable mod" label);
            Atomic.incr completed
          done;
          Atomic.set finished true;
          Printf.printf "e2e_budgets 100 settled baseline_fd=%d end_fd=%d completed=%d\n%!"
            baseline_fd (fd_count ()) (Atomic.get completed));
      assert_attempts_complete ~completed ~finished)

module Active = Pie.Recording.Retry_store.Active

let action_ptt_start = {|{"method":"Action","action":"ptt-start"}|}

let assoc_of_json line =
  match Yojson.Safe.from_string line with
  | `Assoc fields -> fields
  | _ -> []

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
  match Active.open_store ~root:retry_dir with
  | Error (`Retry_store msg) -> Alcotest.fail msg
  | Ok store -> (
      match Active.open_audio_writer store ~mode:Pie.Recording.Retry_store.Transcribe
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
  match Active.open_store ~root:retry_dir with
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
  | Replaced saved -> (try Unix.rename saved path with Unix.Unix_error _ -> ())

let run_e2e ~dir ~stt ~deliver assertions =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let mono_clock = Eio.Stdenv.mono_clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let _audio_q, audio = make_audio () in
  let stamp () = [ Some (1, 1, 0, 0.) ] in
  let load _ = (base_config (), stamp ()) in
  let socket_path, retry_dir, workflow =
    owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
      ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
  in
  with_workflow_background ~sw ~clock workflow (fun () ->
      wait_until ~clock (fun () -> Sys.file_exists socket_path);
      wait_until ~clock (fun () ->
          try
            ignore (rpc ~net ~sw socket_path status_json);
            true
          with _ -> false);
      assertions ~clock ~net ~sw ~socket_path ~retry_dir)

let test_audio_retry_held_cleanup_pause_busy () =
  with_dir (fun dir ->
      let retry_dir = Filename.concat dir "r" in
      let gen0 = seed_audio retry_dir in
      let stt_n = Atomic.make 0 in
      let deliver_n = Atomic.make 0 in
      let cleanup_hold, cleanup_u = Eio.Promise.create () in
      let entered, entered_u = Eio.Promise.create () in
      let stt _ _ _ =
        Atomic.incr stt_n;
        ignore (Eio.Promise.try_resolve entered_u ());
        let hang, _ = Eio.Promise.create () in
        Eta.Effect.sync (fun () -> Eio.Promise.await hang)
        |> Eta.Effect.finally
             (Eta.Effect.sync (fun () -> Eio.Promise.await cleanup_hold))
      in
      let deliver _ _ _ =
        Atomic.incr deliver_n;
        Eta.Effect.unit
      in
      Fun.protect
        ~finally:(fun () -> ignore (Eio.Promise.try_resolve cleanup_u ()))
        (fun () ->
          run_e2e ~dir ~stt ~deliver (fun ~clock ~net ~sw ~socket_path ~retry_dir ->
              ignore (rpc ~net ~sw socket_path action_retry);
              wait_until ~clock (fun () ->
                  Atomic.get stt_n >= 1 && Eio.Promise.is_resolved entered);
              ignore (rpc ~net ~sw socket_path action_pause);
              ignore (rpc ~net ~sw socket_path action_resume);
              let start =
                assoc_of_json (rpc ~net ~sw socket_path action_ptt_start)
              in
              Alcotest.(check bool) "ptt-start rejected" false
                (bool_field "accepted" start);
              Alcotest.(check bool) "still active while cleanup held" true
                (bool_field "active" (snapshot_fields start)
                || status_active ~net ~sw socket_path);
              ignore (Eio.Promise.try_resolve cleanup_u ());
              wait_until ~clock (fun () -> idle ~net ~sw socket_path);
              Alcotest.(check int) "no delivery" 0 (Atomic.get deliver_n);
              match peek_generation retry_dir with
              | None -> Alcotest.fail "audio candidate lost"
              | Some g ->
                  Alcotest.(check bool) "same generation" true
                    (Active.generation_equal g gen0))))

let test_text_retry_held_delivery_cleanup_stt_zero () =
  with_dir (fun dir ->
      let retry_dir = Filename.concat dir "r" in
      let text0 = "e2e-retained-text" in
      let gen0 = seed_text retry_dir text0 in
      let stt_n = Atomic.make 0 in
      let deliver_n = Atomic.make 0 in
      let cleanup_hold, cleanup_u = Eio.Promise.create () in
      let entered, entered_u = Eio.Promise.create () in
      let stt _ _ _ =
        Atomic.incr stt_n;
        Eta.Effect.pure "should-not-run"
      in
      let deliver _ _ _ =
        Atomic.incr deliver_n;
        ignore (Eio.Promise.try_resolve entered_u ());
        let hang, _ = Eio.Promise.create () in
        Eta.Effect.sync (fun () -> Eio.Promise.await hang)
        |> Eta.Effect.finally
             (Eta.Effect.sync (fun () -> Eio.Promise.await cleanup_hold))
      in
      Fun.protect
        ~finally:(fun () -> ignore (Eio.Promise.try_resolve cleanup_u ()))
        (fun () ->
          run_e2e ~dir ~stt ~deliver (fun ~clock ~net ~sw ~socket_path ~retry_dir ->
              ignore (rpc ~net ~sw socket_path action_retry);
              wait_until ~clock (fun () ->
                  Atomic.get deliver_n >= 1 && Eio.Promise.is_resolved entered);
              Alcotest.(check int) "text retry skips STT" 0 (Atomic.get stt_n);
              ignore (rpc ~net ~sw socket_path action_pause);
              ignore (rpc ~net ~sw socket_path action_resume);
              let start =
                assoc_of_json (rpc ~net ~sw socket_path action_ptt_start)
              in
              Alcotest.(check bool) "ptt-start rejected" false
                (bool_field "accepted" start);
              ignore (Eio.Promise.try_resolve cleanup_u ());
              wait_until ~clock (fun () -> idle ~net ~sw socket_path);
              match peek_meta retry_dir with
              | Some (Active.Text { generation; _ })
                when Active.generation_equal generation gen0 -> (
                  match Active.open_store ~root:retry_dir with
                  | Error (`Retry_store msg) -> Alcotest.fail msg
                  | Ok store -> (
                      match Active.load_text store generation with
                      | Ok t -> Alcotest.(check string) "exact text" text0 t
                      | Error (`Retry_store msg) -> Alcotest.fail msg))
              | _ -> Alcotest.fail "text candidate not preserved")))

let rec suppressed_inject_and_publish (cause : e2e_error Eta.Cause.t) =
  match cause with
  | Eta.Cause.Suppressed
      { primary = Eta.Cause.Fail (`Ptt_error "inject:failed"); finalizer } -> (
      match finalizer with Eta.Cause.Finalizer.Fail _ -> true | _ -> false)
  | Sequential xs | Concurrent xs -> List.exists suppressed_inject_and_publish xs
  | Suppressed { primary; finalizer } -> (
      suppressed_inject_and_publish primary
      ||
      match finalizer with
      | Eta.Cause.Finalizer.Fail _ -> false
      | _ -> false)
  | Finalizer f -> (
      match f with
      | Eta.Cause.Finalizer.Concurrent fs | Sequential fs ->
          List.exists
            (fun n ->
              match n with
              | Eta.Cause.Finalizer.Fail _ -> false
              | _ -> false)
            fs
      | _ -> false)
  | Fail _ | Unhandled _ | Die _ | Interrupt _ -> false

let test_audio_retry_delivery_and_text_publish_both_causes () =
  with_dir (fun dir ->
      let retry_dir = Filename.concat dir "r" in
      let gen0 = seed_audio retry_dir in
      let stt_n = Atomic.make 0 in
      let deliver_n = Atomic.make 0 in
      let lock_save = ref Created_dir in
      let stt _ _ _ =
        Atomic.incr stt_n;
        Eta.Effect.pure "recognized"
      in
      let deliver _ _ _ =
        Atomic.incr deliver_n;
        lock_save := install_lock_dir retry_dir;
        Eta.Effect.fail (`Ptt_error "inject:failed")
      in
      Fun.protect
        ~finally:(fun () -> restore_lock retry_dir !lock_save)
        (fun () ->
          Eio_main.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let clock = Eio.Stdenv.clock env in
          let mono_clock = Eio.Stdenv.mono_clock env in
          let net = Eio.Stdenv.net env in
          let process_mgr = Eio.Stdenv.process_mgr env in
          let keyboard = Pie.Keyboard.Monitor.create () in
          let _audio_q, audio = make_audio () in
          let stamp () = [ Some (1, 1, 0, 0.) ] in
          let load _ = (base_config (), stamp ()) in
          let socket_path, retry_dir, workflow =
            owner_workflow ~sw ~net ~process_mgr ~mono_clock ~dir
              ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
          in
          let got = Atomic.make None in
          Eio.Fiber.fork ~sw (fun () ->
              match Eta_eio.run_once ~sw ~clock workflow with
              | Ok () -> ()
              | Error cause -> Atomic.set got (Some cause));
          wait_until ~clock (fun () -> Sys.file_exists socket_path);
          ignore (rpc ~net ~sw socket_path action_retry);
          wait_until ~clock (fun () -> Atomic.get deliver_n >= 1);
          wait_until ~clock (fun () -> Atomic.get got <> None);
          match Atomic.get got with
          | None -> Alcotest.fail "expected compound workflow failure"
          | Some cause ->
              if not (suppressed_inject_and_publish cause) then
                Alcotest.failf "expected inject:failed plus publish Fail, got %a"
                  (Eta.Cause.pp pp_run_error) cause;
              Alcotest.(check int) "stt ran" 1 (Atomic.get stt_n);
              (match peek_kind retry_dir with
              | Some "audio" -> ()
              | other ->
                  Alcotest.failf
                    "expected audio retained after publish fail, got %s"
                    (Option.value ~default:"none" other));
              match peek_generation retry_dir with
              | Some g when Active.generation_equal g gen0 -> ()
              | _ -> Alcotest.fail "audio generation replaced"))

let test_delivery_success_clear_fail_preserves_candidate () =
  with_dir (fun dir ->
      let retry_dir = Filename.concat dir "r" in
      let gen0 = seed_audio retry_dir in
      let deliver_n = Atomic.make 0 in
      let lock_save = ref Created_dir in
      let stt _ _ _ = Eta.Effect.pure "recognized" in
      let deliver _ _ _ =
        Atomic.incr deliver_n;
        lock_save := install_lock_dir retry_dir;
        Eta.Effect.unit
      in
      Fun.protect
        ~finally:(fun () -> restore_lock retry_dir !lock_save)
        (fun () ->
          run_e2e ~dir ~stt ~deliver (fun ~clock ~net ~sw ~socket_path ~retry_dir ->
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
          Alcotest.test_case
            "require_success rejects interrupt-only" `Quick
            test_interrupt_only_rejected_by_require_success;
          Alcotest.test_case "Ok with completed<100 fails after helper" `Quick
            test_ok_with_completed_lt_100_fails;
        ] );
      ( "edges",
        [
          Alcotest.test_case "audio retry held cleanup pause ptt-start busy"
            `Quick test_audio_retry_held_cleanup_pause_busy;
          Alcotest.test_case "text retry held delivery STT stays 0" `Quick
            test_text_retry_held_delivery_cleanup_stt_zero;
          Alcotest.test_case "audio retry delivery+publish fail keeps audio"
            `Quick test_audio_retry_delivery_and_text_publish_both_causes;
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
