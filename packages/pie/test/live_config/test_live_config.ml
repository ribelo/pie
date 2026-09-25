(* PIE006: one published effective config, physical-key ownership, attempt
   thresholds sampled once. Synthetic run_with_inputs only. *)

let transcribe_keysym = 65478
let translate_keysym = 65479
let override_keysym = 65001
let file_keysym = 10

let base_config ?(transcribe = transcribe_keysym)
    ?(translate = translate_keysym) ?(transcribe_shift = false) ?(rms = 0.0)
    ?(peak = 0.0) () : Pie.Config.Stt.t =
  {
    provider = Pie.Config.Stt.Openrouter;
    transcription_model = "openrouter/live-config";
    translation_model = "openrouter/live-config";
    transcription_language = "English";
    translation_source_language = "English";
    translation_target_language = "English";
    ptt_min_capture_rms = rms;
    ptt_min_capture_peak = peak;
    ptt_transcribe_keysym = transcribe;
    ptt_translate_keysym = translate;
    ptt_transcribe_shift = transcribe_shift;
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

let event ?(state = 0) ?(keysym = transcribe_keysym) ?(keycode = 75) ~released
    () : Pie.Keyboard.Monitor.key_event =
  { released; state; keysym; unichar = 0; keycode }

let wait_until ?(steps = 50) ~clock pred =
  let rec loop n =
    if pred () then ()
    else if n <= 0 then Alcotest.fail "deadline"
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
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let with_dir f =
  let dir = Filename.temp_file "pie-live-config" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

type audio_cmd = Chunk of bytes | Fail of Pie.Pulse.Client.error

let make_audio ?(takes = Atomic.make 0) () =
  let cmd = Eio.Stream.create 16 in
  let rec source () =
    Eta_stream.Stream.from_effect
      (Eta.Effect.sync (fun () ->
           let item = Eio.Stream.take cmd in
           Atomic.incr takes;
           item)
      |> Eta.Effect.bind (function
        | Chunk bytes -> Eta.Effect.pure bytes
        | Fail error -> Eta.Effect.fail (`Pulse_error error)))
    |> Eta_stream.Stream.flat_map (fun chunk ->
        Eta_stream.Stream.concat (Eta_stream.Stream.succeed chunk) (source ()))
  in
  (cmd, source ())

let offer_pcm cmd = Eio.Stream.add cmd (Chunk loud_pcm)

let with_workflow_background ~sw ~clock workflow assertions =
  match
    Eta_eio.run_once ~sw ~clock
      (Eta.Effect.with_background ~name:"pie.live-config.workflow" workflow
         (fun () -> Eta.Effect.sync assertions))
  with
  | Ok () -> ()
  | Error cause -> fail_cause cause

let run_live ~sw ~net ~process_mgr ~mono_clock ~dir ~config_control ~stt
    ~deliver ~keyboard ~audio ?ptt_transcribe_keysym ?ptt_translate_keysym () =
  let socket_path = Filename.concat dir "control.sock" in
  let retry_dir = Filename.concat dir "recordings" in
  (try Unix.mkdir retry_dir 0o700
   with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let inputs =
    { Pie.Assistant.Service.keyboard; audio; source_name = Some "test-source" }
  in
  Pie.Assistant.Service.run_with_inputs ~sw ~net ~process_mgr ~mono_clock
    ~inputs ~config_control ~stt ~deliver ~socket_path ~retry_dir
    ~min_duration_ms:0 ?ptt_transcribe_keysym ?ptt_translate_keysym ()

let cycle ~clock ~keyboard ~audio_q ~delivered ?state ?keysym ?keycode () =
  Pie.Keyboard.Monitor.emit keyboard
    (event ?state ?keysym ?keycode ~released:false ());
  offer_pcm audio_q;
  Pie.Keyboard.Monitor.emit keyboard
    (event ?state ?keysym ?keycode ~released:true ());
  wait_until ~clock (fun () -> Atomic.get delivered <> [])

let test_cli_override_before_and_after_reload () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let gen = Atomic.make 0 in
      let file_transcribe = Atomic.make file_keysym in
      let stamp () = [ Some (1, 1, Atomic.get gen, 0.) ] in
      let load _ =
        (base_config ~transcribe:(Atomic.get file_transcribe) (), stamp ())
      in
      let stt _ _ _ = Eta.Effect.pure "ok" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () ->
            Atomic.set delivered (text :: Atomic.get delivered))
      in
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio
          ~ptt_transcribe_keysym:override_keysym ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          cycle ~clock ~keyboard ~audio_q ~delivered ~keysym:override_keysym
            ~keycode:91 ();
          Alcotest.(check (list string))
            "override before reload" [ "ok" ] (Atomic.get delivered);
          Atomic.set delivered [];
          Atomic.set file_transcribe 20;
          Atomic.incr gen;
          Eio.Time.sleep clock 0.7;
          cycle ~clock ~keyboard ~audio_q ~delivered ~keysym:override_keysym
            ~keycode:91 ();
          Alcotest.(check (list string))
            "override after reload" [ "ok" ] (Atomic.get delivered);
          Atomic.set delivered [];
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keysym:20 ~keycode:21 ~released:false ());
          offer_pcm audio_q;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keysym:20 ~keycode:21 ~released:true ());
          Eio.Time.sleep clock 0.4;
          Alcotest.(check (list string))
            "file keysym is not the published binding" [] (Atomic.get delivered)))

let test_physical_release_after_binding_reload () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let gen = Atomic.make 0 in
      let transcribe = Atomic.make transcribe_keysym in
      let stamp () = [ Some (1, 1, Atomic.get gen, 0.) ] in
      let load _ =
        (base_config ~transcribe:(Atomic.get transcribe) (), stamp ())
      in
      let stt _ _ _ = Eta.Effect.pure "held" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () -> Atomic.set delivered [ text ])
      in
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keysym:transcribe_keysym ~keycode:75 ~released:false ());
          offer_pcm audio_q;
          Atomic.set transcribe 111;
          Atomic.incr gen;
          Eio.Time.sleep clock 0.7;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keysym:111 ~keycode:75 ~released:true ());
          wait_until ~clock (fun () -> Atomic.get delivered <> []);
          Alcotest.(check (list string))
            "physical keycode release after reload" [ "held" ]
            (Atomic.get delivered)))

let test_modifier_reload_while_held () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let gen = Atomic.make 0 in
      let shift = Atomic.make true in
      let stamp () = [ Some (1, 1, Atomic.get gen, 0.) ] in
      let load _ =
        (base_config ~transcribe_shift:(Atomic.get shift) (), stamp ())
      in
      let stt _ _ _ = Eta.Effect.pure "shift-held" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () -> Atomic.set delivered [ text ])
      in
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~state:1 ~keycode:75 ~released:false ());
          offer_pcm audio_q;
          Atomic.set shift false;
          Atomic.incr gen;
          Eio.Time.sleep clock 0.7;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~state:0 ~keycode:75 ~released:true ());
          wait_until ~clock (fun () -> Atomic.get delivered <> []);
          Alcotest.(check (list string))
            "release without new shift still ends capture" [ "shift-held" ]
            (Atomic.get delivered)))

let test_mod5_latch_survives_key_release () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let takes = Atomic.make 0 in
      let audio_q, audio = make_audio ~takes () in
      let delivered = Atomic.make [] in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let stt _ _ _ = Eta.Effect.pure "latched" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () -> Atomic.set delivered [ text ])
      in
      let feeding = Atomic.make true in
      Eio.Fiber.fork ~sw (fun () ->
          let rec loop () =
            if not (Atomic.get feeding) then ()
            else (
              offer_pcm audio_q;
              Eio.Time.sleep clock 0.02;
              loop ())
          in
          loop ());
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Pie.Keyboard.Monitor.emit keyboard
            (event ~state:128 ~keysym:translate_keysym ~keycode:80
               ~released:false ());
          wait_until ~clock (fun () -> Atomic.get takes > 0);
          Pie.Keyboard.Monitor.emit keyboard
            (event ~state:0 ~keysym:translate_keysym ~keycode:80 ~released:true
               ());
          let rec quiet n =
            if Atomic.get delivered <> [] then false
            else if n <= 0 then true
            else (
              Eio.Time.sleep clock 0.05;
              quiet (n - 1))
          in
          Alcotest.(check bool)
            "Mod5 key release does not finish latch" true (quiet 50);
          Pie.Keyboard.Monitor.emit keyboard
            (event ~state:128 ~keysym:translate_keysym ~keycode:80
               ~released:false ());
          wait_until ~clock (fun () -> Atomic.get delivered <> []);
          Atomic.set feeding false;
          Alcotest.(check (list string))
            "second Mod5 press finishes latch" [ "latched" ]
            (Atomic.get delivered)))

let test_invalid_reload_keeps_last_good () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let gen = Atomic.make 0 in
      let fail_reload = Atomic.make false in
      let stamp () = [ Some (1, 1, Atomic.get gen, 0.) ] in
      let load _ =
        if Atomic.get fail_reload then
          raise (Pie.Config.Stt.Config_error "synthetic reload failure");
        (base_config (), stamp ())
      in
      let stt _ _ _ = Eta.Effect.pure "last-good" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () -> Atomic.set delivered [ text ])
      in
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Atomic.set fail_reload true;
          Atomic.incr gen;
          Eio.Time.sleep clock 0.7;
          cycle ~clock ~keyboard ~audio_q ~delivered ();
          Alcotest.(check (list string))
            "last-good after invalid reload" [ "last-good" ]
            (Atomic.get delivered)))

let test_threshold_reload_does_not_change_active_attempt () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let gen = Atomic.make 0 in
      let peak = Atomic.make 0.0 in
      let stamp () = [ Some (1, 1, Atomic.get gen, 0.) ] in
      let load _ = (base_config ~peak:(Atomic.get peak) (), stamp ()) in
      let stt _ _ _ = Eta.Effect.pure "snapshot" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () -> Atomic.set delivered [ text ])
      in
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (event ~released:false ());
          offer_pcm audio_q;
          Atomic.set peak 0.99;
          Atomic.incr gen;
          Eio.Time.sleep clock 0.7;
          Pie.Keyboard.Monitor.emit keyboard (event ~released:true ());
          wait_until ~clock (fun () -> Atomic.get delivered <> []);
          Alcotest.(check (list string))
            "attempt keeps start thresholds after peak reload" [ "snapshot" ]
            (Atomic.get delivered)))

let test_held_key_survives_binding_reload_unit () =
  let binding keysym shift : Pie.Assistant.Service.ptt_binding =
    { keysym; shift }
  in
  let bindings = ref (binding 65478 false, binding 65479 false) in
  let recognize =
    Pie.Assistant.Service.make_live_recognizer
      ~load_bindings:(fun () -> !bindings)
      ()
  in
  let event keysym released : Pie.Keyboard.Monitor.key_event =
    { released; state = 0; keysym; unichar = 0; keycode = 42 }
  in
  let first = recognize (event 65478 false) in
  bindings := (binding 65516 false, binding 65508 false);
  let second = recognize (event 65478 false) in
  let third = recognize (event 65478 true) in
  Alcotest.(check bool)
    "first press" true
    (first = Some (Pie.Ptt_loop.Press "transcribe"));
  Alcotest.(check bool)
    "held press after reload" true
    (second = Some (Pie.Ptt_loop.Press "transcribe"));
  Alcotest.(check bool)
    "physical release after reload" true
    (third = Some (Pie.Ptt_loop.Release "transcribe"))

let test_failed_on_press_different_key_not_stuck () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let gen = Atomic.make 0 in
      let model = Atomic.make "openrouter/live-config" in
      let stamp () = [ Some (1, 1, Atomic.get gen, 0.) ] in
      let load _ =
        let cfg = base_config () in
        ( {
            cfg with
            transcription_model = Atomic.get model;
            translation_model = Atomic.get model;
          },
          stamp () )
      in
      let stt _ _ _ = Eta.Effect.pure "ok" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () -> Atomic.set delivered [ text ])
      in
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Atomic.set model "openrouter/";
          Atomic.incr gen;
          Eio.Time.sleep clock 0.7;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:75 ~released:false ());
          offer_pcm audio_q;
          Eio.Time.sleep clock 0.4;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:75 ~released:false ());
          Eio.Time.sleep clock 0.3;
          Alcotest.(check (list string))
            "failed key autorepeat is not a fresh press" []
            (Atomic.get delivered);
          Atomic.set model "openrouter/live-config";
          Atomic.incr gen;
          Eio.Time.sleep clock 0.7;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:91 ~released:false ());
          offer_pcm audio_q;
          Eio.Time.sleep clock 0.4;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:75 ~released:true ());
          Eio.Time.sleep clock 0.5;
          Alcotest.(check (list string))
            "late release of failed key does not finish B" []
            (Atomic.get delivered);
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:91 ~released:true ());
          wait_until ~clock (fun () -> Atomic.get delivered <> []);
          Alcotest.(check (list string))
            "owner key B finishes the capture once" [ "ok" ]
            (Atomic.get delivered)))

let test_autorepeat_press_does_not_restart_capture () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let stt _ _ _ = Eta.Effect.pure "once" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () ->
            Atomic.set delivered (text :: Atomic.get delivered))
      in
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (event ~released:false ());
          offer_pcm audio_q;
          Pie.Keyboard.Monitor.emit keyboard (event ~released:false ());
          Pie.Keyboard.Monitor.emit keyboard (event ~released:true ());
          wait_until ~clock (fun () -> Atomic.get delivered <> []);
          Alcotest.(check (list string))
            "autorepeat is one capture" [ "once" ] (Atomic.get delivered)))

let test_busy_second_key_repeat_ignored_until_keyup () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let stt _ _ _ = Eta.Effect.pure "ok" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () -> Atomic.set delivered [ text ])
      in
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:75 ~released:false ());
          offer_pcm audio_q;
          Eio.Time.sleep clock 0.3;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:91 ~released:false ());
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:75 ~released:true ());
          wait_until ~clock (fun () -> Atomic.get delivered <> []);
          Alcotest.(check (list string))
            "owner A delivered" [ "ok" ] (Atomic.get delivered);
          Atomic.set delivered [];
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:91 ~released:false ());
          offer_pcm audio_q;
          Eio.Time.sleep clock 0.4;
          Alcotest.(check (list string))
            "busy B autorepeat after A settles is not a fresh press" []
            (Atomic.get delivered);
          Pie.Keyboard.Monitor.emit keyboard
            (event ~keycode:91 ~released:true ());
          Eio.Time.sleep clock 0.3;
          cycle ~clock ~keyboard ~audio_q ~delivered ~keycode:91 ();
          Alcotest.(check (list string))
            "fresh B after keyup is accepted" [ "ok" ] (Atomic.get delivered)))

let test_mod5_autorepeat_does_not_toggle_latch () =
  Eio_main.run @@ fun env ->
  with_dir (fun dir ->
      Eio.Switch.run @@ fun sw ->
      let clock = Eio.Stdenv.clock env in
      let keyboard = Pie.Keyboard.Monitor.create () in
      let audio_q, audio = make_audio () in
      let delivered = Atomic.make [] in
      let stamp () = [ Some (1, 1, 0, 0.) ] in
      let load _ = (base_config (), stamp ()) in
      let stt _ _ _ = Eta.Effect.pure "latched" in
      let deliver _ _ text =
        Eta.Effect.sync (fun () -> Atomic.set delivered [ text ])
      in
      let workflow =
        run_live ~sw ~net:(Eio.Stdenv.net env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~dir ~config_control:{ stamp; load } ~stt ~deliver ~keyboard ~audio ()
      in
      with_workflow_background ~sw ~clock workflow (fun () ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~state:128 ~keysym:translate_keysym ~keycode:80
               ~released:false ());
          offer_pcm audio_q;
          Pie.Keyboard.Monitor.emit keyboard
            (event ~state:128 ~keysym:translate_keysym ~keycode:80
               ~released:false ());
          Eio.Time.sleep clock 0.4;
          Alcotest.(check (list string))
            "Mod5 autorepeat does not stop latch" [] (Atomic.get delivered);
          Pie.Keyboard.Monitor.emit keyboard
            (event ~state:0 ~keysym:translate_keysym ~keycode:80 ~released:true
               ());
          Eio.Time.sleep clock 0.3;
          Alcotest.(check (list string))
            "Mod5 keyup still does not finish latch" [] (Atomic.get delivered);
          Pie.Keyboard.Monitor.emit keyboard
            (event ~state:128 ~keysym:translate_keysym ~keycode:80
               ~released:false ());
          wait_until ~clock (fun () -> Atomic.get delivered <> []);
          Alcotest.(check (list string))
            "next physical Mod5 press stops latch" [ "latched" ]
            (Atomic.get delivered)))

let test_cli_override_applied_to_published_record () =
  let file = base_config ~transcribe:file_keysym () in
  let published =
    Pie.Assistant.Service.with_cli_overrides
      ~ptt_transcribe_keysym:override_keysym file
  in
  Alcotest.(check int)
    "CLI override is the published transcribe keysym" override_keysym
    published.ptt_transcribe_keysym;
  Alcotest.(check int)
    "file translate unchanged" translate_keysym published.ptt_translate_keysym

let () =
  Alcotest.run "live-config"
    [
      ( "publication",
        [
          Alcotest.test_case "CLI override is the published record" `Quick
            test_cli_override_applied_to_published_record;
          Alcotest.test_case "CLI override before and after reload" `Quick
            test_cli_override_before_and_after_reload;
          Alcotest.test_case "invalid reload keeps last-good" `Quick
            test_invalid_reload_keeps_last_good;
        ] );
      ( "physical",
        [
          Alcotest.test_case "held key survives binding reload unit" `Quick
            test_held_key_survives_binding_reload_unit;
          Alcotest.test_case "physical release after binding reload" `Quick
            test_physical_release_after_binding_reload;
          Alcotest.test_case "modifier reload while held still releases" `Quick
            test_modifier_reload_while_held;
          Alcotest.test_case "Mod5 latch survives key release" `Quick
            test_mod5_latch_survives_key_release;
          Alcotest.test_case "failed on_press does not pin a different key"
            `Quick test_failed_on_press_different_key_not_stuck;
          Alcotest.test_case "autorepeat press does not restart capture" `Quick
            test_autorepeat_press_does_not_restart_capture;
          Alcotest.test_case "busy second key repeat ignored until keyup" `Quick
            test_busy_second_key_repeat_ignored_until_keyup;
          Alcotest.test_case "Mod5 autorepeat does not toggle latch" `Quick
            test_mod5_autorepeat_does_not_toggle_latch;
        ] );
      ( "thresholds",
        [
          Alcotest.test_case "threshold reload does not change active attempt"
            `Quick test_threshold_reload_does_not_change_active_attempt;
        ] );
    ]
