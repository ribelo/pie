open Cmdliner

let render_effect_error = function
  | `Pulse_error msg
  | `Ptt_error msg
  | `Stt_error msg
  | `Subprocess_error msg
  | `Injection_error msg
  | `Wtype_error msg
  | `Xdotool_error msg
  | `Niri_error msg
  | `Notification_error msg
  | `Portal_error msg
  | `Daemon_error msg -> msg
  | `Daemon_off -> "daemon is not running"
  | `Closed -> "stream closed"
  | `Timeout -> "operation timed out"

let run_effect mk_eff =
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let rt =
      Eta.Runtime.create
        ~sw
        ~clock:(Eio.Stdenv.clock env)
        ~capture_backtrace:true
        ()
    in
    match
      mk_eff ~env ~sw
      |> Eta.Effect.with_error_renderer render_effect_error
      |> Eta.Runtime.run rt
    with
    | Ok () -> ()
    | Error cause ->
        let msg =
          Format.asprintf "%a"
            (Eta.Cause.pp (fun fmt err ->
               Format.pp_print_string fmt (render_effect_error err)))
            cause
        in
        if Eta.Cause.is_interrupt_only cause then (
          Printf.eprintf "Interrupted\n%!";
          exit 130)
        else (
          Printf.eprintf "Error: %s\n%!" msg;
          exit 1)
  with exn ->
    Printf.eprintf "Fatal: %s\n%!" (Printexc.to_string exn);
    exit 1

let env_term = Term.const ()

let render_recording_status (snap : Camelpie.Daemon.Contract.recording_snapshot) =
  if not snap.enabled then "paused"
  else if not snap.active then "armed"
  else if snap.mode = "idle" then "stale"
  else snap.mode

let render_start_result = function
  | Camelpie.Daemon.Contract.Started mode -> mode
  | Busy mode -> "busy:" ^ mode
  | Disabled -> "paused"
  | Error message -> "error:" ^ message

let sources_cmd =
  let doc = "List PulseAudio capture sources" in
  let info = Cmd.info "sources" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Sources.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ()))
      $ env_term)

let meter_cmd =
  let doc = "Print live RMS / peak levels for input verification" in
  let info = Cmd.info "meter" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Meter.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ()))
      $ env_term)

let record_cmd =
  let output_arg =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUT" ~doc:"Output WAV file path")
  in
  let duration_arg =
    Arg.(value & opt float 5.0 & info ["d"; "duration"] ~docv:"SECONDS" ~doc:"Recording duration")
  in
  let raw_flag =
    Arg.(value & flag & info ["raw"] ~doc:"Disable auto-gain normalization")
  in
  let doc = "Capture raw PCM audio to a WAV file" in
  let info = Cmd.info "record" ~doc in
  Cmd.v info
    Term.(
      const (fun output_path duration_sec raw () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Record.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ~output_path
            ~duration_sec
            ~raw
            ()))
      $ output_arg $ duration_arg $ raw_flag $ env_term)

let type_cmd =
  let text_arg =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"TEXT" ~doc:"Text to type into the focused application")
  in
  let doc = "Send text to the focused application" in
  let info = Cmd.info "type" ~doc in
  Cmd.v info
    Term.(
      const (fun text () ->
        run_effect (fun ~env ~sw ->
          let process_mgr = Eio.Stdenv.process_mgr env in
          Eta.Effect.bind (fun (result : Camelpie.Input.Text_injection.result) ->
            Eta.Effect.sync (fun () ->
              Printf.printf "Typed %d chars via %s (%s)\n%!"
                (String.length result.text)
                (match result.backend with | Wtype -> "wtype" | Xdotool -> "xdotool")
                (match result.session_type with | Wayland -> "Wayland" | X11 -> "X11"))
          ) (Camelpie.Input.Text_injection.inject ~process_mgr text)))
      $ text_arg $ env_term)

let status_cmd =
  let doc = "Show daemon recording status" in
  let info = Cmd.info "status" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          Eta.Effect.catch
            (fun e ->
              match e with
              | `Daemon_off ->
                Eta.Effect.sync (fun () -> Printf.printf "off\n%!")
              | `Daemon_error msg ->
                Eta.Effect.sync (fun () -> Printf.printf "error=%s\n%!" msg))
            (Eta.Effect.bind (fun (snap : Camelpie.Daemon.Contract.recording_snapshot) ->
              Eta.Effect.sync (fun () -> Printf.printf "%s\n%!" (render_recording_status snap))
            ) (Camelpie.Daemon.Client.status ~net ~sw ()))))
      $ env_term)

let toggle_cmd =
  let doc = "Toggle daemon recording" in
  let info = Cmd.info "toggle" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          Eta.Effect.bind (fun (result : bool) ->
            Eta.Effect.sync (fun () ->
              Printf.printf "%s\n%!" (if result then "armed" else "paused"))
          ) (Camelpie.Daemon.Client.toggle ~net ~sw ())))
      $ env_term)

let pause_cmd =
  let doc = "Pause daemon recording" in
  let info = Cmd.info "pause" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          Camelpie.Daemon.Client.pause ~net ~sw ()))
      $ env_term)

let resume_cmd =
  let doc = "Resume daemon recording" in
  let info = Cmd.info "resume" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          Camelpie.Daemon.Client.resume ~net ~sw ()))
      $ env_term)

let meeting_start_cmd =
  let doc = "Start meeting transcription" in
  let info = Cmd.info "meeting-start" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          Eta.Effect.bind (fun (r : Camelpie.Daemon.Contract.meeting_start_result) ->
            Eta.Effect.sync (fun () ->
              Printf.printf "%s\n%!" (render_start_result r.Camelpie.Daemon.Contract.result))
          ) (Camelpie.Daemon.Client.meeting_start ~net ~sw ())))
      $ env_term)

let meeting_stop_cmd =
  let doc = "Stop meeting transcription" in
  let info = Cmd.info "meeting-stop" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          Eta.Effect.bind (fun (r : Camelpie.Daemon.Contract.meeting_stop_result) ->
            Eta.Effect.sync (fun () ->
              if not r.stopped then
                Printf.printf "no meeting transcription was active\n%!";
              Printf.printf "%s\n%!" (render_recording_status r.snapshot))
          ) (Camelpie.Daemon.Client.meeting_stop ~net ~sw ())))
      $ env_term)

let meeting_toggle_cmd =
  let doc = "Toggle meeting transcription" in
  let info = Cmd.info "meeting-toggle" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          Eta.Effect.bind (fun (snap : Camelpie.Daemon.Contract.recording_snapshot) ->
            Eta.Effect.sync (fun () ->
              Printf.printf "%s\n%!" (render_recording_status snap))
          ) (Camelpie.Daemon.Client.meeting_toggle ~net ~sw ())))
      $ env_term)

let ptt_portal_cmd =
  let doc = "Register a GlobalShortcuts portal binding" in
  let info = Cmd.info "ptt-portal" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Ptt_portal.run
            ~process_mgr:(Eio.Stdenv.process_mgr env)
            ()))
      $ env_term)

let ptt_cmd =
  let doc = "Push-to-talk capture and transcribe/translate" in
  let info = Cmd.info "ptt" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Ptt.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ~process_mgr:(Eio.Stdenv.process_mgr env)
            ~stdin:(Eio.Stdenv.stdin env)
            ~mode:`Transcribe ()))
      $ env_term)

let ptt_transcribe_cmd =
  let doc = "PTT transcribe mode" in
  let info = Cmd.info "ptt-transcribe" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Ptt.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ~process_mgr:(Eio.Stdenv.process_mgr env)
            ~stdin:(Eio.Stdenv.stdin env)
            ~mode:`Transcribe ()))
      $ env_term)

let ptt_translate_cmd =
  let doc = "PTT translate mode" in
  let info = Cmd.info "ptt-translate" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Ptt.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ~process_mgr:(Eio.Stdenv.process_mgr env)
            ~stdin:(Eio.Stdenv.stdin env)
            ~mode:`Translate ()))
      $ env_term)

let stt_interactive_cmd =
  let doc = "Interactive STT (Enter to start/stop)" in
  let info = Cmd.info "stt-interactive" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Stt_interactive.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ~process_mgr:(Eio.Stdenv.process_mgr env)
            ~stdin:(Eio.Stdenv.stdin env)
            ()))
      $ env_term)

let wakeword_cmd =
  let doc = "Run live wakeword scoring telemetry" in
  let info = Cmd.info "wakeword" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Camelpie.Commands.Wakeword_telemetry.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ()))
      $ env_term)

let wakeword_tune_cmd =
  let doc = "Tune wakeword detection thresholds" in
  let info = Cmd.info "wakeword-tune" ~doc in
  Cmd.v info
    Term.(const (fun () -> run_effect (fun ~env ~sw -> Eta.Effect.sync (fun () -> Camelpie.Commands.Wakeword_tune_cmd.run ()))) $ env_term)

let wakeword_train_cmd =
  let doc = "Train a custom wakeword model" in
  let info = Cmd.info "wakeword-train" ~doc in
  Cmd.v info
    Term.(const (fun () -> run_effect (fun ~env ~sw -> Eta.Effect.sync (fun () -> Camelpie.Commands.Wakeword_train_cmd.run ()))) $ env_term)

let assistant_term =
  let transcribe_keysym_arg =
    Arg.(value & opt int Camelpie.Commands.Assistant.default_transcribe_keysym
         & info ["ptt-transcribe-keysym"] ~docv:"KEYSYM"
             ~doc:"XKB keysym for PTT transcribe (default: F9 / 65478)")
  in
  let translate_keysym_arg =
    Arg.(value & opt int Camelpie.Commands.Assistant.default_translate_keysym
         & info ["ptt-translate-keysym"] ~docv:"KEYSYM"
             ~doc:"XKB keysym for PTT translate (default: F10 / 65479)")
  in
  Term.(
    const (fun ptt_transcribe_keysym ptt_translate_keysym () ->
      run_effect (fun ~env ~sw ->
        Camelpie.Commands.Assistant.run
          ~sw
          ~net:(Eio.Stdenv.net env)
          ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~stdin:(Eio.Stdenv.stdin env)
          ~ptt_transcribe_keysym
          ~ptt_translate_keysym
          ()))
    $ transcribe_keysym_arg $ translate_keysym_arg $ env_term)

let assistant_cmd =
  let doc = "Combined assistant mode (PTT + STT + text injection)" in
  let info = Cmd.info "assistant" ~doc in
  Cmd.v info assistant_term

let default_cmd =
  let doc = "pie voice assistant (run without subcommands for combined wakeword + PTT mode)" in
  let info =
    Cmd.info "pie" ~version:"0.1.0" ~doc
      ~man:[
        `S "DESCRIPTION";
        `P "pie is a voice-activated personal AI assistant for Linux.";
        `S "COMMANDS";
        `P "$(b,sources) — list PulseAudio capture sources";
        `P "$(b,meter) — live RMS / peak level monitor";
        `P "$(b,record) — capture audio to WAV file";
        `P "$(b,type) — send text to the focused app";
        `P "$(b,ptt-portal) — GlobalShortcuts portal registration";
        `P "$(b,ptt) — push-to-talk capture";
        `P "$(b,ptt-transcribe) — PTT transcribe";
        `P "$(b,ptt-translate) — PTT translate";
        `P "$(b,stt-interactive) — interactive STT";
        `P "$(b,wakeword) — live wakeword telemetry";
        `P "$(b,wakeword-tune) — tune wakeword thresholds";
        `P "$(b,wakeword-train) — train wakeword model";
        `P "$(b,status) — show daemon status";
        `P "$(b,toggle) — toggle recording";
        `P "$(b,pause) — pause recording";
        `P "$(b,resume) — resume recording";
        `P "$(b,meeting-start) — start meeting transcription";
        `P "$(b,meeting-stop) — stop meeting transcription";
        `P "$(b,meeting-toggle) — toggle meeting transcription";
      ]
  in
  Cmd.group ~default:assistant_term info [
    sources_cmd; meter_cmd; record_cmd; type_cmd;
    ptt_portal_cmd; ptt_cmd; ptt_transcribe_cmd; ptt_translate_cmd;
    stt_interactive_cmd; wakeword_cmd; wakeword_tune_cmd; wakeword_train_cmd;
    status_cmd; toggle_cmd; pause_cmd; resume_cmd;
    meeting_start_cmd; meeting_stop_cmd; meeting_toggle_cmd;
  ]

let () = exit (Cmd.eval default_cmd)
