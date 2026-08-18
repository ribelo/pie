open Cmdliner

let render_effect_error = function
  | `Pulse_error error -> Pie.Pulse.Client.error_message error
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

let pp_effect_error formatter error =
  Format.pp_print_string formatter (render_effect_error error)

let run_effect mk_eff =
  exit (Pie.Process.run ~pp_err:pp_effect_error mk_eff)

let env_term = Term.const ()

let render_recording_status (snap : Pie.Daemon.Contract.recording_snapshot) =
  if not snap.enabled then "paused"
  else if not snap.active then "armed"
  else if snap.mode = "idle" then "stale"
  else snap.mode

let render_start_result = function
  | Pie.Daemon.Contract.Started mode -> mode
  | Busy mode -> "busy:" ^ mode
  | Disabled -> "paused"
  | Error message -> "error:" ^ message

let render_action_result (result : Pie.Daemon.Contract.action_result) =
  match String.lowercase_ascii result.action, result.message with
  | "status", _ -> render_recording_status result.snapshot
  | _, Some message -> message
  | _ -> "ok"

let json_string json = Yojson.Safe.to_string json

let json_of_daemon_error message =
  `Assoc [
    ("active", `Bool false);
    ("mode", `String "offline");
    ("enabled", `Bool false);
    ("startedAt", `Null);
    ("updatedAt", `Null);
    ("transcriptPath", `Null);
    ("lastError", `String message);
  ]

let print_jsonl json =
  Printf.printf "%s\n%!" (json_string json)

let sources_cmd =
  let doc = "List PulseAudio capture sources" in
  let info = Cmd.info "sources" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Pie.Commands.Sources.run
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
          Pie.Commands.Meter.run
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
          Pie.Commands.Record.run
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
          let open Eta.Syntax in
          let* (result : Pie.Input.Text_injection.result) =
            Pie.Input.Text_injection.inject ~process_mgr text
          in
          Eta.Effect.sync (fun () ->
            Printf.printf "Typed %d chars via %s (%s)\n%!"
              (String.length result.text)
              (match result.backend with | Wtype -> "wtype" | Xdotool -> "xdotool")
              (match result.session_type with | Wayland -> "Wayland" | X11 -> "X11"))))
      $ text_arg $ env_term)

let status_cmd =
  let doc = "Show daemon recording status" in
  let info = Cmd.info "status" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          let status =
            let open Eta.Syntax in
            let* (snap : Pie.Daemon.Contract.recording_snapshot) =
              Pie.Daemon.Client.status ~net ~sw ()
            in
            Eta.Effect.sync (fun () ->
              Printf.printf "%s\n%!" (render_recording_status snap))
          in
          Eta.Effect.bind_error
            (fun e ->
              match e with
              | `Daemon_off ->
                Eta.Effect.sync (fun () -> Printf.printf "off\n%!")
              | `Daemon_error msg ->
                Eta.Effect.sync (fun () -> Printf.printf "error=%s\n%!" msg))
            status))
      $ env_term)

let toggle_cmd =
  let doc = "Toggle daemon recording" in
  let info = Cmd.info "toggle" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          let open Eta.Syntax in
          let* (result : bool) = Pie.Daemon.Client.toggle ~net ~sw () in
          Eta.Effect.sync (fun () ->
            Printf.printf "%s\n%!" (if result then "armed" else "paused"))))
      $ env_term)

let pause_cmd =
  let doc = "Pause daemon recording" in
  let info = Cmd.info "pause" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          Pie.Daemon.Client.pause ~net ~sw ()))
      $ env_term)

let resume_cmd =
  let doc = "Resume daemon recording" in
  let info = Cmd.info "resume" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          Pie.Daemon.Client.resume ~net ~sw ()))
      $ env_term)

let meeting_start_cmd =
  let doc = "Start meeting transcription" in
  let info = Cmd.info "meeting-start" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          let open Eta.Syntax in
          let* (r : Pie.Daemon.Contract.meeting_start_result) =
            Pie.Daemon.Client.meeting_start ~net ~sw ()
          in
          Eta.Effect.sync (fun () ->
            Printf.printf "%s\n%!" (render_start_result r.Pie.Daemon.Contract.result))))
      $ env_term)

let meeting_stop_cmd =
  let doc = "Stop meeting transcription" in
  let info = Cmd.info "meeting-stop" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          let open Eta.Syntax in
          let* (r : Pie.Daemon.Contract.meeting_stop_result) =
            Pie.Daemon.Client.meeting_stop ~net ~sw ()
          in
          Eta.Effect.sync (fun () ->
            if not r.stopped then
              Printf.printf "no meeting transcription was active\n%!";
            Printf.printf "%s\n%!" (render_recording_status r.snapshot))))
      $ env_term)

let meeting_toggle_cmd =
  let doc = "Toggle meeting transcription" in
  let info = Cmd.info "meeting-toggle" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          let open Eta.Syntax in
          let* (snap : Pie.Daemon.Contract.recording_snapshot) =
            Pie.Daemon.Client.meeting_toggle ~net ~sw ()
          in
          Eta.Effect.sync (fun () ->
            Printf.printf "%s\n%!" (render_recording_status snap))))
      $ env_term)

let msg_action_cmd =
  let action_arg =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"ACTION" ~doc:"Action name")
  in
  let doc = "Send an action to the running daemon" in
  let info = Cmd.info "action" ~doc in
  Cmd.v info
    Term.(
      const (fun action () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          let open Eta.Syntax in
          let* result = Pie.Daemon.Client.action ~net ~sw action in
          Eta.Effect.sync (fun () ->
            Printf.printf "%s\n%!" (render_action_result result))))
      $ action_arg $ env_term)

let msg_status_cmd =
  let json_flag =
    Arg.(value & flag & info ["json"] ~doc:"Print the full daemon status as JSON")
  in
  let doc = "Show daemon status through the message API" in
  let info = Cmd.info "status" ~doc in
  Cmd.v info
    Term.(
      const (fun as_json () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          let open Eta.Syntax in
          let* (snap : Pie.Daemon.Contract.recording_snapshot) =
            Pie.Daemon.Client.status ~net ~sw ()
          in
          Eta.Effect.sync (fun () ->
            if as_json then
              print_jsonl (Pie.Daemon.Contract.json_of_snapshot snap)
            else
              Printf.printf "%s\n%!" (render_recording_status snap))))
      $ json_flag $ env_term)

let msg_subscribe_cmd =
  let interval_ms_arg =
    Arg.(
      value
      & opt int 500
      & info ["interval-ms"] ~docv:"MS"
          ~doc:"Daemon status polling interval for the JSONL stream")
  in
  let doc = "Subscribe to daemon status snapshots as JSONL" in
  let info = Cmd.info "subscribe" ~doc in
  Cmd.v info
    Term.(
      const (fun interval_ms () ->
        run_effect (fun ~env ~sw ->
          let net = Eio.Stdenv.net env in
          let clock = Eio.Stdenv.clock env in
          let sleep_seconds =
            let seconds = (float_of_int interval_ms) /. 1000.0 in
            if seconds <= 0.0 then 0.5 else seconds
          in
          let open Eta.Syntax in
          let rec loop last_json =
            let status =
              let* (snap : Pie.Daemon.Contract.recording_snapshot) =
                Pie.Daemon.Client.status ~net ~sw ()
              in
              let json =
                json_string (Pie.Daemon.Contract.json_of_snapshot snap)
              in
              let* () =
                Eta.Effect.sync (fun () ->
                  if Some json <> last_json then Printf.printf "%s\n%!" json)
              in
              Eta.Effect.pure (Some json)
            in
            let* next_last =
              Eta.Effect.bind_error
                (fun err ->
                  let message =
                    match err with
                    | `Daemon_off -> "daemon is not running"
                    | `Daemon_error msg -> msg
                  in
                  let json = json_string (json_of_daemon_error message) in
                  let* () =
                    Eta.Effect.sync (fun () ->
                      if Some json <> last_json then Printf.printf "%s\n%!" json)
                  in
                  Eta.Effect.pure (Some json))
                status
            in
            let* () =
              Eta.Effect.sync (fun () -> Eio.Time.sleep clock sleep_seconds)
            in
            loop next_last
          in
          loop None))
      $ interval_ms_arg $ env_term)

let msg_cmd =
  let doc = "Send a message to the running daemon" in
  let info = Cmd.info "msg" ~doc in
  Cmd.group info [ msg_action_cmd; msg_status_cmd; msg_subscribe_cmd ]

let ptt_portal_cmd =
  let doc = "Register a GlobalShortcuts portal binding" in
  let info = Cmd.info "ptt-portal" ~doc in
  Cmd.v info
    Term.(
      const (fun () ->
        run_effect (fun ~env ~sw ->
          Pie.Commands.Ptt_portal.run
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
          Pie.Commands.Ptt.run
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
          Pie.Commands.Ptt.run
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
          Pie.Commands.Ptt.run
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
          Pie.Commands.Stt_interactive.run
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
          Pie.Commands.Wakeword_telemetry.run
            ~sw
            ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env)
            ()))
      $ env_term)

let wakeword_tune_cmd =
  let doc = "Tune wakeword detection thresholds" in
  let info = Cmd.info "wakeword-tune" ~doc in
  Cmd.v info
    Term.(const (fun () -> run_effect (fun ~env ~sw -> Eta.Effect.sync (fun () -> Pie.Commands.Wakeword_tune_cmd.run ()))) $ env_term)

let wakeword_train_cmd =
  let doc = "Train a custom wakeword model" in
  let info = Cmd.info "wakeword-train" ~doc in
  Cmd.v info
    Term.(const (fun () -> run_effect (fun ~env ~sw -> Eta.Effect.sync (fun () -> Pie.Commands.Wakeword_train_cmd.run ()))) $ env_term)

let assistant_term =
  let transcribe_keysym_arg =
    Arg.(value & opt (some int) None
         & info ["ptt-transcribe-keysym"] ~docv:"KEYSYM"
             ~doc:"Override the XDG config keysym for PTT transcribe")
  in
  let translate_keysym_arg =
    Arg.(value & opt (some int) None
         & info ["ptt-translate-keysym"] ~docv:"KEYSYM"
             ~doc:"Override the XDG config keysym for PTT translate")
  in
  Term.(
    const (fun ptt_transcribe_keysym ptt_translate_keysym () ->
      run_effect (fun ~env ~sw ->
        Pie.Commands.Assistant.run
          ~sw
          ~net:(Eio.Stdenv.net env)
          ~clock:(Eio.Stdenv.clock env)
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~stdin:(Eio.Stdenv.stdin env)
          ?ptt_transcribe_keysym
          ?ptt_translate_keysym
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
        `P "$(b,msg action ACTION) — send an action to the daemon";
        `P "$(b,meeting-start) — start meeting transcription";
        `P "$(b,meeting-stop) — stop meeting transcription";
        `P "$(b,meeting-toggle) — toggle meeting transcription";
      ]
  in
  Cmd.group ~default:assistant_term info [
    sources_cmd; meter_cmd; record_cmd; type_cmd;
    ptt_portal_cmd; ptt_cmd; ptt_transcribe_cmd; ptt_translate_cmd;
    stt_interactive_cmd; wakeword_cmd; wakeword_tune_cmd; wakeword_train_cmd;
    status_cmd; toggle_cmd; pause_cmd; resume_cmd; msg_cmd;
    meeting_start_cmd; meeting_stop_cmd; meeting_toggle_cmd;
  ]

let () = exit (Cmd.eval default_cmd)
