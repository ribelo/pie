open Cmdliner

let ( let* ) = Result.bind

type cli_error =
  [ `Pulse_error of Pie.Pulse.Client.error
  | `Ptt_cause of [ `Ptt_error of string ] Ox.Cause.t
  | `Ptt_error of string
  | `Stt_error of string
  | `Subprocess_error of string
  | `Clipboard_error of string
  | `Injection_error of string
  | `Wtype_error of string
  | `Xdotool_error of string
  | `Niri_error of string
  | `Notification_error of string
  | `Portal_error of string
  | `Daemon_error of string
  | `History of string
  | `Daemon_protocol_error of string
  | `Daemon_off
  | `Closed
  | `Timeout ]

let render_error = function
  | `Pulse_error error -> Pie.Pulse.Client.error_message error
  | `Ptt_cause cause ->
      Format.asprintf "%a"
        (Ox.Cause.pp_compact (fun fmt (`Ptt_error message) ->
             Format.pp_print_string fmt message))
        cause
  | `Ptt_error msg
  | `Stt_error msg
  | `Subprocess_error msg
  | `Clipboard_error msg
  | `Injection_error msg
  | `Wtype_error msg
  | `Xdotool_error msg
  | `Niri_error msg
  | `Notification_error msg
  | `Portal_error msg
  | `Daemon_error msg
  | `History msg ->
      msg
  | `Daemon_protocol_error label -> label
  | `Daemon_off -> "daemon is not running"
  | `Closed -> "stream closed"
  | `Timeout -> "operation timed out"

let pp_error fmt error = Format.pp_print_string fmt (render_error error)

let render_recording_state
    (snap : Pie.Assistant_service.Contract.recording_snapshot) =
  if not snap.enabled then "paused"
  else if not snap.active then "armed"
  else if snap.mode = "idle" then "stale"
  else snap.mode

let client_build_differs
    (snap : Pie.Assistant_service.Contract.recording_snapshot) =
  snap.build_commit <> Some Pie.Build_identity.commit
  || snap.build_time <> Some Pie.Build_identity.time

let render_recording_status
    (snap : Pie.Assistant_service.Contract.recording_snapshot) =
  let running =
    Printf.sprintf "%s\nbuild: %s (%s)"
      (render_recording_state snap)
      (Option.value snap.build_commit ~default:"unknown")
      (Option.value snap.build_time ~default:"unknown")
  in
  if client_build_differs snap then
    running
    ^ Printf.sprintf "\nclient build: %s (%s)" Pie.Build_identity.commit
        Pie.Build_identity.time
  else running

let json_of_recording_status snap =
  match Pie.Assistant_service.Contract.json_of_snapshot snap with
  | `Assoc fields when client_build_differs snap ->
      `Assoc
        (fields
        @ [
            ("clientBuildCommit", `String Pie.Build_identity.commit);
            ("clientBuildTime", `String Pie.Build_identity.time);
          ])
  | json -> json

let render_action_result (result : Pie.Assistant_service.Contract.action_result)
    =
  match (String.lowercase_ascii result.action, result.message) with
  | "status", _ -> render_recording_status result.snapshot
  | _, Some message -> message
  | _ -> "ok"

let json_string json = Yojson.Safe.to_string json

let print_jsonl ~caps json =
  Ox_caps.Cap_stdlib.printf ~caps "%s\n%!" (json_string json)

let json_of_daemon_error message =
  `Assoc
    [
      ("phase", `String "offline");
      ("active", `Bool false);
      ("mode", `String "offline");
      ("enabled", `Bool false);
      ("startedAt", `Null);
      ("updatedAt", `Null);
      ("transcriptPath", `Null);
      ("lastError", `String message);
      ("buildCommit", `Null);
      ("buildTime", `Null);
      ("clientBuildCommit", `String Pie.Build_identity.commit);
      ("clientBuildTime", `String Pie.Build_identity.time);
    ]

let command name doc term = Cmd.v (Cmd.info name ~doc) term
let startup_config_notice = ref (fun () -> ())

let commands_with_env (env : _ Ox_app.Env.t) door_run_owned =
  (startup_config_notice :=
     fun () ->
       Pie.Desktop.Notification.emit ~caps:env.caps
         ~process_mgr:(Eio.Stdenv.process_mgr env.base)
         Config_rejected "pie: config rejected"
         "Startup configuration was rejected. Check the config and restart Pie.");
  let run_owned operation () =
    door_run_owned (fun sw ->
        Result.map_error (fun error -> (error :> cli_error)) (operation sw))
  in
  let base = env.base in
  let net = Eio.Stdenv.net base in
  let clock = Eio.Stdenv.clock base in
  let logger = env.logger in
  let caps = env.caps in
  let source =
    command "sources" "List PulseAudio capture sources"
      Term.(
        const (fun () ->
            run_owned (fun sw ->
                Pie.Commands.Sources.run ~caps ~sw ~net ~clock ~logger ()))
        $ const ())
  in
  let meter =
    command "meter" "Print live RMS / peak levels for input verification"
      Term.(
        const (fun () ->
            run_owned (fun sw ->
                Pie.Commands.Meter.run ~caps ~sw ~net ~clock ~logger ()))
        $ const ())
  in
  let record =
    let output =
      Arg.(
        required
        & pos 0 (some string) None
        & info [] ~docv:"OUTPUT" ~doc:"Output WAV file path")
    in
    let duration =
      Arg.(
        value & opt float 5.0
        & info [ "d"; "duration" ] ~docv:"SECONDS" ~doc:"Recording duration")
    in
    command "record" "Capture raw PCM audio to a WAV file"
      Term.(
        const (fun output_path duration_sec ->
            run_owned (fun sw ->
                Pie.Commands.Record.run ~caps ~sw ~net ~clock ~logger
                  ~output_path ~duration_sec ()))
        $ output $ duration)
  in
  let status =
    command "status" "Show daemon recording status"
      Term.(
        const (fun () ->
            run_owned (fun sw ->
                match
                  Pie.Assistant_service.Client.status ~caps ~net ~clock ~sw ()
                with
                | Error `Daemon_off ->
                    Ox_caps.Cap_stdlib.printf ~caps "off\n%!";
                    Ok ()
                | Error _ as error -> error
                | Ok snap ->
                    Ox_caps.Cap_stdlib.printf ~caps "%s\n%!"
                      (render_recording_status snap);
                    Ok ()))
        $ const ())
  in
  let toggle =
    command "toggle" "Toggle daemon recording"
      Term.(
        const (fun () ->
            run_owned (fun sw ->
                let* result =
                  Pie.Assistant_service.Client.toggle ~caps ~net ~clock ~sw ()
                in
                Ox_caps.Cap_stdlib.printf ~caps "%s\n%!"
                  (if result then "armed" else "paused");
                Ok ()))
        $ const ())
  in
  let pause =
    command "pause" "Pause daemon recording"
      Term.(
        const (fun () ->
            run_owned (fun sw ->
                Pie.Assistant_service.Client.pause ~caps ~net ~clock ~sw ()))
        $ const ())
  in
  let resume =
    command "resume" "Resume daemon recording"
      Term.(
        const (fun () ->
            run_owned (fun sw ->
                Pie.Assistant_service.Client.resume ~caps ~net ~clock ~sw ()))
        $ const ())
  in
  let retry_id =
    Arg.(
      value
      & pos 0 (some string) None
      & info [] ~docv:"ID"
          ~doc:
            "History id of an archived recording. Omit to retry the newest \
             retained failure.")
  in
  let retry =
    command "retry" "Retry a retained failure, or an archived recording by id"
      Term.(
        const (fun id ->
            let action =
              match id with
              | None -> "retry"
              | Some id -> "retry " ^ String.trim id
            in
            run_owned (fun sw ->
                let* result =
                  Pie.Assistant_service.Client.action ~caps ~net ~clock ~sw
                    action
                in
                if result.Pie.Assistant_service.Contract.accepted then (
                  Ox_caps.Cap_stdlib.printf ~caps "%s\n%!"
                    (Option.value ~default:"retry queued" result.message);
                  Ok ())
                else
                  Error
                    (`Daemon_error
                       (Option.value ~default:"retry was rejected"
                          result.message))))
        $ retry_id)
  in
  let action =
    let name =
      Arg.(
        required
        & pos 0 (some string) None
        & info [] ~docv:"ACTION" ~doc:"Action name")
    in
    command "action" "Send an action to the running daemon"
      Term.(
        const (fun action ->
            run_owned (fun sw ->
                let* result =
                  Pie.Assistant_service.Client.action ~caps ~net ~clock ~sw
                    action
                in
                Ox_caps.Cap_stdlib.printf ~caps "%s\n%!"
                  (render_action_result result);
                Ok ()))
        $ name)
  in
  let msg_status =
    let json =
      Arg.(
        value & flag
        & info [ "json" ] ~doc:"Print the full daemon status as JSON")
    in
    command "status" "Show daemon status through the message API"
      Term.(
        const (fun as_json ->
            run_owned (fun sw ->
                let* snap =
                  Pie.Assistant_service.Client.status ~caps ~net ~clock ~sw ()
                in
                if as_json then
                  print_jsonl ~caps (json_of_recording_status snap)
                else
                  Ox_caps.Cap_stdlib.printf ~caps "%s\n%!"
                    (render_recording_status snap);
                Ok ()))
        $ json)
  in
  let subscribe =
    let interval =
      Arg.(
        value & opt int 500
        & info [ "interval-ms" ] ~docv:"MS"
            ~doc:"Daemon status polling interval for the JSONL stream")
    in
    command "subscribe" "Subscribe to daemon status snapshots as JSONL"
      Term.(
        const (fun interval_ms ->
            run_owned (fun sw ->
                let sleep_seconds =
                  if interval_ms <= 0 then 0.5
                  else float_of_int interval_ms /. 1000.0
                in
                let rec loop last_json =
                  let next_json =
                    match
                      Pie.Assistant_service.Client.status ~caps ~net ~clock ~sw
                        ()
                    with
                    | Ok snap -> json_string (json_of_recording_status snap)
                    | Error error ->
                        json_string (json_of_daemon_error (render_error error))
                  in
                  if Some next_json <> last_json then
                    Ox_caps.Cap_stdlib.printf ~caps "%s\n%!" next_json;
                  Eio.Time.sleep clock sleep_seconds;
                  loop (Some next_json)
                in
                loop None))
        $ interval)
  in
  let take n entries =
    let rec loop n acc = function
      | [] -> List.rev acc
      | _ when n <= 0 -> List.rev acc
      | entry :: rest -> loop (n - 1) (entry :: acc) rest
    in
    loop n [] entries
  in
  let newline_terminated text =
    if text = "" || text.[String.length text - 1] <> '\n' then text ^ "\n"
    else text
  in
  let history_list limit as_json =
    run_owned (fun _sw ->
        let path = Pie.History.path ~caps in
        match Pie.History.read ~caps ~path with
        | Error (`History message) -> Error (`History message)
        | Ok entries ->
            let entries =
              match limit with None -> entries | Some n -> take n entries
            in
            if as_json then
              Ox_caps.Cap_stdlib.printf ~caps "%s\n%!"
                (Yojson.Safe.to_string (Pie.History.json_of_entries entries))
            else
              List.iter
                (fun entry ->
                  Ox_caps.Cap_stdlib.printf ~caps "%s\n%!"
                    (Pie.History.format_listing_line entry))
                entries;
            Ok ())
  in
  let find_entry entries selector =
    match entries with
    | newest :: _ when selector = "last" -> Ok newest
    | _ -> (
        match
          List.find_opt
            (fun (entry : Pie.History.entry) -> String.equal entry.id selector)
            entries
        with
        | Some entry -> Ok entry
        | None -> Error ("unknown history entry " ^ selector))
  in
  let history_show selector ~raw ~as_json =
    run_owned (fun _sw ->
        if raw && as_json then
          Error (`History "--raw and --json cannot be combined")
        else
          let path = Pie.History.path ~caps in
          match Pie.History.read ~caps ~path with
          | Error (`History message) -> Error (`History message)
          | Ok [] -> Error (`History "no history")
          | Ok entries -> (
              match find_entry entries selector with
              | Error message -> Error (`History message)
              | Ok entry -> (
                  if as_json then (
                    Ox_caps.Cap_stdlib.printf ~caps "%s\n%!"
                      (Yojson.Safe.to_string (Pie.History.json_of_entry entry));
                    Ok ())
                  else
                    let text =
                      if raw then entry.raw_text else entry.final_text
                    in
                    match text with
                    | Some text ->
                        Ox_caps.Cap_stdlib.printf ~caps "%s%!"
                          (newline_terminated text);
                        Ok ()
                    | None ->
                        Error
                          (`History
                             (Printf.sprintf
                                (if raw then
                                   "history entry %s has no raw transcript"
                                 else "history entry %s has no final text")
                                entry.id)))))
  in
  let positive_int =
    let parse text =
      match int_of_string_opt text with
      | Some n when n > 0 -> Ok n
      | _ -> Error (`Msg "must be a positive integer")
    in
    Arg.conv (parse, fun ppf n -> Format.pp_print_int ppf n)
  in
  let history_limit =
    Arg.(
      value
      & opt (some positive_int) None
      & info [ "limit" ] ~docv:"N"
          ~doc:"Maximum number of entries to list, newest first")
  in
  let list_json =
    Arg.(value & flag & info [ "json" ] ~doc:"Print a JSON array of entries")
  in
  let show_json =
    Arg.(value & flag & info [ "json" ] ~doc:"Print the entry as a JSON object")
  in
  let show_raw =
    Arg.(
      value & flag
      & info [ "raw" ]
          ~doc:"Print the raw pass-1 transcript instead of the final text")
  in
  let show_id =
    Arg.(
      value & pos 0 string "last"
      & info [] ~docv:"ID"
          ~doc:"Attempt id, or last for the newest entry. Defaults to last.")
  in
  let history_man =
    [
      `S Manpage.s_description;
      `P
        "Lists dictation transcripts, newest first, or prints one entry so it \
         can be piped to the clipboard. History is read from \
         XDG_DATA_HOME/pie/history.jsonl (default \
         ~/.local/share/pie/history.jsonl).";
      `S Manpage.s_examples;
      `Pre "pie history --limit 20";
      `Noblank;
      `Pre "pie history --json";
      `Noblank;
      `Pre "pie history show | wl-copy";
      `Noblank;
      `Pre "pie history show --raw last";
    ]
  in
  let history =
    Cmd.group
      ~default:
        Term.(
          const (fun limit as_json -> history_list limit as_json)
          $ history_limit $ list_json)
      (Cmd.info "history" ~doc:"List and recover dictation transcripts"
         ~man:history_man)
      [
        Cmd.v
          (Cmd.info "show"
             ~doc:"Print one entry's final text, or its raw transcript")
          Term.(
            const (fun selector raw as_json ->
                history_show selector ~raw ~as_json)
            $ show_id $ show_raw $ show_json);
      ]
  in
  let glossary_man =
    [
      `S Manpage.s_description;
      `P
        "Prints the resolved global glossary path, whether the file exists, \
         and its physical line count. The file is read in full; this command \
         does not create or edit it. Missing or unreadable files warn and do \
         not stop dictation.";
      `P
        "Pie uses one glossary for all applications: \
         XDG_CONFIG_HOME/pie/glossary.md (default ~/.config/pie/glossary.md). \
         PIE_GLOSSARY overrides it with another file, not an additional file. \
         Symlinks are followed. Edits apply at the next key press, without a \
         daemon restart; there is no size cap or project lookup.";
      `S "FORMAT";
      `P
        "Write one line per term: correct spelling — how speech recognition \
         tends to mishear it. Separate mishearing variants with commas. Use ## \
         headings for sections; Markdown bullet prefixes are optional.";
      `Pre "## OCaml\n- Eio — ejo, e i o\n- Dune — dun, diun";
      `S "EXTEND FROM A PROJECT";
      `P
        "Ask a coding agent: Inspect this project and extend my global Pie \
         glossary at the path printed by pie glossary. Collect names, \
         identifiers, tools, and commands I say aloud, and how speech \
         recognition mishears each. Use observed errors where available; \
         review likely variants with me. Keep correct spellings, add a ## \
         project-name section, and merge duplicate terms by unioning their \
         mishearing variants on one line. Preserve existing terms and \
         sections. Do not create a project glossary or restart Pie.";
      `S Manpage.s_examples;
      `Pre "pie glossary";
      `Noblank;
      `Pre "PIE_GLOSSARY=/path/to/glossary.md pie glossary";
    ]
  in
  let glossary =
    Cmd.v
      (Cmd.info "glossary"
         ~doc:"Inspect and maintain the global dictation glossary"
         ~man:glossary_man)
      Term.(
        const (fun () ->
            run_owned (fun _sw ->
                let status = Pie.Glossary.status ~caps () in
                Ox_caps.Cap_stdlib.printf ~caps
                  "Path: %s\nExists: %s\nLines: %s\n%!" status.path
                  (match status.exists with
                  | Some true -> "yes"
                  | Some false -> "no"
                  | None -> "unknown")
                  (match status.line_count with
                  | Some count -> string_of_int count
                  | None -> "unavailable");
                List.iter
                  (function
                    | Pie.Glossary.Unreadable { path; message } ->
                        Ox_caps.Cap_stdlib.eprintf ~caps "Warning: %s: %s\n%!"
                          path message)
                  status.warnings;
                Ok ()))
        $ const ())
  in
  let msg =
    Cmd.group
      (Cmd.info "msg" ~doc:"Send a message to the running daemon")
      [ action; msg_status; subscribe ]
  in
  [
    source;
    meter;
    record;
    status;
    toggle;
    pause;
    resume;
    retry;
    history;
    glossary;
    msg;
  ]

let service_args =
  let transcribe =
    Arg.(
      value
      & opt (some int) None
      & info
          [ "ptt-transcribe-keysym" ]
          ~docv:"KEYSYM"
          ~doc:"Override the XDG config keysym for PTT transcription")
  in
  let translate =
    Arg.(
      value
      & opt (some int) None
      & info [ "ptt-translate-keysym" ] ~docv:"KEYSYM"
          ~doc:"Override the XDG config keysym for PTT translate")
  in
  Term.(
    const (fun transcribe translate -> (transcribe, translate))
    $ transcribe $ translate)

let validate_source env source _config =
  let caps = env.Ox_app.Env.caps in
  match
    Pie.Config.Stt.load ~caps ~fs:(Eio.Stdenv.fs env.Ox_app.Env.base) ~source ()
  with
  | Ok _ -> Ok ()
  | Error (`Config message) -> Error [ { Ox_app.Config.pointer = ""; message } ]

let () =
  exit
    (Ox_app.run_with_args ~name:"pie" ~service_command:"assistant"
       ~version:(Pie.Build_identity.commit ^ " " ^ Pie.Build_identity.time)
       ~diagnostic:(fun ~kind ~message ->
         (match kind with
         | `Config | `Validation -> !startup_config_notice ()
         | _ -> ());
         message)
       ~config_file:"stt.toml" ~missing:`Required
       ~config:Pie.Config.Stt.door_spec ~service_args
       ~decode_with_env:(fun env json ->
         let warn message =
           Ox_observability.Logger.warn env.Ox_app.Env.logger message ~fields:[]
         in
         Pie.Config.Stt.decode ~warn json)
       ~read_file:(fun env path ->
         Ox_app.Config.read_regular
           ~max_bytes:Pie.Config.Stt.max_config_file_bytes
           Eio.Path.(Eio.Stdenv.fs env / path))
       ~validate_source
       ~telemetry:(fun ~sw env config -> Pie.Process.telemetry ~sw env config)
       ~commands_with_env ~pp_err:pp_error
       (fun env
            source
            config
            (ptt_transcribe_keysym, ptt_translate_keysym)
            sw
          ->
         let base = env.Ox_app.Env.base in
         Result.map_error
           (fun error -> (error :> cli_error))
           (Pie.Assistant.Service.run ~caps:env.caps ~sw
              ~fs:(Eio.Stdenv.fs base) ~net:(Eio.Stdenv.net base)
              ~clock:(Eio.Stdenv.clock base)
              ~mono_clock:(Eio.Stdenv.mono_clock base)
              ~logger:env.logger
              ~process_mgr:(Eio.Stdenv.process_mgr base)
              ~config ~source ?ptt_transcribe_keysym ?ptt_translate_keysym ())))
