open Eta.Syntax

let ptt_error msg = `Ptt_error msg
let ptt_of_stt (`Stt_error msg) = ptt_error msg
let ptt_of_niri (`Niri_error msg) = ptt_error msg
let ptt_of_injection = function
  | `Injection_error msg
  | `Wtype_error msg
  | `Xdotool_error msg -> ptt_error msg
let stt_of_ptt (`Ptt_error msg) = `Stt_error msg

let default_transcribe_keysym = Keyboard.Monitor.keysym_of_name "F9"
let default_translate_keysym = Keyboard.Monitor.keysym_of_name "F10"
let default_min_duration_ms = 120

type ptt_mode =
  | Transcribe
  | Translate

let mode_name = function
  | Transcribe -> "transcribe"
  | Translate -> "translate"

let recording_mode = function
  | Transcribe -> Recording.Coordinator.Ptt_transcribe
  | Translate -> Recording.Coordinator.Ptt_translate

let recognize ~transcribe_keysym ~translate_keysym event =
  let phase mode =
    if event.Keyboard.Monitor.released then
      Some (Ptt_loop.Release (mode_name mode))
    else
      Some (Ptt_loop.Press (mode_name mode))
  in
  if event.keysym = transcribe_keysym then phase Transcribe
  else if event.keysym = translate_keysym then phase Translate
  else None

let mode_of_name = function
  | "transcribe" -> Transcribe
  | "translate" -> Translate
  | mode -> invalid_arg ("unknown assistant PTT mode: " ^ mode)

let normalize_for_stt (config : Config.Stt.t) pcm_bytes =
  (Audio.Pcm.normalize_pcm_for_stt
     ~silence_rms_threshold:config.ptt_min_capture_rms
     ~silence_peak_threshold:config.ptt_min_capture_peak
     pcm_bytes).normalized_bytes

let context_value_max_length = 200

let normalize_space value =
  let buf = Buffer.create (String.length value) in
  let pending_space = ref false in
  let flush_space () =
    if !pending_space && Buffer.length buf > 0 then Buffer.add_char buf ' ';
    pending_space := false
  in
  String.iter
    (function
      | ' ' | '\n' | '\r' | '\t' -> pending_space := true
      | c ->
        flush_space ();
        Buffer.add_char buf c)
    value;
  Buffer.contents buf |> String.trim

let prompt_value = function
  | None -> None
  | Some value ->
    let normalized = normalize_space value in
    if normalized = "" then None
    else if String.length normalized > context_value_max_length then
      Some (String.sub normalized 0 context_value_max_length)
    else Some normalized

let append_focused_window_context prompt_template focused_window =
  match focused_window with
  | None -> prompt_template
  | Some window ->
    let lines =
      [
        Option.map
          (fun app_id -> "- app_id: " ^ app_id)
          (prompt_value window.Niri.app_id);
        Option.map
          (fun title -> "- title: " ^ title)
          (prompt_value window.Niri.title);
      ]
      |> List.filter_map Fun.id
    in
    if lines = [] then prompt_template
    else
      prompt_template
      ^ "\n\nFocused window context:\n"
      ^ String.concat "\n" lines
      ^ "\nUse this as context only when it helps disambiguate the audio."

let prompt_with_focused_window_context ~process_mgr prompt_template =
  let* focused_window = Niri.focused_window ~process_mgr () in
  Eta.Effect.sync (fun () ->
    let prompt_template =
      append_focused_window_context prompt_template focused_window
    in
    let project_context =
      match Option.bind focused_window (fun window -> window.Niri.pid) with
      | None -> None
      | Some pid -> Context.Project.load_for_pid pid
    in
    Context.Project.append_to_prompt prompt_template project_context)

let load_config () =
  Eta.Effect.sync (fun () ->
    try Ok (Config.Stt.load ())
    with Config.Stt.Config_error msg -> Error ("Failed to load STT config: " ^ msg))
  |> Eta.Effect.map (Result.map_error (fun msg -> `Pulse_error msg))
  |> Eta.Effect.bind Eta.Effect.from_result

let run_stt ~sw ~net ~process_mgr (config : Config.Stt.t) mode pcm_bytes =
  let* prompt_template =
    match mode with
    | Transcribe ->
      prompt_with_focused_window_context ~process_mgr config.transcription_prompt
    | Translate ->
      prompt_with_focused_window_context ~process_mgr config.translation_prompt
  in
  match config.provider with
  | Codex_realtime ->
    let pcm_bytes = normalize_for_stt config pcm_bytes in
    (match mode with
     | Transcribe ->
       Stt.Codex_realtime.transcribe_with_prompt
         ~sw
         ~net
         ~model:config.transcription_model
         ~language:config.transcription_language
         ~prompt_template
         pcm_bytes
     | Translate ->
       Stt.Codex_realtime.translate_with_prompt
         ~sw
         ~net
         ~model:config.translation_model
         ~source_language:config.translation_source_language
         ~target_language:config.translation_target_language
         ~prompt_template
         pcm_bytes)
  | Openrouter ->
    let pcm_bytes = normalize_for_stt config pcm_bytes in
    (match mode with
     | Transcribe ->
       Stt.Openrouter.transcribe_with_prompt
         ~process_mgr
         ~model:config.transcription_model
         ~language:config.transcription_language
         ~prompt_template
         pcm_bytes
     | Translate ->
       Stt.Openrouter.translate_with_prompt
         ~process_mgr
         ~model:config.translation_model
         ~source_language:config.translation_source_language
         ~target_language:config.translation_target_language
         ~prompt_template
         pcm_bytes)

let inject_and_report ~process_mgr ~newline_rules mode text =
  let normalized = Input.Text_injection.normalize_text text in
  if String.length normalized = 0 then
    let* () =
      Desktop.Notification.notify_warning
        ~process_mgr
        "pie: no transcript"
        "Speech-to-text produced no transcript text. Try speaking louder or checking microphone input."
      |> Eta.Effect.catch (fun _ -> Eta.Effect.unit)
    in
    Eta.Effect.sync (fun () ->
      Printf.printf "%s: <empty>\n%!"
        (if mode = Transcribe then "Transcript" else "Translation"))
  else
    let* result =
      Input.Text_injection.inject ~newline_rules ~process_mgr normalized
      |> Eta.Effect.map_error ptt_of_injection
    in
    Eta.Effect.sync (fun () ->
      Printf.printf "%s: %s\n%!"
        (if mode = Transcribe then "Transcript" else "Translation")
        normalized;
      Printf.printf "Typed %d chars via %s (%s)\n%!"
        (String.length result.text)
        (match result.backend with | Wtype -> "wtype" | Xdotool -> "xdotool")
        (match result.session_type with | Wayland -> "Wayland" | X11 -> "X11"))

let make_clip_handle ~sw ~net ~process_mgr ~coordinator ~config mode =
  let recording_mode = recording_mode mode in
  {
    Ptt_loop.offer = (fun _chunk -> Eta.Effect.unit);
    finish = (fun clip ->
      let* _ = Eta.Effect.sync (fun () -> Recording.Coordinator.stop coordinator recording_mode) in
      let* text =
        run_stt ~sw ~net ~process_mgr config mode clip.Ptt_loop.pcm_bytes
        |> Eta.Effect.map_error (function
          | `Stt_error msg
          | `Subprocess_error msg
          | `Niri_error msg
          | `Ptt_error msg -> `Ptt_error msg)
      in
      inject_and_report ~process_mgr ~newline_rules:config.Config.Stt.newline_rules mode text);
    cancel = Eta.Effect.sync (fun () -> ignore (Recording.Coordinator.stop coordinator recording_mode));
  }

let make_streaming_transcribe_handle ~sw ~net ~process_mgr ~coordinator ~config =
  let recording_mode = recording_mode Transcribe in
  Eta.Effect.pure
    (Ptt_loop.Scoped
       (fun use ->
          let* prompt_template =
            prompt_with_focused_window_context ~process_mgr config.Config.Stt.transcription_prompt
            |> Eta.Effect.map_error ptt_of_niri
          in
          Stt.Codex_realtime.with_transcription_stream
            ~sw
            ~net
            ~model:config.transcription_model
            ~language:config.transcription_language
            ~prompt_template
            (fun stream ->
               let handle =
                 {
                   Ptt_loop.offer =
                     (fun chunk ->
                        stream.offer_chunk chunk
                        |> Eta.Effect.map_error ptt_of_stt);
                   finish =
                     (fun _clip ->
                        let* _ =
                          Eta.Effect.sync (fun () ->
                            Recording.Coordinator.stop coordinator recording_mode)
                        in
                        let* text =
                          stream.finish_stream ()
                          |> Eta.Effect.map_error ptt_of_stt
                        in
                        inject_and_report
                          ~process_mgr
                          ~newline_rules:config.Config.Stt.newline_rules
                          Transcribe
                          text);
                   cancel =
                     let* _ =
                       Eta.Effect.sync (fun () ->
                         Recording.Coordinator.stop coordinator recording_mode)
                     in
                     stream.cancel_stream
                     |> Eta.Effect.map_error ptt_of_stt;
                 }
               in
               use handle
               |> Eta.Effect.map_error stt_of_ptt)
          |> Eta.Effect.map_error ptt_of_stt))

let make_streaming_translate_handle ~sw ~net ~process_mgr ~coordinator ~config =
  let recording_mode = recording_mode Translate in
  Eta.Effect.pure
    (Ptt_loop.Scoped
       (fun use ->
          let* prompt_template =
            prompt_with_focused_window_context ~process_mgr config.Config.Stt.translation_prompt
            |> Eta.Effect.map_error ptt_of_niri
          in
          Stt.Codex_realtime.with_translation_stream
            ~sw
            ~net
            ~model:config.translation_model
            ~source_language:config.translation_source_language
            ~target_language:config.translation_target_language
            ~prompt_template
            (fun stream ->
               let handle =
                 {
                   Ptt_loop.offer =
                     (fun chunk ->
                        stream.offer_chunk chunk
                        |> Eta.Effect.map_error ptt_of_stt);
                   finish =
                     (fun _clip ->
                        let* _ =
                          Eta.Effect.sync (fun () ->
                            Recording.Coordinator.stop coordinator recording_mode)
                        in
                        let* text =
                          stream.finish_stream ()
                          |> Eta.Effect.map_error ptt_of_stt
                        in
                        inject_and_report
                          ~process_mgr
                          ~newline_rules:config.Config.Stt.newline_rules
                          Translate
                          text);
                   cancel =
                     let* _ =
                       Eta.Effect.sync (fun () ->
                         Recording.Coordinator.stop coordinator recording_mode)
                     in
                     stream.cancel_stream
                     |> Eta.Effect.map_error ptt_of_stt;
                 }
               in
               use handle
               |> Eta.Effect.map_error stt_of_ptt)
          |> Eta.Effect.map_error ptt_of_stt))

let make_handle ~sw ~net ~process_mgr ~coordinator ~config mode =
  match config.Config.Stt.provider, mode with
  | Codex_realtime, Transcribe ->
    make_streaming_transcribe_handle ~sw ~net ~process_mgr ~coordinator ~config
  | Codex_realtime, Translate ->
    make_streaming_translate_handle ~sw ~net ~process_mgr ~coordinator ~config
  | _ ->
    Eta.Effect.pure
      (Ptt_loop.Handle (make_clip_handle ~sw ~net ~process_mgr ~coordinator ~config mode))

let on_press ~sw ~net ~process_mgr ~coordinator ~config mode_name =
  let mode = mode_of_name mode_name in
  let recording_mode = recording_mode mode in
  match Recording.Coordinator.try_start coordinator recording_mode () with
  | Started _ ->
    make_handle ~sw ~net ~process_mgr ~coordinator ~config mode
    |> Eta.Effect.map_error (function
      | `Stt_error msg
      | `Niri_error msg
      | `Ptt_error msg -> `Ptt_error msg)
  | Busy active ->
    Eta.Effect.sync (fun () ->
      Printf.eprintf "PTT ignored: %s is active\n%!" (Recording.Coordinator.string_of_mode active))
    |> Eta.Effect.map (fun () -> Ptt_loop.Skip)
  | Disabled ->
    Eta.Effect.sync (fun () -> Printf.eprintf "PTT ignored: PIE is paused\n%!")
    |> Eta.Effect.map (fun () -> Ptt_loop.Skip)

let run
    ~sw
    ~net
    ~clock
    ~process_mgr
    ?(ptt_transcribe_keysym = default_transcribe_keysym)
    ?(ptt_translate_keysym = default_translate_keysym)
    ?(min_duration_ms = default_min_duration_ms)
    ?stdin:_stdin
    () =
  let* config = load_config () in
  let coordinator = Recording.Coordinator.create () in
  let* () = Eta.Effect.sync (fun () ->
    Daemon.Server.start ~sw ~net coordinator;
    Printf.printf
      "pie assistant mode: hold F9 to transcribe, F10 to translate (Ctrl-C to exit)\n%!")
  in
  Pulse.Client.with_client ~sw ~net ~clock @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun info recv ->
  Keyboard.Monitor.with_monitor ~sw ~net ~process_mgr @@ fun keyboard ->
  let ptt_config : Ptt_loop.config = {
    recognize = recognize ~transcribe_keysym:ptt_transcribe_keysym ~translate_keysym:ptt_translate_keysym;
    min_duration_ms;
    min_capture_rms = config.Config.Stt.ptt_min_capture_rms;
    min_capture_peak = config.Config.Stt.ptt_min_capture_peak;
    log_prefix = (fun mode -> "assistant-ptt-" ^ mode);
    source = info.Pulse.Defs.source_name;
    on_ready = Eta.Effect.sync (fun () ->
      Printf.printf "PTT armed: F9=%d F10=%d source=%s\n%!"
        ptt_transcribe_keysym
        ptt_translate_keysym
        (Option.value ~default:"<default>" info.Pulse.Defs.source_name));
    on_press = on_press ~sw ~net ~process_mgr ~coordinator ~config;
    on_release = (fun _ -> Eta.Effect.unit);
    on_abort = Eta.Effect.sync (fun () -> not (Recording.Coordinator.snapshot coordinator).enabled);
    on_dead_input = Desktop.Notification.notify_warning
      ~process_mgr
      "pie: no microphone input"
      "No input detected during push-to-talk. Your microphone may be muted.";
  } in
  Ptt_loop.run ~keyboard ~recv ptt_config
  |> Eta.Effect.map_error (fun (`Ptt_error msg) -> `Pulse_error msg)
