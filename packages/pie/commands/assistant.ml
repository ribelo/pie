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

type ptt_binding = {
  keysym : int;
  shift : bool;
}

let shift_modifier_mask = 1 lsl 0

let mode_name = function
  | Transcribe -> "transcribe"
  | Translate -> "translate"

let recording_mode = function
  | Transcribe -> Recording.Coordinator.Ptt_transcribe
  | Translate -> Recording.Coordinator.Ptt_translate

let binding_matches binding event =
  let shift = event.Keyboard.Monitor.state land shift_modifier_mask <> 0 in
  event.keysym = binding.keysym && shift = binding.shift

let recognize ~transcribe_binding ~translate_binding event =
  let phase mode =
    if event.Keyboard.Monitor.released then
      Some (Ptt_loop.Release (mode_name mode))
    else
      Some (Ptt_loop.Press (mode_name mode))
  in
  if binding_matches transcribe_binding event then phase Transcribe
  else if binding_matches translate_binding event then phase Translate
  else None

let make_live_recognizer ~load_bindings =
  let held = Hashtbl.create 2 in
  fun event ->
    if event.Keyboard.Monitor.released then
      match Hashtbl.find_opt held event.keycode with
      | None -> None
      | Some mode ->
        Hashtbl.remove held event.keycode;
        Some (Ptt_loop.Release mode)
    else
      match Hashtbl.find_opt held event.keycode with
      | Some mode -> Some (Ptt_loop.Press mode)
      | None ->
        let transcribe_binding, translate_binding = load_bindings () in
        match recognize ~transcribe_binding ~translate_binding event with
        | Some (Ptt_loop.Press mode) as trigger ->
          Hashtbl.add held event.keycode mode;
          trigger
        | trigger -> trigger

let effective_bindings
    ?ptt_transcribe_keysym
    ?ptt_translate_keysym
    (config : Config.Stt.t) =
  let transcribe =
    Option.value ~default:config.ptt_transcribe_keysym ptt_transcribe_keysym
  in
  let translate =
    Option.value ~default:config.ptt_translate_keysym ptt_translate_keysym
  in
  if transcribe <= 0 then
    raise (Config.Stt.Config_error "effective PTT transcribe keysym must be positive");
  if translate <= 0 then
    raise (Config.Stt.Config_error "effective PTT translate keysym must be positive");
  let transcribe_binding = { keysym = transcribe; shift = config.ptt_transcribe_shift } in
  let translate_binding = { keysym = translate; shift = config.ptt_translate_shift } in
  if transcribe_binding = translate_binding then
    raise (Config.Stt.Config_error "effective PTT transcribe and translate bindings must differ");
  transcribe_binding, translate_binding

let mode_of_name = function
  | "transcribe" -> Transcribe
  | "translate" -> Translate
  | mode -> invalid_arg ("unknown assistant PTT mode: " ^ mode)

let normalize_for_stt (config : Config.Stt.t) pcm_bytes =
  (Audio.Pcm.normalize_pcm_for_stt
     ~silence_rms_threshold:config.ptt_min_capture_rms
     ~silence_peak_threshold:config.ptt_min_capture_peak
     pcm_bytes).normalized_bytes

let config_model (config : Config.Stt.t) = function
  | Transcribe -> config.transcription_model
  | Translate -> config.translation_model

let resolved_model_for_mode (config : Config.Stt.t) mode =
  Config.Stt.resolve_model_provider
    ~default_provider:config.provider
    (config_model config mode)

let option_json f = function
  | None -> `Null
  | Some value -> f value

let resolved_model_json (resolved : Config.Stt.resolved_model) =
  `Assoc [
    ("provider", `String (Config.Stt.string_of_provider resolved.provider));
    ("model", `String resolved.model);
  ]

let configured_model_json config mode =
  let configured = config_model config mode in
  let resolved = resolved_model_for_mode config mode in
  `Assoc [
    ("configured", `String configured);
    ("resolved", resolved_model_json resolved);
  ]

let newline_rule_json index (rule : Config.Stt.newline_rule) =
  `Assoc [
    ("index", `Int index);
    ("app_id", option_json (fun value -> `String value) rule.app_id);
    ("title_regex", option_json (fun value -> `String value) rule.title_regex);
    ("action", `String (Config.Stt.string_of_newline_action rule.action));
  ]

let startup_config_json
    ?(config_path = Paths.stt_config_path)
    ~(config : Config.Stt.t)
    ~ptt_transcribe_keysym
    ~ptt_translate_keysym
    ~min_duration_ms
    () =
  `Assoc [
    ("config_path", `String config_path);
    ("provider", `String (Config.Stt.string_of_provider config.provider));
    ("transcription_model", configured_model_json config Transcribe);
    ("translation_model", configured_model_json config Translate);
    ("thinking_effort", option_json (fun value -> `String value) config.thinking_effort);
    ("transcription_language", `String config.transcription_language);
    ("translation_source_language", `String config.translation_source_language);
    ("translation_target_language", `String config.translation_target_language);
    ("wakeword_enabled", `Bool config.wakeword_enabled);
    ("wakeword_dictation_silence_seconds", `Float config.wakeword_dictation_silence_seconds);
    ("wakeword_dictation_max_seconds", `Float config.wakeword_dictation_max_seconds);
    ("wakeword_dictation_speech_rms_threshold", `Float config.wakeword_dictation_speech_rms_threshold);
    ("ptt_min_duration_ms", `Int min_duration_ms);
    ("ptt_min_capture_rms", `Float config.ptt_min_capture_rms);
    ("ptt_min_capture_peak", `Float config.ptt_min_capture_peak);
    ("ptt_transcribe_keysym", `Int ptt_transcribe_keysym);
    ("ptt_translate_keysym", `Int ptt_translate_keysym);
    ("ptt_transcribe_shift", `Bool config.ptt_transcribe_shift);
    ("ptt_translate_shift", `Bool config.ptt_translate_shift);
    ("transcription_prompt", `String config.transcription_prompt);
    ("translation_prompt", `String config.translation_prompt);
    ("newline_rules", `List (List.mapi newline_rule_json config.newline_rules));
  ]
  |> Yojson.Safe.pretty_to_string

let context_value_max_length = 200

(* Truncate [s] to at most [max_bytes] bytes without splitting a multi-byte
   UTF-8 sequence. If the byte at the cut point is a UTF-8 continuation byte
   (0b10xxxxxx), back off to the start of that character so the result is
   always a valid encoding. *)
let truncate_utf8 s max_bytes =
  if String.length s <= max_bytes then s
  else begin
    let i = ref max_bytes in
    while !i > 0 && Char.code s.[!i] land 0xC0 = 0x80 do
      decr i
    done;
    String.sub s 0 !i
  end

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
      Some (truncate_utf8 normalized context_value_max_length)
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
    let project_cwd =
      match Option.bind focused_window (fun window -> window.Niri.pid) with
      | None -> None
      | Some pid -> Context.Project.cwd_for_pid pid
    in
    let prompt_template =
      project_cwd
      |> Option.map Context.Metadata.load_for_cwd
      |> Option.value ~default:[]
      |> Context.Metadata.append_to_prompt prompt_template
    in
    let project_context =
      Option.bind project_cwd Context.Project.load_for_cwd
    in
    Context.Project.append_to_prompt prompt_template project_context)

let load_config () =
  Eta.Effect.sync (fun () ->
      try Ok (Config.Stt.load ())
      with Config.Stt.Config_error msg ->
        Error
          (`Pulse_error
             (Pulse.Client.Protocol_error
                ("Failed to load STT config: " ^ msg))))
  |> Eta.Effect.flatten_result

let run_stt ~sw ~net ~process_mgr (config : Config.Stt.t) mode pcm_bytes =
  let* prompt_template =
    match mode with
    | Transcribe ->
      prompt_with_focused_window_context ~process_mgr config.transcription_prompt
    | Translate ->
      prompt_with_focused_window_context ~process_mgr config.translation_prompt
  in
  let resolved_model = resolved_model_for_mode config mode in
  match resolved_model.provider with
  | Codex_realtime ->
    let pcm_bytes = normalize_for_stt config pcm_bytes in
    (match mode with
     | Transcribe ->
       Stt.Codex_realtime.transcribe_with_prompt
         ~sw
         ~net
         ~model:resolved_model.model
         ~language:config.transcription_language
         ~prompt_template
         pcm_bytes
     | Translate ->
       Stt.Codex_realtime.translate_with_prompt
         ~sw
         ~net
         ~model:resolved_model.model
         ~source_language:config.translation_source_language
         ~target_language:config.translation_target_language
         ~prompt_template
         pcm_bytes)
  | Openrouter ->
    let pcm_bytes = normalize_for_stt config pcm_bytes in
    (match mode with
     | Transcribe ->
       Stt.Openrouter.transcribe_with_prompt
         ~sw
         ~net
         ~model:resolved_model.model
         ~language:config.transcription_language
         ~prompt_template
         ?thinking_effort:config.thinking_effort
         pcm_bytes
     | Translate ->
       Stt.Openrouter.translate_with_prompt
         ~sw
         ~net
         ~model:resolved_model.model
         ~source_language:config.translation_source_language
         ~target_language:config.translation_target_language
         ~prompt_template
         ?thinking_effort:config.thinking_effort
         pcm_bytes)

let inject_and_report ~process_mgr ~newline_rules mode text =
  let normalized = Input.Text_injection.normalize_text text in
  if String.length normalized = 0 then
    let* () =
      Desktop.Notification.notify_warning
        ~process_mgr
        "pie: no transcript"
        "Speech-to-text produced no transcript text. Try speaking louder or checking microphone input."
      |> Eta.Effect.ignore_errors
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
  let model = (resolved_model_for_mode config Transcribe).model in
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
            ~model
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
  let model = (resolved_model_for_mode config Translate).model in
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
            ~model
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
  match (resolved_model_for_mode config mode).provider, mode with
  | Codex_realtime, Transcribe ->
    make_streaming_transcribe_handle ~sw ~net ~process_mgr ~coordinator ~config
  | Codex_realtime, Translate ->
    make_streaming_translate_handle ~sw ~net ~process_mgr ~coordinator ~config
  | _ ->
    Eta.Effect.pure
      (Ptt_loop.Handle (make_clip_handle ~sw ~net ~process_mgr ~coordinator ~config mode))

let on_press ~sw ~net ~process_mgr ~coordinator ~current_config mode_name =
  let mode = mode_of_name mode_name in
  let recording_mode = recording_mode mode in
  let config = current_config () in
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

let action_result coordinator action ?message accepted =
  let snap = Daemon.Server.snapshot_of_coordinator coordinator in
  {
    Daemon.Contract.action;
    accepted;
    message;
    snapshot = snap;
  }

let send_control control trigger =
  Eio.Stream.add control trigger

let action_handler ~control ~coordinator raw_action =
  let action = raw_action |> String.trim |> String.lowercase_ascii in
  let enqueue trigger message =
    send_control control trigger;
    action_result coordinator raw_action ?message true
  in
  match action with
  | "status" -> action_result coordinator raw_action true
  | "pause" ->
    Recording.Coordinator.set_enabled coordinator false;
    action_result coordinator raw_action true
  | "resume" ->
    Recording.Coordinator.set_enabled coordinator true;
    action_result coordinator raw_action true
  | "toggle" ->
    ignore (Recording.Coordinator.toggle_enabled coordinator);
    action_result coordinator raw_action true
  | "ptt-start" | "dictation-start" ->
    enqueue (Ptt_loop.Press "transcribe") (Some "ptt transcribe start queued")
  | "ptt-stop" | "dictation-stop" ->
    enqueue (Ptt_loop.Release "transcribe") (Some "ptt transcribe stop queued")
  | "ptt-toggle" | "dictation-toggle" ->
    let snap = Recording.Coordinator.snapshot coordinator in
    if
      snap.active
      && snap.mode
         = Recording.Coordinator.string_of_mode Recording.Coordinator.Ptt_transcribe
    then
      enqueue (Ptt_loop.Release "transcribe") (Some "ptt transcribe stop queued")
    else
      enqueue (Ptt_loop.Press "transcribe") (Some "ptt transcribe start queued")
  | "ptt-translate-start" ->
    enqueue (Ptt_loop.Press "translate") (Some "ptt translate start queued")
  | "ptt-translate-stop" ->
    enqueue (Ptt_loop.Release "translate") (Some "ptt translate stop queued")
  | "ptt-translate-toggle" ->
    let snap = Recording.Coordinator.snapshot coordinator in
    if
      snap.active
      && snap.mode
         = Recording.Coordinator.string_of_mode Recording.Coordinator.Ptt_translate
    then
      enqueue (Ptt_loop.Release "translate") (Some "ptt translate stop queued")
    else
      enqueue (Ptt_loop.Press "translate") (Some "ptt translate start queued")
  | "meeting-start" ->
    let result =
      Recording.Coordinator.try_start coordinator Recording.Coordinator.Meeting_transcribe ()
    in
    let accepted =
      match result with
      | Started _ -> true
      | Busy _ | Disabled -> false
    in
    action_result coordinator raw_action accepted
  | "meeting-stop" ->
    let stopped =
      Recording.Coordinator.stop coordinator Recording.Coordinator.Meeting_transcribe
    in
    action_result coordinator raw_action stopped
  | "meeting-toggle" ->
    ignore (Recording.Coordinator.toggle_meeting coordinator);
    action_result coordinator raw_action true
  | _ ->
    let message = Some ("unknown action: " ^ raw_action) in
    action_result coordinator raw_action
      ?message
      false

let run
    ~sw
    ~net
    ~clock
    ~process_mgr
    ?ptt_transcribe_keysym
    ?ptt_translate_keysym
    ?(min_duration_ms = default_min_duration_ms)
    ?stdin
    () =
  Assistant_shim.Service.run
    ~sw
    ~net
    ~clock
    ~process_mgr
    ?ptt_transcribe_keysym
    ?ptt_translate_keysym
    ~min_duration_ms
    ?stdin
    ()
