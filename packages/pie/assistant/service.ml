open Eta.Syntax

(* Assistant service orchestrated via Eta_component *)

let default_min_duration_ms = 120

let pp_admission_error ppf = function
  | Eta_component.Context.Context_not_running -> Format.pp_print_string ppf "Context_not_running"
  | Duplicate_entry_id id -> Format.fprintf ppf "Duplicate_entry_id:%a" Eta_component.Entry_id.pp id
  | Entry_kind_changed id -> Format.fprintf ppf "Entry_kind_changed:%a" Eta_component.Entry_id.pp id
  | Duplicate_provider { coeffect; realm; entries } ->
    Format.fprintf ppf "Duplicate_provider:%s:%s:%s" coeffect realm (String.concat "," (List.map (fun id -> Format.asprintf "%a" Eta_component.Entry_id.pp id) entries))
  | Dependency_cycle ids ->
    Format.fprintf ppf "Dependency_cycle:%s" (String.concat "," (List.map (fun id -> Format.asprintf "%a" Eta_component.Entry_id.pp id) ids))
  | Callback_failed { callback; failure } ->
    let cb = match callback with Configuration_equivalence -> "Configuration_equivalence" | Interception_merge s -> "Interception_merge:" ^ s in
    Format.fprintf ppf "Callback_failed:%s:%a" cb Eta_component.Diagnostics.Failure.pp failure
  | Retry_not_available id -> Format.fprintf ppf "Retry_not_available:%a" Eta_component.Entry_id.pp id
  | Stale_source_revision src -> Format.fprintf ppf "Stale_source_revision:%a" Eta_component.Source_revision.pp src
  | Stale_entry_incarnation id -> Format.fprintf ppf "Stale_entry_incarnation:%a" Eta_component.Entry_id.pp id
  | Stale_target_revision id -> Format.fprintf ppf "Stale_target_revision:%a" Eta_component.Entry_id.pp id
  | Wrong_target_context id -> Format.fprintf ppf "Wrong_target_context:%a" Eta_component.Entry_id.pp id
  | Component_identity_mismatch id -> Format.fprintf ppf "Component_identity_mismatch:%a" Eta_component.Entry_id.pp id
  | Quarantined_instance id -> Format.fprintf ppf "Quarantined_instance:%a" Eta_component.Entry_id.pp id

(* Global state for components_field reporting *)
let admission_error_ref : string option ref = ref None
let last_fence_report_ref : Eta_component.Diagnostics.Fence.report option ref = ref None
let components_cache_ref : Assistant_service.Contract.components_field option ref = ref None

let phase_to_string = function
  | Eta_component.Diagnostics.Inactive -> "inactive"
  | Waiting -> "waiting"
  | Activating _ -> "activating"
  | Active _ -> "active"
  | Settling _ -> "settling"
  | Activation_failed (_, _) -> "activation-failed"
  | Recovery_failed (_, _) -> "recovery-failed"

let integrity_to_string = function
  | Eta_component.Diagnostics.Sound -> "Sound"
  | Degraded ids ->
    "Degraded:" ^ String.concat "," (List.map (fun id -> Format.asprintf "%a" Eta_component.Diagnostics.Instance_id.pp id) ids)
  | Failed failure ->
    Format.asprintf "Failed:%a" Eta_component.Diagnostics.Failure.pp failure

let build_components_field snapshot =
  let entries =
    snapshot
    |> Eta_component.Diagnostics.instances
    |> List.map (fun inst ->
      let id = Format.asprintf "%a" Eta_component.Entry_id.pp (Eta_component.Diagnostics.entry_id inst) in
      let phase = phase_to_string (Eta_component.Diagnostics.phase inst) in
      (id, phase))
  in
  let integrity = integrity_to_string (Eta_component.Diagnostics.integrity snapshot) in
  let degraded =
    snapshot
    |> Eta_component.Diagnostics.instances
    |> List.filter (fun inst ->
      match Eta_component.Diagnostics.phase inst with
      | Eta_component.Diagnostics.Active _ -> false
      | _ -> true)
    |> List.map (fun inst -> Format.asprintf "%a" Eta_component.Entry_id.pp (Eta_component.Diagnostics.entry_id inst))
  in
  let failure =
    match Eta_component.Diagnostics.integrity snapshot with
    | Failed f ->
      Some (Format.asprintf "%a" Eta_component.Diagnostics.Failure.pp f)
    | Degraded _ | Sound -> None
  in
  let fence_outcome, fence_complete =
    match !last_fence_report_ref with
    | None -> None, None
    | Some report ->
      let outcome =
        match Eta_component.Diagnostics.Fence.outcome report with
        | Quiescent -> "Quiescent"
        | Superseded -> "Superseded"
        | Rolled_back -> "Rolled_back"
        | Degraded -> "Degraded"
        | Restoration_failed -> "Restoration_failed"
        | Context_failed -> "Context_failed"
      in
      Some outcome, Some true
  in
  let admission_error = !admission_error_ref in
  ({
    entries;
    integrity;
    degraded;
    failure;
    fence_outcome;
    fence_complete;
    admission_error;
  } : Assistant_service.Contract.components_field)

let update_components_cache snapshot =
  let field = build_components_field snapshot in
  components_cache_ref := Some field;
  Assistant_service.Server.set_components_provider (fun () -> !components_cache_ref)

(* Config watcher helpers, similar to Commands.Assistant *)
let config_path = Paths.stt_config_path

let watched_paths () =
  let prompts_dir = Filename.concat (Filename.dirname config_path) "prompts" in
  [
    config_path;
    Filename.concat prompts_dir "transcription.md";
    Filename.concat prompts_dir "translation.md";
  ]

let stamp path =
  try
    let stat = Unix.stat path in
    Some (stat.Unix.st_dev, stat.Unix.st_ino, stat.Unix.st_size, stat.Unix.st_mtime)
  with Unix.Unix_error _ -> None

let config_stamp () = List.map stamp (watched_paths ())

let load_snapshot attempts =
  let before = config_stamp () in
  let config = Config.Stt.load ~config_path () in
  let after = config_stamp () in
  if before = after then config, after
  else if attempts > 0 then
    let rec retry n =
      let b = config_stamp () in
      let c = Config.Stt.load ~config_path () in
      let a = config_stamp () in
      if b = a then c, a else if n > 0 then retry (n-1) else raise (Config.Stt.Config_error "config changed repeatedly while loading")
    in
    retry (attempts-1)
  else raise (Config.Stt.Config_error "config changed repeatedly while loading")

let _effective_bindings
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
  transcribe, translate

(* PTT helpers copied from Commands.Assistant *)
let ptt_error msg = `Ptt_error msg
let ptt_of_stt (`Stt_error msg) = ptt_error msg
let ptt_of_niri (`Niri_error msg) = ptt_error msg
let ptt_of_injection = function
  | `Injection_error msg
  | `Wtype_error msg
  | `Xdotool_error msg -> ptt_error msg
let stt_of_ptt (`Ptt_error msg) = `Stt_error msg

type ptt_mode =
  | Transcribe
  | Translate

let mode_name = function
  | Transcribe -> "transcribe"
  | Translate -> "translate"

let recording_mode = function
  | Transcribe -> Recording.Coordinator.Ptt_transcribe
  | Translate -> Recording.Coordinator.Ptt_translate

let recognize_with_shift ~transcribe_keysym ~transcribe_shift ~translate_keysym ~translate_shift event =
  let has_shift = event.Keyboard.Monitor.state land 1 <> 0 in
  let phase mode =
    if event.Keyboard.Monitor.released then
      Some (Ptt_loop.Release (mode_name mode))
    else
      Some (Ptt_loop.Press (mode_name mode))
  in
  if event.keysym = transcribe_keysym && has_shift = transcribe_shift then phase Transcribe
  else if event.keysym = translate_keysym && has_shift = translate_shift then phase Translate
  else None

let recognize ~transcribe_keysym ~translate_keysym event =
  recognize_with_shift ~transcribe_keysym ~transcribe_shift:false ~translate_keysym ~translate_shift:false event

(* Robust recognize that does not require shift on release: uses coordinator active mode to disambiguate same-keysym case *)
let recognize_robust ~coordinator ~transcribe_keysym ~transcribe_shift ~translate_keysym ~translate_shift event =
  let has_shift = event.Keyboard.Monitor.state land 1 <> 0 in
  if event.Keyboard.Monitor.released then
    let snap = Recording.Coordinator.snapshot coordinator in
    if snap.active then
      if snap.mode = "ptt-transcribe" && event.keysym = transcribe_keysym then Some (Ptt_loop.Release "transcribe")
      else if snap.mode = "ptt-translate" && event.keysym = translate_keysym then Some (Ptt_loop.Release "translate")
      else if event.keysym = transcribe_keysym && has_shift = transcribe_shift then Some (Ptt_loop.Release "transcribe")
      else if event.keysym = translate_keysym && has_shift = translate_shift then Some (Ptt_loop.Release "translate")
      else None
    else
      if event.keysym = transcribe_keysym && has_shift = transcribe_shift then Some (Ptt_loop.Release "transcribe")
      else if event.keysym = translate_keysym && has_shift = translate_shift then Some (Ptt_loop.Release "translate")
      else None
  else
    if event.keysym = transcribe_keysym && has_shift = transcribe_shift then Some (Ptt_loop.Press "transcribe")
    else if event.keysym = translate_keysym && has_shift = translate_shift then Some (Ptt_loop.Press "translate")
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
    ("transcription_prompt", `String config.transcription_prompt);
    ("translation_prompt", `String config.translation_prompt);
    ("newline_rules", `List (List.mapi newline_rule_json config.newline_rules));
  ]
  |> Yojson.Safe.pretty_to_string

let context_value_max_length = 200

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

(* Main run with component tree + real PTT loop *)
let run
    ~sw
    ~net
    ~clock
    ~process_mgr
    ?ptt_transcribe_keysym
    ?ptt_translate_keysym
    ?(min_duration_ms = default_min_duration_ms)
    ?stdin:_stdin
    () =
  (* Load initial config with stamp handling *)
  let* (initial_config, initial_stamp) =
    Eta.Effect.sync_exn
      (fun () ->
        Eio_unix.run_in_systhread ~label:"pie initial config load" (fun () ->
          load_snapshot 3))
      ~catch:(function
        | Config.Stt.Config_error message ->
          `Pulse_error (Pulse.Client.Protocol_error ("Failed to load STT config: " ^ message))
        | exn ->
          `Pulse_error (Pulse.Client.Protocol_error ("Failed to load STT config: " ^ Printexc.to_string exn)))
  in
  let current = ref initial_config in
  let last_stamp = ref initial_stamp in
  let source_revision_counter = ref 0L in
  let effective_transcribe, effective_translate =
    try
      _effective_bindings ?ptt_transcribe_keysym ?ptt_translate_keysym initial_config
    with Config.Stt.Config_error msg ->
      failwith ("effective bindings: " ^ msg)
  in
  (* Coordinator and control outside tree so RPC and PTT can share them *)
  let coordinator = Recording.Coordinator.create () in
  let control : Ptt_loop.trigger Eio.Stream.t = Eio.Stream.create 1024 in
  let send_control trigger = Eio.Stream.add control trigger in
  (* Set Env for component activations *)
  Env.set_sw sw;
  Env.set_net net;
  Env.set_clock clock;
  Env.set_process_mgr process_mgr;
  Env.set_coordinator coordinator;
  Env.set_control control;
  let* () =
    Eta.Effect.sync (fun () ->
      Assistant_service.Server.start ~sw ~net
        ~action_handler:(fun raw_action ->
          let action = raw_action |> String.trim |> String.lowercase_ascii in
          let snap = Assistant_service.Server.snapshot_of_coordinator coordinator in
          let enqueue trigger message =
            send_control trigger;
            { Assistant_service.Contract.action = raw_action; accepted = true; message; snapshot = snap }
          in
          match action with
          | "status" -> { Assistant_service.Contract.action = raw_action; accepted = true; message = None; snapshot = snap }
          | "pause" ->
            Recording.Coordinator.set_enabled coordinator false;
            { action = raw_action; accepted = true; message = None; snapshot = snap }
          | "resume" ->
            Recording.Coordinator.set_enabled coordinator true;
            { action = raw_action; accepted = true; message = None; snapshot = snap }
          | "toggle" ->
            ignore (Recording.Coordinator.toggle_enabled coordinator);
            { action = raw_action; accepted = true; message = None; snapshot = snap }
          | "ptt-start" | "dictation-start" -> enqueue (Ptt_loop.Press "transcribe") (Some "ptt transcribe start queued")
          | "ptt-stop" | "dictation-stop" -> enqueue (Ptt_loop.Release "transcribe") (Some "ptt transcribe stop queued")
          | "ptt-toggle" | "dictation-toggle" ->
            let snap = Recording.Coordinator.snapshot coordinator in
            if snap.active && snap.mode = Recording.Coordinator.string_of_mode Recording.Coordinator.Ptt_transcribe
            then enqueue (Ptt_loop.Release "transcribe") (Some "ptt transcribe stop queued")
            else enqueue (Ptt_loop.Press "transcribe") (Some "ptt transcribe start queued")
          | "ptt-translate-start" -> enqueue (Ptt_loop.Press "translate") (Some "ptt translate start queued")
          | "ptt-translate-stop" -> enqueue (Ptt_loop.Release "translate") (Some "ptt translate stop queued")
          | "meeting-start" ->
            let result = Recording.Coordinator.try_start coordinator Recording.Coordinator.Meeting_transcribe () in
            let accepted = match result with Recording.Coordinator.Started _ -> true | _ -> false in
            { action = raw_action; accepted; message = None; snapshot = snap }
          | "meeting-stop" ->
            let stopped = Recording.Coordinator.stop coordinator Recording.Coordinator.Meeting_transcribe in
            { action = raw_action; accepted = stopped; message = None; snapshot = snap }
          | "meeting-toggle" ->
            ignore (Recording.Coordinator.toggle_meeting coordinator);
            { action = raw_action; accepted = true; message = None; snapshot = snap }
          | _ ->
            { action = raw_action; accepted = false; message = Some ("unknown action: " ^ raw_action); snapshot = snap }
        )
        coordinator;
      Process.emit_sync ~level:Info "assistant_service started" ~attrs:[("config.path", config_path)];
      Printf.printf
        "pie config:\n%s\n%!"
        (startup_config_json
           ~config:initial_config
           ~ptt_transcribe_keysym:effective_transcribe
           ~ptt_translate_keysym:effective_translate
           ~min_duration_ms
           ())
    )
  in
  (* Initialize components provider to empty before context *)
  Assistant_service.Server.set_components_provider (fun () -> !components_cache_ref);
  (* Outer Eta scope for Pulse + Keyboard that lives for the whole Context.run *)
  Pulse.Client.with_client ~net @@ fun _client ->
  Pulse.Client.with_record_stream _client @@ fun info audio ->
  Keyboard.Monitor.with_monitor ~sw ~net ~process_mgr @@ fun keyboard ->
  (* Set Env audio/keyboard so component entries provide the same streams *)
  let () =
    Env.set_audio audio;
    Env.set_keyboard keyboard
  in
  (* Also hook pulse errors to control? For now just log *)
  let initial_tree = Tree.build ~min_duration_ms !current in
  let context_body (context : Eta_component.Context.t) (diagnostics : Eta_component.Diagnostics.t) =
    let* initial_fence =
      Eta_component.Context.reconcile context initial_tree
      |> Eta.Effect.map_error (fun err ->
        let msg = Format.asprintf "%a" pp_admission_error err in
        admission_error_ref := Some msg;
        Process.emit_sync ~level:Error "initial reconcile admission failed" ~attrs:[("error", msg)];
        `Pulse_error (Pulse.Client.Protocol_error msg))
      |> Eta.Effect.fold ~ok:(fun f -> Some f) ~error:(fun _ -> None)
    in
    let* () =
      match initial_fence with
      | None -> Eta.Effect.unit
      | Some fence ->
        let* report =
          Eta_component.Diagnostics.Fence.await fence
          |> Eta.Effect.fold ~ok:(fun r -> Some r) ~error:(fun _ -> None)
        in
        (match report with
         | Some r -> last_fence_report_ref := Some r
         | None -> ());
        let* snapshot_opt =
          Eta_component.Diagnostics.snapshot diagnostics
          |> Eta.Effect.fold ~ok:(fun s -> Some s) ~error:(fun _ -> None)
        in
        (match snapshot_opt with
         | Some s -> Eta.Effect.sync (fun () -> update_components_cache s) |> Eta.Effect.ignore_errors
         | None -> Eta.Effect.unit)
    in
    (* Components updater fiber: watches diagnostics change and updates cache *)
    let components_updater =
      let rec loop () =
        let* snapshot_opt =
          Eta_component.Diagnostics.snapshot diagnostics
          |> Eta.Effect.fold ~ok:(fun s -> Some s) ~error:(fun _ -> None)
        in
        match snapshot_opt with
        | None -> Eta.Effect.unit
        | Some snap ->
          let rev = Eta_component.Diagnostics.revision snap in
          let* change_opt =
            Eta_component.Diagnostics.await_change diagnostics ~after:rev
            |> Eta.Effect.fold ~ok:(fun c -> Some c) ~error:(fun _ -> None)
          in
          match change_opt with
          | None -> Eta.Effect.unit
          | Some (Eta_component.Diagnostics.Closed final_snap) ->
            Eta.Effect.sync (fun () -> update_components_cache final_snap) |> Eta.Effect.ignore_errors
          | Some (Eta_component.Diagnostics.Changed new_snap) ->
            let* () = Eta.Effect.sync (fun () -> update_components_cache new_snap) |> Eta.Effect.ignore_errors in
            loop ()
      in
      loop ()
    in
    let watcher_router =
      let rec loop () =
        let* () = Eta.Effect.delay (Eta.Duration.ms 500) Eta.Effect.unit in
        let expected_stamp = !last_stamp in
        let* result =
          Eta.Effect.sync (fun () ->
            try
              Eio_unix.run_in_systhread ~label:"pie config reload" (fun () ->
                let next_stamp = config_stamp () in
                if next_stamp = expected_stamp then `Unchanged
                else
                  try
                    let config, loaded_stamp = load_snapshot 3 in
                    `Loaded (config, loaded_stamp)
                  with exn -> `Failed (next_stamp, Printexc.to_string exn))
            with exn -> `Failed (expected_stamp, Printexc.to_string exn))
        in
        let* () =
          match result with
          | `Unchanged -> Eta.Effect.unit
          | `Failed (failed_stamp, message) ->
            Eta.Effect.sync (fun () ->
              last_stamp := failed_stamp;
              Process.emit_sync ~level:Error "Pie config reload failed; keeping previous config"
                ~attrs:[("config.path", config_path); ("config.error", message)])
          | `Loaded (new_config, loaded_stamp) ->
            let old_config = !current in
            let is_provider_only = Config_slices.is_only_provider_change old_config new_config in
            if is_provider_only then (
              (* Replacement path *)
              let* snapshot_opt =
                Eta_component.Diagnostics.snapshot diagnostics
                |> Eta.Effect.fold ~ok:(fun s -> Some s) ~error:(fun _ -> None)
              in
              match snapshot_opt with
              | None ->
                Eta.Effect.sync (fun () ->
                  Process.emit_sync ~level:Error "replacement: failed to get snapshot" ~attrs:[])
              | Some snapshot ->
                let instances = Eta_component.Diagnostics.instances snapshot in
                let stt_instance_opt =
                  List.find_opt (fun inst ->
                    Eta_component.Entry_id.equal
                      (Eta_component.Diagnostics.entry_id inst)
                      Stt_backend.entry_id
                  ) instances
                in
                match stt_instance_opt with
                | None ->
                  Eta.Effect.sync (fun () ->
                    Process.emit_sync ~level:Error "replacement: stt-backend instance not found" ~attrs:[])
                | Some stt_inst ->
                  let expected_instance = Eta_component.Diagnostics.instance_id stt_inst in
                  let expected_target =
                    Eta_component.Diagnostics.target_revision stt_inst
                  in
                  let stt_slice = Config_slices.stt_backend_slice new_config in
                  let entry = Stt_backend.make_entry stt_slice in
                  let target = Eta_component.Replacement.target ~entry ~expected_instance ~expected_target in
                  let candidate_result = Eta_component.Replacement.candidate ~target ~component:Stt_backend.component in
                  (match candidate_result with
                   | Error e ->
                     let msg = match e with Eta_component.Replacement.Component_identity_mismatch id ->
                       Format.asprintf "candidate identity mismatch %a" Eta_component.Entry_id.pp id
                     in
                     Eta.Effect.sync (fun () ->
                       admission_error_ref := Some msg;
                       Process.emit_sync ~level:Error "replacement candidate failed" ~attrs:[("error", msg)])
                   | Ok candidate ->
                     let src = Eta_component.Source_revision.of_int64 !source_revision_counter in
                     source_revision_counter := Int64.succ !source_revision_counter;
                     let batch_result = Eta_component.Replacement.batch ~source_revision:src [candidate] in
                     (match batch_result with
                      | Error e ->
                        let msg = match e with
                          | Empty_batch -> "empty batch"
                          | Duplicate_entry id -> Format.asprintf "duplicate entry %a" Eta_component.Entry_id.pp id
                        in
                        Eta.Effect.sync (fun () ->
                          admission_error_ref := Some msg;
                          Process.emit_sync ~level:Error "replacement batch failed" ~attrs:[("error", msg)])
                      | Ok batch ->
                        let* fence_opt =
                          Eta_component.Context.replace context batch
                          |> Eta.Effect.fold ~ok:(fun f -> Some f) ~error:(fun err ->
                            let msg = Format.asprintf "%a" pp_admission_error err in
                            admission_error_ref := Some msg;
                            Process.emit_sync ~level:Error "replacement admission failed" ~attrs:[("error", msg)];
                            None)
                        in
                        match fence_opt with
                        | None -> Eta.Effect.unit
                        | Some fence ->
                          let* report_opt =
                            Eta_component.Diagnostics.Fence.await fence
                            |> Eta.Effect.fold ~ok:(fun r -> Some r) ~error:(fun _ -> None)
                          in
                          (match report_opt with
                           | Some report ->
                             Eta.Effect.sync (fun () ->
                               last_fence_report_ref := Some report;
                               (match Eta_component.Diagnostics.Fence.outcome report with
                                | Rolled_back ->
                                  Process.emit_sync ~level:Info "stt-backend replacement rolled back" ~attrs:[]
                                | Quiescent ->
                                  current := new_config;
                                  last_stamp := loaded_stamp;
                                  admission_error_ref := None;
                                  Process.emit_sync ~level:Info "Pie config reloaded via replacement" ~attrs:[("config.path", config_path)]
                                | _ ->
                                  current := new_config;
                                  last_stamp := loaded_stamp;
                                  admission_error_ref := None))
                           | None -> Eta.Effect.unit)
                    )
                )
            ) else (
              (* Reconcile path *)
              let new_tree = Tree.build ~min_duration_ms new_config in
              let* fence_opt =
                Eta_component.Context.reconcile context new_tree
                |> Eta.Effect.fold ~ok:(fun f -> Some f) ~error:(fun err ->
                  let msg = Format.asprintf "%a" pp_admission_error err in
                  admission_error_ref := Some msg;
                  Process.emit_sync ~level:Error "reconcile admission failed" ~attrs:[("error", msg)];
                  None)
              in
              match fence_opt with
              | None -> Eta.Effect.unit
              | Some fence ->
                let* report_opt =
                  Eta_component.Diagnostics.Fence.await fence
                  |> Eta.Effect.fold ~ok:(fun r -> Some r) ~error:(fun _ -> None)
                in
                (match report_opt with
                 | Some report ->
                   Eta.Effect.sync (fun () ->
                     last_fence_report_ref := Some report;
                     (match Eta_component.Diagnostics.Fence.outcome report with
                      | Quiescent ->
                        current := new_config;
                        last_stamp := loaded_stamp;
                        admission_error_ref := None;
                        Process.emit_sync ~level:Info "Pie config reloaded via reconcile" ~attrs:[("config.path", config_path)]
                      | _ ->
                        Process.emit_sync ~level:Info "reconcile completed non-quiescent" ~attrs:[]))
                 | None -> Eta.Effect.unit)
            )
        in
        loop ()
      in
      loop ()
    in
    let supervision = Supervision.run ~context ~diagnostics in
    (* Real PTT loop that uses the outer audio/keyboard/control and current config ref *)
    let ptt_loop =
      let ptt_config : Ptt_loop.config = {
        recognize = (fun event ->
          let cfg = !current in
          let result =
            recognize_robust ~coordinator
              ~transcribe_keysym:cfg.ptt_transcribe_keysym ~transcribe_shift:cfg.ptt_transcribe_shift
              ~translate_keysym:cfg.ptt_translate_keysym ~translate_shift:cfg.ptt_translate_shift
              event
          in
          (* Debug: log every Control_R event to diagnose shift handling *)
          if event.Keyboard.Monitor.keysym = cfg.ptt_transcribe_keysym || event.Keyboard.Monitor.keysym = cfg.ptt_translate_keysym then
            Process.emit_sync ~level:Info "ptt key event"
              ~attrs:[
                ("keysym", string_of_int event.Keyboard.Monitor.keysym);
                ("state", string_of_int event.Keyboard.Monitor.state);
                ("has_shift", string_of_bool (event.Keyboard.Monitor.state land 1 <> 0));
                ("released", string_of_bool event.Keyboard.Monitor.released);
                ("transcribe_shift", string_of_bool cfg.ptt_transcribe_shift);
                ("translate_shift", string_of_bool cfg.ptt_translate_shift);
                ("result", match result with Some (Ptt_loop.Press m) -> "Press:" ^ m | Some (Ptt_loop.Release m) -> "Release:" ^ m | None -> "None");
                ("active_mode", (Recording.Coordinator.snapshot coordinator).mode);
              ];
          result);
        min_duration_ms;
        capture_thresholds = (fun () -> (!current).Config.Stt.ptt_min_capture_rms, (!current).Config.Stt.ptt_min_capture_peak);
        log_prefix = (fun mode -> "assistant-ptt-" ^ mode);
        source = info.Pulse.Defs.source_name;
        on_ready = Eta.Effect.sync (fun () ->
          let cfg = !current in
          Printf.printf "PTT armed: transcribe=%d%s translate=%d%s source=%s\n%!"
            cfg.ptt_transcribe_keysym (if cfg.ptt_transcribe_shift then "+shift" else "")
            cfg.ptt_translate_keysym (if cfg.ptt_translate_shift then "+shift" else "")
            (Option.value ~default:"<default>" info.Pulse.Defs.source_name));
        on_press = (fun mode_name -> on_press ~sw ~net ~process_mgr ~coordinator ~config:!current mode_name);
        on_release = (fun _ -> Eta.Effect.unit);
        on_abort = Eta.Effect.sync (fun () -> not (Recording.Coordinator.snapshot coordinator).enabled);
        on_dead_input = Desktop.Notification.notify_warning
          ~process_mgr
          "pie: no microphone input"
          "No input detected during push-to-talk. Your microphone may be muted.";
      } in
      Ptt_loop.run ~control ~keyboard ~audio ptt_config
      |> Eta.Effect.map_error (fun (`Ptt_error msg) -> `Pulse_error (Pulse.Client.Protocol_error msg))
      |> Eta.Effect.ignore_errors
    in
    (* Run watcher, supervision, components updater, and PTT loop concurrently *)
    Eta.Effect.par
      (Eta.Effect.par watcher_router supervision)
      (Eta.Effect.par components_updater ptt_loop)
    |> Eta.Effect.ignore_errors
  in
  Eta_component.Context.run context_body
  |> Eta.Effect.map_error (fun (`Pulse_error _ as e) -> e)

