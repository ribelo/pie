open Eta.Syntax

let default_min_duration_ms = 120
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
    Some
      (stat.Unix.st_dev, stat.Unix.st_ino, stat.Unix.st_size, stat.Unix.st_mtime)
  with Unix.Unix_error _ -> None

let config_stamp () = List.map stamp (watched_paths ())

let load_snapshot attempts =
  let before = config_stamp () in
  let config = Config.Stt.load ~config_path () in
  let after = config_stamp () in
  if before = after then (config, after)
  else if attempts > 0 then
    let rec retry n =
      let b = config_stamp () in
      let c = Config.Stt.load ~config_path () in
      let a = config_stamp () in
      if b = a then (c, a)
      else if n > 0 then retry (n - 1)
      else
        raise
          (Config.Stt.Config_error "config changed repeatedly while loading")
    in
    retry (attempts - 1)
  else raise (Config.Stt.Config_error "config changed repeatedly while loading")

type ptt_mode = Transcribe | Translate
type ptt_binding = { keysym : int; shift : bool }

let mode_name = function Transcribe -> "transcribe" | Translate -> "translate"

let recording_mode = function
  | Transcribe -> Recording.Coordinator.Ptt_transcribe
  | Translate -> Recording.Coordinator.Ptt_translate

let shift_modifier_mask = 1 lsl 0

let binding_matches binding event =
  let shift = event.Keyboard.Monitor.state land shift_modifier_mask <> 0 in
  event.keysym = binding.keysym && shift = binding.shift

let recognize ~transcribe_binding ~translate_binding event =
  let phase mode =
    if event.Keyboard.Monitor.released then
      Some (Ptt_loop.Release (mode_name mode))
    else Some (Ptt_loop.Press (mode_name mode))
  in
  if binding_matches transcribe_binding event then phase Transcribe
  else if binding_matches translate_binding event then phase Translate
  else None

(* [down] is every physically-down recognized PTT key, including a busy
   second key and Mod5 latch key, without making those owner. [owner] is the
   key whose capture is current. Only the owner's keyup emits Release.
   Autorepeat of a down key is Press only while that key still owns an
   active admission. Latch toggles only on a fresh physical press. *)
let make_live_recognizer ~load_bindings ?coordinator ?(latch_keycode = ref None)
    () =
  let down = Hashtbl.create 2 in
  let owner = ref None in
  let right_shift_keysym = 65506 in
  let busy () =
    match coordinator with
    | Some c -> Recording.Coordinator.is_active c
    | None -> Hashtbl.length down > 0
  in
  let latch mode =
    match coordinator with
    | None -> Some (Ptt_loop.Press (mode_name mode))
    | Some c ->
        let snap = Recording.Coordinator.snapshot c in
        if
          snap.active
          && snap.mode
             = Recording.Coordinator.string_of_mode (recording_mode mode)
        then Some (Ptt_loop.Release (mode_name mode))
        else Some (Ptt_loop.Press (mode_name mode))
  in
  fun event ->
    if event.Keyboard.Monitor.released then (
      if !latch_keycode = Some event.keycode then (
        latch_keycode := None;
        Hashtbl.remove down event.keycode;
        None)
      else
        let mode = Hashtbl.find_opt down event.keycode in
        Hashtbl.remove down event.keycode;
        match (!owner, mode) with
        | Some code, Some mode when code = event.keycode ->
            owner := None;
            Some (Ptt_loop.Release mode)
        | _ -> None)
    else
      match Hashtbl.find_opt down event.keycode with
      | Some mode ->
          if !owner = Some event.keycode && busy () then
            Some (Ptt_loop.Press mode)
          else None
      | None -> (
          let transcribe_binding, translate_binding = load_bindings () in
          let has_mod5 = event.Keyboard.Monitor.state land 128 <> 0 in
          if has_mod5 && event.keysym = translate_binding.keysym then (
            Hashtbl.replace down event.keycode (mode_name Translate);
            latch_keycode := Some event.keycode;
            latch Translate)
          else if has_mod5 && event.keysym = right_shift_keysym then (
            Hashtbl.replace down event.keycode (mode_name Transcribe);
            latch_keycode := Some event.keycode;
            latch Transcribe)
          else
            match recognize ~transcribe_binding ~translate_binding event with
            | Some (Ptt_loop.Press mode) as trigger ->
                let admitted = not (busy ()) in
                Hashtbl.replace down event.keycode mode;
                if admitted then (
                  owner := Some event.keycode;
                  trigger)
                else None
            | trigger -> trigger)

let effective_bindings ?ptt_transcribe_keysym ?ptt_translate_keysym
    (config : Config.Stt.t) =
  let transcribe =
    Option.value ~default:config.ptt_transcribe_keysym ptt_transcribe_keysym
  in
  let translate =
    Option.value ~default:config.ptt_translate_keysym ptt_translate_keysym
  in
  if transcribe <= 0 then
    raise
      (Config.Stt.Config_error
         "effective PTT transcribe keysym must be positive");
  if translate <= 0 then
    raise
      (Config.Stt.Config_error "effective PTT translate keysym must be positive");
  let transcribe_binding =
    { keysym = transcribe; shift = config.ptt_transcribe_shift }
  in
  let translate_binding =
    { keysym = translate; shift = config.ptt_translate_shift }
  in
  if transcribe_binding = translate_binding then
    raise
      (Config.Stt.Config_error
         "effective PTT transcribe and translate bindings must differ");
  (transcribe_binding, translate_binding)

let with_cli_overrides ?ptt_transcribe_keysym ?ptt_translate_keysym config =
  let transcribe, translate =
    effective_bindings ?ptt_transcribe_keysym ?ptt_translate_keysym config
  in
  {
    config with
    ptt_transcribe_keysym = transcribe.keysym;
    ptt_translate_keysym = translate.keysym;
    ptt_transcribe_shift = transcribe.shift;
    ptt_translate_shift = translate.shift;
  }

let ptt_error msg = `Ptt_error msg
let ptt_of_stt (`Stt_error msg) = ptt_error msg

let injection_error_message = function
  | `Clipboard_error msg
  | `Injection_error msg
  | `Wtype_error msg
  | `Xdotool_error msg ->
      msg

let diagnostic_injection = function
  | `Clipboard_error _ -> "injection:clipboard"
  | `Injection_error _ -> "injection:inject"
  | `Wtype_error _ -> "injection:wtype"
  | `Xdotool_error _ -> "injection:xdotool"

let diagnostic_pulse (error : Pulse.Client.error) =
  match error with
  | Protocol_error _ -> "pulse:protocol"
  | Authentication_error _ -> "pulse:authentication"
  | Server_error _ -> "pulse:server"
  | Timeout _ -> "pulse:timeout"
  | Disconnected _ -> "pulse:disconnected"
  | Malformed_packet _ -> "pulse:malformed"
  | Recording_overload _ -> "pulse:recording_overload"

let rec ptt_status_of_failures = function
  | [] -> "ptt:operation"
  | `Ptt_error "pcm:capture_budget" :: _ -> "ptt:capture_budget"
  | `Ptt_error "pcm:queue_budget" :: _ -> "ptt:queue_budget"
  | `Ptt_error "stt:no_transcript" :: _ -> "stt:no_transcript"
  | _ :: rest -> ptt_status_of_failures rest

let status_of_cause cause =
  let rec pulse acc = function
    | [] -> acc
    | `Pulse_error (Pulse.Client.Recording_overload _ as err) :: _ -> Some err
    | `Pulse_error err :: rest ->
        pulse (match acc with None -> Some err | Some _ -> acc) rest
    | _ :: rest -> pulse acc rest
  in
  match pulse None (Eta.Cause.failures cause) with
  | Some err -> diagnostic_pulse err
  | None -> ptt_status_of_failures (Eta.Cause.failures cause)

let ptt_of_injection error = ptt_error (injection_error_message error)
let stt_of_ptt (`Ptt_error msg) = `Stt_error msg

let recognize_with_shift ~transcribe_keysym ~transcribe_shift ~translate_keysym
    ~translate_shift event =
  let has_shift = event.Keyboard.Monitor.state land 1 <> 0 in
  let phase mode =
    if event.Keyboard.Monitor.released then
      Some (Ptt_loop.Release (mode_name mode))
    else Some (Ptt_loop.Press (mode_name mode))
  in
  if event.keysym = transcribe_keysym && has_shift = transcribe_shift then
    phase Transcribe
  else if event.keysym = translate_keysym && has_shift = translate_shift then
    phase Translate
  else None

let recognize ~transcribe_keysym ~translate_keysym event =
  recognize_with_shift ~transcribe_keysym ~transcribe_shift:false
    ~translate_keysym ~translate_shift:false event

let mode_of_name = function
  | "transcribe" -> Transcribe
  | "translate" -> Translate
  | mode -> invalid_arg ("unknown assistant PTT mode: " ^ mode)

let normalize_for_stt (config : Config.Stt.t) pcm_bytes =
  (Audio.Pcm.normalize_pcm_for_stt
     ~silence_rms_threshold:config.ptt_min_capture_rms
     ~silence_peak_threshold:config.ptt_min_capture_peak pcm_bytes)
    .normalized_bytes

let config_model (config : Config.Stt.t) = function
  | Transcribe -> config.transcription_model
  | Translate -> config.translation_model

let resolved_model_for_mode (config : Config.Stt.t) mode =
  Config.Stt.resolve_model_provider ~default_provider:config.provider
    (config_model config mode)

let option_json f = function None -> `Null | Some value -> f value

let resolved_model_json (resolved : Config.Stt.resolved_model) =
  `Assoc
    [
      ("provider", `String (Config.Stt.string_of_provider resolved.provider));
      ("model", `String resolved.model);
    ]

let configured_model_json config mode =
  let configured = config_model config mode in
  let resolved = resolved_model_for_mode config mode in
  `Assoc
    [
      ("configured", `String configured);
      ("resolved", resolved_model_json resolved);
    ]

let newline_rule_json index (rule : Config.Stt.newline_rule) =
  `Assoc
    [
      ("index", `Int index);
      ("app_id", option_json (fun value -> `String value) rule.app_id);
      ("title_regex", option_json (fun value -> `String value) rule.title_regex);
      ("action", `String (Config.Stt.string_of_newline_action rule.action));
    ]

let text_entry_rule_json index (rule : Config.Stt.text_entry_rule) =
  `Assoc
    [
      ("index", `Int index);
      ("app_id", option_json (fun value -> `String value) rule.app_id);
      ("title_regex", option_json (fun value -> `String value) rule.title_regex);
      ("method", `String (Config.Stt.string_of_entry_method rule.method_));
    ]

let startup_config_json ?(config_path = Paths.stt_config_path)
    ~(config : Config.Stt.t) ~ptt_transcribe_keysym ~ptt_translate_keysym
    ~min_duration_ms () =
  `Assoc
    [
      ("config_path", `String config_path);
      ("provider", `String (Config.Stt.string_of_provider config.provider));
      ("transcription_model", configured_model_json config Transcribe);
      ("translation_model", configured_model_json config Translate);
      ( "thinking_effort",
        option_json (fun value -> `String value) config.thinking_effort );
      ("transcription_language", `String config.transcription_language);
      ("translation_source_language", `String config.translation_source_language);
      ("translation_target_language", `String config.translation_target_language);
      ("ptt_min_duration_ms", `Int min_duration_ms);
      ("ptt_min_capture_rms", `Float config.ptt_min_capture_rms);
      ("ptt_min_capture_peak", `Float config.ptt_min_capture_peak);
      ("ptt_transcribe_keysym", `Int ptt_transcribe_keysym);
      ("ptt_translate_keysym", `Int ptt_translate_keysym);
      ("newline_rules", `List (List.mapi newline_rule_json config.newline_rules));
      ( "text_entry_rules",
        `List (List.mapi text_entry_rule_json config.text_entry_rules) );
    ]
  |> Yojson.Safe.pretty_to_string

let run_stt ~sw ~net ~process_mgr:_ (config : Config.Stt.t) mode pcm_bytes =
  let prompt_template =
    match mode with
    | Transcribe -> config.transcription_prompt
    | Translate -> config.translation_prompt
  in
  let resolved_model = resolved_model_for_mode config mode in
  match resolved_model.provider with
  | Codex_realtime -> (
      let pcm_bytes = normalize_for_stt config pcm_bytes in
      match mode with
      | Transcribe ->
          Stt.Codex_realtime.transcribe_with_prompt ~sw ~net
            ~model:resolved_model.model ~language:config.transcription_language
            ~prompt_template pcm_bytes
      | Translate ->
          Stt.Codex_realtime.translate_with_prompt ~sw ~net
            ~model:resolved_model.model
            ~source_language:config.translation_source_language
            ~target_language:config.translation_target_language ~prompt_template
            pcm_bytes)
  | Openrouter -> (
      let pcm_bytes = normalize_for_stt config pcm_bytes in
      match mode with
      | Transcribe ->
          Stt.Openrouter.transcribe_with_prompt ~sw ~net
            ~model:resolved_model.model ~language:config.transcription_language
            ~prompt_template ?thinking_effort:config.thinking_effort pcm_bytes
      | Translate ->
          Stt.Openrouter.translate_with_prompt ~sw ~net
            ~model:resolved_model.model
            ~source_language:config.translation_source_language
            ~target_language:config.translation_target_language ~prompt_template
            ?thinking_effort:config.thinking_effort pcm_bytes)

let inject_and_report ~process_mgr ?clipboard_owner ~newline_rules
    ~text_entry_rules mode text =
  let normalized = Input.Text_injection.normalize_text text in
  if String.length normalized = 0 then
    let* () =
      Desktop.Notification.notify_warning ~process_mgr "pie: no transcript"
        "Speech-to-text produced no transcript text. Try speaking louder or \
         checking microphone input."
      |> Eta.Effect.ignore_errors
    in
    Eta.Effect.unit
  else
    let* result =
      Input.Text_injection.inject ?clipboard_owner ~newline_rules
        ~text_entry_rules ~process_mgr normalized
      |> Eta.Effect.bind_error (fun error ->
          let* () =
            Desktop.Notification.notify_warning ~process_mgr
              "pie: text entry failed"
              (diagnostic_injection error)
            |> Eta.Effect.ignore_errors
          in
          Eta.Effect.fail (ptt_of_injection error))
    in
    Eta.Effect.sync (fun () ->
        Printf.printf "%s %d chars via %s (%s)\n%!"
          (match result.outcome with
          | Input.Text_injection.Typed -> "Typed"
          | Input.Text_injection.Clipboard_key_attempted ->
              "Paste key dispatched; receipt unconfirmed")
          (String.length result.text)
          (match result.backend with Wtype -> "wtype" | Xdotool -> "xdotool")
          (match result.session_type with Wayland -> "Wayland" | X11 -> "X11"))

module Active = Recording.Retry_store.Active

let retry_mode = function
  | Transcribe -> Recording.Retry_store.Transcribe
  | Translate -> Recording.Retry_store.Translate

let mode_of_retry = function
  | Recording.Retry_store.Transcribe -> Transcribe
  | Recording.Retry_store.Translate -> Translate

let mode_of_active_metadata = function
  | Active.Audio { mode; _ } | Active.Text { mode; _ } -> mode_of_retry mode

let ptt_of_retry (`Retry_store msg) = `Ptt_error ("recording:" ^ msg)

let retry_result = function
  | Ok v -> Eta.Effect.pure v
  | Error e -> Eta.Effect.fail (ptt_of_retry e)

let is_blank text =
  let n = String.length text in
  let rec loop i =
    i >= n
    ||
    match text.[i] with
    | ' ' | '\n' | '\t' | '\r' -> loop (i + 1)
    | _ -> false
  in
  loop 0

let rethrow_non_io = function
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | (Unix.Unix_error _ | Sys_error _) as exn -> exn
  | exn -> raise exn

let recording_file_effect operation f =
  Eta.Effect.sync_exn f ~catch:(fun exn ->
      ignore (rethrow_non_io exn);
      `Ptt_error ("recording:" ^ operation))

(* PIE004: one attempt owner for admission, the private Draft writer and
   final settlement. Admission is released and unpublished drafts aborted on
   success, failure and cancellation. Published recovery is a separate Active
   slot and is not discarded here. *)
type attempt_stage =
  | Capturing of Active.writer
  | Sealed of Active.writer
  | Closed

type attempt_owner = {
  coordinator : Recording.Coordinator.t;
  attempt : Recording.Coordinator.attempt;
  store : Active.store;
  mode : ptt_mode;
  stage : attempt_stage ref;
  settled : bool Atomic.t;
}

(* Create only missing ancestors at 0700. Existing directories are left
   untouched (no chmod), including a historical 0755 parent. *)
let rec ensure_private_missing path =
  if path = "" || path = Filename.dirname path then ()
  else
    match Unix.lstat path with
    | { st_kind = Unix.S_DIR; _ } -> ()
    | _ -> raise (Unix.Unix_error (Unix.EEXIST, "mkdir", path))
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
        ensure_private_missing (Filename.dirname path);
        try Unix.mkdir path 0o700
        with Unix.Unix_error (Unix.EEXIST, _, _) -> (
          match Unix.lstat path with
          | { st_kind = Unix.S_DIR; _ } -> ()
          | _ -> raise (Unix.Unix_error (Unix.EEXIST, "mkdir", path))))

let open_recovery_store ~root =
  match
    try
      ensure_private_missing root;
      Ok ()
    with Unix.Unix_error (err, _, _) ->
      Error (`Retry_store (Unix.error_message err))
  with
  | Error e -> Error e
  | Ok () -> Active.open_store ~root

let create_attempt_owner ~store ~coordinator ~attempt mode =
  match Active.open_audio_writer store ~mode:(retry_mode mode) with
  | Error e -> Error (ptt_of_retry e)
  | Ok writer ->
      Ok
        {
          coordinator;
          attempt;
          store;
          mode;
          stage = ref (Capturing writer);
          settled = Atomic.make false;
        }

let current_writer owner =
  match !(owner.stage) with
  | Capturing writer | Sealed writer -> Some writer
  | Closed -> None

let abort_unpublished owner =
  Eta.Effect.sync (fun () ->
      match current_writer owner with
      | None -> Ok ()
      | Some writer ->
          let result = Active.abort_writer writer in
          owner.stage := Closed;
          result)
  |> Eta.Effect.bind retry_result

let seal_writer owner =
  Eta.Effect.sync (fun () ->
      match !(owner.stage) with
      | Closed | Sealed _ -> Ok ()
      | Capturing writer -> (
          match Active.close_writer writer with
          | Ok () ->
              owner.stage := Sealed writer;
              Ok ()
          | Error e -> Error e))
  |> Eta.Effect.bind retry_result

let publish_sealed_audio owner =
  Eta.Effect.sync (fun () ->
      match !(owner.stage) with
      | Sealed writer -> (
          match Active.publish_audio owner.store writer with
          | Ok _ ->
              owner.stage := Closed;
              Ok ()
          | Error e -> Error e)
      | Capturing _ | Closed -> Ok ())
  |> Eta.Effect.bind retry_result

let publish_recognized_text owner text ~dispatch_uncertain =
  let publish =
    Eta.Effect.sync (fun () ->
        match
          Active.publish_text owner.store ~mode:(retry_mode owner.mode) ~text
            ~dispatch_uncertain
        with
        | Ok _ -> Ok ()
        | Error e -> Error e)
    |> Eta.Effect.bind retry_result
  in
  abort_unpublished owner |> Eta.Effect.finally publish

let set_attempt_phase owner phase =
  Eta.Effect.sync (fun () ->
      ignore
        (Recording.Coordinator.set_phase owner.coordinator owner.attempt phase))

let settle_attempt owner =
  let* () = set_attempt_phase owner Recording.Phase.Finishing in
  Eta.Effect.sync (fun () -> Atomic.compare_and_set owner.settled false true)
  |> Eta.Effect.bind (function
    | false -> Eta.Effect.unit
    | true ->
        abort_unpublished owner
        |> Eta.Effect.finally
             (Eta.Effect.sync (fun () ->
                  ignore
                    (Recording.Coordinator.release owner.coordinator
                       owner.attempt))))

(* One recognition -> delivery stage used by live capture and RetryLatest.
   Blank / provider failure stay on Audio. Delivery failure publishes the
   exact recognized text (dispatch_uncertain is a warning, not a retry veto). *)
let recognize_then_deliver ~recognize ~deliver ~on_provider_failure ~on_blank
    ~on_delivery_failure ~on_success =
  recognize
  |> Eta.Effect.on_error (fun _ -> on_provider_failure)
  |> Eta.Effect.bind (fun text ->
      if is_blank text then
        Eta.Effect.fail (`Ptt_error "stt:no_transcript")
        |> Eta.Effect.finally on_blank
      else
        deliver text
        |> Eta.Effect.on_error (fun _ -> on_delivery_failure text)
        |> Eta.Effect.bind (fun () -> on_success))

let unless_cancelled attempt action =
  Eta.Effect.sync (fun () -> Recording.Coordinator.cancel_signalled attempt)
  |> Eta.Effect.bind (function true -> Eta.Effect.unit | false -> action)

let deliver_retained_text ~deliver ~on_delivery_failure ~on_success text =
  recognize_then_deliver ~recognize:(Eta.Effect.pure text) ~deliver
    ~on_provider_failure:Eta.Effect.unit ~on_blank:Eta.Effect.unit
    ~on_delivery_failure ~on_success

(* Seal capture, then the shared recognition/delivery stage. *)
let after_capture owner ~deliver recognize =
  let* () = set_attempt_phase owner Recording.Phase.Processing in
  let* () = seal_writer owner in
  let deliver text =
    let* () = set_attempt_phase owner Recording.Phase.Delivering in
    deliver text
  in
  let guarded action = unless_cancelled owner.attempt action in
  recognize_then_deliver ~recognize ~deliver
    ~on_provider_failure:(guarded (publish_sealed_audio owner))
    ~on_blank:(guarded (publish_sealed_audio owner))
    ~on_delivery_failure:(fun text ->
      guarded (publish_recognized_text owner text ~dispatch_uncertain:true))
    ~on_success:(abort_unpublished owner)

(* Tee PCM into the attempt Draft. Admission is released only by the
   outermost run_acquired_attempt on_settle, after provider scope finalizers. *)
let attempt_handle owner (handle : Ptt_loop.handle) : Ptt_loop.handle =
  {
    Ptt_loop.offer =
      (fun chunk ->
        let* () =
          match !(owner.stage) with
          | Capturing writer -> retry_result (Active.append_audio writer chunk)
          | Sealed _ | Closed ->
              Eta.Effect.fail (`Ptt_error "recording:writer-closed")
        in
        handle.offer chunk);
    finish = handle.finish;
    cancel =
      handle.cancel |> Eta.Effect.on_exit (fun _ -> abort_unpublished owner);
  }

(* Recording is stopped by the attempt settlement, after provider and delivery
   completion, so a press received while the operation still processes is
   rejected instead of admitted as a second attempt. *)
let production_stt ~sw ~net ~process_mgr config mode bytes =
  run_stt ~sw ~net ~process_mgr config mode bytes
  |> Eta.Effect.map_error (function
      | `Stt_error msg
      | `Subprocess_error msg
      | `Niri_error msg
      | `Ptt_error msg
      -> `Ptt_error msg)

let production_deliver ~process_mgr ?clipboard_owner (config : Config.Stt.t)
    mode text =
  inject_and_report ~process_mgr ?clipboard_owner
    ~newline_rules:config.newline_rules
    ~text_entry_rules:config.text_entry_rules mode text

let make_clip_handle ~owner ~stt ~deliver ~config mode =
  {
    Ptt_loop.offer = (fun _chunk -> Eta.Effect.unit);
    finish =
      Ptt_loop.Buffered
        (fun clip ->
          after_capture owner
            ~deliver:(fun text -> deliver config mode text)
            (stt config mode clip.Ptt_loop.pcm_bytes));
    cancel = Eta.Effect.unit;
  }

let make_streaming_transcribe_handle ~sw ~net ~process_mgr:_ ~owner ~config
    ~deliver =
  let model = (resolved_model_for_mode config Transcribe).model in
  let prompt_template = config.Config.Stt.transcription_prompt in
  Eta.Effect.pure
    (Ptt_loop.Scoped
       (fun use ->
         (* A setup failure before [use] still settles admission and closes the
            retained writer; settlement is idempotent, so the handle's own
            settlement is not repeated. *)
         Stt.Codex_realtime.with_transcription_stream ~sw ~net ~model
           ~language:config.transcription_language ~prompt_template
           (fun stream ->
             let handle =
               {
                 Ptt_loop.offer =
                   (fun chunk ->
                     stream.offer_chunk chunk |> Eta.Effect.map_error ptt_of_stt);
                 finish =
                   Ptt_loop.Streaming
                     (fun ~duration_ms:_ ->
                       after_capture owner
                         ~deliver:(fun text -> deliver config Transcribe text)
                         (stream.finish_stream ()
                         |> Eta.Effect.map_error ptt_of_stt));
                 cancel =
                   stream.cancel_stream |> Eta.Effect.map_error ptt_of_stt;
               }
             in
             use (attempt_handle owner handle)
             |> Eta.Effect.map_error stt_of_ptt)
         |> Eta.Effect.map_error ptt_of_stt))

let make_streaming_translate_handle ~sw ~net ~process_mgr:_ ~owner ~config
    ~deliver =
  let model = (resolved_model_for_mode config Translate).model in
  let prompt_template = config.Config.Stt.translation_prompt in
  Eta.Effect.pure
    (Ptt_loop.Scoped
       (fun use ->
         Stt.Codex_realtime.with_translation_stream ~sw ~net ~model
           ~source_language:config.translation_source_language
           ~target_language:config.translation_target_language ~prompt_template
           (fun stream ->
             let handle =
               {
                 Ptt_loop.offer =
                   (fun chunk ->
                     stream.offer_chunk chunk |> Eta.Effect.map_error ptt_of_stt);
                 finish =
                   Ptt_loop.Streaming
                     (fun ~duration_ms:_ ->
                       after_capture owner
                         ~deliver:(fun text -> deliver config Translate text)
                         (stream.finish_stream ()
                         |> Eta.Effect.map_error ptt_of_stt));
                 cancel =
                   stream.cancel_stream |> Eta.Effect.map_error ptt_of_stt;
               }
             in
             use (attempt_handle owner handle)
             |> Eta.Effect.map_error stt_of_ptt)
         |> Eta.Effect.map_error ptt_of_stt))

let to_ptt_error = function
  | `Stt_error msg | `Niri_error msg | `Subprocess_error msg | `Ptt_error msg ->
      `Ptt_error msg

let acquire_and_run ~sw ~net ~process_mgr ~owner ~config ~stt ~deliver mode use
    =
  let as_ptt body = body |> Eta.Effect.map_error to_ptt_error in
  match
    try Ok (resolved_model_for_mode config mode)
    with Config.Stt.Config_error _ -> Error (`Ptt_error "config:model")
  with
  | Error err -> Eta.Effect.fail err
  | Ok resolved -> (
      match (resolved.provider, mode) with
      | Codex_realtime, Transcribe ->
          make_streaming_transcribe_handle ~sw ~net ~process_mgr ~owner ~config
            ~deliver
          |> as_ptt
          |> Eta.Effect.bind (function
            | Ptt_loop.Scoped run -> as_ptt (run use)
            | Ptt_loop.Handle handle -> as_ptt (use handle)
            | Ptt_loop.Skip -> Eta.Effect.unit)
      | Codex_realtime, Translate ->
          make_streaming_translate_handle ~sw ~net ~process_mgr ~owner ~config
            ~deliver
          |> as_ptt
          |> Eta.Effect.bind (function
            | Ptt_loop.Scoped run -> as_ptt (run use)
            | Ptt_loop.Handle handle -> as_ptt (use handle)
            | Ptt_loop.Skip -> Eta.Effect.unit)
      | _ ->
          as_ptt
            (use
               (attempt_handle owner
                  (make_clip_handle ~owner ~stt ~deliver ~config mode))))

(* Private typed pause marker. catch_some recovers only a clean Owned_pause.
   None preserves composite Body_error causes exactly. map_error then unwraps
   Body_error structurally; a surviving Owned_pause in an uncatchable compound
   becomes a stable Ptt_error without flattening other Fail leaves. *)
let run_acquired_attempt ~attempt ~on_settle body =
  let pause =
    Recording.Coordinator.await_cancel attempt
    |> Eta.Effect.bind (fun () -> Eta.Effect.fail `Owned_pause)
  in
  Eta.Effect.with_background ~name:"pie.attempt.pause" pause (fun () ->
      body |> Eta.Effect.map_error (fun err -> `Body_error err))
  |> Ptt_loop.recover_single_failure (function
    | `Owned_pause -> Some Eta.Effect.unit
    | `Body_error _ -> None)
  |> Eta.Effect.map_error (function
    | `Owned_pause -> `Ptt_error "paused attempt failed during cleanup"
    | `Body_error err -> err)
  |> Eta.Effect.on_exit (fun _ -> on_settle)

let on_press ~sw ~net ~process_mgr ~coordinator ~config ?store ?retry_dir
    ?(stt = production_stt ~sw ~net ~process_mgr)
    ?(deliver =
      fun config mode text -> production_deliver ~process_mgr config mode text)
    mode_name =
  Eta.Effect.sync (fun () ->
      let mode = mode_of_name mode_name in
      ( mode,
        Recording.Coordinator.try_start coordinator (recording_mode mode) () ))
  |> Eta.Effect.bind (function
    | _, Recording.Coordinator.Busy active ->
        Eta.Effect.sync (fun () ->
            Printf.eprintf "PTT ignored: %s is active\n%!"
              (Recording.Coordinator.string_of_mode active))
        |> Eta.Effect.map (fun () -> Ptt_loop.Skip)
    | _, Recording.Coordinator.Disabled ->
        Eta.Effect.sync (fun () ->
            Printf.eprintf "PTT ignored: PIE is paused\n%!")
        |> Eta.Effect.map (fun () -> Ptt_loop.Skip)
    | mode, Recording.Coordinator.Started attempt -> (
        let opened =
          match store with
          | Some store -> Ok store
          | None -> (
              match retry_dir with
              | None -> Error (`Retry_store "no-store")
              | Some root -> open_recovery_store ~root)
        in
        match opened with
        | Error e ->
            ignore (Recording.Coordinator.release coordinator attempt);
            Eta.Effect.fail (ptt_of_retry e)
        | Ok store -> (
            match create_attempt_owner ~store ~coordinator ~attempt mode with
            | Error err ->
                ignore (Recording.Coordinator.release coordinator attempt);
                Eta.Effect.fail err
            | Ok owner ->
                let* () = set_attempt_phase owner Recording.Phase.Recording in
                Eta.Effect.pure
                  (Ptt_loop.Scoped
                     (fun k ->
                       run_acquired_attempt ~attempt
                         ~on_settle:(settle_attempt owner)
                         (acquire_and_run ~sw ~net ~process_mgr ~owner ~config
                            ~stt ~deliver mode k))))))

(* PIE004: keyboard and RPC presses share one control path. The handler queues
   triggers for the same PTT loop and admission owner the keyboard path uses,
   and a press received while an operation owns admission is rejected here
   instead of being replayed as a fresh attempt after settlement. *)
let action_handler ~coordinator ~send_control ~retries ~get_store raw_action =
  let action = raw_action |> String.trim |> String.lowercase_ascii in
  let snap = Assistant_service.Server.snapshot_of_coordinator coordinator in
  let enqueue trigger message =
    send_control trigger;
    {
      Assistant_service.Contract.action = raw_action;
      accepted = true;
      message;
      snapshot = snap;
    }
  in
  match action with
  | "status" ->
      {
        Assistant_service.Contract.action = raw_action;
        accepted = true;
        message = None;
        snapshot = snap;
      }
  | "pause" ->
      Recording.Coordinator.set_enabled coordinator false;
      { action = raw_action; accepted = true; message = None; snapshot = snap }
  | "resume" ->
      Recording.Coordinator.set_enabled coordinator true;
      { action = raw_action; accepted = true; message = None; snapshot = snap }
  | "toggle" ->
      ignore (Recording.Coordinator.toggle_enabled coordinator);
      { action = raw_action; accepted = true; message = None; snapshot = snap }
  | "ptt-start" | "dictation-start" ->
      if snap.active then
        {
          action = raw_action;
          accepted = false;
          message = Some ("cannot start while " ^ snap.mode ^ " is active");
          snapshot = snap;
        }
      else
        enqueue (Ptt_loop.Press "transcribe")
          (Some "ptt transcribe start queued")
  | "ptt-stop" | "dictation-stop" ->
      enqueue (Ptt_loop.Release "transcribe")
        (Some "ptt transcribe stop queued")
  | "ptt-toggle" | "dictation-toggle" ->
      let snap = Recording.Coordinator.snapshot coordinator in
      if
        snap.active
        && snap.mode
           = Recording.Coordinator.string_of_mode
               Recording.Coordinator.Ptt_transcribe
      then
        enqueue (Ptt_loop.Release "transcribe")
          (Some "ptt transcribe stop queued")
      else
        enqueue (Ptt_loop.Press "transcribe")
          (Some "ptt transcribe start queued")
  | "ptt-translate-start" ->
      if snap.active then
        {
          action = raw_action;
          accepted = false;
          message = Some ("cannot start while " ^ snap.mode ^ " is active");
          snapshot = snap;
        }
      else
        enqueue (Ptt_loop.Press "translate") (Some "ptt translate start queued")
  | "ptt-translate-stop" ->
      enqueue (Ptt_loop.Release "translate") (Some "ptt translate stop queued")
  | "ptt-translate-toggle" ->
      let snap = Recording.Coordinator.snapshot coordinator in
      if
        snap.active
        && snap.mode
           = Recording.Coordinator.string_of_mode
               Recording.Coordinator.Ptt_translate
      then
        enqueue (Ptt_loop.Release "translate")
          (Some "ptt translate stop queued")
      else
        enqueue (Ptt_loop.Press "translate") (Some "ptt translate start queued")
  | "retry" | "ptt-retry" -> (
      match get_store () with
      | Error _ ->
          {
            action = raw_action;
            accepted = false;
            message = Some "recovery-store-unavailable";
            snapshot = snap;
          }
      | Ok store -> (
          match Active.peek store with
          | Error _ ->
              {
                action = raw_action;
                accepted = false;
                message = Some "recovery-store-unavailable";
                snapshot = snap;
              }
          | Ok None ->
              {
                action = raw_action;
                accepted = false;
                message = Some "no retained recording to retry";
                snapshot = snap;
              }
          | Ok (Some metadata) -> (
              let mode = mode_of_active_metadata metadata in
              match
                Recording.Coordinator.try_start coordinator
                  (recording_mode mode) ()
              with
              | Recording.Coordinator.Started attempt ->
                  Eio.Stream.add retries (metadata, attempt);
                  {
                    action = raw_action;
                    accepted = true;
                    message = Some "retry queued";
                    snapshot =
                      Assistant_service.Server.snapshot_of_coordinator
                        coordinator;
                  }
              | Recording.Coordinator.Busy active ->
                  {
                    action = raw_action;
                    accepted = false;
                    message =
                      Some
                        ("cannot retry while "
                        ^ Recording.Coordinator.string_of_mode active
                        ^ " is active");
                    snapshot = snap;
                  }
              | Recording.Coordinator.Disabled ->
                  {
                    action = raw_action;
                    accepted = false;
                    message = Some "cannot retry while PIE is paused";
                    snapshot = snap;
                  })))
  | _ ->
      {
        action = raw_action;
        accepted = false;
        message = Some ("unknown action: " ^ raw_action);
        snapshot = snap;
      }

type config_stamp = (int * int * int * float) option list

type config_control = {
  stamp : unit -> config_stamp;
  load : int -> Config.Stt.t * config_stamp;
}

type 'audio owned_inputs = {
  keyboard : Keyboard.Monitor.t;
  audio : 'audio;
  source_name : string option;
}

let run_with_inputs ~sw ~net ~process_mgr ~mono_clock ~inputs ~config_control
    ~stt ~deliver ~socket_path ~retry_dir ?ptt_transcribe_keysym
    ?ptt_translate_keysym ?(min_duration_ms = default_min_duration_ms)
    ?queue_pcm_bytes ?capture_pcm_bytes ?queue_capacity () =
  let* initial_config, initial_stamp =
    Eta.Effect.sync_exn
      (fun () ->
        Eio_unix.run_in_systhread ~label:"pie initial config load" (fun () ->
            config_control.load 3))
      ~catch:(function
        | Config.Stt.Config_error _ ->
            `Pulse_error (Pulse.Client.Protocol_error "config-load")
        | _exn -> `Pulse_error (Pulse.Client.Protocol_error "config-load"))
  in
  let initial_effective =
    try
      with_cli_overrides ?ptt_transcribe_keysym ?ptt_translate_keysym
        initial_config
    with Config.Stt.Config_error _ -> failwith "config:bindings"
  in
  let current = ref initial_effective in
  let last_stamp = ref initial_stamp in
  let coordinator = Recording.Coordinator.create () in
  let control : Ptt_loop.trigger Eio.Stream.t = Eio.Stream.create 1024 in
  let retries : (Active.metadata * Recording.Coordinator.attempt) Eio.Stream.t =
    Eio.Stream.create 16
  in
  let store_cell = ref None in
  let get_store () =
    match !store_cell with
    | Some store -> Ok store
    | None -> (
        match open_recovery_store ~root:retry_dir with
        | Ok store ->
            store_cell := Some store;
            Ok store
        | Error e -> Error e)
  in
  let send_control trigger = Eio.Stream.add control trigger in
  let* () =
    Eta.Effect.sync (fun () ->
        Assistant_service.Server.start ~sw ~net ~mono_clock ~socket_path
          ~action_handler:
            (action_handler ~coordinator ~send_control ~retries ~get_store)
          coordinator;
        Process.emit_sync ~level:Info "assistant_service started"
          ~attrs:[ ("config.path", config_path) ];
        Printf.printf "pie config:\n%s\n%!"
          (startup_config_json ~config:initial_effective
             ~ptt_transcribe_keysym:initial_effective.ptt_transcribe_keysym
             ~ptt_translate_keysym:initial_effective.ptt_translate_keysym
             ~min_duration_ms ()))
  in
  let watcher =
    let rec loop () =
      let* () = Eta.Effect.delay (Eta.Duration.ms 500) Eta.Effect.unit in
      let expected_stamp = !last_stamp in
      let* result =
        Eta.Effect.sync (fun () ->
            Eio_unix.run_in_systhread ~label:"pie config reload" (fun () ->
                let next_stamp = config_control.stamp () in
                if next_stamp = expected_stamp then `Unchanged
                else
                  try
                    let config, loaded_stamp = config_control.load 3 in
                    `Loaded (config, loaded_stamp)
                  with _exn -> `Failed))
      in
      let* () =
        match result with
        | `Unchanged -> Eta.Effect.unit
        | `Failed ->
            Eta.Effect.sync (fun () ->
                Process.emit_sync ~level:Error
                  "Pie config reload failed; keeping previous config"
                  ~attrs:
                    [
                      ("config.path", config_path);
                      ("config.error", "config:reload");
                    ])
        | `Loaded (new_config, loaded_stamp) ->
            Eta.Effect.sync (fun () ->
                try
                  current :=
                    with_cli_overrides ?ptt_transcribe_keysym
                      ?ptt_translate_keysym new_config;
                  last_stamp := loaded_stamp;
                  Process.emit_sync ~level:Info "Pie config reloaded"
                    ~attrs:[ ("config.path", config_path) ]
                with Config.Stt.Config_error _ ->
                  Process.emit_sync ~level:Error
                    "Pie config reload failed; keeping previous config"
                    ~attrs:
                      [
                        ("config.path", config_path);
                        ("config.error", "config:reload");
                      ])
      in
      loop ()
    in
    loop ()
  in
  let ptt_loop =
    let latch_keycode = ref None in
    let ptt_config : Ptt_loop.config =
      {
        recognize =
          make_live_recognizer ~coordinator ~latch_keycode
            ~load_bindings:(fun () ->
              let cfg = !current in
              ( {
                  keysym = cfg.ptt_transcribe_keysym;
                  shift = cfg.ptt_transcribe_shift;
                },
                {
                  keysym = cfg.ptt_translate_keysym;
                  shift = cfg.ptt_translate_shift;
                } ))
            ();
        min_duration_ms;
        capture_thresholds =
          (fun () ->
            ( !current.Config.Stt.ptt_min_capture_rms,
              !current.Config.Stt.ptt_min_capture_peak ));
        log_prefix = (fun mode -> "assistant-ptt-" ^ mode);
        source = inputs.source_name;
        on_ready =
          Eta.Effect.sync (fun () ->
              let cfg = !current in
              Printf.printf
                "PTT armed: transcribe=%d%s translate=%d%s source=%s\n%!"
                cfg.ptt_transcribe_keysym
                (if cfg.ptt_transcribe_shift then "+shift" else "")
                cfg.ptt_translate_keysym
                (if cfg.ptt_translate_shift then "+shift" else "")
                (Option.value ~default:"<default>" inputs.source_name));
        on_press =
          (fun mode_name ->
            on_press ~sw ~net ~process_mgr ~coordinator ~config:!current
              ?store:
                (match get_store () with
                | Ok store -> Some store
                | Error _ -> None)
              ~retry_dir ~stt ~deliver mode_name);
        on_release =
          (fun _ ->
            Eta.Effect.sync (fun () ->
                Option.iter
                  (fun attempt ->
                    ignore
                      (Recording.Coordinator.set_phase coordinator attempt
                         Recording.Phase.Stopping))
                  (Recording.Coordinator.current_attempt coordinator)));
        on_abort =
          Eta.Effect.sync (fun () ->
              not (Recording.Coordinator.snapshot coordinator).enabled);
        admission_active =
          (fun () -> Recording.Coordinator.is_active coordinator);
        on_dead_input =
          Desktop.Notification.notify_warning ~process_mgr
            "pie: no microphone input"
            "No input detected during push-to-talk. Your microphone may \
             be              muted.";
      }
    in
    let publish_current_phase phase =
      Eta.Effect.sync (fun () ->
          Option.iter
            (fun attempt ->
              ignore (Recording.Coordinator.set_phase coordinator attempt phase))
            (Recording.Coordinator.current_attempt coordinator))
    in
    Ptt_loop.run ~mono_clock ~control ~keyboard:inputs.keyboard
      ~audio:inputs.audio ?queue_pcm_bytes ?capture_pcm_bytes ?queue_capacity
      ~on_capture_resumed:(publish_current_phase Recording.Phase.Recording)
      ~on_low_input:
        (Desktop.Notification.notify_warning ~process_mgr
           "pie: microphone input is very quiet"
           "Microphone input has stayed very quiet for 3 seconds. Recording \
            will continue.")
      ptt_config
    |> Eta.Effect.on_error (fun cause ->
        Eta.Effect.sync (fun () ->
            Recording.Coordinator.set_error coordinator (status_of_cause cause)))
  in
  let retry_loop =
    let rec loop () =
      let* metadata, attempt =
        Eta.Effect.sync (fun () -> Eio.Stream.take retries)
      in
      let config = !current in
      let mode = mode_of_active_metadata metadata in
      let generation =
        match metadata with
        | Active.Audio { generation; _ } | Active.Text { generation; _ } ->
            generation
      in
      let release =
        Eta.Effect.sync (fun () ->
            ignore (Recording.Coordinator.release coordinator attempt))
      in
      let publish_text_slot store text =
        unless_cancelled attempt
          (Eta.Effect.sync (fun () ->
               Active.publish_text store ~mode:(retry_mode mode) ~text
                 ~dispatch_uncertain:true)
          |> Eta.Effect.bind retry_result
          |> Eta.Effect.map (fun _ -> ()))
      in
      let clear_selected store =
        Eta.Effect.sync (fun () -> Active.clear store generation)
        |> Eta.Effect.bind retry_result
        |> Eta.Effect.on_error (fun _ ->
            Eta.Effect.sync (fun () ->
                Recording.Coordinator.set_error coordinator "recovery:cleanup";
                Process.emit_sync ~level:Error
                  "retained recording retry cleanup failed"
                  ~attrs:[ ("recording.error", "recovery:cleanup") ]))
      in
      let retry_body store =
        match metadata with
        | Active.Audio _ ->
            let* pcm =
              Eta.Effect.sync (fun () -> Active.load_audio store generation)
              |> Eta.Effect.bind retry_result
            in
            let* () =
              recognize_then_deliver ~recognize:(stt config mode pcm)
                ~deliver:(fun text -> deliver config mode text)
                ~on_provider_failure:Eta.Effect.unit ~on_blank:Eta.Effect.unit
                ~on_delivery_failure:(publish_text_slot store)
                ~on_success:Eta.Effect.unit
            in
            let* () = clear_selected store in
            Eta.Effect.sync (fun () ->
                Process.emit_sync ~level:Info
                  "retained recording retry succeeded"
                  ~attrs:[ ("recording.stage", "cleared") ])
        | Active.Text _ ->
            let* text =
              Eta.Effect.sync (fun () -> Active.load_text store generation)
              |> Eta.Effect.bind retry_result
            in
            let* () =
              deliver_retained_text
                ~deliver:(fun t -> deliver config mode t)
                ~on_delivery_failure:(publish_text_slot store)
                ~on_success:Eta.Effect.unit text
            in
            let* () = clear_selected store in
            Eta.Effect.sync (fun () ->
                Process.emit_sync ~level:Info
                  "retained recording retry succeeded"
                  ~attrs:[ ("recording.stage", "cleared") ])
      in
      let* () =
        (match get_store () with
          | Error e ->
              let* () = release in
              Eta.Effect.fail (ptt_of_retry e)
          | Ok store ->
              run_acquired_attempt ~attempt ~on_settle:release
                (retry_body store))
        |> Eta.Effect.on_error (fun cause ->
            Eta.Effect.sync (fun () ->
                Recording.Coordinator.set_error coordinator
                  (ptt_status_of_failures (Eta.Cause.failures cause));
                Process.emit_sync ~level:Error "retained recording retry failed"
                  ~attrs:[ ("recording.error", "stt:retry") ]))
        |> Ptt_loop.recover_single_failure (function
          | `Ptt_error "pcm:capture_budget" | `Ptt_error "pcm:queue_budget" ->
              None
          | `Ptt_error _ -> Some Eta.Effect.unit)
      in
      loop ()
    in
    loop ()
  in
  Eta.Effect.par watcher
    (Eta.Effect.par ptt_loop
       (retry_loop |> Eta.Effect.map_error Ptt_loop.widen_ptt))
  |> Eta.Effect.map (fun _ -> ())

let run ~sw ~net ~clock:_ ~mono_clock ~process_mgr ?ptt_transcribe_keysym
    ?ptt_translate_keysym ?(min_duration_ms = default_min_duration_ms) ?stdin:_
    () =
  let config_control = { stamp = config_stamp; load = load_snapshot } in
  let stt = production_stt ~sw ~net ~process_mgr in
  Pulse.Client.with_client ~net @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun info audio ->
  Keyboard.Monitor.with_monitor ~sw ~net ~process_mgr @@ fun keyboard ->
  let clipboard_owner = Wayland.Clipboard.create ~sw ~process_mgr () in
  let deliver = production_deliver ~process_mgr ~clipboard_owner in
  run_with_inputs ~sw ~net ~process_mgr ~mono_clock
    ~inputs:{ keyboard; audio; source_name = info.Pulse.Defs.source_name }
    ~config_control ~stt ~deliver ~socket_path:Paths.daemon_socket_path
    ~retry_dir:Paths.failed_recordings_dir ?ptt_transcribe_keysym
    ?ptt_translate_keysym ~min_duration_ms ()
