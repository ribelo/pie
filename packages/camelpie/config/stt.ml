type provider = Codex_realtime | Openrouter

type t = {
  schema_version : int;
  provider : provider;
  transcription_model : string;
  translation_model : string;
  transcription_language : string;
  translation_source_language : string;
  translation_target_language : string;
  wakeword_enabled : bool;
  wakeword_dictation_silence_seconds : float;
  wakeword_dictation_max_seconds : float;
  wakeword_dictation_speech_rms_threshold : float;
  transcription_prompt : string;
  translation_prompt : string;
}

exception Config_error of string

let provider_of_string = function
  | "codex-realtime" -> Codex_realtime
  | "openrouter" -> Openrouter
  | s -> raise (Config_error (Printf.sprintf "unknown provider: %s" s))

let string_of_provider = function
  | Codex_realtime -> "codex-realtime"
  | Openrouter -> "openrouter"

let load_prompt path =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    content
  with Sys_error msg ->
    raise (Config_error (Printf.sprintf "failed to read prompt %s: %s" path msg))

let validate_transcription_prompt content =
  let trimmed = String.trim content in
  if String.length trimmed = 0 then
    raise (Config_error "transcription prompt is empty");
  if not (String.contains trimmed '{') then
    raise (Config_error "transcription prompt missing required placeholder {{language}}")

let validate_translation_prompt content =
  let trimmed = String.trim content in
  if String.length trimmed = 0 then
    raise (Config_error "translation prompt is empty");
  if not (String.contains trimmed '{') then
    raise (Config_error "translation prompt missing required placeholders")

let load ?(config_path = Paths.stt_config_path) () : t =
  let raw =
    try
      let ic = open_in_bin config_path in
      let len = in_channel_length ic in
      let content = really_input_string ic len in
      close_in ic;
      content
    with Sys_error _ ->
      raise (Config_error (Printf.sprintf "STT config not found at %s" config_path))
  in
  let json =
    try Yojson.Safe.from_string raw
    with Yojson.Json_error msg ->
      raise (Config_error (Printf.sprintf "invalid JSON in %s: %s" config_path msg))
  in
  match json with
  | `Assoc fields ->
    let find_string key default_val =
      match List.assoc_opt key fields with
      | Some (`String s) -> s
      | Some _ -> raise (Config_error (Printf.sprintf "%s must be a string" key))
      | None -> default_val
    in
    let find_bool key default_val =
      match List.assoc_opt key fields with
      | Some (`Bool b) -> b
      | Some _ -> raise (Config_error (Printf.sprintf "%s must be a bool" key))
      | None -> default_val
    in
    let find_float key default_val =
      match List.assoc_opt key fields with
      | Some (`Float f) -> f
      | Some (`Int i) -> float_of_int i
      | Some _ -> raise (Config_error (Printf.sprintf "%s must be a number" key))
      | None -> default_val
    in
    let schema_version =
      match List.assoc_opt "schemaVersion" fields with
      | Some (`Int 2) -> 2
      | Some (`Int 1) ->
        raise (Config_error "schemaVersion 1 is no longer supported; use schemaVersion 2")
      | Some _ -> raise (Config_error "schemaVersion must be 2")
      | None -> raise (Config_error "missing schemaVersion")
    in
    let provider =
      try provider_of_string (find_string "provider" "codex-realtime")
      with Config_error _ -> Codex_realtime
    in
    let prompts_dir = Filename.concat (Filename.dirname config_path) "prompts" in
    let transcription_prompt_path = Filename.concat prompts_dir "transcription.md" in
    let translation_prompt_path = Filename.concat prompts_dir "translation.md" in
    let transcription_prompt =
      try load_prompt transcription_prompt_path
      with Config_error _ -> "Transcribe the following audio in {{language}}."
    in
    let translation_prompt =
      try load_prompt translation_prompt_path
      with Config_error _ -> "Translate from {{source_language}} to {{target_language}}."
    in
    validate_transcription_prompt transcription_prompt;
    validate_translation_prompt translation_prompt;
    {
      schema_version;
      provider;
      transcription_model = find_string "transcriptionModel" "gpt-realtime-whisper";
      translation_model = find_string "translationModel" "gpt-realtime-2";
      transcription_language = find_string "transcriptionLanguage" "English";
      translation_source_language = find_string "translationSourceLanguage" "English";
      translation_target_language = find_string "translationTargetLanguage" "English";
      wakeword_enabled = find_bool "wakewordEnabled" true;
      wakeword_dictation_silence_seconds = find_float "wakewordDictationSilenceSeconds" 3.0;
      wakeword_dictation_max_seconds = find_float "wakewordDictationMaxSeconds" 120.0;
      wakeword_dictation_speech_rms_threshold = find_float "wakewordDictationSpeechRmsThreshold" 0.01;
      transcription_prompt;
      translation_prompt;
    }
  | _ ->
    raise (Config_error "STT config must be a JSON object")
