type provider = Codex_realtime | Openrouter

type newline_action = Input.Text_injection.newline_action =
  | Collapse_to_space
  | Shift_enter
  | Ctrl_j

type newline_rule = Input.Text_injection.newline_rule = {
  app_id : string option;
  title_regex : string option;
  action : newline_action;
}

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
  ptt_min_capture_rms : float;
  ptt_min_capture_peak : float;
  thinking_effort : string option;
  transcription_prompt : string;
  translation_prompt : string;
  newline_rules : newline_rule list;
}

exception Config_error of string

type resolved_model = {
  provider : provider;
  model : string;
}

let provider_of_string = function
  | "codex-realtime" -> Codex_realtime
  | "openrouter" -> Openrouter
  | s -> raise (Config_error (Printf.sprintf "unknown provider: %s" s))

let string_of_provider = function
  | Codex_realtime -> "codex-realtime"
  | Openrouter -> "openrouter"

let split_first_slash value =
  match String.index_opt value '/' with
  | None -> None
  | Some index ->
    let prefix = String.sub value 0 index in
    let suffix = String.sub value (index + 1) (String.length value - index - 1) in
    Some (prefix, suffix)

let provider_of_string_opt value =
  try Some (provider_of_string value) with Config_error _ -> None

let resolve_model_provider ~default_provider model =
  match split_first_slash model with
  | Some (prefix, model) ->
    (match provider_of_string_opt prefix with
     | Some provider when String.trim model = "" ->
       raise
         (Config_error
            (Printf.sprintf "model for provider %s is empty" prefix))
     | Some provider -> { provider; model }
     | None -> { provider = default_provider; model = prefix ^ "/" ^ model })
  | None -> { provider = default_provider; model }

let newline_action_of_string = function
  | "collapse"
  | "collapse-to-space" -> Collapse_to_space
  | "shift-enter" -> Shift_enter
  | "ctrl-j" -> Ctrl_j
  | s -> raise (Config_error (Printf.sprintf "unknown newline rule action: %s" s))

let string_of_newline_action = function
  | Collapse_to_space -> "collapse-to-space"
  | Shift_enter -> "shift-enter"
  | Ctrl_j -> "ctrl-j"

let load_prompt path =
  try
    let ic = open_in_bin path in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    content
  with Sys_error msg ->
    raise (Config_error (Printf.sprintf "failed to read prompt %s: %s" path msg))

let contains_substring ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  if needle_len = 0 then true
  else if needle_len > haystack_len then false
  else begin
    let rec scan i =
      if i > haystack_len - needle_len then false
      else if String.sub haystack i needle_len = needle then true
      else scan (i + 1)
    in
    scan 0
  end

let validate_transcription_prompt content =
  let trimmed = String.trim content in
  if String.length trimmed = 0 then
    raise (Config_error "transcription prompt is empty");
  if not (contains_substring ~needle:"{{language}}" trimmed) then
    raise (Config_error "transcription prompt missing required placeholder {{language}}")

let validate_translation_prompt content =
  let trimmed = String.trim content in
  if String.length trimmed = 0 then
    raise (Config_error "translation prompt is empty");
  if not
       (contains_substring ~needle:"{{source_language}}" trimmed
        && contains_substring ~needle:"{{target_language}}" trimmed)
  then
    raise
      (Config_error
         "translation prompt missing required placeholders {{source_language}} and {{target_language}}")

module TT = Toml.Types

let toml_key key = TT.Table.Key.of_string key

let toml_find table key = TT.Table.find_opt (toml_key key) table

let type_error key expected =
  raise (Config_error (Printf.sprintf "%s must be %s" key expected))

let get_required_int table key =
  match toml_find table key with
  | Some (TT.TInt value) -> value
  | Some _ -> type_error key "an integer"
  | None -> raise (Config_error (Printf.sprintf "missing %s" key))

let get_string table key default_val =
  match toml_find table key with
  | Some (TT.TString value) -> value
  | Some _ -> type_error key "a string"
  | None -> default_val

let get_bool table key default_val =
  match toml_find table key with
  | Some (TT.TBool value) -> value
  | Some _ -> type_error key "a bool"
  | None -> default_val

let get_float table key default_val =
  match toml_find table key with
  | Some (TT.TFloat value) -> value
  | Some (TT.TInt value) -> float_of_int value
  | Some _ -> type_error key "a number"
  | None -> default_val

let get_optional_string table key =
  match toml_find table key with
  | Some (TT.TString value) -> Some value
  | Some _ -> type_error key "a string"
  | None -> None

let parse_thinking_effort table =
  match get_optional_string table "thinking_effort" with
  | None -> None
  | Some value ->
    (match String.lowercase_ascii (String.trim value) with
     | "" -> raise (Config_error "thinking_effort must not be empty")
     | "minimal"
     | "low"
     | "medium"
     | "high"
     | "xhigh" as effort -> Some effort
     | effort ->
       raise
         (Config_error
            (Printf.sprintf
               "thinking_effort must be minimal, low, medium, high, or xhigh; got %s"
               effort)))

let validate_title_regex index = function
  | None -> ()
  | Some pattern ->
    try ignore (Re.Perl.compile_pat pattern)
    with Re.Perl.Parse_error ->
      raise (Config_error (Printf.sprintf "newline_rules[%d].title_regex is not a valid regex" index))

let parse_newline_rule index table =
  let app_id = get_optional_string table "app_id" in
  let title_regex = get_optional_string table "title_regex" in
  let action =
    match get_optional_string table "action" with
    | Some action -> newline_action_of_string action
    | None -> raise (Config_error (Printf.sprintf "newline_rules[%d].action is required" index))
  in
  if app_id = None && title_regex = None then
    raise (Config_error (Printf.sprintf "newline_rules[%d] must set app_id or title_regex" index));
  validate_title_regex index title_regex;
  { app_id; title_regex; action }

let parse_newline_rules table =
  match toml_find table "newline_rules" with
  | None -> []
  | Some (TT.TArray (TT.NodeTable rules)) -> List.mapi parse_newline_rule rules
  | Some _ -> raise (Config_error "newline_rules must be an array of tables")

let load_toml config_path =
  try
    match Toml.Parser.from_filename config_path with
    | `Ok table -> table
    | `Error (msg, location) ->
      raise
        (Config_error
           (Printf.sprintf
              "invalid TOML in %s:%d:%d: %s"
              config_path
              location.Toml.Parser.line
              location.column
              msg))
  with Sys_error _ ->
    let legacy_json = Filename.concat (Filename.dirname config_path) "stt.json" in
    if Sys.file_exists legacy_json then
      raise
        (Config_error
           (Printf.sprintf
              "STT config moved to TOML at %s; migrate legacy JSON config from %s"
              config_path
              legacy_json))
    else
      raise (Config_error (Printf.sprintf "STT config not found at %s" config_path))

let load ?(config_path = Paths.stt_config_path) () : t =
  let table = load_toml config_path in
  let schema_version = get_required_int table "schema_version" in
  if schema_version <> 2 then
    raise (Config_error "schema_version must be 2");
  let provider = provider_of_string (get_string table "provider" "codex-realtime") in
  let default_model = "openrouter/xiaomi/mimo-v2.5" in
  let transcription_model = get_string table "transcription_model" default_model in
  let translation_model = get_string table "translation_model" default_model in
  ignore (resolve_model_provider ~default_provider:provider transcription_model);
  ignore (resolve_model_provider ~default_provider:provider translation_model);
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
    transcription_model;
    translation_model;
    transcription_language = get_string table "transcription_language" "English";
    translation_source_language = get_string table "translation_source_language" "English";
    translation_target_language = get_string table "translation_target_language" "English";
    wakeword_enabled = get_bool table "wakeword_enabled" true;
    wakeword_dictation_silence_seconds = get_float table "wakeword_dictation_silence_seconds" 3.0;
    wakeword_dictation_max_seconds = get_float table "wakeword_dictation_max_seconds" 120.0;
    wakeword_dictation_speech_rms_threshold = get_float table "wakeword_dictation_speech_rms_threshold" 0.01;
    ptt_min_capture_rms = get_float table "ptt_min_capture_rms" Audio.Pcm.ptt_min_capture_rms_for_stt;
    ptt_min_capture_peak = get_float table "ptt_min_capture_peak" Audio.Pcm.ptt_min_capture_peak_for_stt;
    thinking_effort = parse_thinking_effort table;
    transcription_prompt;
    translation_prompt;
    newline_rules = parse_newline_rules table;
  }
