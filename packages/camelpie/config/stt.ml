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
  transcription_prompt : string;
  translation_prompt : string;
  newline_rules : newline_rule list;
}

exception Config_error of string

let provider_of_string = function
  | "codex-realtime" -> Codex_realtime
  | "openrouter" -> Openrouter
  | s -> raise (Config_error (Printf.sprintf "unknown provider: %s" s))

let string_of_provider = function
  | Codex_realtime -> "codex-realtime"
  | Openrouter -> "openrouter"

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
    provider = provider_of_string (get_string table "provider" "codex-realtime");
    transcription_model = get_string table "transcription_model" "gpt-realtime-whisper";
    translation_model = get_string table "translation_model" "gpt-realtime-2";
    transcription_language = get_string table "transcription_language" "English";
    translation_source_language = get_string table "translation_source_language" "English";
    translation_target_language = get_string table "translation_target_language" "English";
    wakeword_enabled = get_bool table "wakeword_enabled" true;
    wakeword_dictation_silence_seconds = get_float table "wakeword_dictation_silence_seconds" 3.0;
    wakeword_dictation_max_seconds = get_float table "wakeword_dictation_max_seconds" 120.0;
    wakeword_dictation_speech_rms_threshold = get_float table "wakeword_dictation_speech_rms_threshold" 0.01;
    transcription_prompt;
    translation_prompt;
    newline_rules = parse_newline_rules table;
  }

