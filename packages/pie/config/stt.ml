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

type entry_method = Input.Text_injection.entry_method = Type | Paste

type text_entry_rule = Input.Text_injection.text_entry_rule = {
  app_id : string option;
  title_regex : string option;
  method_ : entry_method;
}

type t = {
  provider : provider;
  transcription_model : string;
  translation_model : string;
  transcription_language : string;
  translation_source_language : string;
  translation_target_language : string;
  ptt_min_capture_rms : float;
  ptt_min_capture_peak : float;
  ptt_transcribe_keysym : int;
  ptt_translate_keysym : int;
  ptt_transcribe_shift : bool;
  ptt_translate_shift : bool;
  thinking_effort : string option;
  transcription_prompt : string;
  translation_prompt : string;
  newline_rules : newline_rule list;
  text_entry_rules : text_entry_rule list;
}

exception Config_error of string

type resolved_model = { provider : provider; model : string }

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
      let suffix =
        String.sub value (index + 1) (String.length value - index - 1)
      in
      Some (prefix, suffix)

let provider_of_string_opt value =
  try Some (provider_of_string value) with Config_error _ -> None

let resolve_model_provider ~default_provider model =
  match split_first_slash model with
  | Some (prefix, model) -> (
      match provider_of_string_opt prefix with
      | Some provider when String.trim model = "" ->
          raise
            (Config_error
               (Printf.sprintf "model for provider %s is empty" prefix))
      | Some provider -> { provider; model }
      | None -> { provider = default_provider; model = prefix ^ "/" ^ model })
  | None -> { provider = default_provider; model }

let newline_action_of_string = function
  | "collapse" | "collapse-to-space" -> Collapse_to_space
  | "shift-enter" -> Shift_enter
  | "ctrl-j" -> Ctrl_j
  | s ->
      raise (Config_error (Printf.sprintf "unknown newline rule action: %s" s))

let string_of_newline_action = function
  | Collapse_to_space -> "collapse-to-space"
  | Shift_enter -> "shift-enter"
  | Ctrl_j -> "ctrl-j"

let entry_method_of_string = function
  | "type" -> Type
  | "paste" -> Paste
  | s -> raise (Config_error (Printf.sprintf "unknown text entry method: %s" s))

let string_of_entry_method = function Type -> "type" | Paste -> "paste"
let max_config_file_bytes = 1024 * 1024

let toml_find table key =
  match table with `Assoc fields -> List.assoc_opt key fields | _ -> None

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
    raise
      (Config_error
         "transcription prompt missing required placeholder {{language}}")

let validate_translation_prompt content =
  let trimmed = String.trim content in
  if String.length trimmed = 0 then
    raise (Config_error "translation prompt is empty");
  if
    not
      (contains_substring ~needle:"{{source_language}}" trimmed
      && contains_substring ~needle:"{{target_language}}" trimmed)
  then
    raise
      (Config_error
         "translation prompt missing required placeholders {{source_language}} \
          and {{target_language}}")

let type_error key expected =
  raise (Config_error (Printf.sprintf "%s must be %s" key expected))

let validate_unit_interval name value =
  if (not (Float.is_finite value)) || value < 0. || value > 1. then
    raise
      (Config_error (Printf.sprintf "%s must be a finite value in 0.0-1.0" name))

let get_string table key default_val =
  match toml_find table key with
  | Some (`String value) -> value
  | Some _ -> type_error key "a string"
  | None -> default_val

let get_bool table key default_val =
  match toml_find table key with
  | Some (`Bool value) -> value
  | Some _ -> type_error key "a bool"
  | None -> default_val

let get_float table key default_val =
  match toml_find table key with
  | Some (`Float value) -> value
  | Some (`Int value) -> float_of_int value
  | Some _ -> type_error key "a number"
  | None -> default_val

let get_int table key default_val =
  match toml_find table key with
  | Some (`Int value) -> value
  | Some _ -> type_error key "an integer"
  | None -> default_val

let get_optional_string table key =
  match toml_find table key with
  | Some (`String value) -> (
      match String.trim value with "" -> None | trimmed -> Some trimmed)
  | Some `Null | None -> None
  | Some _ -> type_error key "a string"

let parse_thinking_effort table =
  match get_optional_string table "thinking_effort" with
  | None -> None
  | Some value -> (
      match String.lowercase_ascii (String.trim value) with
      | "" -> raise (Config_error "thinking_effort must not be empty")
      | ("minimal" | "low" | "medium" | "high" | "xhigh") as effort ->
          Some effort
      | effort ->
          raise
            (Config_error
               (Printf.sprintf
                  "thinking_effort must be minimal, low, medium, high, or \
                   xhigh; got %s"
                  effort)))

let validate_title_regex section index = function
  | None -> ()
  | Some pattern -> (
      try ignore (Re.Perl.compile_pat pattern)
      with Re.Perl.Parse_error ->
        raise
          (Config_error
             (Printf.sprintf "%s[%d].title_regex is not a valid regex" section
                index)))

let parse_newline_rule index table =
  let app_id = get_optional_string table "app_id" in
  let title_regex = get_optional_string table "title_regex" in
  let action =
    match get_optional_string table "action" with
    | Some action -> newline_action_of_string action
    | None ->
        raise
          (Config_error
             (Printf.sprintf "newline_rules[%d].action is required" index))
  in
  if app_id = None && title_regex = None then
    raise
      (Config_error
         (Printf.sprintf "newline_rules[%d] must set app_id or title_regex"
            index));
  validate_title_regex "newline_rules" index title_regex;
  { app_id; title_regex; action }

let parse_newline_rules table =
  match toml_find table "newline_rules" with
  | None -> []
  | Some (`List rules)
    when List.for_all (function `Assoc _ -> true | _ -> false) rules ->
      List.mapi parse_newline_rule rules
  | Some _ -> raise (Config_error "newline_rules must be an array of tables")

let parse_text_entry_rule index table =
  let app_id = get_optional_string table "app_id" in
  let title_regex = get_optional_string table "title_regex" in
  let method_ =
    match get_optional_string table "method" with
    | Some method_ -> entry_method_of_string method_
    | None ->
        raise
          (Config_error
             (Printf.sprintf "text_entry_rules[%d].method is required" index))
  in
  if app_id = None && title_regex = None then
    raise
      (Config_error
         (Printf.sprintf "text_entry_rules[%d] must set app_id or title_regex"
            index));
  validate_title_regex "text_entry_rules" index title_regex;
  { app_id; title_regex; method_ }

let parse_text_entry_rules table =
  match toml_find table "text_entry_rules" with
  | None -> []
  | Some (`List rules)
    when List.for_all (function `Assoc _ -> true | _ -> false) rules ->
      List.mapi parse_text_entry_rule rules
  | Some _ -> raise (Config_error "text_entry_rules must be an array of tables")

let reject_unsupported_wakeword table =
  List.iter
    (fun key ->
      match toml_find table key with
      | None -> ()
      | Some _ -> raise (Config_error ("unsupported wakeword setting: " ^ key)))
    [
      "wakeword_enabled";
      "wakeword_dictation_silence_seconds";
      "wakeword_dictation_max_seconds";
      "wakeword_dictation_speech_rms_threshold";
    ];
  match toml_find table "wakeword" with
  | None -> ()
  | Some _ -> raise (Config_error "unsupported wakeword table")

let of_json table : t =
  (match table with
  | `Assoc _ -> ()
  | _ -> raise (Config_error "STT config must be an object"));
  reject_unsupported_wakeword table;
  let provider =
    provider_of_string (get_string table "provider" "codex-realtime")
  in
  let default_model =
    match provider with
    | Codex_realtime -> "gpt-realtime-2.1"
    | Openrouter -> "openrouter/xiaomi/mimo-v2.5"
  in
  let transcription_model =
    get_string table "transcription_model" default_model
  in
  let translation_model = get_string table "translation_model" default_model in
  let ptt_transcribe_keysym = get_int table "ptt_transcribe_keysym" 65478 in
  let ptt_translate_keysym = get_int table "ptt_translate_keysym" 65479 in
  let ptt_transcribe_shift = get_bool table "ptt_transcribe_shift" false in
  let ptt_translate_shift = get_bool table "ptt_translate_shift" false in
  if ptt_transcribe_keysym <= 0 then
    raise (Config_error "ptt_transcribe_keysym must be positive");
  if ptt_translate_keysym <= 0 then
    raise (Config_error "ptt_translate_keysym must be positive");
  if
    ptt_transcribe_keysym = ptt_translate_keysym
    && ptt_transcribe_shift = ptt_translate_shift
  then raise (Config_error "PTT transcribe and translate bindings must differ");
  ignore (resolve_model_provider ~default_provider:provider transcription_model);
  ignore (resolve_model_provider ~default_provider:provider translation_model);
  let transcription_prompt =
    get_string table "transcription_prompt"
      "Transcribe the following audio in {{language}}."
  in
  let translation_prompt =
    get_string table "translation_prompt"
      "Translate from {{source_language}} to {{target_language}}."
  in
  validate_transcription_prompt transcription_prompt;
  validate_translation_prompt translation_prompt;
  let ptt_min_capture_rms =
    get_float table "ptt_min_capture_rms" Audio.Pcm.ptt_min_capture_rms_for_stt
  in
  let ptt_min_capture_peak =
    get_float table "ptt_min_capture_peak"
      Audio.Pcm.ptt_min_capture_peak_for_stt
  in
  validate_unit_interval "ptt_min_capture_rms" ptt_min_capture_rms;
  validate_unit_interval "ptt_min_capture_peak" ptt_min_capture_peak;
  {
    provider;
    transcription_model;
    translation_model;
    transcription_language = get_string table "transcription_language" "English";
    translation_source_language =
      get_string table "translation_source_language" "English";
    translation_target_language =
      get_string table "translation_target_language" "English";
    ptt_min_capture_rms;
    ptt_min_capture_peak;
    ptt_transcribe_keysym;
    ptt_translate_keysym;
    ptt_transcribe_shift;
    ptt_translate_shift;
    thinking_effort = parse_thinking_effort table;
    transcription_prompt;
    translation_prompt;
    newline_rules = parse_newline_rules table;
    text_entry_rules = parse_text_entry_rules table;
  }

let decode_json json =
  match of_json json with
  | config -> Ok config
  | exception Config_error message -> Error message

let yojson_of_t (config : t) =
  let string x = `String x in
  let opt = function Some x -> string x | None -> `Null in
  let rule action (r : newline_rule) =
    `Assoc
      [
        ("app_id", opt r.app_id);
        ("title_regex", opt r.title_regex);
        ("action", string (string_of_newline_action action));
      ]
  in
  let entry (r : text_entry_rule) =
    `Assoc
      [
        ("app_id", opt r.app_id);
        ("title_regex", opt r.title_regex);
        ("method", string (string_of_entry_method r.method_));
      ]
  in
  `Assoc
    [
      ("provider", string (string_of_provider config.provider));
      ("transcription_model", string config.transcription_model);
      ("translation_model", string config.translation_model);
      ("transcription_language", string config.transcription_language);
      ("translation_source_language", string config.translation_source_language);
      ("translation_target_language", string config.translation_target_language);
      ("ptt_min_capture_rms", `Float config.ptt_min_capture_rms);
      ("ptt_min_capture_peak", `Float config.ptt_min_capture_peak);
      ("ptt_transcribe_keysym", `Int config.ptt_transcribe_keysym);
      ("ptt_translate_keysym", `Int config.ptt_translate_keysym);
      ("ptt_transcribe_shift", `Bool config.ptt_transcribe_shift);
      ("ptt_translate_shift", `Bool config.ptt_translate_shift);
      ("thinking_effort", opt config.thinking_effort);
      ("transcription_prompt", string config.transcription_prompt);
      ("translation_prompt", string config.translation_prompt);
      ( "newline_rules",
        `List (List.map (fun r -> rule r.action r) config.newline_rules) );
      ("text_entry_rules", `List (List.map entry config.text_entry_rules));
    ]

let door_spec : t Ox_app.Config.t =
  {
    of_yojson = decode_json;
    yojson_of_t;
    env = (fun ~getenv:_ config -> config);
  }

let default_source ~caps () =
  Ox_app.Config.resolve_with ~file_name:"stt.toml" ~name:"pie" ~config:None
    ~getenv:(Ox_caps.Cap_sys.getenv_opt ~caps)

let source_path : Ox_app.Config.source -> string = function
  | `Default path | `Explicit path -> path

let load ~caps ~fs ?source ?read () =
  let source = Option.value source ~default:(default_source ~caps ()) in
  let path = source_path source in
  let read =
    Option.value read ~default:(fun path ->
        Ox_app.Config.read_regular ~max_bytes:max_config_file_bytes
          Eio.Path.(fs / path))
  in
  let result =
    Ox_app.Config.load_with ~missing:`Required ~spec:door_spec
      ~getenv:(Ox_caps.Cap_sys.getenv_opt ~caps)
      ~read ~source
  in
  let result =
    match result with
    | Error (`Config _) as err
      when Sys.file_exists (Filename.concat (Filename.dirname path) "stt.json")
      -> (
        match read path with
        | Error `Missing ->
            Error
              (`Config
                 (Printf.sprintf
                    "STT config moved to TOML at %s; migrate legacy JSON \
                     config from %s"
                    path
                    (Filename.concat (Filename.dirname path) "stt.json")))
        | _ -> err)
    | other -> other
  in
  let ( let* ) = Result.bind in
  let* config = result in
  let prompt name default =
    let prompt_path =
      Filename.concat (Filename.concat (Filename.dirname path) "prompts") name
    in
    match read prompt_path with
    | Ok content -> Ok content
    | Error `Missing -> Ok default
    | Error (`Unreadable reason) ->
        Error (`Config (prompt_path ^ ": " ^ reason))
  in
  let* transcription_prompt =
    prompt "transcription.md" config.transcription_prompt
  in
  let* translation_prompt = prompt "translation.md" config.translation_prompt in
  match
    validate_transcription_prompt transcription_prompt;
    validate_translation_prompt translation_prompt;
    { config with transcription_prompt; translation_prompt }
  with
  | config -> Ok config
  | exception Config_error message -> Error (`Config message)
