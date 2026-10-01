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

type mode = Transcribe | Translate
type pipeline = Legacy | One_pass | Two_pass

type correction = {
  model : string;
  reasoning_effort : string option;
  timeout_seconds : float;
  temperature : float;
}

type binding = {
  keysym : int;
  shift : bool;
  mode : mode;
  correct : bool;
  prompt_named : bool;
  prompt_file : string;
  prompt : string;
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
  pipeline : pipeline;
  streaming : bool;
  correction : correction option;
  bindings : binding list;
  audio_retention_hours : float;
  openrouter_provider_order : string list option;
  one_pass_timeout_seconds : float;
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

let string_of_mode = function
  | Transcribe -> "transcribe"
  | Translate -> "translate"

let string_of_pipeline = function
  | Legacy -> "legacy"
  | One_pass -> "one-pass"
  | Two_pass -> "two-pass"

let pipeline_of_string = function
  | "legacy" -> Legacy
  | "one-pass" -> One_pass
  | "two-pass" -> Two_pass
  | other ->
      raise
        (Config_error
           (Printf.sprintf
              "pipeline must be legacy, one-pass, or two-pass; got %s" other))

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
let default_audio_retention_hours = 168.
let default_correction_timeout_seconds = 8.
let default_one_pass_timeout_seconds = 30.
let default_correction_temperature = 0.2
let default_correction_prompt_file = "correction.md"
let default_correction_translate_prompt_file = "correction-translate.md"
let default_one_pass_prompt_file = "one-pass.md"
let default_one_pass_verbatim_prompt_file = "one-pass-verbatim.md"
let default_one_pass_translate_prompt_file = "one-pass-translate.md"
let default_correction_prompt = Embedded_prompts.correction
let default_correction_translate_prompt = Embedded_prompts.correction_translate
let default_one_pass_prompt = Embedded_prompts.one_pass
let default_one_pass_verbatim_prompt = Embedded_prompts.one_pass_verbatim
let default_one_pass_translate_prompt = Embedded_prompts.one_pass_translate
let default_transcribe_keysym = 65478
let default_translate_keysym = 65479

let find_binding config ~keysym ~shift =
  List.find_opt
    (fun binding -> binding.keysym = keysym && binding.shift = shift)
    config.bindings

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
         "translation prompt missing required placeholders \
          {{source_language}}           and {{target_language}}")

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

let parse_audio_retention table =
  let value =
    get_float table "audio_retention_hours" default_audio_retention_hours
  in
  if (not (Float.is_finite value)) || value < 0. then
    raise
      (Config_error "audio_retention_hours must be a finite non-negative number");
  value

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

let optional_number table key =
  match toml_find table key with
  | None | Some `Null -> None
  | Some (`Float value) -> Some value
  | Some (`Int value) -> Some (float_of_int value)
  | Some _ -> type_error key "a number"

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
                  "thinking_effort must be minimal, low, medium, high, \
                   or                    xhigh; got %s"
                  effort)))

let parse_correction_reasoning table =
  match get_optional_string table "correction_reasoning_effort" with
  | None -> None
  | Some value -> (
      match String.lowercase_ascii (String.trim value) with
      | "" ->
          raise (Config_error "correction_reasoning_effort must not be empty")
      | ("none" | "minimal" | "low" | "medium" | "high" | "xhigh") as effort ->
          Some effort
      | effort ->
          raise
            (Config_error
               (Printf.sprintf
                  "correction_reasoning_effort must be none, minimal, \
                   low,                    medium, high, or xhigh; got %s"
                  effort)))

let parse_correction_timeout table =
  match optional_number table "correction_timeout_seconds" with
  | None -> default_correction_timeout_seconds
  | Some value ->
      if (not (Float.is_finite value)) || value <= 0. then
        raise
          (Config_error
             "correction_timeout_seconds must be a positive finite number");
      value

let parse_correction_temperature table =
  match optional_number table "correction_temperature" with
  | None -> default_correction_temperature
  | Some value ->
      if (not (Float.is_finite value)) || value < 0. || value > 2. then
        raise
          (Config_error
             "correction_temperature must be a finite value in 0.0-2.0");
      value

let parse_correction_model table =
  match toml_find table "correction_model" with
  | None | Some `Null -> None
  | Some (`String value) -> (
      let value = String.trim value in
      if value = "" then
        raise (Config_error "correction_model must not be empty");
      let resolved =
        resolve_model_provider ~default_provider:Openrouter value
      in
      match resolved.provider with
      | Openrouter -> Some resolved.model
      | Codex_realtime ->
          raise
            (Config_error
               (Printf.sprintf
                  "correction_model provider %s is not supported; pass 2 \
                   only                    supports openrouter"
                  (string_of_provider resolved.provider))))
  | Some _ -> type_error "correction_model" "a string"

let default_prompt_file pipeline ~mode ~correct =
  match (pipeline, mode, correct) with
  | One_pass, Translate, _ -> default_one_pass_translate_prompt_file
  | One_pass, Transcribe, false -> default_one_pass_verbatim_prompt_file
  | One_pass, Transcribe, true -> default_one_pass_prompt_file
  | (Legacy | Two_pass), Transcribe, _ -> default_correction_prompt_file
  | (Legacy | Two_pass), Translate, _ ->
      default_correction_translate_prompt_file

let embedded_prompt pipeline ~mode ~correct =
  match (pipeline, mode, correct) with
  | One_pass, Translate, _ -> default_one_pass_translate_prompt
  | One_pass, Transcribe, false -> default_one_pass_verbatim_prompt
  | One_pass, Transcribe, true -> default_one_pass_prompt
  | (Legacy | Two_pass), Transcribe, _ -> default_correction_prompt
  | (Legacy | Two_pass), Translate, _ -> default_correction_translate_prompt

let validate_prompt_file_name ~where name =
  let name = String.trim name in
  if name = "" then raise (Config_error (where ^ " must not be empty"));
  let parts = String.split_on_char '/' name in
  if
    String.contains name '/' || name = "."
    || List.exists (fun part -> part = "" || part = "..") parts
  then
    raise
      (Config_error (where ^ " must be a file name in the prompts directory"));
  name

let validate_pass2_prompt mode file content =
  let trimmed = String.trim content in
  if String.length trimmed = 0 then
    raise (Config_error (Printf.sprintf "%s: prompt is empty" file));
  let missing needle = not (contains_substring ~needle trimmed) in
  match mode with
  | Transcribe ->
      if missing "{{glossary}}" then
        raise
          (Config_error
             (Printf.sprintf "%s: missing required placeholder {{glossary}}"
                file))
  | Translate ->
      if
        missing "{{glossary}}"
        || missing "{{source_language}}"
        || missing "{{target_language}}"
      then
        raise
          (Config_error
             (Printf.sprintf
                "%s: missing required placeholders \
                 {{glossary}},                  {{source_language}} and \
                 {{target_language}}"
                file))

let parse_binding index ~pipeline ~correct_default table =
  let where field = Printf.sprintf "bindings[%d].%s" index field in
  let keysym =
    match toml_find table "keysym" with
    | Some (`Int value) -> value
    | Some _ -> type_error (where "keysym") "an integer"
    | None -> raise (Config_error (where "keysym" ^ " is required"))
  in
  if keysym <= 0 then
    raise (Config_error (where "keysym" ^ " must be positive"));
  let shift =
    match toml_find table "shift" with
    | None | Some `Null -> false
    | Some (`Bool value) -> value
    | Some _ -> type_error (where "shift") "a bool"
  in
  let mode =
    match toml_find table "mode" with
    | Some (`String value) -> (
        match value with
        | "transcribe" -> Transcribe
        | "translate" -> Translate
        | other ->
            raise
              (Config_error
                 (Printf.sprintf "%s must be transcribe or translate; got %s"
                    (where "mode") other)))
    | Some _ -> type_error (where "mode") "a string"
    | None -> raise (Config_error (where "mode" ^ " is required"))
  in
  let correct =
    match toml_find table "correct" with
    | None | Some `Null -> correct_default
    | Some (`Bool value) -> value
    | Some _ -> type_error (where "correct") "a bool"
  in
  let prompt_named, prompt_file =
    match toml_find table "prompt" with
    | Some (`String value) ->
        (true, validate_prompt_file_name ~where:(where "prompt") value)
    | Some `Null | None -> (
        match toml_find table "prompt_file" with
        | Some (`String value) ->
            (false, validate_prompt_file_name ~where:(where "prompt_file") value)
        | Some _ -> type_error (where "prompt_file") "a string"
        | None -> (false, default_prompt_file pipeline ~mode ~correct))
    | Some _ -> type_error (where "prompt") "a string"
  in
  {
    keysym;
    shift;
    mode;
    correct;
    prompt_named;
    prompt_file;
    prompt = embedded_prompt pipeline ~mode ~correct;
  }

let check_duplicate_bindings bindings =
  let rec loop seen = function
    | [] -> ()
    | binding :: rest ->
        let key = (binding.keysym, binding.shift) in
        if List.exists (( = ) key) seen then
          raise
            (Config_error
               (Printf.sprintf "duplicate binding for keysym %d shift %b"
                  binding.keysym binding.shift))
        else loop (key :: seen) rest
  in
  loop [] bindings

let derived_binding ~pipeline ~keysym ~shift ~mode ~correct =
  {
    keysym;
    shift;
    mode;
    correct;
    prompt_named = false;
    prompt_file = default_prompt_file pipeline ~mode ~correct;
    prompt = embedded_prompt pipeline ~mode ~correct;
  }

let first_of mode bindings =
  List.find_opt (fun binding -> binding.mode = mode) bindings

let legacy_key mode ~default_keysym ~default_shift bindings =
  match first_of mode bindings with
  | Some binding -> (binding.keysym, binding.shift)
  | None -> (default_keysym, default_shift)

let require_pass2 ~pipeline ~has_model bindings =
  match pipeline with
  | Legacy | One_pass -> ()
  | Two_pass when has_model -> ()
  | Two_pass ->
      List.iteri
        (fun index binding ->
          if binding.mode = Translate then
            raise
              (Config_error
                 (Printf.sprintf
                    "bindings[%d]: translate binding requires correction_model \
                     when pipeline is two-pass"
                    index));
          if binding.correct then
            raise
              (Config_error
                 (Printf.sprintf
                    "bindings[%d]: correct = true requires correction_model \
                     when pipeline is two-pass"
                    index)))
        bindings

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

let parse_bindings table ~pipeline ~correct_default =
  match toml_find table "bindings" with
  | None -> None
  | Some (`List rules)
    when List.for_all (function `Assoc _ -> true | _ -> false) rules ->
      let bindings =
        List.mapi (parse_binding ~pipeline ~correct_default) rules
      in
      check_duplicate_bindings bindings;
      Some bindings
  | Some _ -> raise (Config_error "bindings must be an array of tables")

let reject_replaced_two_pass table =
  match toml_find table "two_pass" with
  | None -> ()
  | Some _ ->
      raise
        (Config_error
           "two_pass was replaced by pipeline; use pipeline = \"legacy\", \
            \"one-pass\", or \"two-pass\"")

let parse_pipeline table =
  match toml_find table "pipeline" with
  | None | Some `Null -> Legacy
  | Some (`String value) -> pipeline_of_string (String.trim value)
  | Some _ -> type_error "pipeline" "a string"

let parse_provider_order table =
  match toml_find table "openrouter_provider_order" with
  | None | Some `Null -> None
  | Some (`List items) ->
      let names =
        List.mapi
          (fun index item ->
            match item with
            | `String value ->
                let value = String.trim value in
                if value = "" then
                  raise
                    (Config_error
                       (Printf.sprintf
                          "openrouter_provider_order[%d] must not be empty"
                          index))
                else value
            | _ -> type_error "openrouter_provider_order" "an array of strings")
          items
      in
      Some names
  | Some _ -> type_error "openrouter_provider_order" "an array of strings"

let parse_one_pass_timeout table =
  match optional_number table "one_pass_timeout_seconds" with
  | None -> default_one_pass_timeout_seconds
  | Some value ->
      if (not (Float.is_finite value)) || value <= 0. then
        raise
          (Config_error
             "one_pass_timeout_seconds must be a positive finite number");
      value

let require_openrouter_model ~provider ~where model =
  let resolved = resolve_model_provider ~default_provider:provider model in
  match resolved.provider with
  | Openrouter -> ()
  | Codex_realtime ->
      raise
        (Config_error
           (Printf.sprintf
              "%s must use the openrouter provider for pipeline one-pass; %s \
               resolves to codex-realtime"
              where model))

let of_json table : t =
  (match table with
  | `Assoc _ -> ()
  | _ -> raise (Config_error "STT config must be an object"));
  reject_unsupported_wakeword table;
  reject_replaced_two_pass table;
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
  let pass1 =
    resolve_model_provider ~default_provider:provider transcription_model
  in
  ignore (resolve_model_provider ~default_provider:provider translation_model);
  let streaming_default =
    match pass1.provider with Codex_realtime -> true | Openrouter -> false
  in
  let streaming = get_bool table "streaming" streaming_default in
  let pipeline = parse_pipeline table in
  (match pipeline with
  | One_pass ->
      require_openrouter_model ~provider ~where:"transcription_model"
        transcription_model;
      require_openrouter_model ~provider ~where:"translation_model"
        translation_model;
      if streaming then
        raise
          (Config_error
             "streaming is not supported for pipeline one-pass; the recording \
              is sent after release")
  | Legacy | Two_pass ->
      if streaming && pass1.provider <> Codex_realtime then
        raise
          (Config_error
             (Printf.sprintf "streaming is not supported for provider %s"
                (string_of_provider pass1.provider))));
  let correction_model = parse_correction_model table in
  let reasoning_effort = parse_correction_reasoning table in
  let timeout_seconds = parse_correction_timeout table in
  let temperature = parse_correction_temperature table in
  let correction =
    match correction_model with
    | None -> None
    | Some model ->
        Some { model; reasoning_effort; timeout_seconds; temperature }
  in
  let correct_default = correction_model <> None in
  let legacy_keys () =
    let ptt_transcribe_keysym =
      get_int table "ptt_transcribe_keysym" default_transcribe_keysym
    in
    let ptt_translate_keysym =
      get_int table "ptt_translate_keysym" default_translate_keysym
    in
    let ptt_transcribe_shift = get_bool table "ptt_transcribe_shift" false in
    let ptt_translate_shift = get_bool table "ptt_translate_shift" false in
    if ptt_transcribe_keysym <= 0 then
      raise (Config_error "ptt_transcribe_keysym must be positive");
    if ptt_translate_keysym <= 0 then
      raise (Config_error "ptt_translate_keysym must be positive");
    if
      ptt_transcribe_keysym = ptt_translate_keysym
      && ptt_transcribe_shift = ptt_translate_shift
    then
      raise (Config_error "PTT transcribe and translate bindings must differ");
    ( ptt_transcribe_keysym,
      ptt_transcribe_shift,
      ptt_translate_keysym,
      ptt_translate_shift )
  in
  let ( ptt_transcribe_keysym,
        ptt_transcribe_shift,
        ptt_translate_keysym,
        ptt_translate_shift,
        bindings ) =
    match parse_bindings table ~pipeline ~correct_default with
    | None ->
        let ( ptt_transcribe_keysym,
              ptt_transcribe_shift,
              ptt_translate_keysym,
              ptt_translate_shift ) =
          legacy_keys ()
        in
        let bindings =
          [
            derived_binding ~pipeline ~keysym:ptt_transcribe_keysym
              ~shift:ptt_transcribe_shift ~mode:Transcribe
              ~correct:correct_default;
            derived_binding ~pipeline ~keysym:ptt_translate_keysym
              ~shift:ptt_translate_shift ~mode:Translate
              ~correct:correct_default;
          ]
        in
        ( ptt_transcribe_keysym,
          ptt_transcribe_shift,
          ptt_translate_keysym,
          ptt_translate_shift,
          bindings )
    | Some bindings ->
        let ptt_transcribe_keysym, ptt_transcribe_shift =
          legacy_key Transcribe ~default_keysym:default_transcribe_keysym
            ~default_shift:false bindings
        in
        let ptt_translate_keysym, ptt_translate_shift =
          legacy_key Translate ~default_keysym:default_translate_keysym
            ~default_shift:false bindings
        in
        ( ptt_transcribe_keysym,
          ptt_transcribe_shift,
          ptt_translate_keysym,
          ptt_translate_shift,
          bindings )
  in
  require_pass2 ~pipeline ~has_model:(correction_model <> None) bindings;
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
    pipeline;
    streaming;
    correction;
    bindings;
    audio_retention_hours = parse_audio_retention table;
    openrouter_provider_order = parse_provider_order table;
    one_pass_timeout_seconds = parse_one_pass_timeout table;
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
  let correction_field name encode =
    match config.correction with
    | None -> (name, `Null)
    | Some correction -> (name, encode correction)
  in
  let binding_json (binding : binding) =
    let fields =
      [
        ("keysym", `Int binding.keysym);
        ("shift", `Bool binding.shift);
        ("mode", string (string_of_mode binding.mode));
        ("correct", `Bool binding.correct);
        ("prompt_file", string binding.prompt_file);
      ]
    in
    let fields =
      if binding.prompt_named then
        ("prompt", string binding.prompt_file) :: fields
      else fields
    in
    `Assoc fields
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
      ("pipeline", string (string_of_pipeline config.pipeline));
      ("streaming", `Bool config.streaming);
      correction_field "correction_model" (fun correction ->
          string correction.model);
      correction_field "correction_reasoning_effort" (fun correction ->
          opt correction.reasoning_effort);
      correction_field "correction_timeout_seconds" (fun correction ->
          `Float correction.timeout_seconds);
      correction_field "correction_temperature" (fun correction ->
          `Float correction.temperature);
      ("bindings", `List (List.map binding_json config.bindings));
      ("audio_retention_hours", `Float config.audio_retention_hours);
      ( "openrouter_provider_order",
        match config.openrouter_provider_order with
        | None -> `Null
        | Some names -> `List (List.map string names) );
      ("one_pass_timeout_seconds", `Float config.one_pass_timeout_seconds);
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

let resolve_binding_prompts ~read ~prompts_dir config =
  match config.pipeline with
  | Legacy -> config
  | One_pass | Two_pass ->
      let bindings =
        List.map
          (fun binding ->
            let path = Filename.concat prompts_dir binding.prompt_file in
            let content =
              match read path with
              | Ok content -> content
              | Error `Missing when not binding.prompt_named ->
                  embedded_prompt config.pipeline ~mode:binding.mode
                    ~correct:binding.correct
              | Error `Missing -> raise (Config_error (path ^ ": no such file"))
              | Error (`Unreadable reason) ->
                  raise (Config_error (path ^ ": " ^ reason))
            in
            validate_pass2_prompt binding.mode binding.prompt_file content;
            { binding with prompt = content })
          config.bindings
      in
      { config with bindings }

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
                    "STT config moved to TOML at %s; migrate legacy \
                     JSON                      config from %s"
                    path
                    (Filename.concat (Filename.dirname path) "stt.json")))
        | _ -> err)
    | other -> other
  in
  let ( let* ) = Result.bind in
  let* config = result in
  let prompts_dir = Filename.concat (Filename.dirname path) "prompts" in
  let prompt name default =
    let prompt_path = Filename.concat prompts_dir name in
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
    resolve_binding_prompts ~read ~prompts_dir
      { config with transcription_prompt; translation_prompt }
  with
  | config -> Ok config
  | exception Config_error message -> Error (`Config message)
