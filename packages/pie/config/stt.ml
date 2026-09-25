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
exception Compound of exn * exn

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

type read_io = {
  open_readonly : string -> Unix.file_descr;
  fstat : Unix.file_descr -> Unix.stats;
  read : Unix.file_descr -> bytes -> int -> int -> int;
  close : Unix.file_descr -> unit;
  after_acquire : Unix.file_descr -> unit;
  after_fstat : Unix.file_descr -> Unix.stats -> unit;
}

let rec unix_read fd buf off len =
  try Unix.read fd buf off len
  with Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd buf off len

let rec open_readonly_nonblock path =
  try Unix.openfile path Unix.[ O_RDONLY; O_NONBLOCK; O_CLOEXEC ] 0
  with Unix.Unix_error (Unix.EINTR, _, _) -> open_readonly_nonblock path

let unix_read_io =
  {
    open_readonly = open_readonly_nonblock;
    fstat = Unix.fstat;
    read = unix_read;
    close = Unix.close;
    after_acquire = ignore;
    after_fstat = (fun _ _ -> ());
  }

let read_unix_error path err =
  Config_error
    (Printf.sprintf "failed to read %s: %s" path (Unix.error_message err))

let close_error path err =
  Config_error
    (Printf.sprintf "failed to close %s: %s" path (Unix.error_message err))

let combine_read_and_close path primary_msg err =
  Config_error
    (Printf.sprintf "%s; also failed to close %s: %s" primary_msg path
       (Unix.error_message err))

let close_once close fd =
  let closed = Atomic.make false in
  fun () -> if Atomic.compare_and_set closed false true then close fd else ()

let read_body read_io path fd =
  read_io.after_acquire fd;
  let stats =
    match read_io.fstat fd with
    | stats -> stats
    | exception Unix.Unix_error (err, _, _) -> raise (read_unix_error path err)
  in
  if stats.Unix.st_kind <> Unix.S_REG then
    raise (Config_error (Printf.sprintf "%s is not a regular file" path));
  if stats.Unix.st_size > max_config_file_bytes then
    raise
      (Config_error
         (Printf.sprintf "%s exceeds the 1MiB configuration size limit" path));
  read_io.after_fstat fd stats;
  let initial =
    if stats.Unix.st_size > 0 then min stats.Unix.st_size max_config_file_bytes
    else 64
  in
  let buf = Buffer.create initial in
  let chunk = Bytes.create 65536 in
  let rec loop total =
    let remaining_with_sentinel = max_config_file_bytes - total + 1 in
    if remaining_with_sentinel <= 0 then
      raise
        (Config_error
           (Printf.sprintf "%s exceeds the 1MiB configuration size limit" path))
    else
      let want = min (Bytes.length chunk) remaining_with_sentinel in
      match read_io.read fd chunk 0 want with
      | 0 -> Buffer.contents buf
      | n ->
          let total = total + n in
          if total > max_config_file_bytes then
            raise
              (Config_error
                 (Printf.sprintf "%s exceeds the 1MiB configuration size limit"
                    path));
          Buffer.add_subbytes buf chunk 0 n;
          loop total
      | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
          raise (Config_error (Printf.sprintf "%s read would block" path))
      | exception Unix.Unix_error (err, _, _) ->
          raise (read_unix_error path err)
  in
  loop 0

let read_bounded_regular_file ?(read_io = unix_read_io) path =
  match read_io.open_readonly path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None
  | exception Unix.Unix_error (err, _, _) -> raise (read_unix_error path err)
  | fd -> (
      let closer = close_once read_io.close fd in
      let outcome =
        match read_body read_io path fd with
        | contents -> Ok contents
        | exception exn -> Error exn
      in
      match closer () with
      | () -> (
          match outcome with
          | Ok contents -> Some contents
          | Error exn -> raise exn)
      | exception Unix.Unix_error (err, _, _) -> (
          match outcome with
          | Ok _ -> raise (close_error path err)
          | Error (Config_error msg) ->
              raise (combine_read_and_close path msg err)
          | Error primary ->
              raise (Compound (primary, Unix.Unix_error (err, "close", path))))
      | exception close_exn -> (
          match outcome with
          | Ok _ -> raise close_exn
          | Error primary -> raise (Compound (primary, close_exn))))

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

module TT = Toml.Types

let toml_key key = TT.Table.Key.of_string key
let toml_find table key = TT.Table.find_opt (toml_key key) table

let type_error key expected =
  raise (Config_error (Printf.sprintf "%s must be %s" key expected))

let validate_unit_interval name value =
  if (not (Float.is_finite value)) || value < 0. || value > 1. then
    raise
      (Config_error (Printf.sprintf "%s must be a finite value in 0.0-1.0" name))

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

let get_int table key default_val =
  match toml_find table key with
  | Some (TT.TInt value) -> value
  | Some _ -> type_error key "an integer"
  | None -> default_val

let get_optional_string table key =
  match toml_find table key with
  | Some (TT.TString value) -> (
      match String.trim value with "" -> None | trimmed -> Some trimmed)
  | Some _ -> type_error key "a string"
  | None -> None

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
  | Some (TT.TArray (TT.NodeTable rules)) -> List.mapi parse_newline_rule rules
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
  | Some (TT.TArray (TT.NodeTable rules)) ->
      List.mapi parse_text_entry_rule rules
  | Some _ -> raise (Config_error "text_entry_rules must be an array of tables")

let load_toml ~read_io config_path =
  match read_bounded_regular_file ~read_io config_path with
  | None ->
      let legacy_json =
        Filename.concat (Filename.dirname config_path) "stt.json"
      in
      if Sys.file_exists legacy_json then
        raise
          (Config_error
             (Printf.sprintf
                "STT config moved to TOML at %s; migrate legacy JSON config \
                 from %s"
                config_path legacy_json))
      else
        raise
          (Config_error
             (Printf.sprintf "STT config not found at %s" config_path))
  | Some contents -> (
      match Toml.Parser.from_string contents with
      | `Ok table -> table
      | `Error (msg, location) ->
          raise
            (Config_error
               (Printf.sprintf "invalid TOML in %s:%d:%d: %s" config_path
                  location.Toml.Parser.line location.column msg)))

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

let load ?(config_path = Paths.stt_config_path) ?(read_io = unix_read_io) () : t
    =
  let table = load_toml ~read_io config_path in
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
  let prompts_dir = Filename.concat (Filename.dirname config_path) "prompts" in
  let transcription_prompt_path =
    Filename.concat prompts_dir "transcription.md"
  in
  let translation_prompt_path = Filename.concat prompts_dir "translation.md" in
  let transcription_prompt =
    match read_bounded_regular_file ~read_io transcription_prompt_path with
    | None -> "Transcribe the following audio in {{language}}."
    | Some content -> content
  in
  let translation_prompt =
    match read_bounded_regular_file ~read_io translation_prompt_path with
    | None -> "Translate from {{source_language}} to {{target_language}}."
    | Some content -> content
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
