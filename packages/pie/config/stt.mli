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
(** Primary failure plus a subsequent close failure. Unknown and cancel
    exceptions are not rewritten as {!Config_error}. A typed {!Config_error}
    with a Unix close error is still combined into one {!Config_error}. *)

type read_io = {
  open_readonly : string -> Unix.file_descr;
  fstat : Unix.file_descr -> Unix.stats;
  read : Unix.file_descr -> bytes -> int -> int -> int;
  close : Unix.file_descr -> unit;
  after_acquire : Unix.file_descr -> unit;
  after_fstat : Unix.file_descr -> Unix.stats -> unit;
}

val unix_read_io : read_io

type resolved_model = { provider : provider; model : string }

val provider_of_string : string -> provider
val string_of_provider : provider -> string

val resolve_model_provider :
  default_provider:provider -> string -> resolved_model

val newline_action_of_string : string -> newline_action
val string_of_newline_action : newline_action -> string
val entry_method_of_string : string -> entry_method
val string_of_entry_method : entry_method -> string
val load : ?config_path:string -> ?read_io:read_io -> unit -> t
