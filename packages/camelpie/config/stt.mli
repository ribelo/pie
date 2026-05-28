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
  transcription_prompt : string;
  translation_prompt : string;
  newline_rules : newline_rule list;
}

exception Config_error of string

val provider_of_string : string -> provider
val string_of_provider : provider -> string
val newline_action_of_string : string -> newline_action
val string_of_newline_action : newline_action -> string
val load : ?config_path:string -> unit -> t
