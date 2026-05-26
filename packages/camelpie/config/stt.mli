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

val provider_of_string : string -> provider
val string_of_provider : provider -> string
val load : ?config_path:string -> unit -> t
