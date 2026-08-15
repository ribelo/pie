(* Configuration slices projected from Config.Stt.t *)

type pulse_client_config = unit
let pulse_client_equal () () = true
let pulse_client_slice (_ : Config.Stt.t) = ()

type keyboard_monitor_config = unit
let keyboard_monitor_equal () () = true
let keyboard_monitor_slice (_ : Config.Stt.t) = ()

type recording_coordinator_config = unit
let recording_coordinator_equal () () = true
let recording_coordinator_slice (_ : Config.Stt.t) = ()

type stt_backend_config = {
  provider : Config.Stt.provider;
  transcription_model : string;
  translation_model : string;
  transcription_language : string;
  translation_source_language : string;
  translation_target_language : string;
  thinking_effort : string option;
  transcription_prompt : string;
  translation_prompt : string;
}

let stt_backend_equal a b =
  a.provider = b.provider
  && String.equal a.transcription_model b.transcription_model
  && String.equal a.translation_model b.translation_model
  && String.equal a.transcription_language b.transcription_language
  && String.equal a.translation_source_language b.translation_source_language
  && String.equal a.translation_target_language b.translation_target_language
  && Option.equal String.equal a.thinking_effort b.thinking_effort
  && String.equal a.transcription_prompt b.transcription_prompt
  && String.equal a.translation_prompt b.translation_prompt

let stt_backend_slice (config : Config.Stt.t) : stt_backend_config =
  {
    provider = config.provider;
    transcription_model = config.transcription_model;
    translation_model = config.translation_model;
    transcription_language = config.transcription_language;
    translation_source_language = config.translation_source_language;
    translation_target_language = config.translation_target_language;
    thinking_effort = config.thinking_effort;
    transcription_prompt = config.transcription_prompt;
    translation_prompt = config.translation_prompt;
  }

type wakeword_config = {
  enabled : bool;
  dictation_silence_seconds : float;
  dictation_max_seconds : float;
  dictation_speech_rms_threshold : float;
}

let wakeword_equal a b =
  a.enabled = b.enabled
  && Float.equal a.dictation_silence_seconds b.dictation_silence_seconds
  && Float.equal a.dictation_max_seconds b.dictation_max_seconds
  && Float.equal a.dictation_speech_rms_threshold b.dictation_speech_rms_threshold

let wakeword_slice (config : Config.Stt.t) : wakeword_config =
  {
    enabled = config.wakeword_enabled;
    dictation_silence_seconds = config.wakeword_dictation_silence_seconds;
    dictation_max_seconds = config.wakeword_dictation_max_seconds;
    dictation_speech_rms_threshold = config.wakeword_dictation_speech_rms_threshold;
  }

type rpc_server_config = unit
let rpc_server_equal () () = true
let rpc_server_slice (_ : Config.Stt.t) = ()

type ptt_loop_config = {
  ptt_transcribe_keysym : int;
  ptt_translate_keysym : int;
  ptt_transcribe_shift : bool;
  ptt_translate_shift : bool;
  ptt_min_capture_rms : float;
  ptt_min_capture_peak : float;
  min_duration_ms : int;
}

let ptt_loop_equal a b =
  a.ptt_transcribe_keysym = b.ptt_transcribe_keysym
  && a.ptt_translate_keysym = b.ptt_translate_keysym
  && a.ptt_transcribe_shift = b.ptt_transcribe_shift
  && a.ptt_translate_shift = b.ptt_translate_shift
  && Float.equal a.ptt_min_capture_rms b.ptt_min_capture_rms
  && Float.equal a.ptt_min_capture_peak b.ptt_min_capture_peak
  && a.min_duration_ms = b.min_duration_ms

let ptt_loop_slice ?(min_duration_ms = 120) (config : Config.Stt.t) : ptt_loop_config =
  {
    ptt_transcribe_keysym = config.ptt_transcribe_keysym;
    ptt_translate_keysym = config.ptt_translate_keysym;
    ptt_transcribe_shift = config.ptt_transcribe_shift;
    ptt_translate_shift = config.ptt_translate_shift;
    ptt_min_capture_rms = config.ptt_min_capture_rms;
    ptt_min_capture_peak = config.ptt_min_capture_peak;
    min_duration_ms;
  }

(* Helper to detect if only STT provider changed *)
let is_only_provider_change (old_c : Config.Stt.t) (new_c : Config.Stt.t) =
  old_c.provider <> new_c.provider
  && String.equal old_c.transcription_model new_c.transcription_model
  && String.equal old_c.translation_model new_c.translation_model
  && String.equal old_c.transcription_language new_c.transcription_language
  && String.equal old_c.translation_source_language new_c.translation_source_language
  && String.equal old_c.translation_target_language new_c.translation_target_language
  && Option.equal String.equal old_c.thinking_effort new_c.thinking_effort
  && String.equal old_c.transcription_prompt new_c.transcription_prompt
  && String.equal old_c.translation_prompt new_c.translation_prompt
  && old_c.wakeword_enabled = new_c.wakeword_enabled
  && Float.equal old_c.wakeword_dictation_silence_seconds new_c.wakeword_dictation_silence_seconds
  && Float.equal old_c.wakeword_dictation_max_seconds new_c.wakeword_dictation_max_seconds
  && Float.equal old_c.wakeword_dictation_speech_rms_threshold new_c.wakeword_dictation_speech_rms_threshold
  && Float.equal old_c.ptt_min_capture_rms new_c.ptt_min_capture_rms
  && Float.equal old_c.ptt_min_capture_peak new_c.ptt_min_capture_peak
  && old_c.ptt_transcribe_keysym = new_c.ptt_transcribe_keysym
  && old_c.ptt_translate_keysym = new_c.ptt_translate_keysym
  && old_c.ptt_transcribe_shift = new_c.ptt_transcribe_shift
  && old_c.ptt_translate_shift = new_c.ptt_translate_shift
  && old_c.newline_rules = new_c.newline_rules
