exception Stt_error of string

let transcribe_stream ?(model = "gpt-realtime-whisper") ~api_key (_stream : bytes Eta.Channel.t) : string =
  ignore api_key;
  ignore model;
  raise (Stt_error "Codex Realtime STT not yet implemented")
