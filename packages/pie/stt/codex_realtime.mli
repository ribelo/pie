type error = [ `Stt_error of string ]
type mode = Transcription | Translation | Conversation
type terminal_status = Completed | Failed | Cancelled | Incomplete | Unknown

type event =
  | Created of string
  | Config_ack of string
  | Transcript_delta of string
  | Transcript_done of string
  | Transcript_final of string
  | Response_terminal of terminal_status
  | Server_error of { message : string; code : string option }
  | Other of string

module First_token_timer : sig
  type t

  val create : unit -> t
  val start : t -> at_ms:int -> unit
  val observe : t -> at_ms:int -> string -> int option
end

module Codex_auth : sig
  type t = { path : string; access_token : string }

  val auth_path : unit -> string
  val load : ?path:string -> unit -> (t, error) result
  val decode_jwt_exp_seconds : string -> int option
  val is_expired : ?leeway_seconds:int -> now:float -> string -> bool
  val access_token : unit -> (string, error) result
end

val oauth_response_idle_timeout : Ox.Duration.t
val websocket_establishment_timeout : Ox.Duration.t
val realtime_access_token : client:'a -> unit -> (string, error) result

val realtime_header_pairs :
  ?session_id:string -> string -> (string * string) list

val build_realtime_url :
  ?base_url:string -> mode:mode -> model:string -> unit -> string

val build_transcription_session_update :
  ?prompt:string -> model:string -> unit -> Ox_json.t

val build_translation_session_update :
  ?prompt:string -> ?target_language:string -> unit -> Ox_json.t

val build_conversation_session_update :
  model:string -> prompt:string -> Ox_json.t

val build_conversation_response_create : prompt:string -> Ox_json.t
val parse_event : string -> (event, error) result
val server_error_diagnostic : string option -> string
val event_should_stop : mode:mode -> event -> bool
val resample_s16le_mono : from_rate:int -> to_rate:int -> bytes -> bytes

val run_realtime_session :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  ?ca_file:string ->
  url:string ->
  access_token:string ->
  mode:mode ->
  ?session_update:Ox_json.t ->
  ?response_create:Ox_json.t ->
  ?translation_output_drain_ms:int ->
  input_sample_rate:int ->
  bytes ->
  (string, error) result

val run_realtime_stream_session :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  ?ca_file:string ->
  url:string ->
  access_token:string ->
  mode:mode ->
  ?session_update:Ox_json.t ->
  ?response_create:Ox_json.t ->
  ?on_text:(string -> unit) ->
  ?translation_output_drain_ms:int ->
  input_sample_rate:int ->
  (unit -> (bytes option, 'audio_error) result) ->
  (string, error) result

type stream_handle = {
  offer_chunk : bytes -> (unit, error) result;
  finish_stream : unit -> (string, error) result;
  cancel_stream : unit -> (unit, error) result;
}

val with_stream_session :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  url:string ->
  access_token:string ->
  mode:mode ->
  operation:string ->
  model:string ->
  ?session_update:Ox_json.t ->
  ?response_create:Ox_json.t ->
  ?sample_rate:int ->
  cancel_message:string ->
  (stream_handle -> ('a, error) result) ->
  ('a, error) result

val with_transcription_stream :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  model:string ->
  language:string ->
  prompt_template:string ->
  ?sample_rate:int ->
  (stream_handle -> ('a, error) result) ->
  ('a, error) result

val with_translation_stream :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  model:string ->
  source_language:string ->
  target_language:string ->
  prompt_template:string ->
  ?sample_rate:int ->
  (stream_handle -> ('a, error) result) ->
  ('a, error) result

val transcribe_with_prompt :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  model:string ->
  language:string ->
  prompt_template:string ->
  ?sample_rate:int ->
  bytes ->
  (string, error) result

val translate_with_prompt :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  model:string ->
  source_language:string ->
  target_language:string ->
  prompt_template:string ->
  ?sample_rate:int ->
  bytes ->
  (string, error) result
