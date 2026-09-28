type error =
  | Protocol_error of string
  | Authentication_error of string
  | Server_error of { message : string; code : int option }
  | Timeout of { operation : string; duration : Eta.Duration.t }
  | Disconnected of string
  | Malformed_packet of string
  | Recording_overload of { stream_index : int; capacity : int }
      (** [capacity] is item count ({!record_stream_capacity}) or PCM bytes
          ({!Dictation_limits.queue_pcm_bytes}). *)

val error_message : error -> string
val pp_error : Format.formatter -> error -> unit
val default_request_timeout : Eta.Duration.t

val record_stream_capacity : int
(** Bounded item count (64) for a recording stream queue. Independent of the
    {!Dictation_limits.queue_pcm_bytes} PCM byte cap. Ordinary queue overflow
    reports {!Recording_overload} and detaches only that stream; control
    commands remain usable. Declared Pulse frames larger than
    {!Protocol.max_frame_bytes} close the connection as {!Protocol_error} and do
    not leave controls usable. *)

type t

val with_client :
  net:[> `Generic ] Eio.Net.ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  logger:Eta_observability.Logger.t ->
  ?socket_path:string ->
  ?cookie:bytes ->
  ?client_name:string ->
  ?protocol_version:int ->
  ?request_timeout:Eta.Duration.t ->
  (t -> ('a, ([> `Pulse_error of error ] as 'err)) result) ->
  ('a, 'err) result

val with_record_stream :
  t ->
  ?options:Defs.record_stream_options ->
  (Defs.record_stream_info ->
  (unit -> (bytes option, [ `Pulse_error of error ]) result) ->
  ('a, ([> `Pulse_error of error ] as 'err)) result) ->
  ('a, 'err) result

val get_server_info : t -> (Defs.server_info, [ `Pulse_error of error ]) result

val list_sources :
  t -> (Defs.source_info list, [ `Pulse_error of error ]) result
