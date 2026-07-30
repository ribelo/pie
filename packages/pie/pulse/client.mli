type error =
  | Protocol_error of string
  | Authentication_error of string
  | Server_error of { message : string; code : int option }
  | Timeout of { operation : string; duration : Eta.Duration.t }
  | Disconnected of string
  | Malformed_packet of string

val error_message : error -> string
val pp_error : Format.formatter -> error -> unit

val default_request_timeout : Eta.Duration.t
val record_stream_capacity : int

type t

val with_client :
  net:[> `Generic ] Eio.Net.ty Eio.Resource.t ->
  ?socket_path:string ->
  ?cookie:bytes ->
  ?client_name:string ->
  ?protocol_version:int ->
  ?request_timeout:Eta.Duration.t ->
  (t -> ('a, [> `Pulse_error of error ] as 'err) Eta.Effect.t) ->
  ('a, 'err) Eta.Effect.t

val with_record_stream :
  t ->
  ?options:Defs.record_stream_options ->
  (Defs.record_stream_info ->
   (bytes, [ `Pulse_error of error ]) Eta_stream.Stream.t ->
   ('a, [> `Pulse_error of error ] as 'err) Eta.Effect.t) ->
  ('a, 'err) Eta.Effect.t

val get_server_info :
  t -> (Defs.server_info, [ `Pulse_error of error ]) Eta.Effect.t

val list_sources :
  t -> (Defs.source_info list, [ `Pulse_error of error ]) Eta.Effect.t
