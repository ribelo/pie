type error = {
  message : string;
  code : int option;
}

exception Client_error of error

val error : string -> ?code:int -> unit -> error
val fail : string -> ?code:int -> unit -> 'a

type t

val with_client :
  sw:Eio.Switch.t ->
  net:[> `Generic ] Eio.Net.ty Eio.Resource.t ->
  clock:[> float Eio.Time.clock_ty ] Eio.Time.clock ->
  ?socket_path:string ->
  ?cookie:bytes ->
  ?client_name:string ->
  ?protocol_version:int ->
  ?request_timeout_ms:int ->
  (t -> ('a, [> `Pulse_error of string ] as 'err) Eta.Effect.t) ->
  ('a, 'err) Eta.Effect.t

val with_record_stream :
  t ->
  ?options:Defs.record_stream_options ->
  (Defs.record_stream_info ->
   (unit -> (bytes, [> `Closed ]) Eta.Effect.t) ->
   ('a, 'err) Eta.Effect.t) ->
  ('a, 'err) Eta.Effect.t

val get_server_info :
  t -> (Defs.server_info, [> `Pulse_error of string ] as 'err) Eta.Effect.t

val list_sources :
  t -> (Defs.source_info list, [> `Pulse_error of string ] as 'err) Eta.Effect.t
