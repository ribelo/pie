type error =
  [ `Daemon_off | `Daemon_error of string | `Daemon_protocol_error of string ]

val status :
  net:_ Eio.Net.t ->
  sw:Eio.Switch.t ->
  unit ->
  (Contract.recording_snapshot, error) Eta.Effect.t

val toggle :
  net:_ Eio.Net.t -> sw:Eio.Switch.t -> unit -> (bool, error) Eta.Effect.t

val action :
  net:_ Eio.Net.t ->
  sw:Eio.Switch.t ->
  string ->
  (Contract.action_result, error) Eta.Effect.t

val pause :
  net:_ Eio.Net.t -> sw:Eio.Switch.t -> unit -> (unit, error) Eta.Effect.t

val resume :
  net:_ Eio.Net.t -> sw:Eio.Switch.t -> unit -> (unit, error) Eta.Effect.t
