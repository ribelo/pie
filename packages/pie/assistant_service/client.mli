type error =
  [ `Daemon_off | `Daemon_error of string | `Daemon_protocol_error of string ]

val status :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  unit ->
  (Contract.recording_snapshot, error) result

val toggle :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  unit ->
  (bool, error) result

val action :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  string ->
  (Contract.action_result, error) result

val pause :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  unit ->
  (unit, error) result

val resume :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  unit ->
  (unit, error) result
