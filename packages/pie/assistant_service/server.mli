val mkdir_p : string -> unit

val snapshot_of_coordinator :
  Recording.Coordinator.t -> Contract.recording_snapshot

type budget = { peer_timeout_s : float }

val default_budget : budget

exception Error of string

val err_endpoint_conflict : string
val err_unsafe_runtime_dir : string
val err_inaccessible_runtime_dir : string
val err_active_listener : string
val err_stale_probe_failed : string
val err_lock_conflict : string
val err_lock_identity : string
val err_malformed_request : string

val start :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  ?socket_path:string ->
  ?budget:budget ->
  ?action_handler:(string -> Contract.action_result) ->
  Recording.Coordinator.t ->
  unit
