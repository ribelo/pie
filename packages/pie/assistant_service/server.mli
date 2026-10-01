val mkdir_p : caps:< Ox_caps.Cap.fs_write ; .. > -> string -> unit

val snapshot_of_coordinator :
  Recording.Coordinator.t -> Contract.recording_snapshot

type budget = { peer_timeout_s : float }

val default_budget : budget

type error = Ox_ipc.Listener.error

val err_endpoint_conflict : string
val err_unsafe_runtime_dir : string
val err_inaccessible_runtime_dir : string
val err_active_listener : string
val err_stale_probe_failed : string
val err_lock_conflict : string
val err_lock_identity : string
val err_malformed_request : string

val start :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.network ; .. > ->
  sw:Eio.Switch.t ->
  fs:_ Eio.Path.t ->
  net:_ Eio.Net.ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  ?socket_path:string ->
  ?budget:budget ->
  ?action_handler:(string -> Contract.action_result) ->
  ?status_metadata:(unit -> string option * string option) ->
  Recording.Coordinator.t ->
  (unit, error) result
(** [status_metadata] supplies configured reasoning effort and schema
    identifier, sampled for each status/action response so live reloads remain
    visible. *)
