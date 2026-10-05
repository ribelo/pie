val mkdir_p : caps:< Ox_caps.Cap.fs_write ; .. > -> string -> unit
(** Suspension: never suspends in Pie, including native helper callbacks; the
    native recursive directory creation implementation depends on [Ox_caps],
    [Unix], [Sys], [Filename] and [Stdlib]. Cancellation: not a cancellation
    point in Pie; native and helper callback behavior depends on [Ox_caps],
    [Unix], [Sys], [Filename] and [Stdlib]. Finalizers: creates no descriptor or
    release hook. Locking: no lock is acquired.
    @raise Unix.Unix_error
      if creation fails or an existing path is not a directory. *)

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
    visible.

    Suspension: may suspend while [Ox_ipc.Listener] serves connections and while
    caller [status_metadata] or [action_handler] callbacks run. Cancellation:
    propagates external [Eio.Cancel.Cancelled]; listener and callback behavior
    depends on [Ox_ipc.Listener], [status_metadata] and [action_handler].
    Finalizers: endpoint ownership and handler lifetime are delegated to
    [Ox_ipc.Listener]; caller callback cleanup remains the callback
    responsibility. Locking: endpoint ownership locks are delegated to
    [Ox_ipc.Listener]; Pie does not hold a server mutex around caller handlers.
    Cancellation safety: unsafe because progress is lost for interrupted
    requests; already-dispatched coordinator or caller actions are not rolled
    back.

    Native runtime-directory setup can raise [Unix.Unix_error]. *)
