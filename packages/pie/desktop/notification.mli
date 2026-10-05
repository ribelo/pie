type error = [ `Notification_error of string ]

val clipboard_message : string option -> string
(** A clipboard-only notice using the key-press title, without the transcript.
    Escapes notification markup and replaces control characters. *)

val report :
  notify:(string -> string -> unit) ->
  log:(string -> unit) ->
  Notification_policy.event ->
  string ->
  string ->
  unit
(** Apply the policy at a replaceable effect seam. Demoted events are logged. *)

val emit :
  caps:< Ox_caps.Cap.stderr ; .. > ->
  process_mgr:_ Eio.Process.mgr ->
  Notification_policy.event ->
  string ->
  string ->
  unit
(** Suspension: may suspend while the notification subprocess runs; execution
    depends on [Eio.Process], and native stderr output depends on [Ox_caps] and
    [Stdlib]. Cancellation: propagates [Eio.Cancel.Cancelled] and
    [Eio.Exn.Multiple]; ordinary subprocess results are classified according to
    the notification policy; dependency behavior depends on [Eio.Process].
    Finalizers: subprocess lifecycle is delegated to [Eio.Process]; no Pie
    switch hook is registered. Locking: no Pie mutex is acquired; dependency
    locking is outside Pie. Cancellation safety: unsafe because progress is lost
    if the notification was sent before cancellation; retry may display it
    twice. *)

val notify_send :
  process_mgr:_ Eio.Process.mgr -> string -> string -> (unit, error) result
(** Suspension: may suspend while the notification subprocess runs; execution
    depends on [Eio.Process], and native stderr output depends on [Ox_caps] and
    [Stdlib]. Cancellation: propagates [Eio.Cancel.Cancelled] and
    [Eio.Exn.Multiple]; ordinary subprocess results are classified according to
    the notification policy; dependency behavior depends on [Eio.Process].
    Finalizers: subprocess lifecycle is delegated to [Eio.Process]; no Pie
    switch hook is registered. Locking: no Pie mutex is acquired; dependency
    locking is outside Pie. Cancellation safety: unsafe because progress is lost
    if the notification was sent before cancellation; retry may display it
    twice. *)

val notify_warning :
  caps:< Ox_caps.Cap.stderr ; .. > ->
  process_mgr:_ Eio.Process.mgr ->
  string ->
  string ->
  (unit, error) result
(** Suspension: may suspend while the notification subprocess runs; execution
    depends on [Eio.Process], and native stderr output depends on [Ox_caps] and
    [Stdlib]. Cancellation: propagates [Eio.Cancel.Cancelled] and
    [Eio.Exn.Multiple]; ordinary subprocess results are classified according to
    the notification policy; dependency behavior depends on [Eio.Process].
    Finalizers: subprocess lifecycle is delegated to [Eio.Process]; no Pie
    switch hook is registered. Locking: no Pie mutex is acquired; dependency
    locking is outside Pie. Cancellation safety: unsafe because progress is lost
    if the notification was sent before cancellation; retry may display it
    twice. *)
