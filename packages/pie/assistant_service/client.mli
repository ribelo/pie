type error =
  [ `Daemon_off | `Daemon_error of string | `Daemon_protocol_error of string ]

val status :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  unit ->
  (Contract.recording_snapshot, error) result
(** Suspension: may suspend in the one-second RPC deadline, socket connect/write
    and response read; transport behavior depends on [Ox.Timeout], [Eio.Net],
    [Eio.Flow] and [Eio.Buf_read]. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; deadline expiry becomes [`Daemon_error]; transport
    behavior depends on [Ox.Timeout] and [Eio]. Finalizers: each request owns a
    child switch and a [Fun.protect] socket-close finalizer; the deadline awaits
    its body before returning; cleanup behavior depends on [Eio.Flow] and
    [Ox.Timeout]. Locking: no Pie mutex is held during the request; transport
    locking is outside Pie. Cancellation safety: safe to re-run the read-only
    status query; each request closes its socket scope, and transport behavior
    depends on [Eio] and [Ox.Timeout]. *)

val toggle :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  unit ->
  (bool, error) result
(** Suspension: may suspend in the one-second RPC deadline, socket connect/write
    and response read; transport behavior depends on [Ox.Timeout], [Eio.Net],
    [Eio.Flow] and [Eio.Buf_read]. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; deadline expiry becomes [`Daemon_error]; transport
    behavior depends on [Ox.Timeout] and [Eio]. Finalizers: each request owns a
    child switch and a [Fun.protect] socket-close finalizer; the deadline awaits
    its body before returning; cleanup behavior depends on [Eio.Flow] and
    [Ox.Timeout]. Locking: no Pie mutex is held during the request; transport
    locking is outside Pie. Cancellation safety: safe to re-run the read-only
    status query; each request closes its socket scope, and transport behavior
    depends on [Eio] and [Ox.Timeout]. *)

val action :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  string ->
  (Contract.action_result, error) result
(** Suspension: may suspend in the one-second RPC deadline, socket connect/write
    and response read; transport behavior depends on [Ox.Timeout], [Eio.Net],
    [Eio.Flow] and [Eio.Buf_read]. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; deadline expiry becomes [`Daemon_error]; transport
    behavior depends on [Ox.Timeout] and [Eio]. Finalizers: each request owns a
    child switch and a [Fun.protect] socket-close finalizer; the deadline awaits
    its body before returning; cleanup behavior depends on [Eio.Flow] and
    [Ox.Timeout]. Locking: no Pie mutex is held during the request; transport
    locking is outside Pie. Cancellation safety: safe to re-run the read-only
    status query; each request closes its socket scope, and transport behavior
    depends on [Eio] and [Ox.Timeout]. *)

val pause :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  unit ->
  (unit, error) result
(** Suspension: may suspend in the one-second RPC deadline, socket connect/write
    and response read; transport behavior depends on [Ox.Timeout], [Eio.Net],
    [Eio.Flow] and [Eio.Buf_read]. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; deadline expiry becomes [`Daemon_error]; transport
    behavior depends on [Ox.Timeout] and [Eio]. Finalizers: each request owns a
    child switch and a [Fun.protect] socket-close finalizer; the deadline awaits
    its body before returning; cleanup behavior depends on [Eio.Flow] and
    [Ox.Timeout]. Locking: no Pie mutex is held during the request; transport
    locking is outside Pie. Cancellation safety: safe to re-run the read-only
    status query; each request closes its socket scope, and transport behavior
    depends on [Eio] and [Ox.Timeout]. *)

val resume :
  caps:< Ox_caps.Cap.env ; .. > ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  sw:Eio.Switch.t ->
  unit ->
  (unit, error) result
(** Suspension: may suspend in the one-second RPC deadline, socket connect/write
    and response read; transport behavior depends on [Ox.Timeout], [Eio.Net],
    [Eio.Flow] and [Eio.Buf_read]. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; deadline expiry becomes [`Daemon_error]; transport
    behavior depends on [Ox.Timeout] and [Eio]. Finalizers: each request owns a
    child switch and a [Fun.protect] socket-close finalizer; the deadline awaits
    its body before returning; cleanup behavior depends on [Eio.Flow] and
    [Ox.Timeout]. Locking: no Pie mutex is held during the request; transport
    locking is outside Pie. Cancellation safety: safe to re-run the read-only
    status query; each request closes its socket scope, and transport behavior
    depends on [Eio] and [Ox.Timeout]. *)
