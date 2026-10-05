(** Bounded, coalescing persistence of the current recording-state projection.
    This is not for retained audio, archives, or append-only history. *)

val create :
  sw:Eio.Switch.t -> on_error:(exn -> unit) -> unit -> (unit -> unit) -> unit
(** The first submission completes synchronously and propagates failure, for
    fail-fast startup. Later submissions replace the one pending native write
    without yielding. One switch-owned worker serializes writes through
    [Fs.blocking]; ordinary failures are reported by [on_error] on the
    scheduler. Shutdown waits for the in-flight write and drains the latest
    pending projection. Callbacks must be native-only, and capture an immutable
    snapshot.

    Suspension: may suspend in the worker condition wait and protected
    [Fs.blocking] writes; callback behavior depends on native write callbacks
    and [on_error]. Cancellation: propagates write-raised [Eio.Cancel.Cancelled]
    rather than sending it to [on_error]; [Fs.blocking] protects native writes,
    and scheduler/callback behavior depends on [Eio], [Ox_blocking] and the
    callbacks. Finalizers: a switch release hook protects the final drain;
    [Fs.blocking] awaits physical write completion and tears down its pool;
    native callback resources remain callback responsibility. Locking: no Pie
    mutex is used; a single worker serializes writes, and the pending slot is
    updated without yielding; dependency locking is outside Pie. Cancellation
    safety: unsafe because progress is lost if a write callback fails after its
    pending projection is consumed; coalesced projections are not a replay log.

    Registering cleanup on an already-finished switch can raise
    [Eio.Switch.Release_error].

    Registering cleanup on an already-finished switch can raise
    [Invalid_argument].

    Raises [Assert_failure] if the internal blocking-pool shutdown invariant is
    violated. *)
