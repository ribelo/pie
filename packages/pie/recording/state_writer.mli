(** Bounded, coalescing persistence of the current recording-state projection.
    This is not for retained audio, archives, or append-only history. *)

val create :
  sw:Eio.Switch.t -> on_error:(exn -> unit) -> unit -> (unit -> unit) -> unit
(** The first submission completes synchronously and propagates failure, for
    fail-fast startup. Later submissions replace the one pending native write
    without yielding. One switch-owned worker serializes writes through
    [Fs.blocking]; failures are reported by [on_error] on the scheduler.
    Shutdown waits for the in-flight write and drains the latest pending
    projection. Callbacks must be native-only, and capture an immutable
    snapshot. *)
