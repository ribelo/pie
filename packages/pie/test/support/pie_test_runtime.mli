val run : caps:< Ox_caps.Cap.env ; .. > -> (Eio_unix.Stdenv.base -> 'a) -> 'a
(** Own a test Eio runtime with the Door's silent posix wakeup daemon on an
    outer switch. The daemon remains alive through nested cancellation and
    cleanup, and disappears when the operation finishes. ^nwake-9uvx

    Suspension: may suspend while caller [operation] runs and during runtime
    cleanup; behavior depends on [operation], [Eio_main], [Eio.Switch] and
    [Ox_app]. Cancellation: propagates external [Eio.Cancel.Cancelled]; callback
    and runtime behavior depends on [operation], [Eio_main] and [Ox_app].
    Finalizers: an outer switch owns the POSIX wakeup daemon through nested
    cleanup; runtime teardown is delegated to [Eio_main], [Eio.Switch] and
    [Ox_app]; caller resources remain [operation] responsibility. Locking: no
    Pie mutex is held around [operation]; runtime and callback locking are
    outside Pie. Cancellation safety: unsafe because progress is lost if
    [operation] committed non-replayable work; runtime teardown does not roll
    back caller effects. *)

module Service : module type of Pie.Assistant.Service
