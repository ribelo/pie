val blocking : (unit -> 'a) -> 'a
(** Execute native filesystem work via Ox blocking workers without stalling Eio.
    Return only after physical completion, including on cancellation. The
    callback must not perform Eio effects. *)

val mkdir_p : caps:< Ox_caps.Cap.fs_write ; .. > -> string -> unit
(** Suspension: never suspends in Pie, including native helper callbacks; the
    native recursive directory creation implementation depends on [Ox_caps],
    [Unix], [Sys], [Filename] and [Stdlib]. Cancellation: not a cancellation
    point in Pie; native and helper callback behavior depends on [Ox_caps],
    [Unix], [Sys], [Filename] and [Stdlib]. Finalizers: creates no descriptor or
    release hook. Locking: no lock is acquired.
    @raise Unix.Unix_error
      if creation fails or an existing path is not a directory. *)
