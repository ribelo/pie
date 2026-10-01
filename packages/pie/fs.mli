val blocking : (unit -> 'a) -> 'a
(** Execute native filesystem work via Ox blocking workers without stalling Eio.
    Return only after physical completion, including on cancellation. The
    callback must not perform Eio effects. *)

val mkdir_p : caps:< Ox_caps.Cap.fs_write ; .. > -> string -> unit
