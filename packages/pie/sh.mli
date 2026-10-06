val find_executable : caps:< Ox_caps.Cap.env ; .. > -> string -> string option
(** Suspension: never suspends in Pie, including native helper callbacks; the
    native executable search implementation depends on [Ox_caps], [Unix],
    [Filename] and [Stdlib]. Cancellation: not a cancellation point in Pie;
    native and helper callback behavior depends on [Ox_caps], [Unix], [Filename]
    and [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val discarded_stderr : unit -> Eio.Flow.sink_ty Eio.Resource.t
(** Suspension: never suspends in Pie, including native helper callbacks; the
    sink construction implementation depends on [Eio.Flow], [Eio.Resource] and
    [Stdlib]. Cancellation: not a cancellation point in Pie; native and helper
    callback behavior depends on [Eio.Flow], [Eio.Resource] and [Stdlib].
    Finalizers: the returned in-memory sink owns no descriptor and registers no
    hook. Locking: no lock is acquired. *)

val failure_message : prog:string -> exn -> string

val run :
  process_mgr:_ Eio.Process.mgr ->
  prog:string ->
  args:string list ->
  (unit, [ `Subprocess_error of string ]) result
(** Suspension: may suspend while [Eio.Process] runs and awaits the subprocess.
    Cancellation: propagates [Eio.Cancel.Cancelled] and [Eio.Exn.Multiple]; only
    supported process/I/O failures become [`Subprocess_error]; dependency
    behavior depends on [Eio.Process]. Finalizers: process lifecycle and reaping
    are delegated to [Eio.Process]; no Pie switch hook is registered. Locking:
    no Pie mutex is acquired; subprocess-internal locking is outside Pie.
    Cancellation safety: unsafe because progress is lost if the subprocess has
    already changed external state; retry can repeat its effects. *)

val capture_output :
  process_mgr:_ Eio.Process.mgr ->
  max_size:int ->
  ?stdin:_ Eio.Flow.source ->
  ?stderr:_ Eio.Flow.sink ->
  ?executable:string ->
  string list ->
  string
(** Suspension: may suspend while reading and awaiting the subprocess.
    Cancellation: propagates cancellation and process failures; external
    behavior depends on [Eio.Process]. Finalizers: a local switch owns the child
    and pipe; the child is killed and reaped on failure. Locking: acquires no
    Pie mutex. Cancellation safety: unsafe because progress is lost when the
    subprocess has changed external state; retry can repeat its effects. Raises
    [Eio.Cancel.Cancelled] on cancellation and
    [Eio.Buf_read.Buffer_limit_exceeded] when the finite [max_size] budget is
    exceeded. *)

val run_capture :
  process_mgr:_ Eio.Process.mgr ->
  prog:string ->
  args:string list ->
  (string, [ `Subprocess_error of string ]) result
(** Suspension: may suspend while [Eio.Process] runs and awaits the subprocess.
    Cancellation: propagates [Eio.Cancel.Cancelled] and [Eio.Exn.Multiple]; only
    supported process/I/O failures become [`Subprocess_error]; dependency
    behavior depends on [Eio.Process]. Finalizers: process lifecycle and reaping
    are delegated to [Eio.Process]; no Pie switch hook is registered. Locking:
    no Pie mutex is acquired; subprocess-internal locking is outside Pie.
    Cancellation safety: unsafe because progress is lost if the subprocess has
    already changed external state; retry can repeat its effects. *)
