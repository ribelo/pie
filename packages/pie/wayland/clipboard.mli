type t
(** Application-owned clipboard offer. One helper once [offer] returns; a ready
    replacement may briefly overlap the previous source. *)

val create :
  caps:< Ox_caps.Cap.env ; .. > ->
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  ?copy_program:string ->
  unit ->
  t
(** Create the owner on [sw]. Shutdown retires the active helper.

    Suspension: may suspend during switch-owned helper retirement; construction
    only allocates state and performs native executable lookup, whose behavior
    depends on [Ox_caps], [Unix], [Filename] and [Stdlib]. Cancellation:
    protected during helper retirement; constructor registration behavior
    depends on [Eio.Switch], and retirement behavior depends on [Eio.Process]
    and [Eio.Promise]. Finalizers: registers a switch release hook that protects
    retirement of the current helper; process and promise behavior is delegated
    to [Eio.Process] and [Eio.Promise]. Locking: construction acquires no lock;
    later owner operations serialize on the owner semaphore, while switch
    teardown protects current-helper retirement. Cancellation safety: safe to
    re-run construction to allocate an independent switch-owned owner;
    construction does not offer text or spawn a helper, and registration
    behavior depends on [Eio.Switch].

    Registering cleanup on an already-finished switch can raise
    [Invalid_argument]. *)

val offer : t -> string -> (unit, [ `Clipboard_error of string ]) result
(** Replace the active offer. The new helper is ready before the previous owned
    helper is retired. A failed replacement leaves the previous helper in place.
    Stdin EOF and helper liveness are not compositor readiness. *)

val shutdown : t -> (unit, [ `Clipboard_error of string ]) result
(** Retire the active helper and await it. Identity-safe: does not clear a newer
    owner. *)
