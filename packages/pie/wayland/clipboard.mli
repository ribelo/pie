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
(** Create the owner on [sw]. Shutdown retires the active helper. *)

val offer : t -> string -> (unit, [ `Clipboard_error of string ]) result
(** Replace the active offer. The new helper is ready before the previous owned
    helper is retired. A failed replacement leaves the previous helper in place.
    Stdin EOF and helper liveness are not compositor readiness. *)

val shutdown : t -> (unit, [ `Clipboard_error of string ]) result
(** Retire the active helper and await it. Identity-safe: does not clear a newer
    owner. *)
