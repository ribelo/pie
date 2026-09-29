type t
(** Application-owned clipboard offer. At most one helper process. *)

val create :
  caps:< Ox_caps.Cap.env ; .. > ->
  sw:Eio.Switch.t ->
  process_mgr:_ Eio.Process.mgr ->
  ?copy_program:string ->
  unit ->
  t
(** Create the owner on [sw]. Shutdown retires the active helper. *)

val offer : t -> string -> (unit, [ `Clipboard_error of string ]) result
(** Replace the active offer. Retires the previous owned helper first. Stdin EOF
    and helper liveness are not compositor readiness. *)

val shutdown : t -> (unit, [ `Clipboard_error of string ]) result
(** Retire the active helper and await it. Identity-safe: does not clear a newer
    owner. *)
