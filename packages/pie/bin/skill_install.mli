(** Install the bundled glossary skill into agent skill directories.

    The installed directory name is [pie-glossary]. An explicit [PATH] is that
    skills root. With no path, user-level agent configuration roots under [HOME]
    are discovered and deduplicated by real path. *)

type requested_action = Install | Replace

type disposition =
  | Installed
  | Replaced
  | Current
  | Declined
  | Planned_install
  | Planned_replace

type outcome = { path : string; disposition : disposition }
type error_kind = Usage | Internal
type failure = { path : string option; kind : error_kind; message : string }
type error = { completed : outcome list; failures : failure list }

val run :
  caps:< Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. > ->
  home:string ->
  path:string option ->
  force:bool ->
  no_input:bool ->
  dry_run:bool ->
  stdin_is_tty:bool ->
  confirm:(requested_action -> string -> bool) ->
  (outcome list, error) result
