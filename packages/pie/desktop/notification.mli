type error = [ `Notification_error of string ]

val clipboard_message : string option -> string
(** A clipboard-only notice using the key-press title, without the transcript.
    Escapes notification markup and replaces control characters. *)

val notify_send :
  process_mgr:_ Eio.Process.mgr -> string -> string -> (unit, error) result

val notify_warning :
  caps:< Ox_caps.Cap.stderr ; .. > ->
  process_mgr:_ Eio.Process.mgr ->
  string ->
  string ->
  (unit, error) result
