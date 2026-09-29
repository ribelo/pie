type error = [ `Notification_error of string ]

val notify_send :
  process_mgr:_ Eio.Process.mgr -> string -> string -> (unit, error) result

val notify_warning :
  caps:< Ox_caps.Cap.stderr ; .. > ->
  process_mgr:_ Eio.Process.mgr ->
  string ->
  string ->
  (unit, error) result
