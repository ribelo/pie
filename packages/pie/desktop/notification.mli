type error = [ `Notification_error of string ]

val notify_send :
  process_mgr:_ Eio.Process.mgr ->
  string ->
  string ->
  ( unit,
    ([> `Notification_error of string | `Subprocess_error of string ] as 'err)
  )
  Eta.Effect.t

val notify_warning :
  process_mgr:_ Eio.Process.mgr -> string -> string -> (unit, 'err) Eta.Effect.t
