type error = [ `Notification_error of string ]

let notify_send ~process_mgr title message =
  Sh.run ~process_mgr ~prog:"notify-send"
    ~args:[ "--urgency=normal"; title; message ]
  |> Result.map_error (fun (`Subprocess_error msg) -> `Notification_error msg)

let notify_warning ~process_mgr title message =
  match notify_send ~process_mgr title message with
  | Ok () -> Ok ()
  | Error _ ->
      Printf.eprintf "Notification failed\n%!";
      Ok ()
