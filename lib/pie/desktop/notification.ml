type error = [`Notification_error of string]

let notify_send ~process_mgr title message =
  Sh.run ~process_mgr ~prog:"notify-send"
    ~args:["--urgency=normal"; title; message]
  |> Eta.Effect.map_error (fun (`Subprocess_error msg) -> `Notification_error msg)

let notify_warning ~process_mgr title message =
  notify_send ~process_mgr title message
  |> Eta.Effect.bind_error (fun _ ->
    Eta.Effect.sync (fun () ->
      Printf.eprintf "Notification failed\n%!"))
