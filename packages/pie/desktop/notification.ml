type error = [ `Notification_error of string ]

let notify_send ~process_mgr title message =
  Sh.run ~process_mgr ~prog:"notify-send"
    ~args:[ "--urgency=normal"; "--"; title; message ]
  |> Result.map_error (fun (`Subprocess_error msg) -> `Notification_error msg)

let clipboard_message title =
  let title = Option.value title ~default:"the original window" in
  let buffer = Buffer.create (String.length title) in
  String.iter
    (function
      | '&' -> Buffer.add_string buffer "&amp;"
      | '<' -> Buffer.add_string buffer "&lt;"
      | '>' -> Buffer.add_string buffer "&gt;"
      | c when Char.code c < 32 || Char.code c = 127 ->
          Buffer.add_char buffer ' '
      | c -> Buffer.add_char buffer c)
    title;
  let title = String.trim (Buffer.contents buffer) in
  let title = if title = "" then "the original window" else title in
  Printf.sprintf "Pie: text for %s is in the clipboard" title

let notify_warning ~caps ~process_mgr title message =
  match notify_send ~process_mgr title message with
  | Ok () -> Ok ()
  | Error _ ->
      Ox_caps.Cap_stdlib.eprintf ~caps "Notification failed\n%!";
      Ok ()
