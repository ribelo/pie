type error = [ `Xdotool_error of string ]

let key ~caps ~process_mgr key =
  match Sh.find_executable ~caps "xdotool" with
  | None -> Error (`Xdotool_error "xdotool not found in PATH")
  | Some xdotool ->
      Sh.run ~process_mgr ~prog:xdotool ~args:[ "key"; "--clearmodifiers"; key ]
      |> Result.map_error (fun (`Subprocess_error msg) -> `Xdotool_error msg)

let type_text ~caps ~process_mgr text =
  if String.length text = 0 then Ok ()
  else
    match Sh.find_executable ~caps "xdotool" with
    | None -> Error (`Xdotool_error "xdotool not found in PATH")
    | Some xdotool ->
        Sh.run ~process_mgr ~prog:xdotool
          ~args:[ "type"; "--clearmodifiers"; "--"; text ]
        |> Result.map_error (fun (`Subprocess_error msg) -> `Xdotool_error msg)
