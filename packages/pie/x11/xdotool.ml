type error = [ `Xdotool_error of string ]

let key ~process_mgr key =
  match Sh.find_executable "xdotool" with
  | None -> Error (`Xdotool_error "xdotool not found in PATH")
  | Some xdotool ->
      Sh.run ~process_mgr ~prog:xdotool ~args:[ "key"; "--clearmodifiers"; key ]
      |> Result.map_error (fun (`Subprocess_error msg) -> `Xdotool_error msg)

let type_text ~process_mgr text =
  if String.length text = 0 then Ok ()
  else
    match Sh.find_executable "xdotool" with
    | None -> Error (`Xdotool_error "xdotool not found in PATH")
    | Some xdotool ->
        Sh.run ~process_mgr ~prog:xdotool
          ~args:[ "type"; "--clearmodifiers"; "--"; text ]
        |> Result.map_error (fun (`Subprocess_error msg) -> `Xdotool_error msg)
