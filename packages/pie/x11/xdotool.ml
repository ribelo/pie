type error = [ `Xdotool_error of string ]

let key ~process_mgr key =
  match Sh.find_executable "xdotool" with
  | None -> Eta.Effect.fail (`Xdotool_error "xdotool not found in PATH")
  | Some xdotool ->
      Sh.run ~process_mgr ~prog:xdotool ~args:[ "key"; "--clearmodifiers"; key ]
      |> Eta.Effect.map_error (fun (`Subprocess_error msg) ->
          `Xdotool_error msg)

let type_text ~process_mgr text =
  if String.length text = 0 then Eta.Effect.unit
  else
    match Sh.find_executable "xdotool" with
    | None -> Eta.Effect.fail (`Xdotool_error "xdotool not found in PATH")
    | Some xdotool ->
        Sh.run ~process_mgr ~prog:xdotool
          ~args:[ "type"; "--clearmodifiers"; "--"; text ]
        |> Eta.Effect.map_error (fun (`Subprocess_error msg) ->
            `Xdotool_error msg)
