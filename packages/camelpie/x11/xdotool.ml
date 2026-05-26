type error = [`Xdotool_error of string]

let type_text ~process_mgr text =
  let normalized = String.trim text in
  if String.length normalized = 0 then Eta.Effect.unit
  else
    match Sh.find_executable "xdotool" with
    | None -> Eta.Effect.fail (`Xdotool_error "xdotool not found in PATH")
    | Some xdotool ->
      Sh.run ~process_mgr ~prog:xdotool ~args:["type"; "--clearmodifiers"; "--"; normalized]
      |> Eta.Effect.catch (fun (`Subprocess_error msg) ->
        Eta.Effect.fail (`Xdotool_error msg))
