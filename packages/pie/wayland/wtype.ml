type error = [ `Wtype_error of string ]

let key ?(modifiers = []) ~process_mgr key =
  match Sh.find_executable "wtype" with
  | None -> Error (`Wtype_error "wtype not found in PATH")
  | Some wtype ->
      let args =
        List.concat_map (fun modifier -> [ "-M"; modifier ]) modifiers
        @ [ "-k"; key ]
        @ List.concat_map
            (fun modifier -> [ "-m"; modifier ])
            (List.rev modifiers)
      in
      Sh.run ~process_mgr ~prog:wtype ~args
      |> Result.map_error (fun (`Subprocess_error msg) -> `Wtype_error msg)

let type_text ~process_mgr text =
  if String.length text = 0 then Ok ()
  else
    match Sh.find_executable "wtype" with
    | None -> Error (`Wtype_error "wtype not found in PATH")
    | Some wtype ->
        Sh.run ~process_mgr ~prog:wtype ~args:[ "--"; text ]
        |> Result.map_error (fun (`Subprocess_error msg) -> `Wtype_error msg)
