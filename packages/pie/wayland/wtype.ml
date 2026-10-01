type error = [ `Wtype_error of string ]

let key ~caps ?(modifiers = []) ~process_mgr key =
  match Sh.find_executable ~caps "wtype" with
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
