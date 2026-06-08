let find_executable name =
  let paths =
    match Sys.getenv_opt "PATH" with
    | Some p -> String.split_on_char ':' p
    | None -> ["/usr/bin"; "/bin"; "/usr/local/bin"]
  in
  let rec search = function
    | [] -> None
    | dir :: rest ->
      let path = Filename.concat dir name in
      let is_executable_file =
        match Unix.stat path with
        | { Unix.st_kind = Unix.S_REG; _ } ->
          (match Unix.access path [ Unix.X_OK ] with
           | () -> true
           | exception Unix.Unix_error _ -> false)
        | _ -> false
        | exception Unix.Unix_error _ -> false
      in
      if is_executable_file then Some path else search rest
  in
  search paths

let run ~process_mgr ~prog ~args =
  Eta.Effect.sync (fun () ->
    try
      Eio.Process.run process_mgr (prog :: args);
      Ok ()
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
      Error (Printf.sprintf "%s failed: %s" prog (Printexc.to_string exn)))
  |> Eta.Effect.map (Result.map_error (fun msg -> `Subprocess_error msg))
  |> Eta.Effect.bind Eta.Effect.from_result

let run_capture ~process_mgr ~prog ~args =
  Eta.Effect.sync (fun () ->
    try
      Ok (Eio.Process.parse_out process_mgr Eio.Buf_read.take_all (prog :: args))
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
      Error (Printf.sprintf "%s failed: %s" prog (Printexc.to_string exn)))
  |> Eta.Effect.map (Result.map_error (fun msg -> `Subprocess_error msg))
  |> Eta.Effect.bind Eta.Effect.from_result
