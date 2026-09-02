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
  Eta.Effect.sync_exn
    (fun () -> Eio.Process.run process_mgr (prog :: args))
    ~catch:(function
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
        `Subprocess_error (Printf.sprintf "%s failed: %s" prog (Printexc.to_string exn)))

let run_capture ~process_mgr ~prog ~args =
  Eta.Effect.sync_exn
    (fun () -> Eio.Process.parse_out process_mgr Eio.Buf_read.take_all (prog :: args))
    ~catch:(function
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
        `Subprocess_error (Printf.sprintf "%s failed: %s" prog (Printexc.to_string exn)))
