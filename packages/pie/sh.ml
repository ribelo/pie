let find_executable name =
  let paths =
    match Sys.getenv_opt "PATH" with
    | Some p -> String.split_on_char ':' p
    | None -> [ "/usr/bin"; "/bin"; "/usr/local/bin" ]
  in
  let rec search = function
    | [] -> None
    | dir :: rest ->
        let path = Filename.concat dir name in
        let is_executable_file =
          match Unix.stat path with
          | { Unix.st_kind = Unix.S_REG; _ } -> (
              match Unix.access path [ Unix.X_OK ] with
              | () -> true
              | exception Unix.Unix_error _ -> false)
          | _ -> false
          | exception Unix.Unix_error _ -> false
        in
        if is_executable_file then Some path else search rest
  in
  search paths

let exit_category = function
  | `Exited n -> Printf.sprintf "exit %d" n
  | `Signaled n -> Printf.sprintf "signaled %d" n

let failure_message ~prog exn =
  let base = Filename.basename prog in
  match exn with
  | Eio.Cancel.Cancelled _ as cancelled -> raise cancelled
  | Eio.Exn.Multiple _ as multiple -> raise multiple
  | Eio.Exn.Io (Eio.Process.E (Eio.Process.Child_error status), _) ->
      Printf.sprintf "%s failed: %s" base (exit_category status)
  | Eio.Exn.Io (Eio.Process.E (Eio.Process.Executable_not_found _), _) ->
      Printf.sprintf "%s failed: not found" base
  | Eio.Exn.Io _ -> Printf.sprintf "%s failed: io-failure" base
  | Unix.Unix_error (_, "execve", _) ->
      Printf.sprintf "%s failed: io-failure" base
  | Failure msg when String.length msg >= 7 && String.sub msg 0 7 = "execve:" ->
      Printf.sprintf "%s failed: io-failure" base
  | other -> raise other

module Discard_sink = struct
  type t = unit

  let single_write () bufs =
    List.fold_left (fun acc buf -> acc + Cstruct.length buf) 0 bufs

  let copy t ~src = Eio.Flow.Pi.simple_copy ~single_write t ~src
end

let discarded_stderr =
  let ops = Eio.Flow.Pi.sink (module Discard_sink) in
  fun () -> Eio.Resource.T ((), ops)

let run ~process_mgr ~prog ~args =
  try
    Eio.Process.run process_mgr ~stdout:(discarded_stderr ())
      ~stderr:(discarded_stderr ()) (prog :: args);
    Ok ()
  with exn -> Error (`Subprocess_error (failure_message ~prog exn))

let run_capture ~process_mgr ~prog ~args =
  try
    Ok
      (Eio.Process.parse_out process_mgr Eio.Buf_read.take_all
         ~stderr:(discarded_stderr ()) (prog :: args))
  with exn -> Error (`Subprocess_error (failure_message ~prog exn))
