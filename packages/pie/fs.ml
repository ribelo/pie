(* A started native callback owns its resources through physical completion.
   Keep its tiny Ox pool in a protected child switch, and drain before closing
   that switch; cancelling the caller must not race writer cleanup. *)
let blocking f =
  Eio.Cancel.protect (fun () ->
      Eio.Switch.run (fun sw ->
          let pool =
            Ox_blocking.Pool.create ~sw ~name:"pie-filesystem"
              { max_threads = 1; max_queued = 0; shutdown_policy = Drain }
          in
          Fun.protect
            ~finally:(fun () -> Ox_blocking.Pool.shutdown pool)
            (fun () ->
              match Ox_blocking.run ~pool f with
              | Ok value -> value
              | Error `Shutting_down -> assert false)))

let rec mkdir_p ~caps path =
  if path = "" || path = Filename.dirname path then ()
  else
    match Ox_caps.Cap_unix.mkdir ~caps path 0o755 with
    | () -> ()
    | exception Unix.Unix_error (Unix.EEXIST, _, _) ->
        (* Already exists: only acceptable if it is a directory. A regular file
         at [path] must not be silently treated as success. *)
        if Sys.is_directory path then ()
        else raise (Unix.Unix_error (Unix.EEXIST, "mkdir", path))
    | exception Unix.Unix_error (Unix.ENOENT, _, _) ->
        (* Parent does not exist yet: create it first, then retry. *)
        mkdir_p ~caps (Filename.dirname path);
        Ox_caps.Cap_unix.mkdir ~caps path 0o755
