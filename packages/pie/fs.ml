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
