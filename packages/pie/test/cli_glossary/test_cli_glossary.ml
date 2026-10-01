let pie_exe =
  Filename.concat (Filename.dirname Sys.executable_name) "../../bin/pie.exe"

let child_deadline_s = 4.0

let string_contains hay needle =
  let rec loop i =
    i <= String.length hay - String.length needle
    && (String.sub hay i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let rec chmod_tree path =
  (try Unix.chmod path 0o700 with Unix.Unix_error _ -> ());
  match Unix.lstat path with
  | exception Unix.Unix_error _ -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      match Sys.readdir path with
      | exception Sys_error _ -> ()
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                chmod_tree (Filename.concat path name))
            names)
  | _ -> ()

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Sys.readdir path with
      | exception Sys_error _ -> ()
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                rm_rf (Filename.concat path name))
            names);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let temp_dir name =
  let path = Filename.temp_file name "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Unix.realpath path

let with_sandbox f =
  let root = temp_dir "pie-glossary" in
  Fun.protect
    ~finally:(fun () ->
      chmod_tree root;
      rm_rf root)
    (fun () -> f root)

let rec mkdir_p path =
  if path = "" || path = "/" || path = "." then ()
  else (
    mkdir_p (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write_file path contents =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

type cli_run = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let read_available buf fd =
  let bytes = Bytes.create 256 in
  try
    match Unix.read fd bytes 0 (Bytes.length bytes) with
    | 0 -> `Eof
    | n ->
        Buffer.add_subbytes buf bytes 0 n;
        `Read
  with
  | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> `Empty
  | Unix.Unix_error (Unix.EBADF, _, _) -> `Eof

let rec drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
    stdout_open stderr_open =
  if not (stdout_open || stderr_open) then (false, false)
  else
    let now = Unix.gettimeofday () in
    if now >= deadline then (stdout_open, stderr_open)
    else
      let timeout = min 0.05 (deadline -. now) in
      let waiting =
        (if stdout_open then [ stdout_r ] else [])
        @ if stderr_open then [ stderr_r ] else []
      in
      let ready, _, _ =
        try Unix.select waiting [] [] timeout
        with Unix.Unix_error (Unix.EBADF, _, _) -> ([], [], [])
      in
      let stdout_open, stdout_more =
        if List.mem stdout_r ready then
          match read_available stdout_buf stdout_r with
          | `Eof ->
              close_fd stdout_r;
              (false, false)
          | `Read -> (true, true)
          | `Empty -> (true, false)
        else (stdout_open, false)
      in
      let stderr_open, stderr_more =
        if List.mem stderr_r ready then
          match read_available stderr_buf stderr_r with
          | `Eof ->
              close_fd stderr_r;
              (false, false)
          | `Read -> (true, true)
          | `Empty -> (true, false)
        else (stderr_open, false)
      in
      if stdout_more || stderr_more then
        drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
          stdout_open stderr_open
      else (stdout_open, stderr_open)

let run_pie ~runtime_dir ?(glossary = "") args =
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock stdout_r;
  Unix.set_nonblock stderr_r;
  let env =
    Array.of_list
      [
        "PATH=" ^ Option.value ~default:"/usr/bin" (Sys.getenv_opt "PATH");
        "HOME=" ^ runtime_dir;
        "XDG_CONFIG_HOME=" ^ runtime_dir;
        "XDG_DATA_HOME=" ^ runtime_dir;
        "XDG_RUNTIME_DIR=" ^ runtime_dir;
        "CAMELPIE_RUNTIME_DIR=" ^ runtime_dir;
        "OTEL_SDK_DISABLED=true";
        "PIE_GLOSSARY=" ^ glossary;
        "TERM=dumb";
      ]
  in
  let pid =
    Unix.create_process_env pie_exe
      (Array.of_list (pie_exe :: args))
      env Unix.stdin stdout_w stderr_w
  in
  close_fd stdout_w;
  close_fd stderr_w;
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 256 in
  let deadline = Unix.gettimeofday () +. child_deadline_s in
  let rec wait_child stdout_open stderr_open =
    let stdout_open, stderr_open =
      drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
        stdout_open stderr_open
    in
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () >= deadline then (
          (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ());
          snd (Unix.waitpid [] pid))
        else wait_child stdout_open stderr_open
    | _, status ->
        ignore
          (drain_select ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
             stdout_open stderr_open);
        status
  in
  let status =
    Fun.protect
      ~finally:(fun () ->
        close_fd stdout_r;
        close_fd stderr_r)
      (fun () -> wait_child true true)
  in
  {
    status;
    stdout = Buffer.contents stdout_buf;
    stderr = Buffer.contents stderr_buf;
  }

let require_exited_zero status =
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Alcotest.failf "expected exit 0, got %d" n
  | Unix.WSIGNALED n -> Alcotest.failf "expected exit 0, signaled %d" n
  | Unix.WSTOPPED n -> Alcotest.failf "expected exit 0, stopped %d" n

let require_exited_nonzero status =
  match status with
  | Unix.WEXITED 0 -> Alcotest.fail "expected nonzero exit, got 0"
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n ->
      Alcotest.failf "expected normal nonzero exit, signaled %d" n
  | Unix.WSTOPPED n ->
      Alcotest.failf "expected normal nonzero exit, stopped %d" n

let check_status run path exists lines =
  require_exited_zero run.status;
  Alcotest.(check string)
    "status"
    (Printf.sprintf "Path: %s\nExists: %s\nLines: %s\n" path exists lines)
    run.stdout

let test_default () =
  with_sandbox (fun root ->
      let path = Filename.concat root "pie/glossary.md" in
      write_file path "## Tools\n- Eio — ejo\n- Dune — dun";
      let run = run_pie ~runtime_dir:root [ "glossary" ] in
      check_status run path "yes" "3";
      Alcotest.(check string) "no warnings" "" run.stderr)

let test_override_and_reload () =
  with_sandbox (fun root ->
      let path = Filename.concat root "custom.md" in
      let run () = run_pie ~runtime_dir:root ~glossary:path [ "glossary" ] in
      write_file path "one\n";
      check_status (run ()) path "yes" "1";
      write_file path "one\ntwo\n";
      check_status (run ()) path "yes" "2")

let test_missing () =
  with_sandbox (fun root ->
      let path = Filename.concat root "pie/glossary.md" in
      let run = run_pie ~runtime_dir:root [ "glossary" ] in
      check_status run path "no" "0";
      Alcotest.(check bool)
        "warning" true
        (string_contains run.stderr "Warning:"))

let test_symlink_and_unreadable () =
  with_sandbox (fun root ->
      let path = Filename.concat root "custom.md" in
      let target = Filename.concat root "target.md" in
      write_file target "";
      Unix.symlink target path;
      check_status
        (run_pie ~runtime_dir:root ~glossary:path [ "glossary" ])
        path "yes" "0";
      Unix.unlink path;
      Unix.mkdir path 0o700;
      let run = run_pie ~runtime_dir:root ~glossary:path [ "glossary" ] in
      check_status run path "yes" "unavailable";
      Alcotest.(check bool)
        "warning reason" true
        (string_contains run.stderr "is a directory"))

let test_help () =
  with_sandbox (fun root ->
      let run = run_pie ~runtime_dir:root [ "glossary"; "--help=plain" ] in
      require_exited_zero run.status;
      let help =
        run.stdout
        |> String.map (function '\n' | '\t' | '\r' -> ' ' | ch -> ch)
        |> String.split_on_char ' '
        |> List.filter (fun word -> word <> "")
        |> String.concat " "
      in
      List.iter
        (fun text ->
          Alcotest.(check bool) text true (string_contains help text))
        [
          "correct spelling";
          "mishear";
          "## headings";
          "PIE_GLOSSARY";
          "next key press";
          "coding agent";
          "names";
          "identifiers";
          "tools";
          "commands";
        ])

let test_skill_retired () =
  with_sandbox (fun root ->
      let run = run_pie ~runtime_dir:root [ "skill"; "install" ] in
      ignore (require_exited_nonzero run.status);
      Alcotest.(check bool)
        "unknown command" true
        (string_contains run.stderr "unknown command"))

let () =
  Alcotest.run "CLI glossary"
    [
      ( "glossary",
        [
          Alcotest.test_case "default path and line count" `Quick test_default;
          Alcotest.test_case "override and next read" `Quick
            test_override_and_reload;
          Alcotest.test_case "missing file warns" `Quick test_missing;
          Alcotest.test_case "symlink and unreadable file" `Quick
            test_symlink_and_unreadable;
          Alcotest.test_case "format and maintenance help" `Quick test_help;
          Alcotest.test_case "skill command retired" `Quick test_skill_retired;
        ] );
    ]
