let pie_exe =
  Filename.concat (Filename.dirname Sys.executable_name) "../../bin/pie.exe"

let child_deadline_s = 4.0

let string_contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let with_temp_dir f =
  let path = Filename.temp_file "pie-cli-status" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name ->
          try Sys.remove (Filename.concat path name) with Sys_error _ -> ())
        (try Sys.readdir path with Sys_error _ -> [||]);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
    (fun () -> f path)

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

let run_pie ~runtime_dir args =
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

let serve_one ~socket_path reply =
  let server = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock server;
  (try Unix.unlink socket_path with Unix.Unix_error _ -> ());
  Unix.bind server (Unix.ADDR_UNIX socket_path);
  Unix.listen server 1;
  let error = ref None in
  let worker =
    Thread.create
      (fun () ->
        let rec accept_loop () =
          try Unix.accept server
          with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
            Thread.delay 0.01;
            accept_loop ()
        in
        try
          let client, _ = accept_loop () in
          Fun.protect
            ~finally:(fun () -> close_fd client)
            (fun () ->
              Unix.clear_nonblock client;
              Unix.setsockopt_float client Unix.SO_RCVTIMEO 2.0;
              Unix.setsockopt_float client Unix.SO_SNDTIMEO 2.0;
              let buf = Bytes.create 1024 in
              let rec read_line acc =
                if string_contains acc "\n" then ()
                else
                  match Unix.read client buf 0 (Bytes.length buf) with
                  | 0 -> ()
                  | n -> read_line (acc ^ Bytes.sub_string buf 0 n)
              in
              read_line "";
              let payload = Bytes.of_string (reply ^ "\n") in
              ignore (Unix.write client payload 0 (Bytes.length payload)))
        with
        | Unix.Unix_error
            ((Unix.EBADF | Unix.EINVAL | Unix.ECONNABORTED | Unix.EINTR), _, _)
          ->
            ()
        | exn -> error := Some exn)
      ()
  in
  (server, worker, error)

let join_worker worker =
  let deadline = Unix.gettimeofday () +. 2.0 in
  let rec loop () =
    (* Thread.join has no timeout; closing the listen socket unblocks accept. *)
    try Thread.join worker
    with exn ->
      if Unix.gettimeofday () >= deadline then raise exn else loop ()
  in
  loop ()

let with_reply ~runtime_dir reply args =
  let socket_path = Filename.concat runtime_dir "control.sock" in
  let server, worker, error = serve_one ~socket_path reply in
  Fun.protect
    ~finally:(fun () ->
      close_fd server;
      (try join_worker worker with _ -> ());
      try Unix.unlink socket_path with Unix.Unix_error _ -> ())
    (fun () ->
      let result = run_pie ~runtime_dir args in
      close_fd server;
      join_worker worker;
      (match !error with Some exn -> raise exn | None -> ());
      result)

let armed_status_json =
  {|{"active":false,"mode":"idle","enabled":true,"startedAt":null,"updatedAt":null,"transcriptPath":null,"lastError":null}|}

let error_status_json = {|{"error":"synthetic protocol error"}|}

let rejected_retry_json =
  {|{"action":"retry","accepted":false,"message":"synthetic rejection","snapshot":{"active":false,"mode":"idle","enabled":true}}|}

let test_status_success () =
  with_temp_dir @@ fun runtime_dir ->
  let run = with_reply ~runtime_dir armed_status_json [ "status" ] in
  require_exited_zero run.status;
  Alcotest.(check string) "stdout armed" "armed\n" run.stdout;
  Alcotest.(check string) "stderr empty" "" run.stderr

let test_status_rpc_error () =
  with_temp_dir @@ fun runtime_dir ->
  let run = with_reply ~runtime_dir error_status_json [ "status" ] in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr contains synthetic protocol error" true
    (string_contains run.stderr "synthetic protocol error")

let test_status_malformed_json () =
  with_temp_dir @@ fun runtime_dir ->
  let run = with_reply ~runtime_dir "not-json" [ "status" ] in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr identifies malformed protocol response" true
    (string_contains run.stderr "malformed-response")

let test_status_protocol_error_nonzero () =
  with_temp_dir @@ fun runtime_dir ->
  let run =
    with_reply ~runtime_dir {|{"active":false,"enabled":true}|} [ "status" ]
  in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check bool)
    "stdout is not invented off" false
    (string_contains run.stdout "off");
  Alcotest.(check bool)
    "stderr names missing-field" true
    (string_contains run.stderr "missing-field")

let test_status_absent_socket () =
  with_temp_dir @@ fun runtime_dir ->
  let run = run_pie ~runtime_dir [ "status" ] in
  require_exited_zero run.status;
  Alcotest.(check string) "stdout off" "off\n" run.stdout

let test_msg_status_rpc_error () =
  with_temp_dir @@ fun runtime_dir ->
  let run = with_reply ~runtime_dir error_status_json [ "msg"; "status" ] in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr contains synthetic protocol error" true
    (string_contains run.stderr "synthetic protocol error")

let test_msg_action_rejected () =
  with_temp_dir @@ fun runtime_dir ->
  let run =
    with_reply ~runtime_dir rejected_retry_json [ "msg"; "action"; "retry" ]
  in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr contains synthetic rejection" true
    (string_contains run.stderr "synthetic rejection")

let test_retry_rejected () =
  with_temp_dir @@ fun runtime_dir ->
  let run = with_reply ~runtime_dir rejected_retry_json [ "retry" ] in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr contains synthetic rejection" true
    (string_contains run.stderr "synthetic rejection")

let test_harness_unknown_command_unblocks_server () =
  with_temp_dir @@ fun runtime_dir ->
  let started = Unix.gettimeofday () in
  let run = with_reply ~runtime_dir armed_status_json [ "nosuchcmd" ] in
  let elapsed = Unix.gettimeofday () -. started in
  Alcotest.(check bool)
    "finished before child deadline" true
    (elapsed < child_deadline_s);
  ignore (require_exited_nonzero run.status)

let test_subscriber_failed_connect_fds ~backend ~refused () =
  with_temp_dir @@ fun runtime_dir ->
  let bound =
    if refused then (
      let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      Unix.bind fd (Unix.ADDR_UNIX (Filename.concat runtime_dir "control.sock"));
      Some fd)
    else None
  in
  Fun.protect ~finally:(fun () -> Option.iter close_fd bound) @@ fun () ->
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let null = Unix.openfile "/dev/null" [ Unix.O_RDWR; Unix.O_CLOEXEC ] 0 in
  let env =
    [|
      "PATH=" ^ Option.value ~default:"/usr/bin" (Sys.getenv_opt "PATH");
      "HOME=" ^ runtime_dir;
      "XDG_RUNTIME_DIR=" ^ runtime_dir;
      "CAMELPIE_RUNTIME_DIR=" ^ runtime_dir;
      "OTEL_SDK_DISABLED=true";
      "EIO_BACKEND=" ^ backend;
    |]
  in
  let pid =
    match
      Unix.create_process_env pie_exe
        [| pie_exe; "msg"; "subscribe"; "--interval-ms=1" |]
        env null stdout_w null
    with
    | pid -> pid
    | exception exn ->
        List.iter close_fd [ stdout_r; stdout_w; null ];
        raise exn
  in
  close_fd stdout_w;
  close_fd null;
  Fun.protect ~finally:(fun () ->
      close_fd stdout_r;
      (try Unix.kill pid Sys.sigterm
       with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
      ignore (Unix.waitpid [] pid))
  @@ fun () ->
  let ready, _, _ = Unix.select [ stdout_r ] [] [] child_deadline_s in
  if ready = [] then
    Alcotest.fail "subscriber did not report unavailable daemon";
  let buffer = Bytes.create 4096 in
  let count = Unix.read stdout_r buffer 0 (Bytes.length buffer) in
  Alcotest.(check bool)
    "expected unavailable daemon result" true
    (string_contains (Bytes.sub_string buffer 0 count) "daemon is not running");
  let fd_count () =
    Array.length (Sys.readdir (Printf.sprintf "/proc/%d/fd" pid))
  in
  let baseline = fd_count () in
  for _ = 1 to 20 do
    Thread.delay 0.01;
    let current = fd_count () in
    Alcotest.(check bool)
      "failed connections do not accumulate (one in-flight allowed)" true
      (current <= baseline + 1)
  done

let () =
  Alcotest.run "cli_status"
    [
      ( "status",
        [
          Alcotest.test_case "valid status stdout and exit 0" `Quick
            test_status_success;
          Alcotest.test_case "rpc error nonzero stderr" `Quick
            test_status_rpc_error;
          Alcotest.test_case "malformed json nonzero stderr" `Quick
            test_status_malformed_json;
          Alcotest.test_case "absent socket off exit 0" `Quick
            test_status_absent_socket;
          Alcotest.test_case "protocol error missing-field nonzero" `Quick
            test_status_protocol_error_nonzero;
        ] );
      ( "controls",
        [
          Alcotest.test_case "msg status rpc error nonzero" `Quick
            test_msg_status_rpc_error;
          Alcotest.test_case "msg action accepted=false nonzero" `Quick
            test_msg_action_rejected;
          Alcotest.test_case "retry accepted=false nonzero" `Quick
            test_retry_rejected;
        ] );
      ( "subscriber_fds",
        List.concat_map
          (fun backend ->
            List.map
              (fun refused ->
                Alcotest.test_case
                  (backend ^ if refused then " refused" else " absent")
                  `Quick
                  (test_subscriber_failed_connect_fds ~backend ~refused))
              [ false; true ])
          [ "linux"; "posix" ] );
      ( "harness",
        [
          Alcotest.test_case "unknown command does not hang server join" `Quick
            test_harness_unknown_command_unblocks_server;
        ] );
    ]
