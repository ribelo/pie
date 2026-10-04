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

let with_temp_dir
    ~(caps :
       < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >) f
    =
  let path = Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-cli-status" "" in
  Ox_caps.Cap_sys.remove ~caps path;
  Ox_caps.Cap_unix.mkdir ~caps path 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name ->
          try Ox_caps.Cap_sys.remove ~caps (Filename.concat path name)
          with Sys_error _ -> ())
        (try Ox_caps.Cap_sys.readdir ~caps path with Sys_error _ -> [||]);
      try Ox_caps.Cap_unix.rmdir ~caps path with Unix.Unix_error _ -> ())
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

let rec drain_select ~(caps : < Ox_caps.Cap.time ; .. >) ~deadline ~stdout_r
    ~stderr_r stdout_buf stderr_buf stdout_open stderr_open =
  if not (stdout_open || stderr_open) then (false, false)
  else
    let now = Ox_caps.Cap_unix.gettimeofday ~caps () in
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
        drain_select ~caps ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
          stdout_open stderr_open
      else (stdout_open, stderr_open)

let run_pie
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; .. >) ~runtime_dir args =
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock stdout_r;
  Unix.set_nonblock stderr_r;
  let env =
    Array.of_list
      [
        "PATH=" ^ runtime_dir ^ ":"
        ^ Option.value ~default:"/usr/bin"
            (Ox_caps.Cap_sys.getenv_opt ~caps "PATH");
        "HOME=" ^ runtime_dir;
        "XDG_CONFIG_HOME=" ^ runtime_dir;
        "XDG_DATA_HOME=" ^ runtime_dir;
        "XDG_RUNTIME_DIR=" ^ runtime_dir;
        "CAMELPIE_RUNTIME_DIR=" ^ runtime_dir;
        "OTEL_SDK_DISABLED=true";
      ]
  in
  let pid =
    Ox_caps.Cap_unix.create_process_env ~caps pie_exe
      (Array.of_list (pie_exe :: args))
      env Unix.stdin stdout_w stderr_w
  in
  close_fd stdout_w;
  close_fd stderr_w;
  let stdout_buf = Buffer.create 256 in
  let stderr_buf = Buffer.create 256 in
  let deadline = Ox_caps.Cap_unix.gettimeofday ~caps () +. child_deadline_s in
  let rec wait_child stdout_open stderr_open =
    let stdout_open, stderr_open =
      drain_select ~caps ~deadline ~stdout_r ~stderr_r stdout_buf stderr_buf
        stdout_open stderr_open
    in
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Ox_caps.Cap_unix.gettimeofday ~caps () >= deadline then (
          (try Ox_caps.Cap_unix.kill ~caps pid Sys.sigkill
           with Unix.Unix_error _ -> ());
          snd (Unix.waitpid [] pid))
        else wait_child stdout_open stderr_open
    | _, status ->
        ignore
          (drain_select ~caps ~deadline ~stdout_r ~stderr_r stdout_buf
             stderr_buf stdout_open stderr_open);
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

(* ^dict-f2vx: exercise the actual CLI diagnostic hook, not just the policy. *)
let test_startup_notice
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) ~config ~expected_notice ~diagnostic () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let write name body =
    let path = Filename.concat runtime_dir name in
    Ox_caps.Cap_stdlib.Out_channel.with_open_text ~caps path (fun channel ->
        output_string channel body);
    path
  in
  let helper =
    write "notify-send"
      "#!/bin/sh\nprintf '%s\\n' \"$@\" >> \"$XDG_RUNTIME_DIR/notice\"\n"
  in
  Ox_caps.Cap_unix.chmod ~caps helper 0o700;
  let args =
    match config with
    | None -> [ "sources" ]
    | Some contents ->
        let path = write "stt.toml" contents in
        [ "assistant"; "--config"; path ]
  in
  let result = run_pie ~caps ~runtime_dir args in
  ignore (require_exited_nonzero result.status);
  if not (string_contains result.stderr diagnostic) then
    Alcotest.failf "expected diagnostic %S in %S" diagnostic result.stderr;
  let notice_path = Filename.concat runtime_dir "notice" in
  let notice =
    if Sys.file_exists notice_path then
      Ox_caps.Cap_stdlib.In_channel.with_open_text ~caps notice_path
        In_channel.input_all
    else ""
  in
  let notices =
    String.split_on_char '\n' notice
    |> List.filter (String.equal "pie: config rejected")
    |> List.length
  in
  Alcotest.(check int)
    "configuration notification count" expected_notice notices

let serve_one ~(caps : < Ox_caps.Cap.fs_write ; Ox_caps.Cap.network ; .. >)
    ~socket_path reply =
  let server = Ox_caps.Cap_unix.socket ~caps Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock server;
  (try Ox_caps.Cap_unix.unlink ~caps socket_path with Unix.Unix_error _ -> ());
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

let join_worker ~(caps : < Ox_caps.Cap.time ; .. >) worker =
  let deadline = Ox_caps.Cap_unix.gettimeofday ~caps () +. 2.0 in
  let rec loop () =
    (* Thread.join has no timeout; closing the listen socket unblocks accept. *)
    try Thread.join worker
    with exn ->
      if Ox_caps.Cap_unix.gettimeofday ~caps () >= deadline then raise exn
      else loop ()
  in
  loop ()

let with_reply
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; .. >) ~runtime_dir reply args =
  let socket_path = Filename.concat runtime_dir "control.sock" in
  let server, worker, error = serve_one ~caps ~socket_path reply in
  Fun.protect
    ~finally:(fun () ->
      close_fd server;
      (try join_worker ~caps worker with _ -> ());
      try Ox_caps.Cap_unix.unlink ~caps socket_path
      with Unix.Unix_error _ -> ())
    (fun () ->
      let result = run_pie ~caps ~runtime_dir args in
      close_fd server;
      join_worker ~caps worker;
      (match !error with Some exn -> raise exn | None -> ());
      result)

let armed_status_json =
  {|{"active":false,"mode":"idle","enabled":true,"startedAt":null,"updatedAt":null,"transcriptPath":null,"lastError":null}|}

let error_status_json = {|{"error":"synthetic protocol error"}|}

let rejected_retry_json =
  {|{"action":"retry","accepted":false,"message":"synthetic rejection","snapshot":{"active":false,"mode":"idle","enabled":true}}|}

let client_identity
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; .. >) ~runtime_dir =
  let run = run_pie ~caps ~runtime_dir [ "--version" ] in
  require_exited_zero run.status;
  match String.split_on_char ' ' (String.trim run.stdout) with
  | [ commit; time ] -> (commit, time)
  | _ -> Alcotest.failf "unexpected build identity: %S" run.stdout

let build_lines
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; .. >) ~runtime_dir ~commit ~time =
  let client_commit, client_time = client_identity ~caps ~runtime_dir in
  Printf.sprintf "build: %s (%s)\n" commit time
  ^
  if commit = client_commit && time = client_time then ""
  else Printf.sprintf "client build: %s (%s)\n" client_commit client_time

let test_status_success
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let run = with_reply ~caps ~runtime_dir armed_status_json [ "status" ] in
  require_exited_zero run.status;
  Alcotest.(check string)
    "stdout armed with unknown daemon identity"
    ("armed\n"
    ^ build_lines ~caps ~runtime_dir ~commit:"unknown" ~time:"unknown")
    run.stdout;
  Alcotest.(check string) "stderr empty" "" run.stderr

let test_status_processing
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let json = {|{"active":false,"processing":2,"mode":"idle","enabled":true}|} in
  let run = with_reply ~caps ~runtime_dir json [ "status" ] in
  require_exited_zero run.status;
  Alcotest.(check string)
    "armed and processing count"
    ("armed\nprocessing: 2\n"
    ^ build_lines ~caps ~runtime_dir ~commit:"unknown" ~time:"unknown")
    run.stdout

let test_status_response_metadata
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let json =
    {|{"active":false,"mode":"idle","enabled":true,"reasoningEffort":"low","responseSchema":"dictation/text-v1","buildCommit":"daemon-old","buildTime":"2026-10-01T10:00:00Z"}|}
  in
  let run = with_reply ~caps ~runtime_dir json [ "status" ] in
  require_exited_zero run.status;
  Alcotest.(check string)
    "configured metadata dict-h8rd"
    ("armed\nreasoning_effort: low\nresponse_schema: dictation/text-v1\n"
    ^ build_lines ~caps ~runtime_dir ~commit:"daemon-old"
        ~time:"2026-10-01T10:00:00Z")
    run.stdout

let test_msg_status_metadata
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) ~same_build () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let client_commit, client_time = client_identity ~caps ~runtime_dir in
  let commit, time =
    if same_build then (client_commit, client_time)
    else ("daemon-old", "2026-10-01T10:00:00Z")
  in
  let json =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("active", `Bool false);
           ("mode", `String "idle");
           ("enabled", `Bool true);
           ("reasoningEffort", `String "low");
           ("responseSchema", `String "dictation/text-v1");
           ("buildCommit", `String commit);
           ("buildTime", `String time);
         ])
  in
  let run = with_reply ~caps ~runtime_dir json [ "msg"; "status"; "--json" ] in
  require_exited_zero run.status;
  let result = Yojson.Safe.from_string run.stdout in
  let field name = Yojson.Safe.Util.member name result in
  List.iter
    (fun (name, expected) ->
      Alcotest.(check string)
        name expected
        (Yojson.Safe.Util.to_string (field name)))
    [
      ("buildCommit", commit);
      ("buildTime", time);
      ("reasoningEffort", "low");
      ("responseSchema", "dictation/text-v1");
    ];
  match result with
  | `Assoc fields ->
      Alcotest.(check bool)
        "client identity only when different" (not same_build)
        (List.mem_assoc "clientBuildCommit" fields);
      if not same_build then (
        Alcotest.(check string)
          "client commit" client_commit
          (Yojson.Safe.Util.to_string (field "clientBuildCommit"));
        Alcotest.(check string)
          "client time" client_time
          (Yojson.Safe.Util.to_string (field "clientBuildTime")))
  | _ -> Alcotest.fail "expected status object"

let test_status_rpc_error
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let run = with_reply ~caps ~runtime_dir error_status_json [ "status" ] in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr contains synthetic protocol error" true
    (string_contains run.stderr "synthetic protocol error")

let test_status_malformed_json
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let run = with_reply ~caps ~runtime_dir "not-json" [ "status" ] in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr identifies malformed protocol response" true
    (string_contains run.stderr "malformed-response")

let test_status_protocol_error_nonzero
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let run =
    with_reply ~caps ~runtime_dir {|{"active":false,"enabled":true}|}
      [ "status" ]
  in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check bool)
    "stdout is not invented off" false
    (string_contains run.stdout "off");
  Alcotest.(check bool)
    "stderr names missing-field" true
    (string_contains run.stderr "missing-field")

let test_status_absent_socket
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let run = run_pie ~caps ~runtime_dir [ "status" ] in
  require_exited_zero run.status;
  Alcotest.(check string) "stdout off" "off\n" run.stdout

let test_msg_status_rpc_error
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let run =
    with_reply ~caps ~runtime_dir error_status_json [ "msg"; "status" ]
  in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr contains synthetic protocol error" true
    (string_contains run.stderr "synthetic protocol error")

let test_msg_action_rejected
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let run =
    with_reply ~caps ~runtime_dir rejected_retry_json
      [ "msg"; "action"; "retry" ]
  in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr contains synthetic rejection" true
    (string_contains run.stderr "synthetic rejection")

let test_retry_rejected
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let run = with_reply ~caps ~runtime_dir rejected_retry_json [ "retry" ] in
  ignore (require_exited_nonzero run.status);
  Alcotest.(check string) "stdout empty" "" run.stdout;
  Alcotest.(check bool)
    "stderr contains synthetic rejection" true
    (string_contains run.stderr "synthetic rejection")

let test_harness_unknown_command_unblocks_server
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let started = Ox_caps.Cap_unix.gettimeofday ~caps () in
  let run = with_reply ~caps ~runtime_dir armed_status_json [ "nosuchcmd" ] in
  let elapsed = Ox_caps.Cap_unix.gettimeofday ~caps () -. started in
  Alcotest.(check bool)
    "finished before child deadline" true
    (elapsed < child_deadline_s);
  ignore (require_exited_nonzero run.status)

let test_subscriber_failed_connect_fds
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.tmp
       ; .. >) ~backend ~refused () =
  with_temp_dir ~caps @@ fun runtime_dir ->
  let bound =
    if refused then (
      let fd =
        Ox_caps.Cap_unix.socket ~caps ~cloexec:true Unix.PF_UNIX
          Unix.SOCK_STREAM 0
      in
      Unix.bind fd (Unix.ADDR_UNIX (Filename.concat runtime_dir "control.sock"));
      Some fd)
    else None
  in
  Fun.protect ~finally:(fun () -> Option.iter close_fd bound) @@ fun () ->
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let null =
    Ox_caps.Cap_unix.openfile ~caps "/dev/null"
      [ Unix.O_RDWR; Unix.O_CLOEXEC ]
      0
  in
  let env =
    [|
      "PATH="
      ^ Option.value ~default:"/usr/bin"
          (Ox_caps.Cap_sys.getenv_opt ~caps "PATH");
      "HOME=" ^ runtime_dir;
      "XDG_RUNTIME_DIR=" ^ runtime_dir;
      "CAMELPIE_RUNTIME_DIR=" ^ runtime_dir;
      "OTEL_SDK_DISABLED=true";
      "EIO_BACKEND=" ^ backend;
    |]
  in
  let pid =
    match
      Ox_caps.Cap_unix.create_process_env ~caps pie_exe
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
      (try Ox_caps.Cap_unix.kill ~caps pid Sys.sigterm
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
    Array.length
      (Ox_caps.Cap_sys.readdir ~caps (Printf.sprintf "/proc/%d/fd" pid))
  in
  let baseline = fd_count () in
  for _ = 1 to 20 do
    Thread.delay 0.01;
    let current = fd_count () in
    Alcotest.(check bool)
      "failed connections do not accumulate (one in-flight allowed)" true
      (current <= baseline + 1)
  done

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "cli_status"
    [
      ( "startup_notice",
        [
          Alcotest.test_case "dict-f2vx audio failure is not config rejection"
            `Quick
            (test_startup_notice ~caps ~config:None ~expected_notice:0
               ~diagnostic:"PulseAudio disconnected");
          Alcotest.test_case "dict-f2vx assistant audio failure stays runtime"
            `Quick
            (test_startup_notice ~caps ~config:(Some "") ~expected_notice:0
               ~diagnostic:"PulseAudio disconnected");
          Alcotest.test_case "dict-f2vx malformed config still notifies" `Quick
            (test_startup_notice ~caps ~config:(Some "[broken")
               ~expected_notice:1 ~diagnostic:"TOML");
          Alcotest.test_case "dict-f2vx source validation still notifies" `Quick
            (test_startup_notice ~caps
               ~config:
                 (Some
                    "[[bindings]]\n\
                     keysym = 65478\n\
                     mode = \"transcribe\"\n\
                     prompt = \"missing.md\"\n")
               ~expected_notice:1 ~diagnostic:"missing.md");
        ] );
      ( "status",
        [
          Alcotest.test_case "status answer and build metadata" `Quick
            (test_status_response_metadata ~caps);
          Alcotest.test_case "JSON answer and differing build metadata" `Quick
            (test_msg_status_metadata ~caps ~same_build:false);
          Alcotest.test_case "JSON matching build omits client identity" `Quick
            (test_msg_status_metadata ~caps ~same_build:true);
          Alcotest.test_case "dict-d4jm armed while processing" `Quick
            (test_status_processing ~caps);
          Alcotest.test_case "valid status stdout and exit 0" `Quick
            (test_status_success ~caps);
          Alcotest.test_case "rpc error nonzero stderr" `Quick
            (test_status_rpc_error ~caps);
          Alcotest.test_case "malformed json nonzero stderr" `Quick
            (test_status_malformed_json ~caps);
          Alcotest.test_case "absent socket off exit 0" `Quick
            (test_status_absent_socket ~caps);
          Alcotest.test_case "protocol error missing-field nonzero" `Quick
            (test_status_protocol_error_nonzero ~caps);
        ] );
      ( "controls",
        [
          Alcotest.test_case "msg status rpc error nonzero" `Quick
            (test_msg_status_rpc_error ~caps);
          Alcotest.test_case "msg action accepted=false nonzero" `Quick
            (test_msg_action_rejected ~caps);
          Alcotest.test_case "retry accepted=false nonzero" `Quick
            (test_retry_rejected ~caps);
        ] );
      ( "subscriber_fds",
        List.concat_map
          (fun backend ->
            List.map
              (fun refused ->
                Alcotest.test_case
                  (backend ^ if refused then " refused" else " absent")
                  `Quick
                  (test_subscriber_failed_connect_fds ~caps ~backend ~refused))
              [ false; true ])
          [ "linux"; "posix" ] );
      ( "harness",
        [
          Alcotest.test_case "unknown command does not hang server join" `Quick
            (test_harness_unknown_command_unblocks_server ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
