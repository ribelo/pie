(* ^pcaps-n1rg *)

(* PIE019: user markers stay out of diagnostics; intended output remains.
   Direct style: the provider call and the helper subprocesses return results,
   stdio stays captured around each call, and every deadline is bounded. *)

let marker = "PIE019-UNIQUE-MARKER-7f3c9e"

(* Fixtures rewrite the process environment (private PATH, provider endpoint,
   delivery path); the alert only guards against multi-domain mutation. *)
let putenv ~(caps : < Ox_caps.Cap.env ; .. >) =
  Ox_caps.Cap_unix.putenv ~caps [@alert "-unsafe_multidomain"]

let string_contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let rec rm_rf ~(caps : < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >) path
    =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Ox_caps.Cap_sys.readdir ~caps path with
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                rm_rf ~caps (Filename.concat path name))
            names
      | exception Sys_error _ -> ());
      try Ox_caps.Cap_unix.rmdir ~caps path with Unix.Unix_error _ -> ())
  | _ -> (
      try Ox_caps.Cap_unix.unlink ~caps path with Unix.Unix_error _ -> ())

let with_sandbox
    ~(caps :
       < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >) f
    =
  let dir =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-diagnostic-content" ""
  in
  Ox_caps.Cap_sys.remove ~caps dir;
  Ox_caps.Cap_unix.mkdir ~caps dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf ~caps dir) (fun () -> f dir)

let write_file ~(caps : < Ox_caps.Cap.fs_write ; .. >) path contents =
  let oc =
    Ox_caps.Cap_stdlib.open_out_gen ~caps
      [ Open_creat; Open_trunc; Open_wronly ]
      0o700 path
  in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let with_captured_stdio
    ~(caps :
       < Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) f =
  let out_path =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-diag-stdout" ".log"
  in
  let err_path =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-diag-stderr" ".log"
  in
  let out_oc = Ox_caps.Cap_stdlib.open_out ~caps out_path in
  let err_oc = Ox_caps.Cap_stdlib.open_out ~caps err_path in
  let old_out = Unix.dup Unix.stdout in
  let old_err = Unix.dup Unix.stderr in
  Unix.dup2 (Unix.descr_of_out_channel out_oc) Unix.stdout;
  Unix.dup2 (Unix.descr_of_out_channel err_oc) Unix.stderr;
  Fun.protect
    ~finally:(fun () ->
      flush (Ox_caps.Cap_stdlib.stdout ~caps);
      flush (Ox_caps.Cap_stdlib.stderr ~caps);
      Unix.dup2 old_out Unix.stdout;
      Unix.dup2 old_err Unix.stderr;
      Unix.close old_out;
      Unix.close old_err;
      close_out_noerr out_oc;
      close_out_noerr err_oc)
    (fun () ->
      let result = f () in
      flush (Ox_caps.Cap_stdlib.stdout ~caps);
      flush (Ox_caps.Cap_stdlib.stderr ~caps);
      close_out_noerr out_oc;
      close_out_noerr err_oc;
      let stdout_body =
        Ox_caps.Cap_stdlib.In_channel.with_open_text ~caps out_path
          In_channel.input_all
      in
      let stderr_body =
        Ox_caps.Cap_stdlib.In_channel.with_open_text ~caps err_path
          In_channel.input_all
      in
      (result, stdout_body, stderr_body))

let has_suffix suffix s =
  let sl = String.length suffix in
  let n = String.length s in
  n >= sl && String.sub s (n - sl) sl = suffix

let read_http_request flow =
  let scratch = Cstruct.create 1 in
  let buffer = Buffer.create 512 in
  let rec headers () =
    match Eio.Flow.single_read flow scratch with
    | 0 -> raise End_of_file
    | _ ->
        Buffer.add_char buffer (Cstruct.get_char scratch 0);
        let contents = Buffer.contents buffer in
        if has_suffix "\r\n\r\n" contents then contents else headers ()
    | exception End_of_file -> raise End_of_file
  in
  let head = headers () in
  let content_length =
    let lower = String.lowercase_ascii head in
    match String.split_on_char '\n' lower with
    | lines ->
        List.find_map
          (fun line ->
            if String.starts_with ~prefix:"content-length:" line then
              int_of_string_opt
                (String.trim (String.sub line 15 (String.length line - 15)))
            else None)
          lines
  in
  (match content_length with
  | None | Some 0 -> ()
  | Some n ->
      let body = Cstruct.create n in
      let rec loop off =
        if off < n then
          match Eio.Flow.single_read flow (Cstruct.sub body off (n - off)) with
          | 0 -> raise End_of_file
          | k -> loop (off + k)
          | exception End_of_file -> raise End_of_file
      in
      loop 0);
  head

let write_http flow ~status body =
  let response =
    Printf.sprintf
      "HTTP/1.1 %s\r\n\
       Content-Type: application/json\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n\
       %s"
      status (String.length body) body
  in
  Eio.Flow.copy_string response flow

let completed_body text =
  Yojson.Safe.to_string (`Assoc [ ("text", `String text) ])

let pcm = Bytes.make 320 '\001'

(* The call's own structured records go through this reporter straight into the
   captured stderr, so a diagnostic that leaks the marker is seen where the old
   logfmt tee wrote it. *)
let stderr_logger ~(caps : < Ox_caps.Cap.stderr ; .. >) ~clock ~mono_clock =
  Ox_observability.Logger.create ~service:"pie" ~clock ~mono_clock
    ~reporter:
      (Ox_observability.Logger.Reporter.writer (fun line ->
           Ox_caps.Cap_stdlib.prerr_string ~caps line))
    ()

let openrouter_deadline_seconds = 30
let helper_deadline_seconds = 10.0

let run_openrouter ~(caps : < Ox_caps.Cap.env ; .. >) ~body ~status () =
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "expected TCP listener"
  in
  putenv ~caps "OPENROUTER_BASE_URL" (Printf.sprintf "http://127.0.0.1:%d" port);
  putenv ~caps "OPENROUTER_API_KEY" "synthetic-pie019";
  (* A daemon: the switch cancels the fixture peer when the call under test is
     done, so it cannot hold the release open. *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      Eio.Switch.run (fun conn_sw ->
          let flow, _ = Eio.Net.accept ~sw:conn_sw socket in
          ignore (read_http_request flow);
          write_http flow ~status body);
      `Stop_daemon);
  match
    Ox.Timeout.timeout ~clock (Ox.Duration.seconds openrouter_deadline_seconds)
      (fun () ->
        match
          Pie.Stt.Openrouter.transcribe ~caps ~sw ~net ~clock
            ~api_key:(Ox_observability.Redact.make "synthetic")
            ~model:"test-model" ~language:"en" pcm
        with
        | Ok text -> Ok text
        | Error (`Stt_error message) -> Error (`Stt_error message))
  with
  | Ok text -> Ok text
  | Error (`Stt_error message) -> Error (`Stt_error message)
  | Error `Timeout -> Error (`Stt_error "openrouter-deadline")

let run_helper ~(caps : < Ox_caps.Cap.env ; .. >) f =
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Time.with_timeout_exn clock helper_deadline_seconds (fun () ->
        f ~env ~sw)
  with Eio.Time.Timeout -> Alcotest.fail "helper operation timed out"

let assert_absent label hay =
  Alcotest.(check bool) label false (string_contains hay marker)

let with_fake_helper
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) name script f =
  with_sandbox ~caps @@ fun dir ->
  let bin = Filename.concat dir "bin" in
  Ox_caps.Cap_unix.mkdir ~caps bin 0o700;
  let helper = Filename.concat bin name in
  write_file ~caps helper script;
  Ox_caps.Cap_unix.chmod ~caps helper 0o700;
  let old_path =
    Option.value ~default:"/usr/bin" (Ox_caps.Cap_sys.getenv_opt ~caps "PATH")
  in
  putenv ~caps "PATH" (bin ^ ":" ^ old_path);
  Fun.protect
    ~finally:(fun () -> putenv ~caps "PATH" old_path)
    (fun () -> f dir)

let test_success_keeps_text_out_of_diagnostics
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  List.iter
    (fun debug ->
      (match debug with
      | None -> (
          (* Preserve the exception as data before this test classifies its outcome. *)
          match
            try Result.Ok (putenv ~caps "CAMELPIE_OPENROUTER_DEBUG" "")
            with exn -> Result.Error exn
          with
          | Result.Ok value -> value
          | Result.Error _ -> ())
      | Some value -> putenv ~caps "CAMELPIE_OPENROUTER_DEBUG" value);
      let result, stdout, stderr =
        with_captured_stdio ~caps (fun () ->
            run_openrouter ~caps ~body:(completed_body marker) ~status:"200 OK"
              ())
      in
      (match result with
      | Ok text -> Alcotest.(check string) "delivered" marker text
      | Error (`Stt_error message) ->
          Alcotest.failf "expected success: %s" message);
      assert_absent "stdout dump" stdout;
      assert_absent "stderr dump" stderr)
    [ None; Some "0"; Some "false"; Some "1" ]

let test_provider_error_omits_body
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  putenv ~caps "CAMELPIE_OPENROUTER_DEBUG" "1";
  let body =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("provider", `String marker);
           ( "error",
             `Assoc [ ("message", `String marker); ("code", `String marker) ] );
         ])
  in
  let result, stdout, stderr =
    with_captured_stdio ~caps (fun () ->
        run_openrouter ~caps ~body ~status:"401 Unauthorized" ())
  in
  (match result with
  | Ok text -> Alcotest.failf "expected provider error, got %s" text
  | Error (`Stt_error message) -> assert_absent "error cause" message);
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_malformed_response_omits_payload
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  putenv ~caps "CAMELPIE_OPENROUTER_DEBUG" "1";
  let result, stdout, stderr =
    with_captured_stdio ~caps (fun () ->
        run_openrouter ~caps ~body:marker ~status:"200 OK" ())
  in
  (match result with
  | Ok text -> Alcotest.failf "expected malformed, got %s" text
  | Error (`Stt_error message) -> assert_absent "error cause" message);
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_parse_event_omits_payload () =
  match Pie.Stt.Codex_realtime.parse_event ("not-json " ^ marker) with
  | Ok _ -> Alcotest.fail "expected parse error"
  | Error (`Stt_error text) -> assert_absent "json error" text

let test_helper_nonzero_omits_argv_and_stderr
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_fake_helper ~caps "wtype"
    ("#!/bin/sh\necho " ^ marker ^ "\necho " ^ marker ^ " >&2\nexit 1\n")
  @@ fun _dir ->
  let result, stdout, stderr =
    with_captured_stdio ~caps (fun () ->
        run_helper ~caps (fun ~env ~sw:_ ->
            Pie.Wayland.Wtype.key ~caps ~modifiers:[]
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              marker))
  in
  (match result with
  | Ok () -> Alcotest.fail "expected wtype failure"
  | Error (`Wtype_error message) ->
      Alcotest.(check bool)
        "basename" true
        (string_contains message "wtype failed");
      assert_absent "helper error" message);
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_notification_failure_omits_message
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_fake_helper ~caps "notify-send"
    ("#!/bin/sh\necho " ^ marker ^ "\necho " ^ marker ^ " >&2\nexit 1\n")
  @@ fun _dir ->
  let result, stdout, stderr =
    with_captured_stdio ~caps (fun () ->
        run_helper ~caps (fun ~env ~sw:_ ->
            Pie.Desktop.Notification.notify_send
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              "pie" marker))
  in
  (match result with
  | Ok () -> Alcotest.fail "expected notify-send failure"
  | Error (`Notification_error message) ->
      Alcotest.(check bool)
        "basename" true
        (string_contains message "notify-send failed");
      assert_absent "notify error" message);
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_server_error_omits_remote_code () =
  let payload =
    Yojson.Safe.to_string
      (`Assoc
         [
           ("type", `String "error");
           ( "error",
             `Assoc [ ("message", `String marker); ("code", `String marker) ] );
         ])
  in
  match Pie.Stt.Codex_realtime.parse_event payload with
  | Error (`Stt_error text) ->
      Alcotest.failf "expected Server_error, got %s" text
  | Ok event ->
      let code =
        match event with
        | Pie.Stt.Codex_realtime.Server_error { code; _ } -> code
        | _ -> Alcotest.fail "expected Server_error"
      in
      let diag = Pie.Stt.Codex_realtime.server_error_diagnostic code in
      assert_absent "server error diagnostic" diag

let test_auth_load_omits_json_snippet
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  let path = Filename.concat dir "auth.json" in
  write_file ~caps path ("{\"tokens\": \"" ^ marker ^ " not-json");
  match Pie.Stt.Codex_realtime.Codex_auth.load ~caps ~path () with
  | Ok _ -> Alcotest.fail "expected auth load failure"
  | Error (`Stt_error message) -> assert_absent "auth json snippet" message

let test_large_helper_stderr_discarded
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  let blob = String.make 262144 'A' ^ marker in
  with_fake_helper ~caps "wtype"
    ("#!/bin/sh\nprintf '%s' " ^ Filename.quote blob ^ "\nprintf '%s' "
   ^ Filename.quote blob ^ " >&2\nexit 1\n")
  @@ fun _dir ->
  let result, stdout, stderr =
    with_captured_stdio ~caps (fun () ->
        run_helper ~caps (fun ~env ~sw:_ ->
            Pie.Wayland.Wtype.key ~caps ~modifiers:[]
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              "ok"))
  in
  (match result with
  | Ok () -> Alcotest.fail "expected wtype failure"
  | Error (`Wtype_error message) ->
      Alcotest.(check bool)
        "exit metadata" true
        (string_contains message "wtype failed");
      assert_absent "typed error" message);
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_composite_failure_is_not_flattened () =
  let bt = Printexc.get_raw_backtrace () in
  let multiple =
    Eio.Exn.Multiple
      [ (Failure ("cleanup " ^ marker), bt); (Failure "other", bt) ]
  in
  match
    (* Preserve the exception as data before this test classifies its outcome. *)
    match
      try
        Result.Ok
          (ignore (Pie.Sh.failure_message ~prog:"wtype" multiple);
           `Returned)
      with exn -> Result.Error exn
    with
    | Result.Ok value -> value
    | Result.Error (Eio.Exn.Multiple _) -> `Rethrown
    | Result.Error exn -> `Other (Printexc.to_string exn)
  with
  | `Rethrown -> ()
  | `Returned -> Alcotest.fail "Multiple flattened to Subprocess_error"
  | `Other text -> Alcotest.failf "unexpected %s" text

let test_helper_success_stdout_discarded_delivery_file
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  let intended = "PIE019-INTENDED-DELIVERY" in
  with_fake_helper ~caps "wtype"
    ("#!/bin/sh\necho " ^ marker ^ "\necho " ^ marker
   ^ " >&2\nprintf '%s' \"$2\" > \"$PIE019_DELIVERY\"\nexit 0\n")
  @@ fun dir ->
  let delivery = Filename.concat dir "delivery" in
  putenv ~caps "PIE019_DELIVERY" delivery;
  let result, stdout, stderr =
    with_captured_stdio ~caps (fun () ->
        run_helper ~caps (fun ~env ~sw:_ ->
            Pie.Wayland.Wtype.key ~caps ~modifiers:[]
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              intended))
  in
  (match result with
  | Ok () -> ()
  | Error (`Wtype_error message) ->
      Alcotest.failf "expected wtype success: %s" message);
  let delivered =
    Ox_caps.Cap_stdlib.In_channel.with_open_text ~caps delivery
      In_channel.input_all
  in
  Alcotest.(check string) "delivery file" intended delivered;
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_nonexecutable_spawn_is_io_failure
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  let helper = Filename.concat dir "blocked-helper" in
  write_file ~caps helper "#!/bin/sh\necho should-not-run\n";
  Ox_caps.Cap_unix.chmod ~caps helper 0o600;
  let result, stdout, stderr =
    with_captured_stdio ~caps (fun () ->
        run_helper ~caps (fun ~env ~sw:_ ->
            Pie.Sh.run
              ~process_mgr:(Eio.Stdenv.process_mgr env)
              ~prog:helper ~args:[ "--"; marker ]))
  in
  (match result with
  | Ok () -> Alcotest.fail "expected spawn failure"
  | Error (`Subprocess_error message) ->
      Alcotest.(check bool)
        "basename" true
        (string_contains message "blocked-helper failed");
      Alcotest.(check bool)
        "io-failure" true
        (string_contains message "io-failure");
      assert_absent "spawn error" message);
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "diagnostic-content"
    [
      ( "provider",
        [
          Alcotest.test_case "success keeps marker out of diagnostics" `Quick
            (test_success_keeps_text_out_of_diagnostics ~caps);
          Alcotest.test_case "provider error omits remote body" `Quick
            (test_provider_error_omits_body ~caps);
          Alcotest.test_case "malformed response omits payload" `Quick
            (test_malformed_response_omits_payload ~caps);
          Alcotest.test_case "parse_event omits payload" `Quick
            test_parse_event_omits_payload;
          Alcotest.test_case "server error omits remote code" `Quick
            test_server_error_omits_remote_code;
          Alcotest.test_case "auth load omits JSON snippet" `Quick
            (test_auth_load_omits_json_snippet ~caps);
        ] );
      ( "helpers",
        [
          Alcotest.test_case "wtype success discards stdio and writes delivery"
            `Quick
            (test_helper_success_stdout_discarded_delivery_file ~caps);
          Alcotest.test_case "wtype failure omits argv and stderr" `Quick
            (test_helper_nonzero_omits_argv_and_stderr ~caps);
          Alcotest.test_case "notify-send failure omits message" `Quick
            (test_notification_failure_omits_message ~caps);
          Alcotest.test_case "large helper stderr is discarded" `Quick
            (test_large_helper_stderr_discarded ~caps);
          Alcotest.test_case "composite cancellation is not flattened" `Quick
            test_composite_failure_is_not_flattened;
          Alcotest.test_case "non-executable spawn is io-failure metadata"
            `Quick
            (test_nonexecutable_spawn_is_io_failure ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
