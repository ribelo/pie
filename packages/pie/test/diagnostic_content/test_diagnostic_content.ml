(* PIE019: user markers stay out of diagnostics; intended output remains. *)

let marker = "PIE019-UNIQUE-MARKER-7f3c9e"

let string_contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Sys.readdir path with
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                rm_rf (Filename.concat path name))
            names
      | exception Sys_error _ -> ());
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let with_sandbox f =
  let dir = Filename.temp_file "pie-diagnostic-content" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let write_file path contents =
  let oc = open_out_gen [ Open_creat; Open_trunc; Open_wronly ] 0o700 path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let with_captured_stdio f =
  let out_path = Filename.temp_file "pie-diag-stdout" ".log" in
  let err_path = Filename.temp_file "pie-diag-stderr" ".log" in
  let out_oc = open_out out_path in
  let err_oc = open_out err_path in
  let old_out = Unix.dup Unix.stdout in
  let old_err = Unix.dup Unix.stderr in
  Unix.dup2 (Unix.descr_of_out_channel out_oc) Unix.stdout;
  Unix.dup2 (Unix.descr_of_out_channel err_oc) Unix.stderr;
  Fun.protect
    ~finally:(fun () ->
      flush stdout;
      flush stderr;
      Unix.dup2 old_out Unix.stdout;
      Unix.dup2 old_err Unix.stderr;
      Unix.close old_out;
      Unix.close old_err;
      close_out_noerr out_oc;
      close_out_noerr err_oc)
    (fun () ->
      let result = f () in
      flush stdout;
      flush stderr;
      close_out_noerr out_oc;
      close_out_noerr err_oc;
      let stdout_body =
        In_channel.with_open_text out_path In_channel.input_all
      in
      let stderr_body =
        In_channel.with_open_text err_path In_channel.input_all
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
  Yojson.Safe.to_string
    (`Assoc
       [
         ("status", `String "completed");
         ( "output",
           `List
             [
               `Assoc
                 [
                   ("type", `String "message");
                   ( "content",
                     `List
                       [
                         `Assoc
                           [
                             ( "text",
                               `String
                                 (Yojson.Safe.to_string
                                    (`Assoc [ ("text", `String text) ])) );
                           ];
                       ] );
                 ];
             ] );
       ])

let pcm = Bytes.make 320 '\001'

let run_openrouter ~body ~status () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "expected TCP listener"
  in
  Unix.putenv "OPENROUTER_BASE_URL" (Printf.sprintf "http://127.0.0.1:%d" port);
  Unix.putenv "OPENROUTER_API_KEY" "synthetic-pie019";
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run (fun conn_sw ->
          let flow, _ = Eio.Net.accept ~sw:conn_sw socket in
          ignore (read_http_request flow);
          write_http flow ~status body));
  Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
    (Pie.Stt.Openrouter.transcribe_with_prompt ~sw ~net ~model:"test-model"
       ~language:"en" ~prompt_template:"Transcribe." pcm)

let cause_text cause =
  Format.asprintf "%a"
    (Eta.Cause.pp (fun fmt (`Stt_error m) -> Format.pp_print_string fmt m))
    cause

let assert_absent label hay =
  Alcotest.(check bool) label false (string_contains hay marker)

let test_success_keeps_text_out_of_diagnostics () =
  List.iter
    (fun debug ->
      (match debug with
      | None -> ( try Unix.putenv "CAMELPIE_OPENROUTER_DEBUG" "" with _ -> ())
      | Some value -> Unix.putenv "CAMELPIE_OPENROUTER_DEBUG" value);
      let result, stdout, stderr =
        with_captured_stdio (fun () ->
            run_openrouter ~body:(completed_body marker) ~status:"200 OK" ())
      in
      (match result with
      | Eta.Exit.Ok text -> Alcotest.(check string) "delivered" marker text
      | Eta.Exit.Error cause ->
          Alcotest.failf "expected success: %s" (cause_text cause));
      assert_absent "stdout dump" stdout;
      assert_absent "stderr dump" stderr)
    [ None; Some "0"; Some "false"; Some "1" ]

let test_provider_error_omits_body () =
  Unix.putenv "CAMELPIE_OPENROUTER_DEBUG" "1";
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
    with_captured_stdio (fun () ->
        run_openrouter ~body ~status:"401 Unauthorized" ())
  in
  (match result with
  | Eta.Exit.Ok text -> Alcotest.failf "expected provider error, got %s" text
  | Eta.Exit.Error cause -> assert_absent "error cause" (cause_text cause));
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_malformed_response_omits_payload () =
  Unix.putenv "CAMELPIE_OPENROUTER_DEBUG" "1";
  let result, stdout, stderr =
    with_captured_stdio (fun () ->
        run_openrouter ~body:marker ~status:"200 OK" ())
  in
  (match result with
  | Eta.Exit.Ok text -> Alcotest.failf "expected malformed, got %s" text
  | Eta.Exit.Error cause -> assert_absent "error cause" (cause_text cause));
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_parse_event_omits_payload () =
  match Pie.Stt.Codex_realtime.parse_event ("not-json " ^ marker) with
  | Ok _ -> Alcotest.fail "expected parse error"
  | Error (`Stt_error text) -> assert_absent "json error" text

let test_helper_nonzero_omits_argv_and_stderr () =
  with_sandbox @@ fun dir ->
  let bin = Filename.concat dir "bin" in
  Unix.mkdir bin 0o700;
  let wtype = Filename.concat bin "wtype" in
  write_file wtype
    ("#!/bin/sh\necho " ^ marker ^ "\necho " ^ marker ^ " >&2\nexit 1\n");
  Unix.chmod wtype 0o700;
  let old_path = Option.value ~default:"/usr/bin" (Sys.getenv_opt "PATH") in
  Unix.putenv "PATH" (bin ^ ":" ^ old_path);
  let result, stdout, stderr =
    with_captured_stdio (fun () ->
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
          (Pie.Wayland.Wtype.type_text
             ~process_mgr:(Eio.Stdenv.process_mgr env)
             marker))
  in
  Unix.putenv "PATH" old_path;
  (match result with
  | Eta.Exit.Ok () -> Alcotest.fail "expected wtype failure"
  | Eta.Exit.Error cause ->
      let rendered =
        Format.asprintf "%a"
          (Eta.Cause.pp (fun fmt (err : [> `Wtype_error of string ]) ->
               match err with
               | `Wtype_error m -> Format.pp_print_string fmt m
               | _ -> Format.pp_print_string fmt "error"))
          cause
      in
      Alcotest.(check bool)
        "basename" true
        (string_contains rendered "wtype failed");
      assert_absent "helper error" rendered);
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_notification_failure_omits_message () =
  with_sandbox @@ fun dir ->
  let bin = Filename.concat dir "bin" in
  Unix.mkdir bin 0o700;
  let notify = Filename.concat bin "notify-send" in
  write_file notify
    ("#!/bin/sh\necho " ^ marker ^ "\necho " ^ marker ^ " >&2\nexit 1\n");
  Unix.chmod notify 0o700;
  let old_path = Option.value ~default:"/usr/bin" (Sys.getenv_opt "PATH") in
  Unix.putenv "PATH" (bin ^ ":" ^ old_path);
  let result, stdout, stderr =
    with_captured_stdio (fun () ->
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
          (Pie.Desktop.Notification.notify_send
             ~process_mgr:(Eio.Stdenv.process_mgr env)
             "pie" marker))
  in
  Unix.putenv "PATH" old_path;
  (match result with
  | Eta.Exit.Ok () -> Alcotest.fail "expected notify-send failure"
  | Eta.Exit.Error cause ->
      let rendered =
        Format.asprintf "%a"
          (Eta.Cause.pp (fun fmt (err : [> `Notification_error of string ]) ->
               match err with
               | `Notification_error m -> Format.pp_print_string fmt m
               | _ -> Format.pp_print_string fmt "error"))
          cause
      in
      Alcotest.(check bool)
        "basename" true
        (string_contains rendered "notify-send failed");
      assert_absent "notify error" rendered);
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

let test_auth_load_omits_json_snippet () =
  with_sandbox @@ fun dir ->
  let path = Filename.concat dir "auth.json" in
  write_file path ("{\"tokens\": \"" ^ marker ^ " not-json");
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
      (Pie.Stt.Codex_realtime.Codex_auth.load ~path ())
  in
  match result with
  | Eta.Exit.Ok _ -> Alcotest.fail "expected auth load failure"
  | Eta.Exit.Error cause -> assert_absent "auth json snippet" (cause_text cause)

let test_large_helper_stderr_discarded () =
  with_sandbox @@ fun dir ->
  let bin = Filename.concat dir "bin" in
  Unix.mkdir bin 0o700;
  let wtype = Filename.concat bin "wtype" in
  let blob = String.make 262144 'A' ^ marker in
  write_file wtype
    ("#!/bin/sh\nprintf '%s' " ^ Filename.quote blob ^ "\nprintf '%s' "
   ^ Filename.quote blob ^ " >&2\nexit 1\n");
  Unix.chmod wtype 0o700;
  let old_path = Option.value ~default:"/usr/bin" (Sys.getenv_opt "PATH") in
  Unix.putenv "PATH" (bin ^ ":" ^ old_path);
  let result, stdout, stderr =
    with_captured_stdio (fun () ->
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
          (Pie.Wayland.Wtype.type_text
             ~process_mgr:(Eio.Stdenv.process_mgr env)
             "ok"))
  in
  Unix.putenv "PATH" old_path;
  (match result with
  | Eta.Exit.Ok () -> Alcotest.fail "expected wtype failure"
  | Eta.Exit.Error cause ->
      let rendered =
        Format.asprintf "%a"
          (Eta.Cause.pp (fun fmt (err : [> `Wtype_error of string ]) ->
               match err with
               | `Wtype_error m -> Format.pp_print_string fmt m
               | _ -> Format.pp_print_string fmt "error"))
          cause
      in
      Alcotest.(check bool)
        "exit metadata" true
        (string_contains rendered "wtype failed");
      assert_absent "typed error" rendered);
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_composite_failure_is_not_flattened () =
  let bt = Printexc.get_raw_backtrace () in
  let multiple =
    Eio.Exn.Multiple
      [ (Failure ("cleanup " ^ marker), bt); (Failure "other", bt) ]
  in
  match
    try
      ignore (Pie.Sh.failure_message ~prog:"wtype" multiple);
      `Returned
    with
    | Eio.Exn.Multiple _ -> `Rethrown
    | exn -> `Other (Printexc.to_string exn)
  with
  | `Rethrown -> ()
  | `Returned -> Alcotest.fail "Multiple flattened to Subprocess_error"
  | `Other text -> Alcotest.failf "unexpected %s" text

let test_helper_success_stdout_discarded_delivery_file () =
  with_sandbox @@ fun dir ->
  let bin = Filename.concat dir "bin" in
  Unix.mkdir bin 0o700;
  let delivery = Filename.concat dir "delivery" in
  let wtype = Filename.concat bin "wtype" in
  let intended = "PIE019-INTENDED-DELIVERY" in
  write_file wtype
    ("#!/bin/sh\necho " ^ marker ^ "\necho " ^ marker
   ^ " >&2\nprintf '%s' \"$2\" > \"$PIE019_DELIVERY\"\nexit 0\n");
  Unix.chmod wtype 0o700;
  let old_path = Option.value ~default:"/usr/bin" (Sys.getenv_opt "PATH") in
  Unix.putenv "PATH" (bin ^ ":" ^ old_path);
  Unix.putenv "PIE019_DELIVERY" delivery;
  let result, stdout, stderr =
    with_captured_stdio (fun () ->
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
          (Pie.Wayland.Wtype.type_text
             ~process_mgr:(Eio.Stdenv.process_mgr env)
             intended))
  in
  Unix.putenv "PATH" old_path;
  (match result with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error _ -> Alcotest.fail "expected wtype success");
  let delivered = In_channel.with_open_text delivery In_channel.input_all in
  Alcotest.(check string) "delivery file" intended delivered;
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let test_nonexecutable_spawn_is_io_failure () =
  with_sandbox @@ fun dir ->
  let helper = Filename.concat dir "blocked-helper" in
  write_file helper "#!/bin/sh\necho should-not-run\n";
  Unix.chmod helper 0o600;
  let result, stdout, stderr =
    with_captured_stdio (fun () ->
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
          (Pie.Sh.run
             ~process_mgr:(Eio.Stdenv.process_mgr env)
             ~prog:helper ~args:[ "--"; marker ]))
  in
  (match result with
  | Eta.Exit.Ok () -> Alcotest.fail "expected spawn failure"
  | Eta.Exit.Error cause ->
      let rendered =
        Format.asprintf "%a"
          (Eta.Cause.pp (fun fmt (err : [> `Subprocess_error of string ]) ->
               match err with
               | `Subprocess_error m -> Format.pp_print_string fmt m
               | _ -> Format.pp_print_string fmt "error"))
          cause
      in
      Alcotest.(check bool)
        "basename" true
        (string_contains rendered "blocked-helper failed");
      Alcotest.(check bool)
        "io-failure" true
        (string_contains rendered "io-failure");
      assert_absent "spawn error" rendered);
  assert_absent "stdout" stdout;
  assert_absent "stderr" stderr

let () =
  Alcotest.run "diagnostic-content"
    [
      ( "provider",
        [
          Alcotest.test_case "success keeps marker out of diagnostics" `Quick
            test_success_keeps_text_out_of_diagnostics;
          Alcotest.test_case "provider error omits remote body" `Quick
            test_provider_error_omits_body;
          Alcotest.test_case "malformed response omits payload" `Quick
            test_malformed_response_omits_payload;
          Alcotest.test_case "parse_event omits payload" `Quick
            test_parse_event_omits_payload;
          Alcotest.test_case "server error omits remote code" `Quick
            test_server_error_omits_remote_code;
          Alcotest.test_case "auth load omits JSON snippet" `Quick
            test_auth_load_omits_json_snippet;
        ] );
      ( "helpers",
        [
          Alcotest.test_case "wtype success discards stdio and writes delivery"
            `Quick test_helper_success_stdout_discarded_delivery_file;
          Alcotest.test_case "wtype failure omits argv and stderr" `Quick
            test_helper_nonzero_omits_argv_and_stderr;
          Alcotest.test_case "notify-send failure omits message" `Quick
            test_notification_failure_omits_message;
          Alcotest.test_case "large helper stderr is discarded" `Quick
            test_large_helper_stderr_discarded;
          Alcotest.test_case "composite cancellation is not flattened" `Quick
            test_composite_failure_is_not_flattened;
          Alcotest.test_case "non-executable spawn is io-failure metadata"
            `Quick test_nonexecutable_spawn_is_io_failure;
        ] );
    ]
