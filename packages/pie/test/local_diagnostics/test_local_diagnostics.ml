(* Local diagnostics (^pienext-ipne, ^pienext-gaq3): the Door classifies exits,
   writes one structured record per event to the local channel, and optional
   OTLP export neither duplicates nor replaces that channel. The suite owns a
   private HOME/XDG tree and a loopback fixture collector only: no provider,
   control socket, microphone or installed executable. *)

let putenv ~(caps : < Ox_caps.Cap.env ; .. >) =
  Ox_caps.Cap_unix.putenv ~caps [@alert "-unsafe_multidomain"]

let contains hay needle =
  let rec loop i =
    i + String.length needle <= String.length hay
    && (String.sub hay i (String.length needle) = needle || loop (i + 1))
  in
  needle = "" || loop 0

let count_needle hay needle =
  let rec loop i n =
    if i + String.length needle > String.length hay then n
    else if String.sub hay i (String.length needle) = needle then
      loop (i + 1) (n + 1)
    else loop (i + 1) n
  in
  loop 0 0

let with_captured_stderr
    ~(caps :
       < Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.tmp
       ; .. >) f =
  let tmp =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-local-diag" ".log"
  in
  let oc = Ox_caps.Cap_stdlib.open_out ~caps tmp in
  let old = Unix.dup Unix.stderr in
  Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stderr;
  Fun.protect
    ~finally:(fun () ->
      flush (Ox_caps.Cap_stdlib.stderr ~caps);
      Unix.dup2 old Unix.stderr;
      Unix.close old;
      close_out_noerr oc)
    (fun () ->
      let result = f () in
      flush (Ox_caps.Cap_stdlib.stderr ~caps);
      close_out_noerr oc;
      let body =
        Ox_caps.Cap_stdlib.In_channel.with_open_text ~caps tmp
          In_channel.input_all
      in
      (result, body))

let with_env ~(caps : < Ox_caps.Cap.env ; .. >) bindings f =
  let previous =
    List.map
      (fun (key, _) -> (key, Ox_caps.Cap_sys.getenv_opt ~caps key))
      bindings
  in
  List.iter (fun (key, value) -> putenv ~caps key value) bindings;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (key, old) ->
          putenv ~caps key (match old with None -> "" | Some value -> value))
        previous)
    f

let with_private_runtime
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) f =
  let dir =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-local-diag-home" ""
  in
  Ox_caps.Cap_sys.remove ~caps dir;
  Ox_caps.Cap_unix.mkdir ~caps dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name ->
          try Ox_caps.Cap_sys.remove ~caps (Filename.concat dir name)
          with Sys_error _ -> ())
        (try Ox_caps.Cap_sys.readdir ~caps dir with Sys_error _ -> [||]);
      try Ox_caps.Cap_unix.rmdir ~caps dir with Unix.Unix_error _ -> ())
    (fun () ->
      with_env ~caps
        [
          ("HOME", dir);
          ("XDG_RUNTIME_DIR", dir);
          ("XDG_CONFIG_HOME", dir);
          ("XDG_DATA_HOME", dir);
        ]
        (fun () -> f dir))

(* The suite exercises the Door, so its configuration is the empty document with
   no fields: no config file is written or read. *)
type settings = unit

let settings_of_yojson (_ : Yojson.Safe.t) : (settings, string) result = Ok ()
let yojson_of_settings () : Yojson.Safe.t = `Assoc []

let door_config : settings Ox_app.Config.t =
  {
    of_yojson = settings_of_yojson;
    yojson_of_t = yojson_of_settings;
    env = (fun ~getenv:_ () -> ());
  }

let pp_err formatter = function
  | `Stt_error message -> Format.pp_print_string formatter message

(* One Door invocation. The journal goes to both a memory store (typed records)
   and the caller's stderr buffer, which is where production writes it. *)
let run_door ~env ?on_switch ?telemetry program =
  let errors = Buffer.create 256 in
  let memory = Ox_observability.Logger.Memory.create () in
  let code =
    Ox_app.run_for_test ?on_switch ?telemetry ~env ~name:"pie"
      ~config:door_config ~pp_err
      ~journal:
        (Ox_observability.Logger.Reporter.tee
           [
             Ox_observability.Logger.Memory.reporter memory;
             Ox_observability.Logger.Reporter.writer (fun line ->
                 Buffer.add_string errors line);
           ])
      ~getenv:(fun _ -> None)
      ~read:(fun _ -> Ok "")
      ~stderr:(fun text -> Buffer.add_string errors text)
      ~stdout:ignore ~argv:[| "pie" |] program
  in
  (code, Buffer.contents errors, Ox_observability.Logger.Memory.records memory)

let read_http_body flow =
  let reader = Eio.Buf_read.of_flow ~max_size:4_000_000 flow in
  let rec headers acc =
    match Eio.Buf_read.line reader with
    | line ->
        if String.trim line = "" then List.rev acc else headers (line :: acc)
    | exception End_of_file -> List.rev acc
  in
  let head = headers [] in
  let content_length =
    List.find_map
      (fun line ->
        let lower = String.lowercase_ascii line in
        if String.starts_with ~prefix:"content-length:" lower then
          int_of_string_opt
            (String.trim (String.sub line 15 (String.length line - 15)))
        else None)
      head
  in
  match content_length with
  | Some n when n > 0 -> Eio.Buf_read.take n reader
  | _ -> ""

(* A private loopback collector: what the exporter posts, recorded verbatim. *)
let with_collector f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:8 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "expected TCP listener"
  in
  let bodies = ref [] in
  (* A daemon: the switch cancels it when the Door is done, so a collector still
     parked in accept cannot hold the release open. *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let rec accept_loop () =
        match Eio.Net.accept ~sw socket with
        | exception Eio.Io _ -> ()
        | flow, _ ->
            Fun.protect
              ~finally:(fun () -> try Eio.Flow.close flow with _ -> ())
              (fun () ->
                (try bodies := read_http_body flow :: !bodies with _ -> ());
                Eio.Flow.copy_string
                  "HTTP/1.1 200 OK\r\n\
                   Content-Length: 0\r\n\
                   Connection: close\r\n\
                   \r\n"
                  flow);
            accept_loop ()
      in
      accept_loop ();
      `Stop_daemon);
  (* The switch owns the listener; closing it here would wait on the in-flight
     accept the collector is parked in. *)
  f ~env ~port ~bodies

let log_info env message =
  Ox_observability.Logger.info env.Ox_app.Env.logger message

let test_success_exit
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_private_runtime ~caps @@ fun _ ->
  Eio_main.run @@ fun env ->
  let code, _, _ = run_door ~env (fun _env _settings _sw -> Ok ()) in
  Alcotest.(check int) "success" 0 code

let test_typed_failure_exit
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_private_runtime ~caps @@ fun _ ->
  Eio_main.run @@ fun env ->
  let code, stderr, _ =
    run_door ~env (fun _env _settings _sw -> Error (`Stt_error "typed boom"))
  in
  Alcotest.(check int) "failure" 1 code;
  Alcotest.(check bool)
    "renders typed failure" true
    (contains stderr "typed boom")

(* The Door's signal stop: an interruption it requested itself is a clean stop,
   and the root switch's finalizers have run before it returns. *)
let test_requested_stop_exit
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_private_runtime ~caps @@ fun _ ->
  Eio_main.run @@ fun env ->
  let id = Ox.Interrupt_id.create () in
  let released = ref false in
  let code, _, _ =
    run_door ~env
      ~on_switch:(fun sw ->
        Eio.Switch.on_release sw (fun () -> released := true);
        Ox_app.request_stop ~sw id)
      (fun _env _settings _sw -> Eio.Fiber.await_cancel ())
  in
  Alcotest.(check int) "requested stop is a clean stop" 0 code;
  Alcotest.(check bool) "root finalizers ran before return" true !released

(* An interruption the Door did not request is not a clean stop and not a typed
   failure: it is classified as its own nonzero exit. (The pre-migration Pie
   runner mapped an interrupt to 130; ox_app's Door owns this mapping.)
   ^napp-f7d6 *)
let test_anonymous_interruption_exit
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_private_runtime ~caps @@ fun _ ->
  Eio_main.run @@ fun env ->
  let code, stderr, _ =
    run_door ~env (fun _env _settings sw ->
        Eio.Switch.fail sw (Eio.Cancel.Cancelled (Failure "anonymous stop"));
        Eio.Fiber.await_cancel ())
  in
  Alcotest.(check int) "anonymous interruption is classified" 2 code;
  Alcotest.(check bool)
    "interruption rendered" true
    (String.length (String.trim stderr) > 0)

let test_default_export_off_stderr_once
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_private_runtime ~caps @@ fun _ ->
  with_collector @@ fun ~env ~port ~bodies ->
  with_env ~caps
    [
      ("PIE_OTLP", "");
      ("OTEL_EXPORTER_OTLP_ENDPOINT", Printf.sprintf "http://127.0.0.1:%d" port);
    ]
  @@ fun () ->
  let code, stderr, records =
    run_door ~env (fun env _settings _sw ->
        log_info env "process door ready";
        Ok ())
  in
  Alcotest.(check int) "success" 0 code;
  Alcotest.(check int)
    "stderr exactly once" 1
    (count_needle stderr "process door ready");
  Alcotest.(check int) "one record" 1 (List.length records);
  (match records with
  | [ record ] ->
      Alcotest.(check string) "record message" "process door ready" record.msg;
      Alcotest.(check bool)
        "record level" true
        (record.level = Ox_observability.Logger.Info)
  | _ -> Alcotest.fail "expected exactly one record");
  Alcotest.(check int) "no exporter contact" 0 (List.length !bodies)

let test_filtering_retained
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_private_runtime ~caps @@ fun _ ->
  Eio_main.run @@ fun env ->
  let code, stderr, records =
    run_door ~env (fun env _settings _sw ->
        Ox_observability.Logger.debug env.Ox_app.Env.logger
          "filtered-debug-line";
        log_info env "visible-info-line";
        Ok ())
  in
  Alcotest.(check int) "success" 0 code;
  Alcotest.(check int)
    "debug filtered" 0
    (count_needle stderr "filtered-debug-line");
  Alcotest.(check int) "info once" 1 (count_needle stderr "visible-info-line");
  Alcotest.(check int) "only the info record" 1 (List.length records)

let test_redacted_field_stays_out_of_stderr
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_private_runtime ~caps @@ fun _ ->
  Eio_main.run @@ fun env ->
  let secret = "local-diagnostics-secret" in
  let code, stderr, records =
    run_door ~env (fun env _settings _sw ->
        Ox_observability.Logger.info
          ~fields:
            [
              ( "credential",
                Ox_observability.Logger.Redacted
                  (Ox_observability.Redact.make secret) );
            ]
          env.Ox_app.Env.logger "redaction-line";
        Ok ())
  in
  Alcotest.(check int) "success" 0 code;
  Alcotest.(check int) "record once" 1 (count_needle stderr "redaction-line");
  Alcotest.(check bool)
    "secret absent from stderr" false (contains stderr secret);
  match records with
  | [ record ] -> (
      match List.assoc_opt "credential" record.fields with
      | Some value ->
          Alcotest.(check string)
            "redacted field rendering" "<redacted>"
            (Format.asprintf "%a" Ox_observability.Logger.pp_value value)
      | None -> Alcotest.fail "credential field missing from the record")
  | _ -> Alcotest.fail "expected exactly one record"

let test_enabled_export_private_collector
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_private_runtime ~caps @@ fun _ ->
  with_collector @@ fun ~env ~port ~bodies ->
  with_env ~caps
    [
      ("PIE_OTLP", "true");
      ("OTEL_EXPORTER_OTLP_ENDPOINT", Printf.sprintf "http://127.0.0.1:%d" port);
    ]
  @@ fun () ->
  let code, stderr, _ =
    run_door ~env
      ~telemetry:(fun ~sw env _settings -> Pie.Process.telemetry ~sw env ())
      (fun env _settings _sw ->
        Ox_observability.Logger.info
          ~fields:[ ("diag", Ox_observability.Logger.String "meta") ]
          env.Ox_app.Env.logger "export-local-once";
        Ok ())
  in
  Alcotest.(check int) "success" 0 code;
  Alcotest.(check int)
    "local once not duplicated" 1
    (count_needle stderr "export-local-once");
  let posted = String.concat "\n" !bodies in
  Alcotest.(check bool)
    "collector received the record" true
    (List.length !bodies > 0 && contains posted "export-local-once")

(* An enabled exporter that cannot reach its collector leaves the local channel
   intact and its own failure visible. ^pienext-gaq3 *)
let test_failing_export_keeps_local_diagnostics
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_private_runtime ~caps @@ fun _ ->
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
  (* Nothing listens here any more, so every export is refused. *)
  Eio.Net.close socket;
  with_env ~caps
    [
      ("PIE_OTLP", "true");
      ("OTEL_EXPORTER_OTLP_ENDPOINT", Printf.sprintf "http://127.0.0.1:%d" port);
    ]
  @@ fun () ->
  let (code, stderr, _), exporter_stderr =
    with_captured_stderr ~caps (fun () ->
        run_door ~env
          ~telemetry:(fun ~sw env _settings -> Pie.Process.telemetry ~sw env ())
          (fun env _settings _sw ->
            log_info env "export-failing-once";
            Ok ()))
  in
  Alcotest.(check int) "success" 0 code;
  Alcotest.(check int)
    "local once" 1
    (count_needle stderr "export-failing-once");
  Alcotest.(check bool)
    "exporter failure reported" true
    (contains exporter_stderr "Pie OTLP export failed")

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "local_diagnostics"
    [
      ( "process_door",
        [
          Alcotest.test_case "success exit 0" `Quick (test_success_exit ~caps);
          Alcotest.test_case "typed failure exit 1" `Quick
            (test_typed_failure_exit ~caps);
          Alcotest.test_case "requested stop is a clean stop" `Quick
            (test_requested_stop_exit ~caps);
          Alcotest.test_case "anonymous interruption is classified" `Quick
            (test_anonymous_interruption_exit ~caps);
          Alcotest.test_case "export off by default, stderr once" `Quick
            (test_default_export_off_stderr_once ~caps);
          Alcotest.test_case "filtering retained" `Quick
            (test_filtering_retained ~caps);
          Alcotest.test_case "redacted field stays out of stderr" `Quick
            (test_redacted_field_stays_out_of_stderr ~caps);
          Alcotest.test_case "enabled export private collector" `Quick
            (test_enabled_export_private_collector ~caps);
          Alcotest.test_case "failing export keeps local diagnostics" `Quick
            (test_failing_export_keeps_local_diagnostics ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
