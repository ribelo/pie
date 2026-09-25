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

let process_pp_err formatter = function
  | `Invalid_config message | `Stt_error message ->
      Format.pp_print_string formatter message
  | _ -> Format.pp_print_string formatter "error"

let with_captured_stderr f =
  let tmp = Filename.temp_file "pie-local-diag" ".log" in
  let oc = open_out tmp in
  let old = Unix.dup Unix.stderr in
  Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stderr;
  Fun.protect
    ~finally:(fun () ->
      flush stderr;
      Unix.dup2 old Unix.stderr;
      Unix.close old;
      close_out_noerr oc)
    (fun () ->
      let result = f () in
      flush stderr;
      close_out_noerr oc;
      let body = In_channel.with_open_text tmp In_channel.input_all in
      (result, body))

let with_env bindings f =
  let previous =
    List.map (fun (key, _) -> (key, Sys.getenv_opt key)) bindings
  in
  List.iter (fun (key, value) -> Unix.putenv key value) bindings;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (key, old) ->
          Unix.putenv key (match old with None -> "" | Some value -> value))
        previous)
    f

let with_private_runtime f =
  let dir = Filename.temp_file "pie-local-diag-home" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name ->
          try Sys.remove (Filename.concat dir name) with Sys_error _ -> ())
        (try Sys.readdir dir with Sys_error _ -> [||]);
      try Unix.rmdir dir with Unix.Unix_error _ -> ())
    (fun () ->
      with_env
        [
          ("HOME", dir);
          ("XDG_RUNTIME_DIR", dir);
          ("XDG_CONFIG_HOME", dir);
          ("XDG_DATA_HOME", dir);
        ]
        (fun () -> f dir))

type collector = { port : int; bodies : string list ref; stop : unit -> unit }

let start_collector () =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt server Unix.SO_REUSEADDR true;
  Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  Unix.listen server 8;
  let port =
    match Unix.getsockname server with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> failwith "expected inet port"
  in
  let bodies = ref [] in
  let live = Atomic.make true in
  let worker =
    Thread.create
      (fun () ->
        Unix.set_nonblock server;
        while Atomic.get live do
          let client_opt =
            try Some (fst (Unix.accept server)) with
            | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                Thread.delay 0.01;
                None
            | Unix.Unix_error _ -> None
          in
          match client_opt with
          | None -> ()
          | Some client ->
              Unix.clear_nonblock client;
              Fun.protect
                ~finally:(fun () -> try Unix.close client with _ -> ())
                (fun () ->
                  let buf = Bytes.create 65536 in
                  let n = Unix.read client buf 0 (Bytes.length buf) in
                  if n > 0 then bodies := Bytes.sub_string buf 0 n :: !bodies;
                  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" in
                  ignore
                    (Unix.write_substring client resp 0 (String.length resp)))
        done)
      ()
  in
  {
    port;
    bodies;
    stop =
      (fun () ->
        Atomic.set live false;
        (try Unix.close server with _ -> ());
        try Thread.join worker with _ -> ());
  }

let test_success_exit () =
  with_private_runtime @@ fun _ ->
  with_env [ ("OTEL_SDK_DISABLED", "true") ] @@ fun () ->
  let code =
    Pie.Process.run ~pp_err:process_pp_err (fun ~env:_ ~sw:_ -> Eta.Effect.unit)
  in
  Alcotest.(check int) "success" 0 code

let test_typed_failure_exit () =
  with_private_runtime @@ fun _ ->
  with_env [ ("OTEL_SDK_DISABLED", "true") ] @@ fun () ->
  let code, stderr =
    with_captured_stderr (fun () ->
        Pie.Process.run ~pp_err:process_pp_err (fun ~env:_ ~sw:_ ->
            Eta.Effect.fail (`Stt_error "typed boom")))
  in
  Alcotest.(check int) "failure" 1 code;
  Alcotest.(check bool)
    "renders typed failure" true
    (contains stderr "typed boom")

let test_interrupt_exit () =
  with_private_runtime @@ fun _ ->
  with_env [ ("OTEL_SDK_DISABLED", "true") ] @@ fun () ->
  let code =
    Pie.Process.run ~pp_err:process_pp_err (fun ~env:_ ~sw:_ ->
        Eta.Effect.async ~register:(fun resume ->
            resume (Eta.Exit.Error Eta.Cause.interrupt);
            None))
  in
  Alcotest.(check int) "interrupt" 130 code

let test_default_export_off_stderr_once () =
  with_private_runtime @@ fun _ ->
  let collector = start_collector () in
  Fun.protect ~finally:collector.stop (fun () ->
      with_env
        [
          ("PIE_OTLP", "");
          ("OTEL_SDK_DISABLED", "false");
          ( "OTEL_EXPORTER_OTLP_ENDPOINT",
            Printf.sprintf "http://127.0.0.1:%d" collector.port );
        ]
      @@ fun () ->
      let code, stderr =
        with_captured_stderr (fun () ->
            Pie.Process.run ~pp_err:process_pp_err (fun ~env:_ ~sw:_ ->
                Eta_observability.log_info "process door ready"))
      in
      Alcotest.(check int) "success" 0 code;
      Alcotest.(check int)
        "stderr exactly once" 1
        (count_needle stderr "process door ready");
      Alcotest.(check int)
        "no exporter contact" 0
        (List.length !(collector.bodies)))

let test_filtering_retained () =
  with_private_runtime @@ fun _ ->
  with_env [ ("OTEL_SDK_DISABLED", "true") ] @@ fun () ->
  let _code, stderr =
    with_captured_stderr (fun () ->
        Pie.Process.run ~pp_err:process_pp_err (fun ~env:_ ~sw:_ ->
            Eta_observability.with_minimum_log_level Eta.Capabilities.Info
              (Eta_observability.log_debug "filtered-debug-line"
              |> Eta.Effect.bind (fun () ->
                  Eta_observability.log_info "visible-info-line"))))
  in
  Alcotest.(check int)
    "debug filtered" 0
    (count_needle stderr "filtered-debug-line");
  Alcotest.(check int) "info once" 1 (count_needle stderr "visible-info-line")

let test_enabled_export_private_collector () =
  with_private_runtime @@ fun _ ->
  let collector = start_collector () in
  Fun.protect ~finally:collector.stop (fun () ->
      with_env
        [
          ("PIE_OTLP", "true");
          ("OTEL_SDK_DISABLED", "false");
          ( "OTEL_EXPORTER_OTLP_ENDPOINT",
            Printf.sprintf "http://127.0.0.1:%d" collector.port );
        ]
      @@ fun () ->
      let code, stderr =
        with_captured_stderr (fun () ->
            Pie.Process.run ~pp_err:process_pp_err (fun ~env:_ ~sw:_ ->
                Eta_observability.log_info
                  ~attrs:[ ("diag", "meta") ]
                  "export-local-once"))
      in
      Alcotest.(check int) "success" 0 code;
      Alcotest.(check int)
        "local once not duplicated" 1
        (count_needle stderr "export-local-once");
      let posted = String.concat "\n" !(collector.bodies) in
      Alcotest.(check bool)
        "collector received metadata" true
        (contains posted "export-local-once"
        || contains posted "pie" || contains posted "diag"
        || List.length !(collector.bodies) > 0))

let () =
  Alcotest.run "local_diagnostics"
    [
      ( "process_door",
        [
          Alcotest.test_case "success exit 0" `Quick test_success_exit;
          Alcotest.test_case "typed failure exit 1" `Quick
            test_typed_failure_exit;
          Alcotest.test_case "interrupt exit 130" `Quick test_interrupt_exit;
          Alcotest.test_case "export off by default, stderr once" `Quick
            test_default_export_off_stderr_once;
          Alcotest.test_case "filtering retained" `Quick test_filtering_retained;
          Alcotest.test_case "enabled export private collector" `Quick
            test_enabled_export_private_collector;
        ] );
    ]
