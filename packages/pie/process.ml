(** Process door: OTEL scope, stderr logfmt tee, HTTP client, and exit mapping.

    [Eta_eio.run_main] does not expose the Eio environment. This module uses the
    same public [Exit] verbs and signal-to-interrupt mapping as [run_main], and
    installs HTTP independently of whether OTEL export is enabled. *)

exception Process_interrupt

let stderr_logger : Eta.Capabilities.logger =
  object
    method log record =
      try
        Printf.eprintf "%s\n%!" (Eta_observability.Logger.format_logfmt record)
      with Invalid_argument _ ->
        Printf.eprintf
          "level=error body=\"invalid log attributes\" component=pie\n%!"
  end

let process_http_client : Eta_http.Client.t option ref = ref None

let http_client ~sw ~net =
  match !process_http_client with
  | Some client -> client
  | None -> Eta_http_eio.Client.make_h1 ~sw ~net ()

let emit_sync ?(attrs = []) ~level body =
  let record : Eta_observability.Logger.record =
    {
      level;
      body;
      ts_ms = int_of_float (Unix.gettimeofday () *. 1000.0);
      attrs = ("component", "pie") :: attrs;
      trace_id = "";
      span_id = "";
    }
  in
  stderr_logger#log record

let with_stderr_tee computation =
  Eta_observability.intercept_log
    (fun record ->
      stderr_logger#log record;
      Eta_observability.Keep)
    computation

let install_signal_interrupt ~sw ~clock =
  let fired = Atomic.make false in
  let live = Atomic.make true in
  let fire _ = Atomic.set fired true in
  let previous_int = Sys.Safe.signal Sys.sigint (Sys.Signal_handle fire) in
  let previous_term = Sys.Safe.signal Sys.sigterm (Sys.Signal_handle fire) in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      while (not (Atomic.get fired)) && Atomic.get live do
        Eio.Time.sleep clock 0.05
      done;
      if Atomic.get live && Atomic.get fired then
        Eio.Switch.fail sw Process_interrupt;
      `Stop_daemon);
  fun () ->
    Atomic.set live false;
    Atomic.set fired true;
    Sys.Safe.set_signal Sys.sigint previous_int;
    Sys.Safe.set_signal Sys.sigterm previous_term

let run ~pp_err mk_eff =
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let clock = Eio.Stdenv.clock env in
    let net = Eio.Stdenv.net env in
    let restore = install_signal_interrupt ~sw ~clock in
    Fun.protect ~finally:restore @@ fun () ->
    let client = Eta_http_eio.Client.make_h1 ~sw ~net () in
    process_http_client := Some client;
    Fun.protect
      ~finally:(fun () -> process_http_client := None)
      (fun () ->
        let body () =
          mk_eff ~env ~sw
          |> Eta.Effect.map_error (fun err -> `App err)
          |> Eta_observability.annotate_logs [ ("component", "pie") ]
          |> with_stderr_tee
        in
        let pp fmt = function
          | `Invalid_config message -> Format.pp_print_string fmt message
          | `App err -> pp_err fmt err
        in
        let exit =
          Eta_otel_eio.with_otlp ~service_name:"pie"
            ~attrs:[ ("service.version", "0.1.0") ]
            ~sw ~net ~clock body
        in
        match exit with
        | Ok _ -> 0
        | Error cause when Eta.Cause.is_interrupt_only cause -> 130
        | Error _ as error ->
            (match Eta.Exit.render_error ~pp_err:pp error with
            | Some message -> Printf.eprintf "%s\n%!" message
            | None -> ());
            Eta.Exit.process_code error ~ok:(fun _ -> 0))
  with
  | Process_interrupt -> 130
  | Eio.Cancel.Cancelled _ -> 130
