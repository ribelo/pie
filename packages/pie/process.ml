(** Process door: OTEL scope, stderr logfmt tee, and exit mapping.
    
    [Eta_eio.run_main] does not expose the Eio environment or accept HTTP
    runtime services. [Eta_otel_eio.with_otlp] owns the one runtime, installs
    OTEL and the HTTP service, and this module maps the resulting exit with
    the same public [Exit] verbs [run_main] uses. *)

let stderr_logger : Eta.Capabilities.logger =
  object
    method log record =
      try Printf.eprintf "%s\n%!" (Eta_observability.Logger.format_logfmt record)
      with Invalid_argument _ ->
        Printf.eprintf "level=error body=\"invalid log attributes\" component=pie\n%!"
  end

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

let with_stderr_tee effect =
  Eta_observability.intercept_log
    (fun record ->
      stderr_logger#log record;
      Eta_observability.Keep)
    effect

let run ~pp_err mk_eff =
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let clock = Eio.Stdenv.clock env in
    let net = Eio.Stdenv.net env in
    let exit =
      Eta_otel_eio.with_otlp ~service_name:"pie"
        ~attrs:[ ("service.version", "0.1.0") ]
        ~sw ~net ~clock
        (fun () ->
          mk_eff ~env ~sw
          |> Eta_observability.annotate_logs [ ("component", "pie") ]
          |> with_stderr_tee)
    in
    match exit with
    | Ok _ -> 0
    | Error cause when Eta.Cause.is_interrupt_only cause -> 130
    | Error _ as error ->
      (match Eta.Exit.render_error ~pp_err error with
       | Some message -> Printf.eprintf "%s\n%!" message
       | None -> ());
      Eta.Exit.process_code error ~ok:(fun _ -> 0)
  with Eio.Cancel.Cancelled _ -> 130
