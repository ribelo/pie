(** HTTP and optional telemetry assembly for the Door. The Door owns signals,
    the logger, the root switch, and the final process exit. *)

let make_http ~sw ~net ~clock =
  Ox_http.Client.create ~sw ~net ~clock ~tls:(Ox_http.Client.tls ()) ()

let otel_enabled ~caps () =
  match Ox_caps.Cap_sys.getenv_opt ~caps "PIE_OTLP" with
  | None -> false
  | Some value -> (
      match String.lowercase_ascii (String.trim value) with
      | "1" | "true" | "yes" | "on" -> true
      | _ -> false)

let telemetry ~sw (env : _ Ox_app.Env.t) _config : Ox_app.telemetry =
  let caps = env.caps in
  if not (otel_enabled ~caps ()) then { reporters = []; span_ids = None }
  else
    let endpoint =
      match Ox_caps.Cap_sys.getenv_opt ~caps "OTEL_EXPORTER_OTLP_ENDPOINT" with
      | Some url when String.trim url <> "" -> url
      | _ -> "http://localhost:4318"
    in
    let http_client = make_http ~sw ~net:env.base#net ~clock:env.base#clock in
    let on_error error =
      Format.fprintf
        (Ox_caps.Cap_stdlib.err_formatter ~caps)
        "Pie OTLP export failed: %a@." Ox_otel.pp_error error
    in
    match
      Ox_otel.create ~caps ~sw ~clock:env.base#clock
        ~mono_clock:env.base#mono_clock ~http_client ~endpoint
        ~service_name:"pie" ~ending_budget:(Ox.Duration.seconds 5)
        ~resource:[ ("service.version", "0.1.0") ]
        ~on_error ()
    with
    | Ok exporter ->
        {
          reporters = [ Ox_otel.reporter exporter ];
          span_ids = Some (Ox_otel.span_ids exporter);
        }
    | Error error ->
        failwith
          (Format.asprintf "Pie OTLP startup failed: %a" Ox_otel.pp_error error)
