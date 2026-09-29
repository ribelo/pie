(** HTTP and optional telemetry assembly for the Door. The Door owns signals,
    the logger, the root switch, and the final process exit. *)

let make_http ~sw ~net ~clock =
  Ox_http.Client.create ~sw ~net ~clock ~tls:(Ox_http.Client.tls ()) ()

let otel_enabled () =
  match Sys.getenv_opt "PIE_OTLP" with
  | None -> false
  | Some value -> (
      match String.lowercase_ascii (String.trim value) with
      | "1" | "true" | "yes" | "on" -> true
      | _ -> false)

let telemetry ~sw (env : _ Ox_app.Env.t) _config : Ox_app.telemetry =
  if not (otel_enabled ()) then { reporters = []; span_ids = None }
  else
    let endpoint =
      match Sys.getenv_opt "OTEL_EXPORTER_OTLP_ENDPOINT" with
      | Some url when String.trim url <> "" -> url
      | _ -> "http://localhost:4318"
    in
    let http_client = make_http ~sw ~net:env.base#net ~clock:env.base#clock in
    let on_error error =
      Format.eprintf "Pie OTLP export failed: %a@." Ox_otel.pp_error error
    in
    match
      Ox_otel.create ~sw ~clock:env.base#clock ~http_client ~endpoint
        ~service_name:"pie"
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
