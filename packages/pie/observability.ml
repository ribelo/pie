let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.0)

type otlp_state = {
  mutex : Mutex.t;
  condition : Condition.t;
  mutable ready : (Eta.Capabilities.logger, string) result option;
  mutable stop : bool;
}

type otlp_sink = {
  state : otlp_state;
  thread : Thread.t;
  logger : Eta.Capabilities.logger;
}

let otlp_sink : otlp_sink option ref = ref None
let otlp_mutex = Mutex.create ()

let env_truthy = function
  | "1" | "true" | "TRUE" | "yes" | "YES" | "on" | "ON" -> true
  | _ -> false

let env_enabled () =
  match Sys.getenv_opt "PIE_OTLP" with
  | Some value -> env_truthy value
  | None -> false

let env_string name default =
  match Sys.getenv_opt name with
  | Some value when String.trim value <> "" -> value
  | _ -> default

let env_int name default =
  match Sys.getenv_opt name with
  | Some value -> Option.value (int_of_string_opt value) ~default
  | None -> default

let set_ready state value =
  Mutex.lock state.mutex;
  state.ready <- Some value;
  Condition.broadcast state.condition;
  Mutex.unlock state.mutex

let should_stop state =
  Mutex.lock state.mutex;
  let stop = state.stop in
  Mutex.unlock state.mutex;
  stop

let otlp_thread state ~host ~port ~service_version =
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let clock = Eio.Stdenv.clock env in
    let net = Eio.Stdenv.net env in
    let runtime_factory tracer = Eta_eio.Runtime.create ~sw ~clock ~tracer () in
    let http_client = Eta_http_eio.Client.make_h1 ~sw ~net () in
    let exporter =
      Eta_otel.create ~runtime_factory ~http_client ~clock:(Eta_eio.clock clock)
        ~host ~port ~service_name:"pie" ~service_version ~scope_name:"pie"
        ~disable_self_metrics:true
        ~on_error:(fun msg -> Printf.eprintf "pie otlp: %s\n%!" msg)
        ()
    in
    set_ready state (Ok (Eta_otel.logger exporter));
    while not (should_stop state) do
      Eio.Time.sleep clock 0.1
    done;
    Eta_otel.shutdown ~timeout_s:0.5 exporter
  with exn ->
    set_ready state (Error (Printexc.to_string exn))

let wait_ready state =
  Mutex.lock state.mutex;
  while Option.is_none state.ready do
    Condition.wait state.condition state.mutex
  done;
  let ready = Option.get state.ready in
  Mutex.unlock state.mutex;
  ready

let init_from_env ~service_version () =
  if env_enabled () then (
    Mutex.lock otlp_mutex;
    let already_started = Option.is_some !otlp_sink in
    if not already_started then (
      let state =
        {
          mutex = Mutex.create ();
          condition = Condition.create ();
          ready = None;
          stop = false;
        }
      in
      let host = env_string "PIE_OTLP_HOST" "127.0.0.1" in
      let port = env_int "PIE_OTLP_PORT" 4318 in
      let thread =
        Thread.create
          (fun () -> otlp_thread state ~host ~port ~service_version)
          ()
      in
      match wait_ready state with
      | Ok logger ->
          otlp_sink := Some { state; thread; logger };
          Mutex.unlock otlp_mutex
      | Error msg ->
          Mutex.unlock otlp_mutex;
          Printf.eprintf "pie otlp init failed: %s\n%!" msg)
    else Mutex.unlock otlp_mutex)

let shutdown () =
  Mutex.lock otlp_mutex;
  let sink = !otlp_sink in
  otlp_sink := None;
  Mutex.unlock otlp_mutex;
  match sink with
  | None -> ()
  | Some sink ->
      Mutex.lock sink.state.mutex;
      sink.state.stop <- true;
      Condition.broadcast sink.state.condition;
      Mutex.unlock sink.state.mutex;
      Thread.join sink.thread

let log_to_otlp record =
  Mutex.lock otlp_mutex;
  let sink = !otlp_sink in
  Mutex.unlock otlp_mutex;
  match sink with
  | None -> ()
  | Some sink -> (
      try sink.logger#log record with _ -> ())

let emit level ?(attrs = []) body =
  let record : Eta_observability.Logger.record =
    {
      level;
      body;
      ts_ms = now_ms ();
      attrs = ("component", "pie") :: attrs;
      trace_id = "";
      span_id = "";
    }
  in
  let line =
    try Eta_observability.Logger.format_logfmt record
    with Invalid_argument _ ->
      "level=error body=\"invalid log attributes\" component=pie"
  in
  Printf.eprintf "%s\n%!" line;
  log_to_otlp record

let debug ?attrs message = emit Debug ?attrs message
let info ?attrs message = emit Info ?attrs message
let warn ?attrs message = emit Warn ?attrs message
let error ?attrs message = emit Error ?attrs message
