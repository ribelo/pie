let mkdir_p = Fs.mkdir_p
let max_handlers = 16
let listen_backlog = 16

type budget = { peer_timeout_s : float }

let default_budget = { peer_timeout_s = 1.0 }

type error = Eta_ipc.Listener.error

let err_endpoint_conflict = "endpoint-conflict"
let err_unsafe_runtime_dir = "unsafe-runtime-dir"
let err_inaccessible_runtime_dir = "inaccessible-runtime-dir"
let err_active_listener = "active-listener"
let err_stale_probe_failed = "stale-probe-failed"
let err_lock_conflict = "lock-conflict"
let err_lock_identity = "lock-identity"
let err_malformed_request = "malformed-request"

let snapshot_of_coordinator coordinator =
  let snap = Recording.Coordinator.snapshot coordinator in
  {
    Contract.phase = snap.phase;
    active = snap.active;
    mode = snap.mode;
    enabled = snap.enabled;
    started_at = snap.started_at;
    updated_at = Some snap.updated_at;
    transcript_path = snap.transcript_path;
    last_error = snap.last_error;
  }

let rejected_action coordinator action message =
  Contract.Action_resp
    {
      Contract.action;
      accepted = false;
      message = Some message;
      snapshot = snapshot_of_coordinator coordinator;
    }

let handle_request ?action_handler coordinator = function
  | Contract.Status ->
      Contract.Status_resp (snapshot_of_coordinator coordinator)
  | Contract.Pause ->
      Recording.Coordinator.set_enabled coordinator false;
      Contract.Pause_resp
  | Contract.Resume ->
      Recording.Coordinator.set_enabled coordinator true;
      Contract.Resume_resp
  | Contract.Toggle ->
      Contract.Toggle_resp (Recording.Coordinator.toggle_enabled coordinator)
  | Contract.Action action -> (
      match action_handler with
      | Some handler -> Contract.Action_resp (handler action)
      | None ->
          rejected_action coordinator action "daemon actions are unavailable")

let start ~sw ~fs ~net ~clock ?(socket_path = Paths.daemon_socket_path)
    ?(budget = default_budget) ?action_handler coordinator =
  let policy =
    {
      Eta_ipc.Listener.default_policy with
      peers = max_handlers;
      backlog = listen_backlog;
      ingress = Eta.Duration.ms (int_of_float (budget.peer_timeout_s *. 300.));
      egress = Eta.Duration.ms (int_of_float (budget.peer_timeout_s *. 300.));
    }
  in
  Eta_ipc.Listener.run ~sw ~fs ~net ~clock ~path:socket_path ~policy
    ~decode:Contract.decode_line ~encode:Contract.encode_response
    ~budget:(fun _ ->
      Some (Eta.Duration.ms (int_of_float (budget.peer_timeout_s *. 400.))))
    ~handler:(fun request ->
      Ok (handle_request ?action_handler coordinator request))
    ~error_response:(function
      | `Decode _ | `Ingress_too_large ->
          Some (Contract.Error_resp err_malformed_request)
      | _ -> None)
    ()
