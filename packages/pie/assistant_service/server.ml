let mkdir_p = Fs.mkdir_p
let max_handlers = 16
let listen_backlog = 16

type budget = { peer_timeout_s : float }

let default_budget = { peer_timeout_s = 1.0 }

type error = Ox_ipc.Listener.error

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
    reasoning_effort = None;
    response_schema = None;
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

(* ox_ipc binds only in a private directory (no group/other bits). Pie's runtime
   directory is also where Coordinator persists recording.json, and [Fs.mkdir_p] (or an
   older Pie, which created it 0755) may already have made it under the service umask.
   Pie owns this directory: create it 0700, or tighten our own real directory that
   nobody else can write (0755, 0750) to 0700. A group/other-writable directory, a
   symlink, or one owned by someone else is left alone, and ox_ipc refuses it (as v1
   refused a writable one). *)
let secure_socket_dir ~caps dir =
  match Unix.lstat dir with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
      Fs.mkdir_p ~caps (Filename.dirname dir);
      try Ox_caps.Cap_unix.mkdir ~caps dir 0o700
      with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  | st
    when st.Unix.st_kind = Unix.S_DIR
         && st.st_uid = Unix.getuid ()
         && st.st_perm land 0o077 <> 0
         && st.st_perm land 0o022 = 0 ->
      Ox_caps.Cap_unix.chmod ~caps dir 0o700
  | _ -> ()

let start ~caps ~sw ~fs ~net ~clock ?socket_path ?(budget = default_budget)
    ?action_handler ?status_metadata coordinator =
  let socket_path =
    Option.value socket_path ~default:(Paths.daemon_socket_path ~caps)
  in
  let policy =
    {
      Ox_ipc.Listener.default_policy with
      peers = max_handlers;
      backlog = listen_backlog;
      ingress = Ox.Duration.ms (int_of_float (budget.peer_timeout_s *. 300.));
      egress = Ox.Duration.ms (int_of_float (budget.peer_timeout_s *. 300.));
    }
  in
  secure_socket_dir ~caps (Filename.dirname socket_path);
  Ox_ipc.Listener.run ~caps ~sw ~fs ~net ~clock ~path:socket_path ~policy
    ~decode:Contract.decode_line ~encode:Contract.encode_response
    ~budget:(fun _ ->
      Some (Ox.Duration.ms (int_of_float (budget.peer_timeout_s *. 400.))))
    ~handler:(fun request ->
      let metadata (snap : Contract.recording_snapshot) =
        match status_metadata with
        | None -> snap
        | Some get ->
            let reasoning_effort, response_schema = get () in
            { snap with reasoning_effort; response_schema }
      in
      let response =
        match handle_request ?action_handler coordinator request with
        | Contract.Status_resp snap -> Contract.Status_resp (metadata snap)
        | Contract.Action_resp result ->
            Contract.Action_resp
              { result with snapshot = metadata result.snapshot }
        | response -> response
      in
      Ok response)
    ~error_response:(function
      | `Decode _ | `Ingress_too_large ->
          Some (Contract.Error_resp err_malformed_request)
      | _ -> None)
    ()
