let mkdir_p = Fs.mkdir_p

let snapshot_of_coordinator coordinator =
  let snap = Recording.Coordinator.snapshot coordinator in
  {
    Contract.active = snap.active;
    mode = snap.mode;
    enabled = snap.enabled;
    started_at = snap.started_at;
    updated_at = Some snap.updated_at;
    transcript_path = snap.transcript_path;
    last_error = snap.last_error;
  }

let start_result_of_coordinator = function
  | Recording.Coordinator.Started mode ->
    Contract.Started (Recording.Coordinator.string_of_mode mode)
  | Recording.Coordinator.Busy mode ->
    Contract.Busy (Recording.Coordinator.string_of_mode mode)
  | Recording.Coordinator.Disabled -> Contract.Disabled

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
    | None -> rejected_action coordinator action "daemon actions are unavailable")
  | Contract.Meeting_start ->
    let result =
      Recording.Coordinator.try_start coordinator Recording.Coordinator.Meeting_transcribe ()
      |> start_result_of_coordinator
    in
    Contract.Meeting_start_resp { result; snapshot = snapshot_of_coordinator coordinator }
  | Contract.Meeting_stop ->
    let stopped =
      Recording.Coordinator.stop coordinator Recording.Coordinator.Meeting_transcribe
    in
    Contract.Meeting_stop_resp
      { stopped; snapshot = snapshot_of_coordinator coordinator }
  | Contract.Meeting_toggle ->
    let snap = Recording.Coordinator.toggle_meeting coordinator in
    Contract.Meeting_toggle_resp {
      active = snap.active;
      mode = snap.mode;
      enabled = snap.enabled;
      started_at = snap.started_at;
      updated_at = Some snap.updated_at;
      transcript_path = snap.transcript_path;
      last_error = snap.last_error;
    }

let handle_flow ?action_handler coordinator flow _addr =
  try
    let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
    let line = Eio.Buf_read.line reader in
    let response_json =
      match Yojson.Safe.from_string (String.trim line) |> Contract.request_of_json with
      | Ok request ->
        Contract.response_to_json (handle_request ?action_handler coordinator request)
      | Error msg -> `Assoc [("error", `String msg)]
    in
    let payload = Yojson.Safe.to_string response_json ^ "\n" in
    Eio.Flow.write flow [Cstruct.of_string payload]
  with exn ->
    let payload =
      Yojson.Safe.to_string (`Assoc [("error", `String (Printexc.to_string exn))]) ^ "\n"
    in
    (try Eio.Flow.write flow [Cstruct.of_string payload] with _ -> ())

let start ~sw ~net ?(socket_path = Paths.daemon_socket_path) ?action_handler coordinator =
  mkdir_p (Filename.dirname socket_path);
  (try Unix.unlink socket_path with
   | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
   | exn -> raise exn);
  let socket =
    Eio.Net.listen ~reuse_addr:true ~sw ~backlog:10 net (`Unix socket_path)
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      Eio.Net.accept_fork ~sw socket
        ~on_error:(fun exn ->
          Observability.error
            ~attrs:[ ("error", Printexc.to_string exn) ]
            "daemon server error")
        (handle_flow ?action_handler coordinator);
      loop ()
    in
    try loop () with
    | Eio.Cancel.Cancelled _ -> `Stop_daemon
    | exn ->
      Observability.error
        ~attrs:[ ("error", Printexc.to_string exn) ]
        "daemon server stopped";
      `Stop_daemon)
