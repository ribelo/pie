type error = [`Daemon_off | `Daemon_error of string]

let ( let* ) eff f = Eta.Effect.bind f eff

let send_request ~net ~sw (req : Contract.request) : (Contract.response, error) result =
  try
    let socket = Eio.Net.connect ~sw net (`Unix Paths.daemon_socket_path) in
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close socket)
      (fun () ->
         let req_str = Yojson.Safe.to_string (Contract.request_to_json req) ^ "\n" in
         Eio.Flow.write socket [Cstruct.of_string req_str];
         let resp_str = Eio.Buf_read.parse_exn ~max_size:65536 Eio.Buf_read.line socket in
         let resp_json = Yojson.Safe.from_string (String.trim resp_str) in
         Ok (Contract.response_of_json req resp_json))
  with
  | Eio.Io _ -> Error `Daemon_off
  | exn -> Error (`Daemon_error (Printexc.to_string exn))

let rpc ~net ~sw req =
  Eta.Effect.sync (fun () -> send_request ~net ~sw req)
  |> Eta.Effect.bind (function
    | Ok resp -> Eta.Effect.pure resp
    | Error e -> Eta.Effect.fail e)

let status ~net ~sw () =
  let* resp = rpc ~net ~sw Status in
  match resp with
  | Status_resp snap -> Eta.Effect.pure snap
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Status")

let toggle ~net ~sw () =
  let* resp = rpc ~net ~sw Toggle in
  match resp with
  | Toggle_resp b -> Eta.Effect.pure b
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Toggle")

let pause ~net ~sw () =
  let* resp = rpc ~net ~sw Pause in
  match resp with
  | Pause_resp -> Eta.Effect.unit
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Pause")

let resume ~net ~sw () =
  let* resp = rpc ~net ~sw Resume in
  match resp with
  | Resume_resp -> Eta.Effect.unit
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Resume")

let meeting_start ~net ~sw () =
  let* resp = rpc ~net ~sw Meeting_start in
  match resp with
  | Meeting_start_resp r -> Eta.Effect.pure r
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for MeetingStart")

let meeting_stop ~net ~sw () =
  let* resp = rpc ~net ~sw Meeting_stop in
  match resp with
  | Meeting_stop_resp snap -> Eta.Effect.pure snap
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for MeetingStop")

let meeting_toggle ~net ~sw () =
  let* resp = rpc ~net ~sw Meeting_toggle in
  match resp with
  | Meeting_toggle_resp snap -> Eta.Effect.pure snap
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for MeetingToggle")
