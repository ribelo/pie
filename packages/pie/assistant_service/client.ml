type error = [`Daemon_off | `Daemon_error of string]

open Eta.Syntax

let send_request ~net ~sw (req : Contract.request) : (Contract.response, error) result =
  try
    let socket = Eio.Net.connect ~sw net (`Unix Paths.daemon_socket_path) in
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close socket)
      (fun () ->
         let req_str = Yojson.Safe.to_string (Contract.request_to_json req) ^ "\n" in
         Eio.Flow.write socket [Cstruct.of_string req_str];
         let reader = Eio.Buf_read.of_flow ~max_size:65536 socket in
         let resp_str = Eio.Buf_read.line reader in
         let resp_json = Yojson.Safe.from_string (String.trim resp_str) in
         Ok (Contract.response_of_json req resp_json))
  with
  | Eio.Io _ -> Error `Daemon_off
  | exn -> Error (`Daemon_error (Printexc.to_string exn))

let call ~net ~sw req =
  (let* resp =
     Eta.Effect.sync (fun () -> send_request ~net ~sw req)
     |> Eta.Effect.flatten_result
   in
   match resp with
   | Contract.Error_resp message -> Eta.Effect.fail (`Daemon_error message)
   | other -> Eta.Effect.pure other)
  |> Eta.Effect.timeout_as
       (Eta.Duration.seconds 1)
       ~on_timeout:(`Daemon_error "daemon RPC timed out")

let status ~net ~sw () =
  let* resp = call ~net ~sw Status in
  match resp with
  | Contract.Status_resp snap -> Eta.Effect.pure snap
  | Contract.Status_with_components (snap, _) -> Eta.Effect.pure snap
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Status")

let status_with_components ~net ~sw () =
  let* resp = call ~net ~sw Status in
  match resp with
  | Contract.Status_resp snap -> Eta.Effect.pure (snap, None)
  | Contract.Status_with_components (snap, comp) -> Eta.Effect.pure (snap, Some comp)
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Status")

let toggle ~net ~sw () =
  let* resp = call ~net ~sw Toggle in
  match resp with
  | Contract.Toggle_resp b -> Eta.Effect.pure b
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Toggle")

let action ~net ~sw name =
  let* resp = call ~net ~sw (Action name) in
  match resp with
  | Contract.Action_resp result ->
    if result.accepted then Eta.Effect.pure result
    else
      let message =
        Option.value result.message ~default:("action rejected: " ^ result.action)
      in
      Eta.Effect.fail (`Daemon_error message)
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Action")

let pause ~net ~sw () =
  let* resp = call ~net ~sw Pause in
  match resp with
  | Contract.Pause_resp -> Eta.Effect.unit
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Pause")

let resume ~net ~sw () =
  let* resp = call ~net ~sw Resume in
  match resp with
  | Contract.Resume_resp -> Eta.Effect.unit
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for Resume")

let meeting_start ~net ~sw () =
  let* resp = call ~net ~sw Meeting_start in
  match resp with
  | Contract.Meeting_start_resp r -> Eta.Effect.pure r
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for MeetingStart")

let meeting_stop ~net ~sw () =
  let* resp = call ~net ~sw Meeting_stop in
  match resp with
  | Contract.Meeting_stop_resp r -> Eta.Effect.pure r
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for MeetingStop")

let meeting_toggle ~net ~sw () =
  let* resp = call ~net ~sw Meeting_toggle in
  match resp with
  | Contract.Meeting_toggle_resp snap -> Eta.Effect.pure snap
  | _ -> Eta.Effect.fail (`Daemon_error "unexpected response for MeetingToggle")
