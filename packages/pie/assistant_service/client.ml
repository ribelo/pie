type error =
  [ `Daemon_off | `Daemon_error of string | `Daemon_protocol_error of string ]

open Eta.Syntax

let send_request ~net ~sw:_ (req : Contract.request) :
    (Contract.response, error) result =
  try
    (* A failed connect may already have registered its socket with the switch.
       Scope the entire attempt, not only the successfully connected flow, so
       a long-lived status subscriber cannot retain failed sockets. *)
    Eio.Switch.run @@ fun sw ->
    let socket = Eio.Net.connect ~sw net (`Unix Paths.daemon_socket_path) in
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close socket)
      (fun () ->
        let req_str =
          Yojson.Safe.to_string (Contract.request_to_json req) ^ "\n"
        in
        Eio.Flow.write socket [ Cstruct.of_string req_str ];
        let reader = Eio.Buf_read.of_flow ~max_size:65536 socket in
        let resp_str = Eio.Buf_read.line reader in
        let resp_json = Yojson.Safe.from_string (String.trim resp_str) in
        match Contract.response_of_json req resp_json with
        | Ok response -> Ok response
        | Error label -> Error (`Daemon_protocol_error label))
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Eio.Io _ -> Error `Daemon_off
  | exn -> Error (`Daemon_error (Printexc.to_string exn))

let call ~net ~sw req : (Contract.response, error) Eta.Effect.t =
  (let* resp =
     Eta.Effect.sync (fun () -> send_request ~net ~sw req)
     |> Eta.Effect.flatten_result
   in
   match resp with
   | Contract.Error_resp message -> Eta.Effect.fail (`Daemon_error message)
   | other -> Eta.Effect.pure other)
  |> Eta.Effect.timeout_as (Eta.Duration.seconds 1)
       ~on_timeout:(`Daemon_error "daemon RPC timed out")

let status ~net ~sw () : (Contract.recording_snapshot, error) Eta.Effect.t =
  let* resp = call ~net ~sw Status in
  match resp with
  | Contract.Status_resp snap -> Eta.Effect.pure snap
  | _ -> Eta.Effect.fail (`Daemon_protocol_error Contract.wrong_type)

let toggle ~net ~sw () : (bool, error) Eta.Effect.t =
  let* resp = call ~net ~sw Toggle in
  match resp with
  | Contract.Toggle_resp b -> Eta.Effect.pure b
  | _ -> Eta.Effect.fail (`Daemon_protocol_error Contract.wrong_type)

let action ~net ~sw name : (Contract.action_result, error) Eta.Effect.t =
  let* resp = call ~net ~sw (Action name) in
  match resp with
  | Contract.Action_resp result ->
      if result.accepted then Eta.Effect.pure result
      else
        let message =
          Option.value result.message
            ~default:("action rejected: " ^ result.action)
        in
        Eta.Effect.fail (`Daemon_error message)
  | _ -> Eta.Effect.fail (`Daemon_protocol_error Contract.wrong_type)

let pause ~net ~sw () : (unit, error) Eta.Effect.t =
  let* resp = call ~net ~sw Pause in
  match resp with
  | Contract.Pause_resp -> Eta.Effect.unit
  | _ -> Eta.Effect.fail (`Daemon_protocol_error Contract.wrong_type)

let resume ~net ~sw () : (unit, error) Eta.Effect.t =
  let* resp = call ~net ~sw Resume in
  match resp with
  | Contract.Resume_resp -> Eta.Effect.unit
  | _ -> Eta.Effect.fail (`Daemon_protocol_error Contract.wrong_type)
