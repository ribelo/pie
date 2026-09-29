type error =
  [ `Daemon_off | `Daemon_error of string | `Daemon_protocol_error of string ]

let ( let* ) = Result.bind

let send_request ~net ~sw:_ (req : Contract.request) =
  try
    Eio.Switch.run @@ fun sw ->
    let socket = Eio.Net.connect ~sw net (`Unix Paths.daemon_socket_path) in
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close socket)
      (fun () ->
        let payload =
          Yojson.Safe.to_string (Contract.request_to_json req) ^ "\n"
        in
        Eio.Flow.write socket [ Cstruct.of_string payload ];
        let reader = Eio.Buf_read.of_flow ~max_size:65536 socket in
        let line = Eio.Buf_read.line reader in
        let json =
          match Yojson.Safe.from_string (String.trim line) with
          | json -> Ok json
          | exception Yojson.Json_error _ ->
              Error (`Daemon_protocol_error "malformed-response")
        in
        let* json = json in
        match Contract.response_of_json req json with
        | Ok response -> Ok response
        | Error label -> Error (`Daemon_protocol_error label))
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Eio.Io _ -> Error `Daemon_off
  | End_of_file -> Error (`Daemon_protocol_error "missing-response")
  | Eio.Buf_read.Buffer_limit_exceeded ->
      Error (`Daemon_protocol_error "response-too-large")

let call ~net ~clock ~sw req =
  match
    Ox.Timeout.timeout ~clock (Ox.Duration.seconds 1) (fun () ->
        (send_request ~net ~sw req
          : (Contract.response, [> error | `Timeout ]) result))
  with
  | Error `Timeout -> Error (`Daemon_error "daemon RPC timed out")
  | Error `Daemon_off -> Error `Daemon_off
  | Error (`Daemon_error message) -> Error (`Daemon_error message)
  | Error (`Daemon_protocol_error message) ->
      Error (`Daemon_protocol_error message)
  | Ok (Contract.Error_resp message) -> Error (`Daemon_error message)
  | Ok response -> Ok response

let status ~net ~clock ~sw () =
  let* response = call ~net ~clock ~sw Contract.Status in
  match response with
  | Contract.Status_resp snapshot -> Ok snapshot
  | _ -> Error (`Daemon_protocol_error Contract.wrong_type)

let toggle ~net ~clock ~sw () =
  let* response = call ~net ~clock ~sw Contract.Toggle in
  match response with
  | Contract.Toggle_resp value -> Ok value
  | _ -> Error (`Daemon_protocol_error Contract.wrong_type)

let action ~net ~clock ~sw name =
  let* response = call ~net ~clock ~sw (Contract.Action name) in
  match response with
  | Contract.Action_resp result when result.accepted -> Ok result
  | Contract.Action_resp result ->
      Error
        (`Daemon_error
           (Option.value result.message
              ~default:("action rejected: " ^ result.action)))
  | _ -> Error (`Daemon_protocol_error Contract.wrong_type)

let pause ~net ~clock ~sw () =
  let* response = call ~net ~clock ~sw Contract.Pause in
  match response with
  | Contract.Pause_resp -> Ok ()
  | _ -> Error (`Daemon_protocol_error Contract.wrong_type)

let resume ~net ~clock ~sw () =
  let* response = call ~net ~clock ~sw Contract.Resume in
  match response with
  | Contract.Resume_resp -> Ok ()
  | _ -> Error (`Daemon_protocol_error Contract.wrong_type)
