let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

(* Temporary control: measures descriptor and runtime-worker settlement for
   realtime WebSocket operations against the installed Ox artifact.
   Loopback only; no provider traffic. Not part of the registered suite. *)

open Pie

let fd_count () = Array.length (Sys.readdir "/proc/self/fd")

let tcp_port = function
  | `Tcp (_, port) -> port
  | _ -> failwith "expected tcp address"

let has_suffix suffix value =
  let suffix_len = String.length suffix in
  let value_len = String.length value in
  value_len >= suffix_len
  && String.equal suffix (String.sub value (value_len - suffix_len) suffix_len)

let header_value name head =
  let lines = String.split_on_char '\n' head in
  let prefix = String.lowercase_ascii name ^ ":" in
  let rec loop = function
    | [] -> None
    | line :: rest ->
        let line = String.trim line in
        let lower = String.lowercase_ascii line in
        if String.starts_with ~prefix lower then
          Some
            (String.sub line (String.length prefix)
               (String.length line - String.length prefix)
            |> String.trim)
        else loop rest
  in
  loop lines

let read_http_head flow =
  let scratch = Cstruct.create 1 in
  let buffer = Buffer.create 512 in
  let rec loop () =
    let read = Eio.Flow.single_read flow scratch in
    if read = 0 then raise End_of_file;
    Buffer.add_char buffer (Cstruct.get_char scratch 0);
    let contents = Buffer.contents buffer in
    if has_suffix "\r\n\r\n" contents then contents else loop ()
  in
  loop ()

let write_ws_text flow payload =
  Ox_http.Ws_codec.encode
    { fin = true; opcode = Text; payload = Bytes.of_string payload }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let write_ws_switching_response flow key =
  let response =
    "HTTP/1.1 101 Switching Protocols\r\n" ^ "Upgrade: websocket\r\n"
    ^ "Connection: keep-alive, Upgrade\r\n" ^ "Sec-WebSocket-Accept: "
    ^ Ox_http.Ws_codec.accept_key ~sha1:Ox_http.Ox_http_tls_openssl.sha1 key
    ^ "\r\n\r\n"
  in
  Eio.Flow.copy_string response flow

(* Gated holds the peer open. Error_frame sends one pre-readiness server error
   and then holds the peer open. Error_close sends the same error and closes
   the peer. *)
type server_mode = Gated | Error_frame | Error_close

let handle_connection ~mode ~release flow =
  let head = read_http_head flow in
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> failwith "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key;
  (match mode with
  | Error_frame | Error_close ->
      write_ws_text flow
        {|{"type":"error","error":{"message":"boom","code":"bad_request"}}|}
  | Gated -> ());
  match mode with
  | Error_close -> (
      (try Eio.Flow.shutdown flow `Send with _ -> ());
      try Eio.Flow.close flow with _ -> ())
  | Gated | Error_frame -> (
      Eio.Promise.await release;
      try Eio.Flow.close flow with _ -> ())

let start_server ~sw ~net ~mode =
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:16 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = tcp_port (Eio.Net.listening_addr socket) in
  let held = ref 0 in
  let release, release_resolver = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let rec loop () =
        match Eio.Net.accept ~sw socket with
        | exception Eio.Io _ -> ()
        | flow, _addr ->
            incr held;
            Eio.Fiber.fork ~sw (fun () -> handle_connection ~mode ~release flow);
            loop ()
      in
      loop ());
  (port, held, fun () -> Eio.Promise.resolve release_resolver ())

let url port =
  Printf.sprintf "ws://127.0.0.1:%d/v1/realtime?model=gpt-realtime-2" port

let set_url port =
  Unix.putenv "CODEX_REALTIME_BASE_URL_OVERRIDE"
    (Printf.sprintf "ws://127.0.0.1:%d" port)

let direct_close ~sw ~net ~clock ~url =
  Eio.Switch.run (fun sw ->
      match
        Ox_ai_realtime.Session.connect ~sw ~net ~clock
          ~api_key:(Ox_observability.Redact.make "test-token")
          url
      with
      | Error _ -> Error ()
      | Ok session ->
          Result.map_error (fun _ -> ()) (Ox_ai_realtime.Session.close session))

let pie_buffered ~sw ~net ~clock =
  Stt.Codex_realtime.transcribe_with_prompt ~caps ~sw ~net ~clock
    ~model:"gpt-realtime-2" ~language:"pl"
    ~prompt_template:"Transcribe in {{language}}." ~sample_rate:16000
    (Bytes.make 3200 '\000')

let report ~clock ~label ~index ~held =
  Eio.Time.sleep clock 0.05;
  let raw = fd_count () in
  Printf.printf "%-30s %2d raw=%3d held=%2d adjusted=%3d\n%!" label index raw
    !held
    (raw - !held - 1)

let run_operation ~clock ~label ~index ~held operation =
  let started = Unix.gettimeofday () in
  let result = operation () in
  let elapsed = (Unix.gettimeofday () -. started) *. 1000. in
  report ~clock ~label ~index ~held;
  Printf.printf "    %s exit=%s (%.0fms)\n%!" label
    (match result with Ok _ -> "Ok" | Error _ -> "Error")
    elapsed

let main () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  Unix.putenv "OPENAI_API_KEY" "test-token";
  let sessions = 12 in
  let direct_control ~label ~mode ~scoped =
    let port, held, release = start_server ~sw ~net ~mode in
    Printf.printf "=== %s (base fd=%d) ===\n%!" label (fd_count ());
    for index = 1 to sessions do
      (if scoped then
         Eio.Switch.run (fun op_sw ->
             direct_close ~sw:op_sw ~net ~clock ~url:(url port))
       else direct_close ~sw ~net ~clock ~url:(url port))
      |> ignore;
      report ~clock ~label ~index ~held
    done;
    release ()
  in
  direct_control ~label:"direct-close/long-lived-sw" ~mode:Gated ~scoped:false;
  direct_control ~label:"direct-close/op-switch" ~mode:Gated ~scoped:true;
  let pie_control ~label ~mode ~scoped =
    let port, held, release = start_server ~sw ~net ~mode in
    set_url port;
    Printf.printf "=== %s (base fd=%d) ===\n%!" label (fd_count ());
    for index = 1 to sessions do
      let operation () =
        if scoped then
          Eio.Switch.run (fun op_sw -> pie_buffered ~sw:op_sw ~net ~clock)
        else pie_buffered ~sw ~net ~clock
      in
      run_operation ~clock ~label ~index ~held operation
    done;
    Eio.Time.sleep clock 0.1;
    release ()
  in
  pie_control ~label:"pie-error/long-lived-sw" ~mode:Error_frame ~scoped:false;
  pie_control ~label:"pie-error/op-switch" ~mode:Error_frame ~scoped:true;
  pie_control ~label:"pie-error/peer-close" ~mode:Error_close ~scoped:false;
  let port, held, release = start_server ~sw ~net ~mode:Gated in
  set_url port;
  Printf.printf "=== pie-stalled-cancel (base fd=%d) ===\n%!" (fd_count ());
  for index = 1 to 4 do
    let operation () =
      try
        Eio.Time.with_timeout_exn clock 0.3 (fun () ->
            pie_buffered ~sw ~net ~clock)
      with Eio.Time.Timeout -> Error (`Stt_error "local cancel")
    in
    run_operation ~clock ~label:"pie-stalled-cancel" ~index ~held operation
  done;
  release ()

let () = main ()
