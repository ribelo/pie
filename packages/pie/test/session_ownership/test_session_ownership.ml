let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

(* PIE002 private acceptance. Public Codex buffered/streaming. *)

let ( let* ) = Result.bind
let putenv = Unix.putenv [@alert "-unsafe_multidomain"]

type scenario =
  | Normal
  | Pre_ready_error
  | Malformed_readiness
  | Never_ready_cancel
  | Send_failure
  | Midstream_cancel

let scenario_name = function
  | Normal -> "normal"
  | Pre_ready_error -> "pre-ready-error"
  | Malformed_readiness -> "malformed-readiness"
  | Never_ready_cancel -> "never-ready-cancel"
  | Send_failure -> "send-failure"
  | Midstream_cancel -> "midstream-cancel"

let fd_count () =
  let dir = Unix.opendir "/proc/self/fd" in
  Fun.protect
    ~finally:(fun () -> Unix.closedir dir)
    (fun () ->
      let rec loop n =
        match Unix.readdir dir with
        | exception End_of_file -> n
        | "." | ".." -> loop n
        | _ -> loop (n + 1)
      in
      loop 0 - 1)

let fd_is_open flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> false
  | Some fd -> Eio_unix.Fd.use fd (fun _ -> true) ~if_closed:(fun () -> false)

let has_suffix suffix s =
  let sl = String.length suffix in
  let n = String.length s in
  n >= sl && String.sub s (n - sl) sl = suffix

let read_http_head flow =
  let scratch = Cstruct.create 1 in
  let buffer = Buffer.create 512 in
  let rec loop () =
    match Eio.Flow.single_read flow scratch with
    | 0 -> raise End_of_file
    | _ ->
        Buffer.add_char buffer (Cstruct.get_char scratch 0);
        let contents = Buffer.contents buffer in
        if has_suffix "\r\n\r\n" contents then contents else loop ()
    | exception End_of_file -> raise End_of_file
  in
  loop ()

let header_value name raw =
  let expected = String.lowercase_ascii name in
  raw |> String.split_on_char '\n'
  |> List.find_map (fun line ->
      let line =
        let len = String.length line in
        if len > 0 && Char.equal line.[len - 1] '\r' then
          String.sub line 0 (len - 1)
        else line
      in
      match String.split_on_char ':' line with
      | key :: rest when String.lowercase_ascii (String.trim key) = expected ->
          Some (String.trim (String.concat ":" rest))
      | _ -> None)

let write_ws_text flow payload =
  Ox_http.Ws_codec.encode
    { fin = true; opcode = Text; payload = Bytes.of_string payload }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let write_ws_switching_response flow key =
  let accept =
    Ox_http.Ws_codec.accept_key ~sha1:Ox_http.Ox_http_tls_openssl.sha1 key
  in
  let response =
    String.concat ""
      [
        "HTTP/1.1 101 Switching Protocols\r\n";
        "Upgrade: websocket\r\n";
        "Connection: keep-alive, Upgrade\r\n";
        "Sec-WebSocket-Accept: ";
        accept;
        "\r\n\r\n";
      ]
  in
  Eio.Flow.copy_string response flow

let upgrade flow =
  let head = read_http_head flow in
  match header_value "Sec-WebSocket-Key" head with
  | Some key -> write_ws_switching_response flow key
  | None -> failwith "missing Sec-WebSocket-Key"

let session_updated = "{\"type\":\"session.updated\"}"

let response_done =
  "{\"type\":\"response.done\",\"response\":{\"status\":\"completed\"}}"

let server_error =
  "{\"type\":\"error\",\"error\":{\"message\":\"pre-ready\",\"code\":\"bad_request\"}}"

let drain_peer ~clock flow =
  let buf = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read flow buf with
    | 0 -> ()
    | _ -> loop ()
    | exception End_of_file -> ()
  in
  try Eio.Time.with_timeout_exn clock 0.4 loop with _ -> ()

let handle_peer ~clock:_ scenario ~on_connected ~hold ~close_flow flow =
  upgrade flow;
  let hold_then_close () =
    Eio.Promise.await hold;
    try close_flow () with _ -> ()
  in
  match scenario with
  | Normal ->
      on_connected ();
      write_ws_text flow session_updated;
      write_ws_text flow response_done;
      hold_then_close ()
  | Pre_ready_error ->
      on_connected ();
      write_ws_text flow server_error;
      hold_then_close ()
  | Malformed_readiness ->
      on_connected ();
      write_ws_text flow "not-json";
      hold_then_close ()
  | Never_ready_cancel ->
      on_connected ();
      hold_then_close ()
  | Send_failure ->
      on_connected ();
      write_ws_text flow session_updated;
      close_flow ()
  | Midstream_cancel ->
      write_ws_text flow session_updated;
      on_connected ();
      hold_then_close ()

let pcm = Bytes.make 320 '\001'

let session_update =
  Pie.Stt.Codex_realtime.build_conversation_session_update
    ~model:"gpt-realtime-2" ~prompt:"test"

let response_create =
  Pie.Stt.Codex_realtime.build_conversation_response_create ~prompt:"test"

let run_buffered ~sw ~net ~clock ?ca_file ~url () =
  Pie.Stt.Codex_realtime.run_realtime_session ~caps ~sw ~net ~clock ?ca_file
    ~url ~access_token:"synthetic-ownership-token"
    ~mode:Pie.Stt.Codex_realtime.Conversation ~session_update ~response_create
    ~input_sample_rate:16000 pcm

let run_streaming ~sw ~net ~clock ?ca_file ~url ?on_audio_pull () =
  let supplied = ref false in
  let audio () =
    if !supplied then Ok None
    else (
      supplied := true;
      Option.iter (fun resolve -> resolve ()) on_audio_pull;
      Ok (Some pcm))
  in
  Pie.Stt.Codex_realtime.run_realtime_stream_session ~caps ~sw ~net ~clock
    ?ca_file ~url ~access_token:"synthetic-ownership-token"
    ~mode:Pie.Stt.Codex_realtime.Conversation ~session_update ~response_create
    ~input_sample_rate:16000 audio

let pp_stt ppf = function `Stt_error msg -> Format.pp_print_string ppf msg

let contains hay needle =
  let n = String.length needle in
  let rec loop i =
    i <= String.length hay - n && (String.sub hay i n = needle || loop (i + 1))
  in
  n = 0 || loop 0

let expect_error ~needle = function
  | Ok _ -> Error (`Stt_error "expected failure, got success")
  | Error (`Stt_error message as error) ->
      if contains message needle then Ok () else Error error

let timeout_fail label = `Stt_error ("ownership-deadline:" ^ label)

type job = {
  connected : unit Eio.Promise.t;
  resolve_connected : unit Eio.Promise.u;
  client_ready : unit Eio.Promise.t;
  resolve_client_ready : unit Eio.Promise.u;
  hold : unit Eio.Promise.t;
  resolve_hold : unit Eio.Promise.u;
}

let make_job () =
  let connected, resolve_connected = Eio.Promise.create () in
  let client_ready, resolve_client_ready = Eio.Promise.create () in
  let hold, resolve_hold = Eio.Promise.create () in
  {
    connected;
    resolve_connected;
    client_ready;
    resolve_client_ready;
    hold;
    resolve_hold;
  }

let settle ~sw ~net ~clock ?ca_file ~url ~job scenario streaming =
  let body () =
    match scenario with
    | Midstream_cancel ->
        run_streaming ~sw ~net ~clock ?ca_file ~url
          ~on_audio_pull:(fun () ->
            Eio.Promise.resolve job.resolve_client_ready ())
          ()
    | _ ->
        if streaming then run_streaming ~sw ~net ~clock ?ca_file ~url ()
        else run_buffered ~sw ~net ~clock ?ca_file ~url ()
  in
  let timed () =
    try Eio.Time.with_timeout_exn clock 1.5 body
    with Eio.Time.Timeout -> Error (timeout_fail (scenario_name scenario))
  in
  match scenario with
  | Normal -> (
      match timed () with
      | Ok _ -> Ok ()
      | Error (`Stt_error message) ->
          Error (`Stt_error ("normal-op: " ^ message)))
  | Pre_ready_error -> expect_error ~needle:"server error" (timed ())
  | Malformed_readiness -> expect_error ~needle:"invalid event" (timed ())
  | Send_failure -> (
      match timed () with
      | Ok _ -> Error (`Stt_error "expected send failure, got success")
      | Error (`Stt_error message) when contains message "ownership-deadline" ->
          Error (`Stt_error "send-failure timed out")
      | Error _ -> Ok ())
  | Never_ready_cancel | Midstream_cancel ->
      let gate =
        if scenario = Midstream_cancel then job.client_ready else job.connected
      in
      Eio.Fiber.first
        (fun () ->
          Eio.Promise.await gate;
          Ok ())
        (fun () ->
          ignore (timed ());
          Error (`Stt_error "expected cancel winner, body succeeded"))

let classify_fds () =
  let dir = Unix.opendir "/proc/self/fd" in
  Fun.protect
    ~finally:(fun () -> Unix.closedir dir)
    (fun () ->
      let rec loop acc =
        match Unix.readdir dir with
        | exception End_of_file -> List.rev acc
        | "." | ".." -> loop acc
        | name ->
            let path = Filename.concat "/proc/self/fd" name in
            let target =
              try Unix.readlink path with Unix.Unix_error _ -> "unreadable"
            in
            loop ((name, target) :: acc)
      in
      loop [])

let format_fds () =
  classify_fds ()
  |> List.map (fun (name, target) -> name ^ "->" ^ target)
  |> String.concat "; "

let socket_inode target =
  let prefix = "socket:[" in
  let n = String.length target in
  let p = String.length prefix in
  if n > p && String.sub target 0 p = prefix && target.[n - 1] = ']' then
    Some (String.sub target p (n - p - 1))
  else None

let owned_tcp_by_inode () = []

let format_owned_sockets () =
  classify_fds ()
  |> List.filter_map (fun (name, target) ->
      match socket_inode target with
      | None -> None
      | Some inode -> Some (name ^ "->" ^ target ^ " inode=" ^ inode))
  |> String.concat "; "

let fd_targets () = classify_fds () |> List.map snd |> List.sort String.compare

let peer_inode_of flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> None
  | Some fd ->
      Eio_unix.Fd.use fd
        ~if_closed:(fun () -> None)
        (fun unix_fd -> Some (string_of_int (Unix.fstat unix_fd).st_ino))

let extra_socket_inodes ~baseline =
  let base = List.sort_uniq String.compare baseline in
  classify_fds ()
  |> List.filter_map (fun (_n, target) ->
      if List.mem target base then None else socket_inode target)

let assert_held_no_client ~label ~baseline ~peer_flow ~held =
  match Atomic.get peer_flow with
  | None -> Alcotest.failf "%s no accepted peer" label
  | Some flow ->
      if held && not (fd_is_open flow) then
        Alcotest.failf "%s peer closed before client settlement" label;
      let extras = extra_socket_inodes ~baseline in
      let peer_inos =
        match peer_inode_of flow with None -> [] | Some i -> [ i ]
      in
      let client = List.filter (fun i -> not (List.mem i peer_inos)) extras in
      if client <> [] then
        Alcotest.failf
          "%s extra client socket inodes=[%s] fds=[%s] sockets=[%s]" label
          (String.concat "," client) (format_fds ()) (format_owned_sockets ());
      if held && peer_inos = [] && extras = [] then
        Alcotest.failf "%s expected held peer socket, extras empty fds=[%s]"
          label (format_fds ())

let assert_back_to_baseline ~label ~baseline =
  let now = fd_targets () in
  let extra = List.filter (fun t -> not (List.mem t baseline)) now in
  let extra_sockets =
    List.filter
      (fun t -> String.length t >= 7 && String.sub t 0 7 = "socket:")
      extra
  in
  if extra_sockets <> [] then
    Alcotest.failf "%s after peer join extra sockets=[%s] fds=[%s] sockets=[%s]"
      label
      (String.concat "; " extra_sockets)
      (format_fds ()) (format_owned_sockets ())

let listen_v4 ~sw net =
  Eio.Net.listen ~sw ~reuse_addr:true ~backlog:8 net
    (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))

let tcp_port socket =
  match Eio.Net.listening_addr socket with
  | `Tcp (_, port) -> port
  | `Unix _ -> Alcotest.fail "expected TCP listener"

let close_peer flow = try Eio.Flow.close flow with _ -> ()

let run_one ~sw ~net ~clock ?ca_file ~url_of_port ~serve ~baseline scenario
    streaming =
  let socket = listen_v4 ~sw net in
  let port = tcp_port socket in
  let url = url_of_port port in
  let job = make_job () in
  let peer_flow = Atomic.make None in
  let peer_done, peer_done_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Fun.protect
        ~finally:(fun () -> ignore (Eio.Promise.try_resolve peer_done_u ()))
        (fun () ->
          let flow, _ = Eio.Net.accept ~sw socket in
          Atomic.set peer_flow (Some flow);
          serve ~clock ~job scenario flow));
  let result = settle ~sw ~net ~clock ?ca_file ~url ~job scenario streaming in
  (try Eio.Resource.close socket with _ -> ());
  (match result with
  | Ok () -> ()
  | Error cause ->
      Alcotest.failf "%s %s: %a"
        (if streaming then "streaming" else "buffered")
        (scenario_name scenario) pp_stt cause);
  let label =
    (if streaming then "streaming " else "buffered ") ^ scenario_name scenario
  in
  assert_held_no_client ~label ~baseline ~peer_flow
    ~held:(scenario <> Send_failure);
  ignore (Eio.Promise.try_resolve job.resolve_hold ());
  Eio.Promise.await peer_done;
  assert_back_to_baseline ~label ~baseline

let all_scenarios =
  [
    Normal;
    Pre_ready_error;
    Malformed_readiness;
    Never_ready_cancel;
    Send_failure;
    Midstream_cancel;
  ]

let serve_plain ~clock ~job scenario flow =
  handle_peer ~clock scenario
    ~on_connected:(fun () -> Eio.Promise.resolve job.resolve_connected ())
    ~hold:job.hold
    ~close_flow:(fun () -> Eio.Flow.close flow)
    flow

let ws_url port =
  Printf.sprintf "ws://localhost:%d/v1/realtime?model=gpt-realtime-2" port

let wss_url port =
  Printf.sprintf "wss://localhost:%d/v1/realtime?model=gpt-realtime-2" port

let warm_timer ~sw clock =
  let hold, hold_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      try
        Eio.Time.with_timeout_exn clock 120.0 (fun () -> Eio.Promise.await hold)
      with _ -> ());
  Eio.Time.sleep clock 0.02;
  hold_u

let test_gates_ws () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let stop_timer = warm_timer ~sw clock in
  let baseline = fd_targets () in
  List.iter
    (fun scenario ->
      List.iter
        (fun streaming ->
          run_one ~sw ~net ~clock ~url_of_port:ws_url ~serve:serve_plain
            ~baseline scenario streaming)
        [ false; true ])
    all_scenarios;
  ignore (Eio.Promise.try_resolve stop_timer ())

let repeat_ws ~rounds () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let stop_timer = warm_timer ~sw clock in
  let baseline = fd_targets () in
  for _round = 1 to rounds do
    List.iter
      (fun scenario ->
        List.iter
          (fun streaming ->
            run_one ~sw ~net ~clock ~url_of_port:ws_url ~serve:serve_plain
              ~baseline scenario streaming)
          [ false; true ])
      all_scenarios
  done;
  ignore (Eio.Promise.try_resolve stop_timer ())

let with_localhost_tls f =
  let cert = Filename.temp_file "pie-own-cert" ".pem" in
  let key = Filename.temp_file "pie-own-key" ".pem" in
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove cert with _ -> ());
      try Sys.remove key with _ -> ())
    (fun () ->
      let cmd =
        String.concat " "
          [
            "openssl req -x509 -newkey rsa:2048 -nodes -sha256 -days 1";
            "-subj";
            Filename.quote "/CN=localhost";
            "-addext";
            Filename.quote "subjectAltName=DNS:localhost,IP:127.0.0.1";
            "-keyout";
            Filename.quote key;
            "-out";
            Filename.quote cert;
            ">/dev/null 2>&1";
          ]
      in
      if Sys.command cmd <> 0 then
        Alcotest.fail "failed to generate synthetic localhost TLS material";
      f cert key)

let serve_tls ctx ~clock ~job scenario raw =
  let raw_flow =
    (raw :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t)
  in
  let tls =
    Ox_http.Tls_flow.create
      ~raw:(raw_flow :> Eio.Flow.two_way_ty Eio.Resource.t)
      ~ssl:(Ox_http.Ox_http_tls_openssl.create_server_ssl ctx)
  in
  Fun.protect
    ~finally:(fun () -> try Eio.Flow.close raw with _ -> ())
    (fun () ->
      handle_peer ~clock scenario
        ~on_connected:(fun () -> Eio.Promise.resolve job.resolve_connected ())
        ~hold:job.hold
        ~close_flow:(fun () -> Eio.Flow.close raw)
        tls)

let repeat_wss_in_process ~cert ~key ~rounds =
  let ctx =
    Ox_http.Ox_http_tls_openssl.create_server_ctx ~certificate_chain_file:cert
      ~private_key_file:key ~alpn_protocols:[ "http/1.1" ] ()
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let stop_timer = warm_timer ~sw clock in
  let baseline = fd_targets () in
  for _round = 1 to rounds do
    List.iter
      (fun scenario ->
        List.iter
          (fun streaming ->
            run_one ~sw ~net ~clock ~ca_file:cert ~url_of_port:wss_url
              ~serve:(serve_tls ctx) ~baseline scenario streaming)
          [ false; true ])
      all_scenarios
  done;
  ignore (Eio.Promise.try_resolve stop_timer ())

let env_without keys =
  Unix.environment () |> Array.to_list
  |> List.filter (fun entry ->
      not
        (List.exists
           (fun key ->
             String.length entry >= String.length key + 1
             && String.sub entry 0 (String.length key) = key
             && entry.[String.length key] = '=')
           keys))
  |> Array.of_list

let repeat_wss ~rounds () =
  with_localhost_tls @@ fun cert key ->
  let env =
    Array.append
      (env_without
         [
           "SSL_CERT_FILE";
           "PIE_OWN_WSS_CHILD";
           "PIE_OWN_TLS_KEY";
           "PIE_OWN_ROUNDS";
         ])
      [|
        "SSL_CERT_FILE=" ^ cert;
        "PIE_OWN_WSS_CHILD=1";
        "PIE_OWN_TLS_KEY=" ^ key;
        "PIE_OWN_ROUNDS=" ^ string_of_int rounds;
      |]
  in
  let exe =
    if Array.length Sys.argv > 0 && Sys.file_exists Sys.argv.(0) then
      Sys.argv.(0)
    else Sys.executable_name
  in
  let pid =
    Unix.create_process_env exe [| exe |] env Unix.stdin Unix.stdout Unix.stderr
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Alcotest.failf "wss ownership child exited %d" n
  | Unix.WSIGNALED s -> Alcotest.failf "wss ownership child signaled %d" s
  | Unix.WSTOPPED s -> Alcotest.failf "wss ownership child stopped %d" s

let probe_flow label flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> label ^ ":fd_opt=none"
  | Some fd ->
      Eio_unix.Fd.use fd
        ~if_closed:(fun () -> label ^ ":rcfd=closed")
        (fun unix_fd ->
          let local =
            try
              match Unix.getsockname unix_fd with
              | Unix.ADDR_INET (ip, p) ->
                  Unix.string_of_inet_addr ip ^ ":" ^ string_of_int p
              | Unix.ADDR_UNIX path -> "unix:" ^ path
            with _ -> "?"
          in
          let peer =
            try
              match Unix.getpeername unix_fd with
              | Unix.ADDR_INET (ip, p) ->
                  Unix.string_of_inet_addr ip ^ ":" ^ string_of_int p
              | Unix.ADDR_UNIX path -> "unix:" ^ path
            with _ -> "?"
          in
          Printf.sprintf "%s:rcfd=open local=%s peer=%s" label local peer)

let snap label extra =
  Printf.printf "own_diag %s count=%d extra=[%s] fds=[%s] sockets=[%s]\n%!"
    label (fd_count ()) extra (format_fds ()) (format_owned_sockets ())

let test_fd_identity_diagnosis () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  snap "after-runtime-switch" "runtime/eventfd/pools";
  let run_held ~join_peer i =
    let socket = listen_v4 ~sw net in
    let port = tcp_port socket in
    let url = ws_url port in
    let job = make_job () in
    let peer_flow = Atomic.make None in
    let hold, hold_u = Eio.Promise.create () in
    let peer_done, peer_done_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
        Fun.protect
          ~finally:(fun () -> ignore (Eio.Promise.try_resolve peer_done_u ()))
          (fun () ->
            let flow, _ = Eio.Net.accept ~sw socket in
            Atomic.set peer_flow (Some flow);
            upgrade flow;
            Eio.Promise.resolve job.resolve_connected ();
            write_ws_text flow session_updated;
            write_ws_text flow response_done;
            Eio.Promise.await hold;
            close_peer flow));
    let result = settle ~sw ~net ~clock ~url ~job Normal false in
    (try Eio.Resource.close socket with _ -> ());
    let peer = Atomic.get peer_flow in
    let peer_diag =
      match peer with
      | None -> "peer=none"
      | Some flow -> probe_flow "accepted-peer" flow
    in
    snap ("held-after-op-" ^ string_of_int i) peer_diag;
    (match result with
    | Ok () -> ()
    | Error cause -> Alcotest.failf "diagnosis op %d failed: %a" i pp_stt cause);
    ignore (Eio.Promise.try_resolve hold_u ());
    Eio.Promise.await peer_done;
    snap ("after-peer-join-" ^ string_of_int i) "peer released+joined";
    ()
  in
  run_held ~join_peer:true 0;
  snap "after-warmup-join" "warmup peer joined";
  run_held ~join_peer:true 1;
  run_held ~join_peer:true 2;
  run_held ~join_peer:true 3

let () =
  match Sys.getenv_opt "PIE_OWN_WSS_CHILD" with
  | Some "1" ->
      let cert =
        match Sys.getenv_opt "SSL_CERT_FILE" with
        | Some p -> p
        | None -> failwith "wss child missing SSL_CERT_FILE"
      in
      let key =
        match Sys.getenv_opt "PIE_OWN_TLS_KEY" with
        | Some p -> p
        | None -> failwith "wss child missing PIE_OWN_TLS_KEY"
      in
      let rounds =
        match Sys.getenv_opt "PIE_OWN_ROUNDS" with
        | Some s -> int_of_string s
        | None -> 18
      in
      repeat_wss_in_process ~cert ~key ~rounds
  | _ ->
      Alcotest.run "session_ownership"
        [
          ( "diagnosis",
            [
              Alcotest.test_case
                "fd identity warmup then 3 held-peer buffered normal" `Slow
                test_fd_identity_diagnosis;
            ] );
          ( "gates",
            [
              Alcotest.test_case "buffered+streaming six scenarios localhost ws"
                `Slow test_gates_ws;
            ] );
          ( "repeat",
            [
              Alcotest.test_case "18 rounds x 12 ops ws localhost (216)" `Slow
                (repeat_ws ~rounds:18);
              Alcotest.test_case
                "18 rounds x 12 ops wss localhost dual-SAN (216)" `Slow
                (repeat_wss ~rounds:18);
            ] );
        ]
