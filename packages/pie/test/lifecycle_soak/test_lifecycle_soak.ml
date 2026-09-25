open Eta.Syntax

type scenario =
  | Normal
  | Pre_ready_error
  | Malformed_readiness
  | Stalled_cancel
  | Send_failure
  | Midstream_cancel

let scenario_name = function
  | Normal -> "normal"
  | Pre_ready_error -> "pre-ready-error"
  | Malformed_readiness -> "malformed-readiness"
  | Stalled_cancel -> "stalled-cancel"
  | Send_failure -> "send-failure"
  | Midstream_cancel -> "midstream-cancel"

type job = {
  scenario : scenario;
  connected : unit Eio.Promise.t;
  resolve_connected : unit Eio.Promise.u;
  client_ready : unit Eio.Promise.t;
  resolve_client_ready : unit Eio.Promise.u;
  done_ : unit Eio.Promise.t;
  resolve_done : unit Eio.Promise.u;
}

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

let last_peer_diag = ref ""

let socket_inode target =
  let prefix = "socket:[" in
  let n = String.length target in
  let p = String.length prefix in
  if n > p && String.sub target 0 p = prefix && target.[n - 1] = ']' then
    Some (String.sub target p (n - p - 1))
  else None

let tcp_state = function
  | "01" -> "ESTABLISHED"
  | "02" -> "SYN_SENT"
  | "03" -> "SYN_RECV"
  | "04" -> "FIN_WAIT1"
  | "05" -> "FIN_WAIT2"
  | "06" -> "TIME_WAIT"
  | "07" -> "CLOSE"
  | "08" -> "CLOSE_WAIT"
  | "09" -> "LAST_ACK"
  | "0A" -> "LISTEN"
  | "0B" -> "CLOSING"
  | other -> other

let hex_ipv4_port hex =
  match String.split_on_char ':' hex with
  | [ ip; port ] when String.length ip = 8 ->
      let b i = int_of_string ("0x" ^ String.sub ip i 2) in
      Printf.sprintf "%d.%d.%d.%d:%d" (b 6) (b 4) (b 2) (b 0)
        (int_of_string ("0x" ^ port))
  | _ -> hex

let owned_tcp_by_inode () =
  let load path =
    if not (Sys.file_exists path) then []
    else
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec loop acc =
            match input_line ic with
            | exception End_of_file -> acc
            | line -> (
                let cols =
                  line |> String.split_on_char ' '
                  |> List.filter (fun c -> c <> "")
                in
                match cols with
                | _sl :: local :: remote :: st :: _tx :: _rx :: _tr :: _tm
                  :: _retr :: _uid :: _timeout :: inode :: _ ->
                    loop ((inode, local, remote, st) :: acc)
                | _ -> loop acc)
          in
          ignore (try input_line ic with End_of_file -> "");
          loop [])
  in
  load "/proc/self/net/tcp" @ load "/proc/self/net/tcp6"

let format_owned_sockets () =
  let rows = classify_fds () in
  let table = owned_tcp_by_inode () in
  rows
  |> List.filter_map (fun (name, target) ->
      match socket_inode target with
      | None -> None
      | Some inode ->
          let tcp =
            match List.find_opt (fun (ino, _, _, _) -> ino = inode) table with
            | None -> "inode=" ^ inode ^ " tcp=absent"
            | Some (_, local, remote, st) ->
                Printf.sprintf "inode=%s local=%s remote=%s state=%s" inode
                  (hex_ipv4_port local) (hex_ipv4_port remote) (tcp_state st)
          in
          Some (name ^ "->" ^ target ^ " " ^ tcp))
  |> String.concat "; "

let pp_sockaddr = function
  | Unix.ADDR_INET (ip, port) ->
      Unix.string_of_inet_addr ip ^ ":" ^ string_of_int port
  | Unix.ADDR_UNIX path -> "unix:" ^ path

let probe_flow label flow =
  match Eio_unix.Resource.fd_opt flow with
  | None -> label ^ ":fd_opt=none"
  | Some fd ->
      Eio_unix.Fd.use fd
        ~if_closed:(fun () -> label ^ ":rcfd=closed")
        (fun unix_fd ->
          let local =
            try pp_sockaddr (Unix.getsockname unix_fd) with _ -> "?"
          in
          let peer =
            try pp_sockaddr (Unix.getpeername unix_fd) with _ -> "?"
          in
          Printf.sprintf "%s:rcfd=open local=%s peer=%s" label local peer)

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
  Eta_http_eio.Ws.Codec.encode
    { fin = true; opcode = Text; payload = Bytes.of_string payload }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let close_payload code reason =
  let payload = Bytes.create (2 + String.length reason) in
  Bytes.set payload 0 (Char.chr ((code lsr 8) land 0xff));
  Bytes.set payload 1 (Char.chr (code land 0xff));
  Bytes.blit_string reason 0 payload 2 (String.length reason);
  payload

let write_ws_close flow =
  Eta_http_eio.Ws.Codec.encode
    { fin = true; opcode = Close; payload = close_payload 1000 "" }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let write_ws_switching_response flow key =
  let response =
    "HTTP/1.1 101 Switching Protocols\r\n" ^ "Upgrade: websocket\r\n"
    ^ "Connection: keep-alive, Upgrade\r\n" ^ "Sec-WebSocket-Accept: "
    ^ Eta_http_eio.Ws.Codec.accept_key ~sha1:Eta_http_tls_openssl.sha1 key
    ^ "\r\n\r\n"
  in
  Eio.Flow.copy_string response flow

let drain_until_eof ~clock flow =
  let buf = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read flow buf with
    | 0 -> ()
    | _ -> loop ()
    | exception End_of_file -> ()
  in
  Eio.Time.with_timeout_exn clock 2.0 loop

let upgrade flow =
  let head = read_http_head flow in
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> failwith "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key

let session_updated = {|{"type":"session.updated"}|}

let transcript_done =
  {|{"type":"conversation.item.input_audio_transcription.completed","transcript":"ok"}|}

let server_error =
  {|{"type":"error","error":{"message":"pre-ready","code":"bad_request"}}|}

let handle_peer ~clock ~on_connected scenario flow =
  upgrade flow;
  match scenario with
  | Normal ->
      on_connected ();
      write_ws_text flow session_updated;
      write_ws_text flow transcript_done;
      write_ws_close flow;
      drain_until_eof ~clock flow
  | Pre_ready_error ->
      on_connected ();
      write_ws_text flow server_error;
      write_ws_close flow;
      drain_until_eof ~clock flow
  | Malformed_readiness ->
      on_connected ();
      write_ws_text flow "not-json";
      write_ws_close flow;
      drain_until_eof ~clock flow
  | Stalled_cancel ->
      on_connected ();
      drain_until_eof ~clock flow
  | Send_failure ->
      on_connected ();
      write_ws_text flow session_updated;
      Eio.Flow.close flow
  | Midstream_cancel ->
      write_ws_text flow session_updated;
      on_connected ();
      drain_until_eof ~clock flow

let pcm = Bytes.make 320 '\001'

let session_update =
  Pie.Stt.Codex_realtime.build_transcription_session_update
    ~model:"gpt-realtime-2" ()

let run_buffered ~sw ~net ~url =
  Pie.Stt.Codex_realtime.run_realtime_session ~sw ~net ~url
    ~access_token:"synthetic-soak-token"
    ~mode:Pie.Stt.Codex_realtime.Transcription ~session_update
    ~input_sample_rate:16000 pcm

let run_streaming ~sw ~net ~url ?on_audio_pull () =
  let audio =
    match on_audio_pull with
    | None -> Eta_stream.Stream.from_iterable [ pcm ]
    | Some resolve ->
        Eta_stream.Stream.from_effect
          (Eta.Effect.sync (fun () ->
               resolve ();
               pcm))
  in
  Pie.Stt.Codex_realtime.run_realtime_stream_session ~sw ~net ~url
    ~access_token:"synthetic-soak-token"
    ~mode:Pie.Stt.Codex_realtime.Transcription ~session_update
    ~input_sample_rate:16000 audio

let expect_error ~needle timed =
  timed
  |> Eta.Effect.bind (fun _ ->
      Eta.Effect.fail (`Stt_error "expected failure, got success"))
  |> Eta.Effect.bind_error (fun (`Stt_error msg) ->
      if
        let n = String.length needle in
        let h = String.length msg in
        let rec loop i =
          i <= h - n && (String.sub msg i n = needle || loop (i + 1))
        in
        n = 0 || loop 0
      then Eta.Effect.unit
      else
        Eta.Effect.fail
          (`Stt_error ("unexpected error (wanted " ^ needle ^ "): " ^ msg)))

let cause_contains needle cause =
  let rendered =
    Format.asprintf "%a"
      (Eta.Cause.pp (fun fmt (`Stt_error m) -> Format.pp_print_string fmt m))
      cause
  in
  let n = String.length needle in
  let h = String.length rendered in
  let rec loop i =
    i <= h - n && (String.sub rendered i n = needle || loop (i + 1))
  in
  n = 0 || loop 0

let settle_op ~sw ~net ~url ~job scenario streaming =
  let body =
    match scenario with
    | Midstream_cancel ->
        run_streaming ~sw ~net ~url
          ~on_audio_pull:(fun () ->
            Eio.Promise.resolve job.resolve_client_ready ())
          ()
    | _ ->
        if streaming then run_streaming ~sw ~net ~url ()
        else run_buffered ~sw ~net ~url
  in
  match scenario with
  | Normal ->
      body
      |> Eta.Effect.timeout_as (Eta.Duration.ms 800)
           ~on_timeout:(`Stt_error "soak-timeout")
      |> Eta.Effect.bind (fun _ -> Eta.Effect.unit)
      |> Eta.Effect.bind_error (fun (`Stt_error msg) ->
          Eta.Effect.fail (`Stt_error ("normal-op: " ^ msg)))
  | Pre_ready_error ->
      expect_error ~needle:"Codex realtime server error"
        (body
        |> Eta.Effect.timeout_as (Eta.Duration.ms 800)
             ~on_timeout:(`Stt_error "soak-timeout"))
  | Malformed_readiness ->
      expect_error ~needle:"not valid JSON"
        (body
        |> Eta.Effect.timeout_as (Eta.Duration.ms 800)
             ~on_timeout:(`Stt_error "soak-timeout"))
  | Send_failure ->
      body
      |> Eta.Effect.timeout_as (Eta.Duration.ms 800)
           ~on_timeout:(`Stt_error "soak-timeout")
      |> Eta.Effect.to_exit
      |> Eta.Effect.bind (function
        | Eta.Exit.Ok _ ->
            Eta.Effect.fail (`Stt_error "expected send failure, got success")
        | Eta.Exit.Error cause when cause_contains "soak-timeout" cause ->
            Eta.Effect.fail (`Stt_error "send-failure timed out")
        | Eta.Exit.Error _ -> Eta.Effect.unit)
  | Stalled_cancel | Midstream_cancel ->
      let gate =
        if scenario = Midstream_cancel then job.client_ready else job.connected
      in
      let cancel =
        Eta.Effect.sync (fun () -> Eio.Promise.await gate)
        |> Eta.Effect.map (fun () -> `Cancel)
      in
      Eta.Effect.race
        [ body |> Eta.Effect.map (fun result -> `Body result); cancel ]
      |> Eta.Effect.bind (function
        | `Cancel -> Eta.Effect.unit
        | `Body _ ->
            Eta.Effect.fail
              (`Stt_error "expected cancel winner, body succeeded"))

type tls_material = { cert : string; key : string }

let with_localhost_tls f =
  let cert = Filename.temp_file "pie-soak-cert" ".pem" in
  let key = Filename.temp_file "pie-soak-key" ".pem" in
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
      f { cert; key })

let close_tls_and_raw tls raw =
  let tls_exn =
    try
      Eio.Flow.close tls;
      None
    with exn -> Some exn
  in
  let raw_exn =
    try
      Eio.Flow.close raw;
      None
    with exn -> Some exn
  in
  match (tls_exn, raw_exn) with
  | Some exn, _ | None, Some exn -> raise exn
  | None, None -> ()

let serve_accepted ~clock ~tls_ctx socket job =
  Eio.Switch.run (fun conn_sw ->
      let raw, _ = Eio.Net.accept ~sw:conn_sw socket in
      match tls_ctx with
      | None ->
          handle_peer ~clock
            ~on_connected:(fun () ->
              Eio.Promise.resolve job.resolve_connected ())
            job.scenario raw
      | Some ctx ->
          let raw_flow =
            (raw
              :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t)
          in
          let pre_raw = probe_flow "accepted-raw" raw in
          let tls, _epoch =
            try Eta_http_eio.Tls.Eio.server_of_flow_with_context ctx raw_flow
            with exn ->
              (try Eio.Flow.close raw with _ -> ());
              raise exn
          in
          let pre_tls = probe_flow "accepted-tls" tls in
          Fun.protect
            ~finally:(fun () ->
              close_tls_and_raw tls raw;
              last_peer_diag :=
                String.concat " | "
                  [
                    pre_raw;
                    pre_tls;
                    probe_flow "post-close-tls" tls;
                    probe_flow "post-close-raw" raw;
                  ])
            (fun () ->
              handle_peer ~clock
                ~on_connected:(fun () ->
                  Eio.Promise.resolve job.resolve_connected ())
                job.scenario tls))

let log_localhost_addrs () =
  let addrs =
    Unix.getaddrinfo "localhost" "" [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
  in
  let pp ai =
    match ai.Unix.ai_addr with
    | Unix.ADDR_INET (ip, _) -> Unix.string_of_inet_addr ip
    | Unix.ADDR_UNIX path -> "unix:" ^ path
  in
  Printf.printf "lifecycle_soak getaddrinfo localhost=[%s]\n%!"
    (String.concat "," (List.map pp addrs))

let test_repeated_realtime_fd_soak ~scheme ~host ~tls_ctx () =
  log_localhost_addrs ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let jobs = Eio.Stream.create 8 in
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:8 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "expected TCP listener"
  in
  let url =
    Printf.sprintf "%s://%s:%d/v1/realtime?model=gpt-realtime-2" scheme host
      port
  in
  let int_env name default =
    match Sys.getenv_opt name with None -> default | Some s -> int_of_string s
  in
  let warmup = int_env "PIE_SOAK_WARMUP" 8 in
  let per_scenario = int_env "PIE_SOAK_PER_SCENARIO" 34 in
  let scenarios =
    [
      Normal;
      Pre_ready_error;
      Malformed_readiness;
      Stalled_cancel;
      Send_failure;
      Midstream_cancel;
    ]
  in
  let total_jobs = warmup + (List.length scenarios * per_scenario) in
  Eio.Fiber.fork ~sw (fun () ->
      let rec loop remaining =
        if remaining = 0 then ()
        else
          let job = Eio.Stream.take jobs in
          (try serve_accepted ~clock ~tls_ctx socket job
           with Eio.Time.Timeout ->
             failwith
               ("lifecycle_soak peer drain timed out scenario="
              ^ scenario_name job.scenario));
          Eio.Promise.resolve job.resolve_done ();
          loop (remaining - 1)
      in
      loop total_jobs);
  let enqueue scenario =
    let connected, resolve_connected = Eio.Promise.create () in
    let client_ready, resolve_client_ready = Eio.Promise.create () in
    let done_, resolve_done = Eio.Promise.create () in
    let job =
      {
        scenario;
        connected;
        resolve_connected;
        client_ready;
        resolve_client_ready;
        done_;
        resolve_done;
      }
    in
    Eio.Stream.add jobs job;
    job
  in
  let samples = ref [] in
  let op_index = ref 0 in
  let record label =
    let n = fd_count () in
    samples := (label, n) :: !samples;
    Printf.printf "lifecycle_soak %s %s fd=%d\n%!" scheme label n;
    n
  in
  let progress label =
    incr op_index;
    let n = fd_count () in
    Printf.printf
      "lifecycle_soak %s op=%s i=%d fd=%d sockets=[%s] peer=[%s]\n%!" scheme
      label !op_index n (format_owned_sockets ()) !last_peer_diag
  in
  let operation =
    let rec warmup_loop i =
      if i = 0 then Eta.Effect.unit
      else
        let job = enqueue Normal in
        let* () = settle_op ~sw ~net ~url ~job Normal false in
        let* () =
          Eta.Effect.sync (fun () ->
              Eio.Promise.await job.done_;
              progress "warmup")
        in
        warmup_loop (i - 1)
    in
    let rec phase scenario streaming remaining baseline =
      if remaining = 0 then Eta.Effect.unit
      else
        let job = enqueue scenario in
        let* () = settle_op ~sw ~net ~url ~job scenario streaming in
        let* () =
          Eta.Effect.sync (fun () ->
              Eio.Promise.await job.done_;
              progress (scenario_name scenario))
        in
        let* () =
          Eta.Effect.sync (fun () ->
              let n = record (scenario_name scenario) in
              if n > baseline then
                Alcotest.failf
                  "%s fd drift after op: baseline=%d now=%d fds=[%s] \
                   sockets=[%s] peer=[%s]"
                  (scenario_name scenario) baseline n (format_fds ())
                  (format_owned_sockets ()) !last_peer_diag)
        in
        phase scenario (not streaming) (remaining - 1) baseline
    in
    let rec all remaining_scenarios baseline =
      match remaining_scenarios with
      | [] -> Eta.Effect.unit
      | scenario :: rest ->
          let* () = phase scenario false per_scenario baseline in
          let* () =
            Eta.Effect.sync (fun () ->
                let n = record ("phase-end:" ^ scenario_name scenario) in
                if n > baseline then
                  Alcotest.failf
                    "%s phase did not return to baseline %d (now %d) fds=[%s]"
                    (scenario_name scenario) baseline n (format_fds ()))
          in
          all rest baseline
    in
    let* () = warmup_loop warmup in
    let* baseline = Eta.Effect.sync (fun () -> record "warmup") in
    all scenarios baseline
  in
  match
    Eta_eio.run_once ~sw ~clock
      (operation
      |> Eta.Effect.timeout_as (Eta.Duration.seconds 120)
           ~on_timeout:(`Stt_error "soak overall timeout"))
  with
  | Ok () ->
      let rows = List.rev !samples in
      (match rows with
      | ("warmup", n) :: _ ->
          Printf.printf "lifecycle_soak %s warmup_fd=%d samples=%d\n%!" scheme n
            (List.length rows)
      | _ -> Alcotest.fail "missing warmup sample");
      List.iter
        (fun (label, n) ->
          Printf.printf "lifecycle_soak %s %s fd=%d\n%!" scheme label n)
        rows
  | Error cause ->
      Printf.printf "lifecycle_soak %s last_index=%d samples=%d\n%!" scheme
        !op_index (List.length !samples);
      List.iter
        (fun (label, n) ->
          Printf.printf "lifecycle_soak %s %s fd=%d\n%!" scheme label n)
        (List.rev !samples);
      Alcotest.failf "soak failed: %a"
        (Eta.Cause.pp (fun fmt (`Stt_error m) -> Format.pp_print_string fmt m))
        cause

let test_plain_127 () =
  test_repeated_realtime_fd_soak ~scheme:"ws" ~host:"127.0.0.1" ~tls_ctx:None ()

let test_plain_localhost () =
  test_repeated_realtime_fd_soak ~scheme:"ws" ~host:"localhost" ~tls_ctx:None ()

let tls_child_env = "PIE_SOAK_TLS_CHILD"
let tls_key_env = "PIE_SOAK_TLS_KEY"

let run_tls_soak_in_process ~cert ~key =
  let server_config =
    Eta_http.Tls.Config.default_server ~certificate_chain_file:cert
      ~private_key_file:key ~alpn_protocols:[ "http/1.1" ] ()
  in
  let tls_ctx = Some (Eta_http_eio.Tls.Eio.server_context server_config) in
  let host =
    match Sys.getenv_opt "PIE_SOAK_HOST" with
    | Some h -> h
    | None -> "127.0.0.1"
  in
  test_repeated_realtime_fd_soak ~scheme:"wss" ~host ~tls_ctx ()

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

let spawn_tls_child material =
  let env =
    Array.append
      (env_without
         [
           "SSL_CERT_FILE";
           "SSL_CERT_DIR";
           "NIX_SSL_CERT_FILE";
           "OPENSSL_CONF";
           tls_key_env;
           tls_child_env;
           "PIE_SOAK_HOST";
         ])
      [|
        "SSL_CERT_FILE=" ^ material.cert;
        tls_key_env ^ "=" ^ material.key;
        tls_child_env ^ "=1";
        "PIE_SOAK_HOST=127.0.0.1";
      |]
  in
  let exe =
    if Array.length Sys.argv > 0 && Sys.file_exists Sys.argv.(0) then Sys.argv.(0)
    else Sys.executable_name
  in
  let pid =
    Unix.create_process_env exe [| exe |] env Unix.stdin Unix.stdout Unix.stderr
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Alcotest.failf "tls soak child exited %d" n
  | Unix.WSIGNALED s -> Alcotest.failf "tls soak child signaled %d" s
  | Unix.WSTOPPED s -> Alcotest.failf "tls soak child stopped %d" s

let test_tls_soak () = with_localhost_tls spawn_tls_child

let () =
  match Sys.getenv_opt tls_child_env with
  | Some "1" ->
      let cert =
        match Sys.getenv_opt "SSL_CERT_FILE" with
        | Some path -> path
        | None -> failwith "tls soak child missing SSL_CERT_FILE"
      in
      let key =
        match Sys.getenv_opt tls_key_env with
        | Some path -> path
        | None -> failwith "tls soak child missing PIE_SOAK_TLS_KEY"
      in
      run_tls_soak_in_process ~cert ~key
  | _ ->
      Alcotest.run "lifecycle_soak"
        [
          ( "fd",
            [
              Alcotest.test_case "plain ws 127.0.0.1 short" `Slow test_plain_127;
              Alcotest.test_case "plain ws localhost short" `Slow
                test_plain_localhost;
              Alcotest.test_case "tls wss 127.0.0.1 short" `Slow test_tls_soak;
            ] );
        ]
