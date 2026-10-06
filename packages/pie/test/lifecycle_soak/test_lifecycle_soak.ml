(* ^pcaps-n1rg *)

(* PIE003 soak: repeated realtime lifetimes (buffered and streaming, ws and
   wss) must settle their owned socket readers and return every descriptor they
   opened (^pienext-jlmd, ^pienext-4ps6, ^pienext-b3x8). Direct style: one Eio
   switch owns each session, the scripted peer is an Eio fiber, and every wait
   is bounded so a regression fails instead of hanging. *)

let ( let* ) = Result.bind

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

(* One local lifetime gets 2 s, the scripted peer gets 5 s to drain and join
   after it, and the whole soak stays inside 120 s. *)
let operation_timeout_seconds = 2
let peer_join_timeout_seconds = 5
let soak_timeout_seconds = 120

let fd_count ~(caps : < Ox_caps.Cap.fs_read ; .. >) () =
  let dir = Ox_caps.Cap_unix.opendir ~caps "/proc/self/fd" in
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

let classify_fds ~(caps : < Ox_caps.Cap.fs_read ; .. >) () =
  let dir = Ox_caps.Cap_unix.opendir ~caps "/proc/self/fd" in
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

let format_fds ~(caps : < Ox_caps.Cap.fs_read ; .. >) () =
  classify_fds ~caps ()
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

let owned_tcp_by_inode ~(caps : < Ox_caps.Cap.fs_read ; .. >) () =
  let load path =
    if not (Sys.file_exists path) then []
    else
      let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
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

let format_owned_sockets ~(caps : < Ox_caps.Cap.fs_read ; .. >) () =
  let rows = classify_fds ~caps () in
  let table = owned_tcp_by_inode ~caps () in
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
            (* Preserve the exception as data before this test classifies its outcome. *)
            match
              try Result.Ok (pp_sockaddr (Unix.getsockname unix_fd))
              with exn -> Result.Error exn
            with
            | Result.Ok value -> value
            | Result.Error _ -> "?"
          in
          let peer =
            (* Preserve the exception as data before this test classifies its outcome. *)
            match
              try Result.Ok (pp_sockaddr (Unix.getpeername unix_fd))
              with exn -> Result.Error exn
            with
            | Result.Ok value -> value
            | Result.Error _ -> "?"
          in
          Printf.sprintf "%s:rcfd=open local=%s peer=%s" label local peer)

let has_suffix suffix s =
  let sl = String.length suffix in
  let n = String.length s in
  n >= sl && String.sub s (n - sl) sl = suffix

let contains hay needle =
  let n = String.length needle in
  let rec loop i =
    i <= String.length hay - n && (String.sub hay i n = needle || loop (i + 1))
  in
  n = 0 || loop 0

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

let close_payload code reason =
  let payload = Bytes.create (2 + String.length reason) in
  Bytes.set payload 0 (Char.chr ((code lsr 8) land 0xff));
  Bytes.set payload 1 (Char.chr (code land 0xff));
  Bytes.blit_string reason 0 payload 2 (String.length reason);
  payload

let write_ws_close flow =
  Ox_http.Ws_codec.encode
    { fin = true; opcode = Close; payload = close_payload 1000 "" }
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

(* nhttp-7pp8 pienext-jlmd: complete-record TCP EOF is End_of_file.
   A reset may end the drain, but damaged TLS records must not be swallowed. *)
let drain_until_eof ~clock flow =
  let buf = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read flow buf with
    | 0 -> ()
    | _ -> loop ()
    | exception End_of_file -> ()
    | exception Eio.Io _ -> ()
  in
  Eio.Time.Timeout.run_exn (Eio.Time.Timeout.seconds clock 2.0) loop

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

let handle_peer ~clock ~on_connected ~close_flow scenario flow =
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
      (* Cancel while waiting for readiness, not while the client is still
         consuming the upgrade response. The first frame byte proves the
         WebSocket was acquired and the session update was sent. *)
      let first_frame_byte = Cstruct.create 1 in
      ignore (Eio.Flow.single_read flow first_frame_byte);
      on_connected ();
      drain_until_eof ~clock flow
  | Send_failure ->
      on_connected ();
      write_ws_text flow session_updated;
      close_flow ()
  | Midstream_cancel ->
      write_ws_text flow session_updated;
      on_connected ();
      drain_until_eof ~clock flow

let pcm = Bytes.make 320 '\001'

let session_update =
  Pie.Stt.Codex_realtime.build_pass1_session_update ~model:"gpt-realtime-2" ()

let run_buffered ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) ~sw ~net
    ~clock ?ca_file ~url () =
  Pie.Stt.Codex_realtime.run_realtime_session ~caps ~sw ~net ~mono_clock:clock
    ?ca_file ~url ~access_token:"synthetic-soak-token"
    ~mode:Pie.Stt.Codex_realtime.Transcription ~session_update
    ~input_sample_rate:16000 pcm

let run_streaming ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) ~sw ~net
    ~clock ?ca_file ~url ?on_audio_pull () =
  let supplied = ref false in
  let audio () =
    if !supplied then Ok None
    else (
      supplied := true;
      Option.iter (fun resolve -> resolve ()) on_audio_pull;
      Ok (Some pcm))
  in
  Pie.Stt.Codex_realtime.run_realtime_stream_session ~caps ~sw ~net
    ~mono_clock:clock ?ca_file ~url ~access_token:"synthetic-soak-token"
    ~mode:Pie.Stt.Codex_realtime.Transcription ~session_update
    ~input_sample_rate:16000 audio

(* One operation's outcome, normalized so the deadline is a case like any other. *)
type outcome = [ `Text of string | `Failed of string | `Timed_out ]

let expect_error ~needle (outcome : outcome) =
  match outcome with
  | `Text text -> Error (`Stt_error ("expected failure, got " ^ text))
  | `Failed message ->
      if contains message needle then Ok ()
      else
        Error
          (`Stt_error ("unexpected error (wanted " ^ needle ^ "): " ^ message))
  | `Timed_out ->
      Error
        (`Stt_error ("unexpected error (wanted " ^ needle ^ "): soak-timeout"))

let settle_op ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) ~sw ~net
    ~clock ?ca_file ~url ~job scenario streaming =
  let body () =
    match scenario with
    | Midstream_cancel ->
        run_streaming ~caps ~sw ~net ~clock ?ca_file ~url
          ~on_audio_pull:(fun () ->
            Eio.Promise.resolve job.resolve_client_ready ())
          ()
    | _ ->
        if streaming then run_streaming ~caps ~sw ~net ~clock ?ca_file ~url ()
        else run_buffered ~caps ~sw ~net ~clock ?ca_file ~url ()
  in
  let timed () =
    match
      Ox.Timeout.timeout_mono ~clock
        (Ox.Duration.seconds operation_timeout_seconds) (fun () ->
          match body () with
          | Ok text -> Ok text
          | Error (`Stt_error message) -> Error (`Stt_error message))
    with
    | Ok text -> `Text text
    | Error (`Stt_error message) -> `Failed message
    | Error `Timeout -> `Timed_out
  in
  match scenario with
  | Normal -> (
      match timed () with
      | `Text "ok" -> Ok ()
      | `Text text ->
          Error (`Stt_error ("normal-op: unexpected transcript " ^ text))
      | `Failed message -> Error (`Stt_error ("normal-op: " ^ message))
      | `Timed_out -> Error (`Stt_error "normal-op: soak-timeout"))
  | Pre_ready_error ->
      expect_error ~needle:"Codex realtime server error" (timed ())
  (* The ported session validates the frame before parse_event sees it, so a
     malformed readiness payload is "invalid event" (same rejection, kernel
     wording). *)
  | Malformed_readiness -> expect_error ~needle:"invalid event" (timed ())
  | Send_failure -> (
      match timed () with
      | `Timed_out -> Error (`Stt_error "send-failure timed out")
      | `Text text ->
          Error (`Stt_error ("expected send failure, got success: " ^ text))
      | `Failed _ -> Ok ())
  | Stalled_cancel | Midstream_cancel -> (
      let gate =
        if scenario = Midstream_cancel then job.client_ready else job.connected
      in
      match
        Eio.Fiber.first
          (fun () ->
            Eio.Promise.await gate;
            `Cancel)
          (fun () ->
            match timed () with
            | `Text _ -> `Body_success
            | `Failed message -> `Body_error message
            | `Timed_out -> `Body_error "soak-timeout")
      with
      | `Cancel -> Ok ()
      | `Body_success ->
          Error (`Stt_error "expected cancel winner, body succeeded")
      | `Body_error message ->
          Error (`Stt_error ("cancel-losing body finished first: " ^ message)))

type tls_material = { cert : string; key : string }

let with_localhost_tls
    ~(caps :
       < Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) f =
  let cert =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-soak-cert" ".pem"
  in
  let key = Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-soak-key" ".pem" in
  Fun.protect
    ~finally:(fun () ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      (match
         try Result.Ok (Ox_caps.Cap_sys.remove ~caps cert)
         with exn -> Result.Error exn
       with
      | Result.Ok value -> value
      | Result.Error _ -> ());
      (* Preserve the exception as data before this test classifies its outcome. *)
      match
        try Result.Ok (Ox_caps.Cap_sys.remove ~caps key)
        with exn -> Result.Error exn
      with
      | Result.Ok value -> value
      | Result.Error _ -> ())
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
      if Ox_caps.Cap_sys.command ~caps cmd <> 0 then
        Alcotest.fail "failed to generate synthetic localhost TLS material";
      f { cert; key })

let serve_accepted ~clock ~tls_ctx socket job =
  Eio.Switch.run (fun conn_sw ->
      let raw, _ = Eio.Net.accept ~sw:conn_sw socket in
      match tls_ctx with
      | None ->
          handle_peer ~clock
            ~on_connected:(fun () ->
              Eio.Promise.resolve job.resolve_connected ())
            ~close_flow:(fun () -> Eio.Flow.close raw)
            job.scenario raw
      | Some ctx ->
          let raw_flow =
            (raw
              :> [ Eio.Flow.two_way_ty | Eio.Resource.close_ty ] Eio.Resource.t)
          in
          let pre_raw = probe_flow "accepted-raw" raw in
          let tls =
            Ox_http.Tls_flow.create ~raw:raw_flow
              ~ssl:(Ox_http.Ox_http_tls_openssl.create_server_ssl ctx)
          in
          let pre_tls = probe_flow "accepted-tls" tls in
          Fun.protect
            ~finally:(fun () ->
              (* nhttp-rs5p pienext-4ps6: close the wrapped TLS flow. *)
              (try Eio.Flow.close tls with Eio.Io _ -> ());
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
                ~close_flow:(fun () -> Eio.Flow.close tls)
                job.scenario tls))

let log_localhost_addrs ~(caps : < Ox_caps.Cap.stdout ; .. >) () =
  let addrs =
    Unix.getaddrinfo "localhost" "" [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM ]
  in
  let pp ai =
    match ai.Unix.ai_addr with
    | Unix.ADDR_INET (ip, _) -> Unix.string_of_inet_addr ip
    | Unix.ADDR_UNIX path -> "unix:" ^ path
  in
  Ox_caps.Cap_stdlib.printf ~caps
    "lifecycle_soak getaddrinfo localhost=[%s]\n%!"
    (String.concat "," (List.map pp addrs))

let test_repeated_realtime_fd_soak
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; .. >) ~scheme ~host ~tls_ctx () =
  log_localhost_addrs ~caps ();
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.mono_clock env in
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
  let ca_file =
    match tls_ctx with
    | None -> None
    | Some _ -> Ox_caps.Cap_sys.getenv_opt ~caps "SSL_CERT_FILE"
  in
  let int_env name default =
    match Ox_caps.Cap_sys.getenv_opt ~caps name with
    | None -> default
    | Some s -> int_of_string s
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
  (* The scripted peer is a daemon: the switch cancels it only when the driving
     fiber is done, so a peer that never got its next job cannot hold the soak's
     release open. *)
  Eio.Fiber.fork_daemon ~sw (fun () ->
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
      loop total_jobs;
      `Stop_daemon);
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
    let n = fd_count ~caps () in
    samples := (label, n) :: !samples;
    Ox_caps.Cap_stdlib.printf ~caps "lifecycle_soak %s %s fd=%d\n%!" scheme
      label n;
    n
  in
  let progress label =
    incr op_index;
    let n = fd_count ~caps () in
    Ox_caps.Cap_stdlib.printf ~caps
      "lifecycle_soak %s op=%s i=%d fd=%d sockets=[%s] peer=[%s]\n%!" scheme
      label !op_index n
      (format_owned_sockets ~caps ())
      !last_peer_diag
  in
  let join_peer job =
    match
      Ox.Timeout.timeout_mono ~clock
        (Ox.Duration.seconds peer_join_timeout_seconds) (fun () ->
          Eio.Promise.await job.done_;
          Ok ())
    with
    | Ok () -> Ok ()
    | Error `Timeout ->
        Error
          (`Stt_error
             ("peer join timed out scenario=" ^ scenario_name job.scenario))
  in
  let operation () =
    let rec warmup_loop i =
      if i = 0 then Ok ()
      else
        let job = enqueue Normal in
        let* () =
          settle_op ~caps ~sw ~net ~clock ?ca_file ~url ~job Normal false
        in
        let* () = join_peer job in
        progress "warmup";
        warmup_loop (i - 1)
    in
    let rec phase scenario streaming remaining baseline =
      if remaining = 0 then Ok ()
      else
        let job = enqueue scenario in
        let* () =
          settle_op ~caps ~sw ~net ~clock ?ca_file ~url ~job scenario streaming
        in
        let* () = join_peer job in
        progress (scenario_name scenario);
        let n = record (scenario_name scenario) in
        if n > baseline then
          Error
            (`Stt_error
               (Printf.sprintf
                  "%s fd drift after op: baseline=%d now=%d fds=[%s] \
                   sockets=[%s] peer=[%s]"
                  (scenario_name scenario) baseline n (format_fds ~caps ())
                  (format_owned_sockets ~caps ())
                  !last_peer_diag))
        else phase scenario (not streaming) (remaining - 1) baseline
    in
    let rec all remaining_scenarios baseline =
      match remaining_scenarios with
      | [] -> Ok ()
      | scenario :: rest ->
          let* () = phase scenario false per_scenario baseline in
          let n = record ("phase-end:" ^ scenario_name scenario) in
          if n > baseline then
            Error
              (`Stt_error
                 (Printf.sprintf
                    "%s phase did not return to baseline %d (now %d) fds=[%s]"
                    (scenario_name scenario) baseline n (format_fds ~caps ())))
          else all rest baseline
    in
    let* () = warmup_loop warmup in
    let baseline = record "warmup" in
    all scenarios baseline
  in
  let outcome =
    match
      Ox.Timeout.timeout_mono ~clock (Ox.Duration.seconds soak_timeout_seconds)
        (fun () ->
          match operation () with
          | Ok () -> Ok ()
          | Error (`Stt_error message) -> Error (`Stt_error message))
    with
    | Ok () -> `Finished
    | Error (`Stt_error message) -> `Failed message
    | Error `Timeout -> `Soak_timed_out
  in
  let dump_samples () =
    Ox_caps.Cap_stdlib.printf ~caps
      "lifecycle_soak %s last_index=%d samples=%d\n%!" scheme !op_index
      (List.length !samples);
    List.iter
      (fun (label, n) ->
        Ox_caps.Cap_stdlib.printf ~caps "lifecycle_soak %s %s fd=%d\n%!" scheme
          label n)
      (List.rev !samples)
  in
  match outcome with
  | `Finished ->
      let rows = List.rev !samples in
      (match rows with
      | ("warmup", n) :: _ ->
          Ox_caps.Cap_stdlib.printf ~caps
            "lifecycle_soak %s warmup_fd=%d samples=%d\n%!" scheme n
            (List.length rows)
      | _ -> Alcotest.fail "missing warmup sample");
      List.iter
        (fun (label, n) ->
          Ox_caps.Cap_stdlib.printf ~caps "lifecycle_soak %s %s fd=%d\n%!"
            scheme label n)
        rows
  | `Failed message ->
      dump_samples ();
      Alcotest.failf "soak failed: %s" message
  | `Soak_timed_out ->
      dump_samples ();
      Alcotest.failf "soak failed: overall timeout after %ds"
        soak_timeout_seconds

let test_plain_127
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; .. >) () =
  test_repeated_realtime_fd_soak ~caps ~scheme:"ws" ~host:"127.0.0.1"
    ~tls_ctx:None ()

let test_plain_localhost
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; .. >) () =
  test_repeated_realtime_fd_soak ~caps ~scheme:"ws" ~host:"localhost"
    ~tls_ctx:None ()

let tls_child_env = "PIE_SOAK_TLS_CHILD"
let tls_key_env = "PIE_SOAK_TLS_KEY"

let run_tls_soak_in_process
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; .. >) ~cert ~key =
  let ctx =
    Ox_http.Ox_http_tls_openssl.create_server_ctx ~certificate_chain_file:cert
      ~private_key_file:key ~alpn_protocols:[ "http/1.1" ] ()
  in
  let host =
    match Ox_caps.Cap_sys.getenv_opt ~caps "PIE_SOAK_HOST" with
    | Some h -> h
    | None -> "127.0.0.1"
  in
  test_repeated_realtime_fd_soak ~caps ~scheme:"wss" ~host ~tls_ctx:(Some ctx)
    ()

let env_without ~(caps : < Ox_caps.Cap.env ; .. >) keys =
  Ox_caps.Cap_unix.environment ~caps ()
  |> Array.to_list
  |> List.filter (fun entry ->
      not
        (List.exists
           (fun key ->
             String.length entry >= String.length key + 1
             && String.sub entry 0 (String.length key) = key
             && entry.[String.length key] = '=')
           keys))
  |> Array.of_list

let spawn_tls_child
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdin
       ; Ox_caps.Cap.stdout
       ; .. >) material =
  let env =
    Array.append
      (env_without ~caps
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
    if Array.length Sys.argv > 0 && Sys.file_exists Sys.argv.(0) then
      Sys.argv.(0)
    else Sys.executable_name
  in
  let pid =
    Ox_caps.Cap_unix.create_process_env ~caps exe [| exe |] env
      (Unix.descr_of_in_channel (Ox_caps.Cap_stdlib.stdin ~caps))
      (Unix.descr_of_out_channel (Ox_caps.Cap_stdlib.stdout ~caps))
      (Unix.descr_of_out_channel (Ox_caps.Cap_stdlib.stderr ~caps))
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Alcotest.failf "tls soak child exited %d" n
  | Unix.WSIGNALED s -> Alcotest.failf "tls soak child signaled %d" s
  | Unix.WSTOPPED s -> Alcotest.failf "tls soak child stopped %d" s

let test_tls_soak
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_localhost_tls ~caps (spawn_tls_child ~caps)

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) =
  match Ox_caps.Cap_sys.getenv_opt ~caps tls_child_env with
  | Some "1" ->
      let cert =
        match Ox_caps.Cap_sys.getenv_opt ~caps "SSL_CERT_FILE" with
        | Some path -> path
        | None -> failwith "tls soak child missing SSL_CERT_FILE"
      in
      let key =
        match Ox_caps.Cap_sys.getenv_opt ~caps tls_key_env with
        | Some path -> path
        | None -> failwith "tls soak child missing PIE_SOAK_TLS_KEY"
      in
      run_tls_soak_in_process ~caps ~cert ~key
  | _ ->
      Alcotest.run "lifecycle_soak"
        [
          ( "fd",
            [
              Alcotest.test_case "plain ws 127.0.0.1 short" `Slow
                (test_plain_127 ~caps);
              Alcotest.test_case "plain ws localhost short" `Slow
                (test_plain_localhost ~caps);
              Alcotest.test_case "tls wss 127.0.0.1 short" `Slow
                (test_tls_soak ~caps);
            ] );
        ]

let () = main ~caps:(Ox_test.powerbox ())
