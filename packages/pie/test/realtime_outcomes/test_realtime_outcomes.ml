let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

(* PIE011: readiness vs terminal status on a local scripted WebSocket. *)

module Codex = Pie.Stt.Codex_realtime

let pcm = Bytes.make 320 '\001'
let pp_stt ppf = function `Stt_error msg -> Format.pp_print_string ppf msg

let contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let cause_text (`Stt_error message) = message

let has_suffix suffix value =
  let suffix_len = String.length suffix in
  let value_len = String.length value in
  value_len >= suffix_len
  && String.equal suffix (String.sub value (value_len - suffix_len) suffix_len)

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
      match String.index_opt line ':' with
      | None -> None
      | Some index ->
          let actual =
            String.sub line 0 index |> String.trim |> String.lowercase_ascii
          in
          if String.equal actual expected then
            Some
              (String.sub line (index + 1) (String.length line - index - 1)
              |> String.trim)
          else None)

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

let write_ws_close flow =
  let payload = Bytes.create 2 in
  Bytes.set payload 0 (Char.chr 3);
  Bytes.set payload 1 (Char.chr 232);
  Ox_http.Ws_codec.encode { fin = true; opcode = Close; payload }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

type ws_frame = Text of string | Close | Other

let read_ws_frame flow =
  let read_exact len =
    let cs = Cstruct.create len in
    let rec loop off =
      if off < len then
        match Eio.Flow.single_read flow (Cstruct.sub cs off (len - off)) with
        | 0 -> raise End_of_file
        | n -> loop (off + n)
    in
    loop 0;
    Cstruct.to_bytes cs
  in
  let header = read_exact 2 in
  let opcode = Char.code (Bytes.get header 0) land 0x0f in
  let masked = Char.code (Bytes.get header 1) land 0x80 <> 0 in
  let len0 = Char.code (Bytes.get header 1) land 0x7f in
  let payload_len =
    if len0 = 126 then
      let ext = read_exact 2 in
      (Char.code (Bytes.get ext 0) lsl 8) lor Char.code (Bytes.get ext 1)
    else if len0 = 127 then (
      let ext = read_exact 8 in
      let v = ref 0 in
      for i = 0 to 7 do
        v := (!v lsl 8) lor Char.code (Bytes.get ext i)
      done;
      !v)
    else len0
  in
  let mask_key = if masked then Some (read_exact 4) else None in
  let payload = read_exact payload_len in
  (match mask_key with
  | Some key ->
      for i = 0 to payload_len - 1 do
        let b =
          Char.code (Bytes.get payload i)
          lxor Char.code (Bytes.get key (i mod 4))
        in
        Bytes.set payload i (Char.chr b)
      done
  | None -> ());
  match opcode with
  | 1 -> Text (Bytes.to_string payload)
  | 8 -> Close
  | _ -> Other

let write_ws_switching_response flow key =
  let response =
    "HTTP/1.1 101 Switching Protocols\r\n" ^ "Upgrade: websocket\r\n"
    ^ "Connection: keep-alive, Upgrade\r\n" ^ "Sec-WebSocket-Accept: "
    ^ Ox_http.Ws_codec.accept_key ~sha1:Ox_http.Ox_http_tls_openssl.sha1 key
    ^ "\r\n\r\n"
  in
  Eio.Flow.copy_string response flow

let tcp_port = function
  | `Tcp (_, port) -> port
  | _ -> Alcotest.fail "expected tcp port"

let is_audio_append type_name =
  String.equal type_name "input_audio_buffer.append"
  || String.equal type_name "session.input_audio_buffer.append"

let handshake flow =
  let head = read_http_head flow in
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> Alcotest.fail "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key

type script = {
  after_handshake : string list;
  after_session_update : string list;
  after_audio : string list;
}

let serve ?(close_peer = write_ws_close) ~saw_audio script flow =
  handshake flow;
  List.iter (write_ws_text flow) script.after_handshake;
  let rec loop stage =
    match read_ws_frame flow with
    | exception End_of_file -> ()
    | Close -> ()
    | Other -> loop stage
    | Text payload -> (
        match Yojson.Safe.from_string payload with
        | exception _ -> loop stage
        | `Assoc fields -> (
            match List.assoc_opt "type" fields with
            | Some (`String "session.update") ->
                List.iter (write_ws_text flow) script.after_session_update;
                let has_session_updated =
                  List.exists
                    (fun msg -> contains msg "\"session.updated\"")
                    script.after_session_update
                in
                let has_error =
                  List.exists
                    (fun msg -> contains msg "\"type\":\"error\"")
                    script.after_session_update
                in
                if has_session_updated || has_error then loop `Updated
                else close_peer flow
            | Some (`String type_name) when is_audio_append type_name ->
                saw_audio := true;
                if stage <> `Audio then (
                  List.iter (write_ws_text flow) script.after_audio;
                  close_peer flow);
                loop `Audio
            | _ -> loop stage)
        | _ -> loop stage)
  in
  loop `Init

let run_session_with ?tls ?(peer_close = `Websocket) ~mode ~script ~drain_ms ()
    =
  let saw_audio = ref false in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = tcp_port (Eio.Net.listening_addr socket) in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run @@ fun conn_sw ->
      let raw, _ = Eio.Net.accept ~sw:conn_sw socket in
      let flow =
        match tls with
        | None -> (raw :> Ox_http.Tls_flow.ty Eio.Resource.t)
        | Some (ctx, _) ->
            Ox_http.Tls_flow.create
              ~raw:(raw :> Ox_http.Tls_flow.ty Eio.Resource.t)
              ~ssl:(Ox_http.Ox_http_tls_openssl.create_server_ssl ctx)
      in
      let close_peer flow =
        match peer_close with
        | `Websocket -> write_ws_close flow
        | `Notify -> Eio.Flow.close flow
        | `Tcp_eof -> Eio.Flow.close raw
        | `Truncated ->
            Eio.Flow.copy_string "\x17\x03\x03\x00" raw;
            Eio.Flow.close raw
      in
      try serve ~close_peer ~saw_audio script flow with _ -> ());
  let scheme = if Option.is_some tls then "wss" else "ws" in
  let ca_file = Option.map snd tls in
  let url = Printf.sprintf "%s://127.0.0.1:%d" scheme port in
  let session_update =
    match mode with
    | Codex.Transcription ->
        Some
          (Codex.build_transcription_session_update
             ~model:"gpt-4o-mini-transcribe" ())
    | Codex.Conversation ->
        Some
          (Codex.build_conversation_session_update ~model:"gpt-realtime-2.1"
             ~prompt:"p")
    | Codex.Translation ->
        Some (Codex.build_translation_session_update ~prompt:"p" ())
  in
  let result =
    try
      Eio.Time.with_timeout_exn clock 3.0 (fun () ->
          Codex.run_realtime_session ~caps ~sw ~net ~clock ?ca_file ~url
            ~access_token:"synthetic" ~mode ?session_update
            ~translation_output_drain_ms:drain_ms ~input_sample_rate:16000 pcm)
    with Eio.Time.Timeout -> Error (`Stt_error "timeout")
  in
  (result, !saw_audio)

let run_session ~mode ~script ~drain_ms =
  run_session_with ~mode ~script ~drain_ms ()

let created = {|{"type":"session.created"}|}
let created_tx = {|{"type":"transcription_session.created"}|}
let updated = {|{"type":"session.updated"}|}
let updated_tx = {|{"type":"transcription_session.updated"}|}

let error_evt =
  {|{"type":"error","error":{"message":"SECRET_MARKER","code":"bad"}}|}

let delta = {|{"type":"response.output_text.delta","delta":"hel"}|}

let tx_final =
  {|{"type":"conversation.item.input_audio_transcription.completed","transcript":"hello"}|}

let done_status status =
  Printf.sprintf {|{"type":"response.done","response":{"status":"%s"}}|} status

let done_missing = {|{"type":"response.done"}|}
let done_malformed = {|{"type":"response.done","response":"nope"}|}

let expect_error needle (result, _) =
  match result with
  | Ok text -> Alcotest.failf "expected error, got %s" text
  | Error cause ->
      let text = cause_text cause in
      Alcotest.(check bool) needle true (contains text needle);
      Alcotest.(check bool) "no secret" false (contains text "SECRET_MARKER")

let test_created_only_sends_no_audio () =
  let result, saw_audio =
    run_session ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [];
          after_audio = [];
        }
      ~drain_ms:20
  in
  expect_error "acknowledgement" (result, saw_audio);
  Alcotest.(check bool) "no audio before ack" false saw_audio

let test_config_reject_sends_no_audio () =
  let result, saw_audio =
    run_session ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ error_evt ];
          after_audio = [];
        }
      ~drain_ms:20
  in
  expect_error "server error" (result, saw_audio);
  Alcotest.(check bool) "no audio after reject" false saw_audio

let test_delayed_ack_then_transcription_completed () =
  let result, saw_audio =
    run_session ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created_tx ];
          after_session_update = [ updated ];
          after_audio = [ tx_final ];
        }
      ~drain_ms:20
  in
  Alcotest.(check bool) "audio after ack" true saw_audio;
  match result with
  | Ok text -> Alcotest.(check string) "final" "hello" text
  | Error cause -> Alcotest.failf "expected success: %s" (cause_text cause)

let test_conversation_completed () =
  let result, _ =
    run_session ~mode:Codex.Conversation
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta; done_status "completed" ];
        }
      ~drain_ms:20
  in
  match result with
  | Ok text -> Alcotest.(check string) "delta text" "hel" text
  | Error cause -> Alcotest.failf "expected success: %s" (cause_text cause)

let test_failed_after_partial () =
  let result, _ =
    run_session ~mode:Codex.Conversation
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta; done_status "failed" ];
        }
      ~drain_ms:20
  in
  expect_error "failed" (result, ());
  match result with
  | Ok text -> Alcotest.failf "partial injected as success: %s" text
  | Error _ -> ()

let test_cancelled_after_partial () =
  let result, _ =
    run_session ~mode:Codex.Conversation
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta; done_status "cancelled" ];
        }
      ~drain_ms:20
  in
  expect_error "cancelled" (result, ())

let test_incomplete_after_partial () =
  let result, _ =
    run_session ~mode:Codex.Conversation
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta; done_status "incomplete" ];
        }
      ~drain_ms:20
  in
  expect_error "incomplete" (result, ())

let test_missing_status_is_not_success () =
  let result, _ =
    run_session ~mode:Codex.Conversation
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta; done_missing ];
        }
      ~drain_ms:20
  in
  expect_error "incomplete" (result, ())

let test_malformed_terminal_is_not_success () =
  let result, _ =
    run_session ~mode:Codex.Conversation
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta; done_malformed ];
        }
      ~drain_ms:20
  in
  expect_error "incomplete" (result, ())

let test_wrong_ack_sends_no_audio () =
  let result, saw_audio =
    run_session ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated_tx ];
          after_audio = [ tx_final ];
        }
      ~drain_ms:20
  in
  expect_error "acknowledgement" (result, saw_audio);
  Alcotest.(check bool) "no audio on wrong ack" false saw_audio

let test_transcription_ignores_conversation_done () =
  let result, saw_audio =
    run_session ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta; done_status "completed" ];
        }
      ~drain_ms:20
  in
  Alcotest.(check bool) "audio after correct ack" true saw_audio;
  expect_error "incomplete" (result, ())

let test_conversation_ignores_transcription_final () =
  let result, _ =
    run_session ~mode:Codex.Conversation
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ tx_final ];
        }
      ~drain_ms:20
  in
  expect_error "incomplete" (result, ())

let test_translation_ignores_response_done () =
  let result, saw_audio =
    run_session ~mode:Codex.Translation
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta; done_status "completed" ];
        }
      ~drain_ms:20
  in
  Alcotest.(check bool) "audio after ack" true saw_audio;
  expect_error "incomplete" (result, ())

let test_translation_drain_is_incomplete () =
  let result, saw_audio =
    run_session ~mode:Codex.Translation
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta ];
        }
      ~drain_ms:20
  in
  Alcotest.(check bool) "audio after ack" true saw_audio;
  expect_error "incomplete" (result, ())

(* nhttp-7pp8 nhttp-rs5p pienext-jlmd pienext-4ps6: TLS EOF must reach the
   Codex session as a closed WebSocket, never as a spurious TLS failure or a
   successful incomplete response. TLS close_notify alone is not a WS close. *)
let with_tls test =
  let cert = Filename.temp_file "pie-r38-cert-" ".pem" in
  let key = Filename.temp_file "pie-r38-key-" ".pem" in
  Fun.protect
    ~finally:(fun () ->
      Sys.remove cert;
      Sys.remove key)
    (fun () ->
      let command =
        Printf.sprintf
          "openssl req -x509 -newkey rsa:2048 -nodes -sha256 -days 1 -subj \
           /CN=localhost -addext %s -keyout %s -out %s >/dev/null 2>&1"
          (Filename.quote "subjectAltName=DNS:localhost,IP:127.0.0.1")
          (Filename.quote key) (Filename.quote cert)
      in
      if Sys.command command <> 0 then
        Alcotest.fail "TLS fixture generation failed";
      let ctx =
        Ox_http.Ox_http_tls_openssl.create_server_ctx
          ~certificate_chain_file:cert ~private_key_file:key
          ~alpn_protocols:[ "http/1.1" ] ()
      in
      test (ctx, cert))

let test_tls_peer_close peer_close diagnostic () =
  with_tls (fun tls ->
      let result, saw_audio =
        run_session_with ~tls ~peer_close ~mode:Codex.Conversation
          ~script:
            {
              after_handshake = [ created ];
              after_session_update = [];
              after_audio = [];
            }
          ~drain_ms:20 ()
      in
      expect_error diagnostic (result, ());
      Alcotest.(check bool)
        "closed before readiness sends no audio" false saw_audio)

let () =
  Alcotest.run "realtime-outcomes"
    [
      ( "readiness",
        [
          Alcotest.test_case "created-only sends no audio" `Quick
            test_created_only_sends_no_audio;
          Alcotest.test_case "config reject sends no audio" `Quick
            test_config_reject_sends_no_audio;
          Alcotest.test_case "wrong ack before session.updated sends no audio"
            `Quick test_wrong_ack_sends_no_audio;
          Alcotest.test_case "delayed ack then transcription completed" `Quick
            test_delayed_ack_then_transcription_completed;
        ] );
      ( "TLS close (nhttp-7pp8 nhttp-rs5p pienext-jlmd)",
        [
          Alcotest.test_case "TCP EOF is a closed session" `Quick
            (test_tls_peer_close `Tcp_eof "closed (1006)");
          Alcotest.test_case "close_notify is a closed session" `Quick
            (test_tls_peer_close `Notify "closed (1006)");
          Alcotest.test_case "truncated record is still TLS failure" `Quick
            (test_tls_peer_close `Truncated "TLS failed");
        ] );
      ( "terminal",
        [
          Alcotest.test_case "conversation completed" `Quick
            test_conversation_completed;
          Alcotest.test_case "failed after partial is not success" `Quick
            test_failed_after_partial;
          Alcotest.test_case "cancelled after partial" `Quick
            test_cancelled_after_partial;
          Alcotest.test_case "incomplete after partial" `Quick
            test_incomplete_after_partial;
          Alcotest.test_case "missing status is not success" `Quick
            test_missing_status_is_not_success;
          Alcotest.test_case "malformed terminal is not success" `Quick
            test_malformed_terminal_is_not_success;
          Alcotest.test_case "translation drain is incomplete" `Quick
            test_translation_drain_is_incomplete;
          Alcotest.test_case "transcription ignores conversation response.done"
            `Quick test_transcription_ignores_conversation_done;
          Alcotest.test_case "conversation ignores transcription final" `Quick
            test_conversation_ignores_transcription_final;
          Alcotest.test_case "translation ignores response.done" `Quick
            test_translation_ignores_response_done;
        ] );
    ]
