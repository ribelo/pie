(* ^pcaps-n1rg *)

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

let run_session_with ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) ?tls
    ?(peer_close = `Websocket) ~mode ~script () =
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
    Some (Codex.build_pass1_session_update ~model:"gpt-live-transcribe" ())
  in
  let result =
    try
      Eio.Time.with_timeout_exn clock 3.0 (fun () ->
          Codex.run_realtime_session ~caps ~sw ~net ~clock ?ca_file ~url
            ~access_token:"synthetic" ~mode ?session_update
            ~input_sample_rate:16000 pcm)
    with Eio.Time.Timeout -> Error (`Stt_error "timeout")
  in
  (result, !saw_audio)

let run_session ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) ~mode
    ~script =
  run_session_with ~caps ~mode ~script ()

let created = {|{"type":"session.created"}|}
let created_tx = {|{"type":"transcription_session.created"}|}
let updated = {|{"type":"session.updated"}|}
let updated_tx = {|{"type":"transcription_session.updated"}|}

let error_evt =
  {|{"type":"error","error":{"message":"SECRET_MARKER","code":"bad"}}|}

let delta =
  {|{"type":"conversation.item.input_audio_transcription.delta","delta":"hel"}|}

let tx_final =
  {|{"type":"conversation.item.input_audio_transcription.completed","transcript":"hello"}|}

let done_status status =
  Printf.sprintf {|{"type":"response.done","response":{"status":"%s"}}|} status

let expect_error needle (result, _) =
  match result with
  | Ok text -> Alcotest.failf "expected error, got %s" text
  | Error cause ->
      let text = cause_text cause in
      Alcotest.(check bool) needle true (contains text needle);
      Alcotest.(check bool) "no secret" false (contains text "SECRET_MARKER")

let test_created_only_sends_no_audio
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  let result, saw_audio =
    run_session ~caps ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [];
          after_audio = [];
        }
  in
  expect_error "acknowledgement" (result, saw_audio);
  Alcotest.(check bool) "no audio before ack" false saw_audio

let test_config_reject_sends_no_audio
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  let result, saw_audio =
    run_session ~caps ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ error_evt ];
          after_audio = [];
        }
  in
  expect_error "server error" (result, saw_audio);
  Alcotest.(check bool) "no audio after reject" false saw_audio

let test_delayed_ack_then_transcription_completed
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  let result, saw_audio =
    run_session ~caps ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created_tx ];
          after_session_update = [ updated ];
          after_audio = [ tx_final ];
        }
  in
  Alcotest.(check bool) "audio after ack" true saw_audio;
  match result with
  | Ok text -> Alcotest.(check string) "final" "hello" text
  | Error cause -> Alcotest.failf "expected success: %s" (cause_text cause)

let test_wrong_ack_sends_no_audio
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  let result, saw_audio =
    run_session ~caps ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated_tx ];
          after_audio = [ tx_final ];
        }
  in
  expect_error "acknowledgement" (result, saw_audio);
  Alcotest.(check bool) "no audio on wrong ack" false saw_audio

let test_transcription_ignores_conversation_done
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  let result, saw_audio =
    run_session ~caps ~mode:Codex.Transcription
      ~script:
        {
          after_handshake = [ created ];
          after_session_update = [ updated ];
          after_audio = [ delta; done_status "completed" ];
        }
  in
  Alcotest.(check bool) "audio after correct ack" true saw_audio;
  expect_error "incomplete" (result, ())

let with_tls
    ~(caps :
       < Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) test =
  let cert =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-r38-cert-" ".pem"
  in
  let key = Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-r38-key-" ".pem" in
  Fun.protect
    ~finally:(fun () ->
      Ox_caps.Cap_sys.remove ~caps cert;
      Ox_caps.Cap_sys.remove ~caps key)
    (fun () ->
      let command =
        Printf.sprintf
          "openssl req -x509 -newkey rsa:2048 -nodes -sha256 -days 1 -subj \
           /CN=localhost -addext %s -keyout %s -out %s >/dev/null 2>&1"
          (Filename.quote "subjectAltName=DNS:localhost,IP:127.0.0.1")
          (Filename.quote key) (Filename.quote cert)
      in
      if Ox_caps.Cap_sys.command ~caps command <> 0 then
        Alcotest.fail "TLS fixture generation failed";
      let ctx =
        Ox_http.Ox_http_tls_openssl.create_server_ctx
          ~certificate_chain_file:cert ~private_key_file:key
          ~alpn_protocols:[ "http/1.1" ] ()
      in
      test (ctx, cert))

let test_tls_peer_close
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) peer_close diagnostic () =
  with_tls ~caps (fun tls ->
      let result, saw_audio =
        run_session_with ~caps ~tls ~peer_close ~mode:Codex.Transcription
          ~script:
            {
              after_handshake = [ created ];
              after_session_update = [];
              after_audio = [];
            }
          ()
      in
      expect_error diagnostic (result, ());
      Alcotest.(check bool)
        "closed before readiness sends no audio" false saw_audio)

let ( let* ) = Result.bind

let frame_type payload =
  try
    match Yojson.Safe.from_string payload with
    | `Assoc fields -> (
        match List.assoc_opt "type" fields with
        | Some (`String name) -> Some name
        | _ -> None)
    | _ -> None
  with Yojson.Json_error _ -> None

let request_line head =
  match String.split_on_char '\n' head with
  | line :: _ ->
      let len = String.length line in
      if len > 0 && line.[len - 1] = '\r' then String.sub line 0 (len - 1)
      else line
  | [] -> ""

let json_member path payload =
  let rec loop value = function
    | [] -> Some value
    | key :: rest -> (
        match value with
        | `Assoc fields -> (
            match List.assoc_opt key fields with
            | Some next -> loop next rest
            | None -> None)
        | _ -> None)
  in
  match Yojson.Safe.from_string payload with
  | exception Yojson.Json_error _ -> None
  | value -> loop value path

let string_at path payload =
  match json_member path payload with
  | Some (`String value) -> Some value
  | _ -> None

let transcription_path = [ "session"; "audio"; "input"; "transcription" ]

let session_update_payload frames =
  match
    List.filter
      (fun payload -> frame_type payload = Some "session.update")
      frames
  with
  | [ payload ] -> payload
  | found ->
      Alcotest.failf "expected one session.update, got %d" (List.length found)

let frame_types frames = List.filter_map frame_type frames

let assert_commit_after_audio frames =
  let rec loop saw_audio = function
    | [] -> Alcotest.fail "missing input_audio_buffer.commit"
    | "input_audio_buffer.commit" :: rest ->
        Alcotest.(check bool) "audio before commit" true saw_audio;
        Alcotest.(check bool)
          "no audio after commit" false
          (List.exists is_audio_append rest)
    | name :: rest -> loop (saw_audio || is_audio_append name) rest
  in
  loop false (frame_types frames)

type peer_terminal = On_commit

let serve_recording ~terminal ~frames ~request flow =
  let head = read_http_head flow in
  request := request_line head;
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> Alcotest.fail "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key;
  write_ws_text flow created;
  let rec loop () =
    match read_ws_frame flow with
    | exception End_of_file -> ()
    | Close -> ()
    | Other -> loop ()
    | Text payload ->
        frames := payload :: !frames;
        (match (frame_type payload, terminal) with
        | Some "session.update", _ -> write_ws_text flow updated
        | Some "input_audio_buffer.commit", On_commit ->
            write_ws_text flow tx_final
        | _ -> ());
        loop ()
  in
  try loop () with _ -> ()

let with_saved_env ~(caps : < Ox_caps.Cap.env ; .. >) pairs f =
  let saved =
    List.map (fun (key, _) -> (key, Ox_caps.Cap_sys.getenv_opt ~caps key)) pairs
  in
  List.iter (fun (key, value) -> Ox_caps.Cap_unix.putenv ~caps key value) pairs;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (key, previous) ->
          Ox_caps.Cap_unix.putenv ~caps key (Option.value previous ~default:""))
        saved)
    f

let run_peer ~(caps : < Ox_caps.Cap.env ; .. >) ~terminal client =
  let frames = ref [] in
  let request = ref "" in
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
      try
        let flow, _ = Eio.Net.accept ~sw socket in
        serve_recording ~terminal ~frames ~request flow
      with _ -> ());
  let result =
    with_saved_env ~caps
      [
        ( "CODEX_REALTIME_BASE_URL_OVERRIDE",
          Printf.sprintf "ws://127.0.0.1:%d" port );
        ("OPENAI_API_KEY", "synthetic-pass1");
      ]
      (fun () ->
        try
          Eio.Time.with_timeout_exn clock 3.0 (fun () ->
              client env sw net clock)
        with Eio.Time.Timeout -> Error (`Stt_error "timeout"))
  in
  (result, !request, List.rev !frames)

let expect_transcript expected result =
  match result with
  | Ok text -> Alcotest.(check string) "completed transcript" expected text
  | Error (`Stt_error message) ->
      Alcotest.failf "expected transcript: %s" message

let pass1_logger env clock =
  Ox_observability.Logger.create ~service:"pie-pass1" ~clock
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~reporter:
      (Ox_observability.Logger.Memory.reporter
         (Ox_observability.Logger.Memory.create ()))
    ()

let test_language_codes () =
  let check input expected =
    Alcotest.(check (option string))
      input expected
      (Pie.Stt.Language.iso639_1 input)
  in
  List.iter
    (fun (name, code) -> check name (Some code))
    [
      ("Polish", "pl");
      ("english", "en");
      ("GERMAN", "de");
      (" French ", "fr");
      ("Spanish", "es");
      ("Italian", "it");
      ("Portuguese", "pt");
      ("Ukrainian", "uk");
      ("Russian", "ru");
      ("Czech", "cs");
      ("Dutch", "nl");
      ("Japanese", "ja");
      ("Chinese", "zh");
      ("PL", "pl");
      ("en", "en");
      ("  zh  ", "zh");
    ];
  List.iter
    (fun name -> check name None)
    [ "Klingon"; "pol"; "eng"; "zh-cn"; ""; "  "; "e"; "Polish." ]

let test_pass1_update_fields () =
  let with_context =
    Codex.build_pass1_session_update ~model:"gpt-live-transcribe" ~language:"pl"
      ~context:"Nema\nPie" ()
  in
  let rendered = Yojson.Safe.to_string with_context in
  Alcotest.(check (option string))
    "model" (Some "gpt-live-transcribe")
    (string_at (transcription_path @ [ "model" ]) rendered);
  (match json_member (transcription_path @ [ "languages" ]) rendered with
  | Some (`List [ `String "pl" ]) -> ()
  | _ -> Alcotest.fail "languages should be the ISO-639-1 code");
  Alcotest.(check (option string))
    "glossary prompt" (Some "Nema\nPie")
    (string_at (transcription_path @ [ "prompt" ]) rendered);
  Alcotest.(check bool)
    "singular language is not sent" false
    (json_member (transcription_path @ [ "language" ]) rendered <> None);
  Alcotest.(check bool)
    "manual commit" true
    (json_member [ "session"; "audio"; "input"; "turn_detection" ] rendered
    = Some `Null);
  let bare =
    Yojson.Safe.to_string
      (Codex.build_pass1_session_update ~model:"gpt-transcribe" ())
  in
  match json_member transcription_path bare with
  | Some (`Assoc fields) ->
      Alcotest.(check bool)
        "no languages" false
        (List.mem_assoc "languages" fields);
      Alcotest.(check bool)
        "no language" false
        (List.mem_assoc "language" fields);
      Alcotest.(check bool) "no prompt" false (List.mem_assoc "prompt" fields)
  | _ -> Alcotest.fail "missing transcription object"

let test_pass1_stream_commits
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. >)
    () =
  let result, request, frames =
    run_peer ~caps ~terminal:On_commit (fun env sw net clock ->
        let logger = pass1_logger env clock in
        Codex.with_pass1_stream ~caps ~sw ~net ~clock ~logger
          ~model:"gpt-live-transcribe" ~language:"pl" ~context:"Nema\nPie"
          (fun handle ->
            let* () = handle.offer_chunk pcm in
            let* () = handle.offer_chunk pcm in
            handle.finish_stream ()))
  in
  expect_transcript "hello" result;
  Alcotest.(check bool)
    "transcription intent" true
    (contains request "intent=transcription");
  Alcotest.(check bool)
    "model stays out of the url" false
    (contains request "model=");
  let update = session_update_payload frames in
  let expected =
    Yojson.Safe.to_string
      (Codex.build_pass1_session_update ~model:"gpt-live-transcribe"
         ~language:"pl" ~context:"Nema\nPie" ())
  in
  Alcotest.(check string) "session.update on the wire" expected update;
  assert_commit_after_audio frames;
  Alcotest.(check bool)
    "no conversation response.create" false
    (List.mem "response.create" (frame_types frames))

let test_pass1_buffered_sends_all_audio
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. >)
    () =
  let result, request, frames =
    run_peer ~caps ~terminal:On_commit (fun _env sw net clock ->
        Codex.transcribe_pass1 ~caps ~sw ~net ~clock ~model:"gpt-transcribe" pcm)
  in
  expect_transcript "hello" result;
  Alcotest.(check bool)
    "transcription intent" true
    (contains request "intent=transcription");
  let update = session_update_payload frames in
  (match json_member transcription_path update with
  | Some (`Assoc fields) ->
      Alcotest.(check (option string))
        "model" (Some "gpt-transcribe")
        (match List.assoc_opt "model" fields with
        | Some (`String value) -> Some value
        | _ -> None);
      Alcotest.(check bool)
        "no languages" false
        (List.mem_assoc "languages" fields);
      Alcotest.(check bool)
        "no language" false
        (List.mem_assoc "language" fields);
      Alcotest.(check bool) "no prompt" false (List.mem_assoc "prompt" fields)
  | _ -> Alcotest.fail "missing transcription object");
  let audio =
    frames
    |> List.filter (fun payload ->
        match frame_type payload with
        | Some name -> is_audio_append name
        | None -> false)
    |> List.filter_map (fun payload ->
        match json_member [ "audio" ] payload with
        | Some (`String value) -> Some value
        | _ -> None)
  in
  let expected =
    Pie.Stt.Openrouter.base64_encode
      (Codex.resample_s16le_mono ~from_rate:16000 ~to_rate:24000 pcm)
  in
  Alcotest.(check (list string))
    "all resampled audio before commit" [ expected ] audio;
  assert_commit_after_audio frames

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "realtime-outcomes"
    [
      ( "readiness",
        [
          Alcotest.test_case "created-only sends no audio" `Quick
            (test_created_only_sends_no_audio ~caps);
          Alcotest.test_case "config reject sends no audio" `Quick
            (test_config_reject_sends_no_audio ~caps);
          Alcotest.test_case "wrong ack before session.updated sends no audio"
            `Quick
            (test_wrong_ack_sends_no_audio ~caps);
          Alcotest.test_case "delayed ack then transcription completed" `Quick
            (test_delayed_ack_then_transcription_completed ~caps);
        ] );
      ( "TLS close (nhttp-7pp8 nhttp-rs5p pienext-jlmd)",
        [
          Alcotest.test_case "TCP EOF is a closed session" `Quick
            (test_tls_peer_close ~caps `Tcp_eof "closed (1006)");
          Alcotest.test_case "close_notify is a closed session" `Quick
            (test_tls_peer_close ~caps `Notify "closed (1006)");
          Alcotest.test_case "truncated record is still TLS failure" `Quick
            (test_tls_peer_close ~caps `Truncated "TLS failed");
        ] );
      ( "terminal",
        [
          Alcotest.test_case "transcription ignores conversation response.done"
            `Quick
            (test_transcription_ignores_conversation_done ~caps);
        ] );
      ( "pass 1 (dict-dl3q dict-m2xr dict-q6xd dict-wha3)",
        [
          Alcotest.test_case "configured language names and codes (dict-wha3)"
            `Quick test_language_codes;
          Alcotest.test_case
            "session.update carries model, languages and prompt (dict-dl3q \
             dict-wha3)"
            `Quick test_pass1_update_fields;
          Alcotest.test_case
            "stream commits and returns the transcript (dict-q6xd dict-dl3q)"
            `Quick
            (test_pass1_stream_commits ~caps);
          Alcotest.test_case
            "buffered sends all audio then commits (dict-q6xd dict-m2xr)" `Quick
            (test_pass1_buffered_sends_all_audio ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
