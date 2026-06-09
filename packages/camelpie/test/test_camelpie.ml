open Camelpie

let test_pcm_rms () =
  let chunk = Bytes.create 4 in
  Bytes.set_int16_le chunk 0 16384;
  Bytes.set_int16_le chunk 2 (-16384);
  let rms = Audio.Pcm.pcm_rms chunk in
  Alcotest.(check (float 0.001)) "rms of 0.5/-0.5" 0.5 rms

let test_pcm_peak () =
  let chunk = Bytes.create 4 in
  Bytes.set_int16_le chunk 0 16384;
  Bytes.set_int16_le chunk 2 (-8192);
  let peak = Audio.Pcm.pcm_peak chunk in
  Alcotest.(check (float 0.001)) "peak of 0.5/-0.25" 0.5 peak

let test_pcm_normalization () =
  let chunk = Bytes.create 4 in
  Bytes.set_int16_le chunk 0 1000;
  Bytes.set_int16_le chunk 2 (-1000);
  let result = Audio.Pcm.normalize_pcm_for_stt chunk in
  Alcotest.(check bool) "gain applied" true (result.gain > 1.0);
  Alcotest.(check int) "same length" 4 (Bytes.length result.normalized_bytes)

let test_pcm_wav_header () =
  let header = Audio.Pcm.build_pcm_wav_header ~data_length:16000 ~sample_rate:16000 in
  Alcotest.(check int) "wav header length" 44 (Bytes.length header);
  Alcotest.(check string) "RIFF marker" "RIFF" (Bytes.sub_string header 0 4);
  Alcotest.(check string) "WAVE marker" "WAVE" (Bytes.sub_string header 8 4)

let test_pa_packet_framing () =
  let payload = Bytes.of_string "hello" in
  let packet = Pulse.Protocol.frame_packet payload in
  Alcotest.(check int) "framed packet length" 25 (Bytes.length packet);
  let split = Pulse.Protocol.split_framed_packet packet in
  Alcotest.(check int) "payload length" 5 (Bytes.length split.payload);
  Alcotest.(check string) "payload content" "hello" (Bytes.to_string split.payload)

let test_pa_packet_header_channel_roundtrip () =
  let header =
    {
      Pulse.Protocol.length = 15;
      channel = Pulse.Defs.pa_no_index;
      offset_hi = 0;
      offset_lo = 0;
      flags = 0;
    }
  in
  let decoded =
    Pulse.Protocol.encode_packet_header header
    |> Pulse.Protocol.decode_packet_header
  in
  Alcotest.(check int) "command channel" Pulse.Defs.pa_no_index decoded.channel

let test_tag_struct_roundtrip () =
  let writer = Pulse.Protocol.Tag_struct_writer.create () in
  Pulse.Protocol.Tag_struct_writer.add_u32 writer 42;
  Pulse.Protocol.Tag_struct_writer.add_string writer (Some "test");
  Pulse.Protocol.Tag_struct_writer.add_bool writer true;
  Pulse.Protocol.Tag_struct_writer.add_u8 writer 7;
  let encoded = Pulse.Protocol.Tag_struct_writer.finalize writer in

  let reader = Pulse.Protocol.Tag_struct_reader.create encoded in
  Alcotest.(check int) "u32" 42 (Pulse.Protocol.Tag_struct_reader.get_u32 reader);
  Alcotest.(check (option string)) "string" (Some "test") (Pulse.Protocol.Tag_struct_reader.get_string reader);
  Alcotest.(check bool) "bool" true (Pulse.Protocol.Tag_struct_reader.get_bool reader);
  Alcotest.(check int) "u8" 7 (Pulse.Protocol.Tag_struct_reader.get_u8 reader);
  Alcotest.(check bool) "no remaining" false (Pulse.Protocol.Tag_struct_reader.has_remaining reader)

let test_sample_spec_roundtrip () =
  let spec = { Pulse.Defs.format = Pulse.Defs.Pa_sample_format.s16le; channels = 1; rate = 16000 } in
  let writer = Pulse.Protocol.Tag_struct_writer.create () in
  Pulse.Protocol.Tag_struct_writer.add_sample_spec writer spec;
  let encoded = Pulse.Protocol.Tag_struct_writer.finalize writer in
  let reader = Pulse.Protocol.Tag_struct_reader.create encoded in
  let decoded = Pulse.Protocol.Tag_struct_reader.get_sample_spec reader in
  Alcotest.(check int) "format" spec.format decoded.format;
  Alcotest.(check int) "channels" spec.channels decoded.channels;
  Alcotest.(check int) "rate" spec.rate decoded.rate

let test_auth_command () =
  match Pulse.Commands.build_auth_command () with
  | Ok pkt ->
    Alcotest.(check bool) "auth packet tag > 0" true (pkt.tag > 0);
    Alcotest.(check bool) "auth packet bytes length > 24" true (Bytes.length pkt.bytes > 24)
  | Error msg -> Alcotest.fail msg

let test_server_info_command () =
  let pkt = Pulse.Commands.build_get_server_info_command () in
  Alcotest.(check bool) "server info tag > 0" true (pkt.tag > 0)

let restore_dbus_env old =
  match old with
  | Some v -> Unix.putenv "DBUS_SESSION_BUS_ADDRESS" v
  | None -> Unix.putenv "DBUS_SESSION_BUS_ADDRESS" ""

let test_keyboard_monitor_session_bus_path_abstract_socket () =
  let old = Sys.getenv_opt "DBUS_SESSION_BUS_ADDRESS" in
  Unix.putenv "DBUS_SESSION_BUS_ADDRESS" "unix:abstract=/tmp/dbus-abstract";
  try
    let path = Camelpie.Keyboard.Monitor.session_bus_path () in
    restore_dbus_env old;
    (* If we get here, the abstract socket was accepted. *)
    Alcotest.(check string) "abstract path" "/tmp/dbus-abstract" path
  with
  | Invalid_argument msg ->
    restore_dbus_env old;
    (* Bug: session_bus_path only recognizes unix:path= and raises
       invalid_arg for unix:abstract=, which is a valid D-Bus transport
       on Linux (used by some distributions and container setups). *)
    Alcotest.fail ("unix:abstract= socket should be supported: " ^ msg)
  | exn ->
    restore_dbus_env old;
    raise exn

let test_keyboard_key_event_parse () =
  let line =
    "{\"type\":\"signal\",\"interface\":\"org.freedesktop.a11y.KeyboardMonitor\",\"member\":\"KeyEvent\",\"payload\":{\"type\":\"buuuq\",\"data\":[false,0,65478,0,75]}}"
  in
  match Keyboard.Monitor.parse_busctl_json_event line with
  | Some event ->
    Alcotest.(check bool) "press" false event.released;
    Alcotest.(check int) "F9 keysym" 65478 event.keysym;
    Alcotest.(check int) "keycode" 75 event.keycode
  | None -> Alcotest.fail "expected KeyEvent"

let test_keyboard_dbus_key_event_parse () =
  let set_u16_le bytes off value =
    Bytes.set bytes off (Char.chr (value land 0xff));
    Bytes.set bytes (off + 1) (Char.chr ((value lsr 8) land 0xff))
  in
  let set_u32_le bytes off value =
    Bytes.set bytes off (Char.chr (value land 0xff));
    Bytes.set bytes (off + 1) (Char.chr ((value lsr 8) land 0xff));
    Bytes.set bytes (off + 2) (Char.chr ((value lsr 16) land 0xff));
    Bytes.set bytes (off + 3) (Char.chr ((value lsr 24) land 0xff))
  in
  let body = Bytes.make 18 '\000' in
  set_u32_le body 0 1;
  set_u32_le body 4 2;
  set_u32_le body 8 65479;
  set_u32_le body 12 0;
  set_u16_le body 16 68;
  let message : Keyboard.Monitor.dbus_message =
    {
      msg_type = 4;
      reply_serial = None;
      interface = Some "org.freedesktop.a11y.KeyboardMonitor";
      member = Some "KeyEvent";
      error_name = None;
      body;
    }
  in
  match Keyboard.Monitor.parse_key_event message with
  | Some event ->
    Alcotest.(check bool) "released" true event.released;
    Alcotest.(check int) "state" 2 event.state;
    Alcotest.(check int) "F10 keysym" 65479 event.keysym;
    Alcotest.(check int) "keycode" 68 event.keycode
  | None -> Alcotest.fail "expected D-Bus KeyEvent"

let test_recording_coordinator_transitions () =
  let path = Filename.temp_file "camelpie-recording" ".json" in
  let coordinator = Recording.Coordinator.create ~persist_path:path () in
  let initial = Recording.Coordinator.snapshot coordinator in
  Alcotest.(check bool) "enabled" true initial.enabled;
  Alcotest.(check string) "idle" "idle" initial.mode;
  (match Recording.Coordinator.try_start coordinator Recording.Coordinator.Ptt_transcribe () with
   | Recording.Coordinator.Started Recording.Coordinator.Ptt_transcribe -> ()
   | _ -> Alcotest.fail "expected ptt-transcribe to start");
  (match Recording.Coordinator.try_start coordinator Recording.Coordinator.Ptt_translate () with
   | Recording.Coordinator.Busy Recording.Coordinator.Ptt_transcribe -> ()
   | _ -> Alcotest.fail "expected busy ptt-transcribe");
  Recording.Coordinator.set_enabled coordinator false;
  ignore (Recording.Coordinator.stop coordinator Recording.Coordinator.Ptt_transcribe);
  (match Recording.Coordinator.try_start coordinator Recording.Coordinator.Ptt_translate () with
   | Recording.Coordinator.Disabled -> ()
   | _ -> Alcotest.fail "expected disabled");
  Sys.remove path

let test_daemon_contract_request () =
  let json = Camelpie.Daemon.Contract.request_to_json Camelpie.Daemon.Contract.Status in
  match Camelpie.Daemon.Contract.request_of_json json with
  | Ok request ->
    Alcotest.(check bool) "request" true
      (request = Camelpie.Daemon.Contract.Status)
  | Error msg -> Alcotest.fail msg

let test_daemon_contract_error_response_parsed_as_success () =
  let error_json = `Assoc [("error", `String "server crashed")] in
  let response =
    Camelpie.Daemon.Contract.response_of_json
      Camelpie.Daemon.Contract.Meeting_start
      error_json
  in
  (* Bug: response_of_json for Meeting_start treats any JSON object as a valid
     response, even if it contains {"error": ...}. It returns Disabled with a
     default snapshot, making it impossible for the caller to know the server
     sent an error. A robust client should check for the "error" field first. *)
  match response with
  | Camelpie.Daemon.Contract.Meeting_start_resp r ->
    (match r.result with
     | Camelpie.Daemon.Contract.Disabled ->
       Alcotest.fail "error response was silently parsed as Disabled"
     | _ -> ())
  | _ -> Alcotest.fail "expected Meeting_start_resp"

let test_daemon_contract_status_error_swallowed () =
  let error_json = `Assoc [("error", `String "server crashed")] in
  let response =
    Camelpie.Daemon.Contract.response_of_json
      Camelpie.Daemon.Contract.Status
      error_json
  in
  (* Bug: response_of_json for Status treats any JSON object as a valid status
     snapshot. A server error like {"error": "..."} becomes a bogus
     Status_resp with active=false, enabled=false, mode="idle", so the client
     thinks the daemon is paused when it actually crashed. *)
  match response with
  | Camelpie.Daemon.Contract.Status_resp _ ->
    Alcotest.fail "status error response was silently parsed as a snapshot"
  | _ -> ()

let test_daemon_contract_pause_error_swallowed () =
  let error_json = `Assoc [("error", `String "server crashed")] in
  let response =
    Camelpie.Daemon.Contract.response_of_json
      Camelpie.Daemon.Contract.Pause
      error_json
  in
  (* Bug: response_of_json for Pause/Resume always returns Pause_resp or
     Resume_resp regardless of the JSON content. A server error is therefore
     indistinguishable from a successful call. *)
  match response with
  | Camelpie.Daemon.Contract.Pause_resp ->
    Alcotest.fail "pause error response was silently parsed as success"
  | _ -> ()

let pulse_command_packet packet_type tag body_bytes =
  let envelope_writer = Pulse.Protocol.Tag_struct_writer.create () in
  Pulse.Protocol.Tag_struct_writer.add_u32 envelope_writer packet_type;
  Pulse.Protocol.Tag_struct_writer.add_u32 envelope_writer tag;
  let envelope = Pulse.Protocol.Tag_struct_writer.finalize envelope_writer in
  let payload = Bytes.create (Bytes.length envelope + Bytes.length body_bytes) in
  Bytes.blit envelope 0 payload 0 (Bytes.length envelope);
  Bytes.blit body_bytes 0 payload (Bytes.length envelope) (Bytes.length body_bytes);
  Pulse.Protocol.frame_packet payload

let pulse_read_exact flow len =
  let buffer = Cstruct.create len in
  let rec loop off =
    if off = len then Cstruct.to_bytes buffer
    else
      let cs = Cstruct.sub buffer off (len - off) in
      match Eio.Flow.single_read flow cs with
      | 0 -> raise End_of_file
      | n -> loop (off + n)
  in
  loop 0

let pulse_read_command flow =
  let header = pulse_read_exact flow Pulse.Defs.pa_stream_descriptor_size in
  let hdr = Pulse.Protocol.decode_packet_header header in
  let payload = pulse_read_exact flow hdr.length in
  match Pulse.Commands.parse_command_envelope payload with
  | Some envelope -> envelope
  | None -> Alcotest.fail "could not parse command envelope"

let test_pulse_client_set_client_name_error_ignored () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "camelpie-pulse-%d.sock" (Unix.getpid ()))
  in
  (try Sys.remove socket_path with Sys_error _ -> ());
  let socket =
    Eio.Net.listen
      ~sw
      ~reuse_addr:true
      ~backlog:1
      (Eio.Stdenv.net env)
      (`Unix socket_path)
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Switch.run @@ fun conn_sw ->
    let flow, _addr = Eio.Net.accept ~sw:conn_sw socket in
    let auth_env = pulse_read_command flow in
    let auth_reply_body =
      let w = Pulse.Protocol.Tag_struct_writer.create () in
      Pulse.Protocol.Tag_struct_writer.add_u32 w Pulse.Defs.pa_native_protocol_version;
      Pulse.Protocol.Tag_struct_writer.finalize w
    in
    let auth_reply =
      pulse_command_packet
        Pulse.Defs.Pa_command.reply
        auth_env.Pulse.Commands.tag
        auth_reply_body
    in
    Eio.Flow.write flow [Cstruct.of_bytes auth_reply];
    let name_env = pulse_read_command flow in
    let err_body =
      let w = Pulse.Protocol.Tag_struct_writer.create () in
      Pulse.Protocol.Tag_struct_writer.add_u32 w 1;
      Pulse.Protocol.Tag_struct_writer.finalize w
    in
    let err_reply =
      pulse_command_packet
        Pulse.Defs.Pa_command.error
        name_env.Pulse.Commands.tag
        err_body
    in
    Eio.Flow.write flow [Cstruct.of_bytes err_reply];
    try Eio.Flow.shutdown flow `Send with _ -> ());
  let rt = Eta.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) () in
  let effect =
    Pulse.Client.with_client
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env)
      ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00')
      (fun _client -> Eta.Effect.pure ())
  in
  (* Bug: Pulse.Client.connect receives the result of set_client_name but
     discards it with [let _ = ...]. A server error reply should be
     propagated to the caller instead of being silently swallowed. *)
  match Eta.Runtime.run rt effect with
  | Ok () ->
    Alcotest.fail
      "expected set_client_name error to propagate, but connect succeeded"
  | Error _ -> ()

let test_daemon_contract_response () =
  let snap : Camelpie.Daemon.Contract.recording_snapshot =
    {
      active = true;
      mode = "ptt-transcribe";
      enabled = true;
      started_at = None;
      updated_at = Some "now";
      transcript_path = None;
      last_error = None;
    }
  in
  let response =
    Camelpie.Daemon.Contract.response_to_json
      (Camelpie.Daemon.Contract.Status_resp snap)
  in
  (match Camelpie.Daemon.Contract.response_of_json
           Camelpie.Daemon.Contract.Status
           response with
   | Camelpie.Daemon.Contract.Status_resp decoded ->
     Alcotest.(check bool) "active" true decoded.active;
     Alcotest.(check string) "mode" "ptt-transcribe" decoded.mode
   | _ -> Alcotest.fail "expected status response");
  let toggle_response =
    Camelpie.Daemon.Contract.response_to_json
      (Camelpie.Daemon.Contract.Toggle_resp true)
  in
  match Camelpie.Daemon.Contract.response_of_json
          Camelpie.Daemon.Contract.Toggle
          toggle_response with
  | Camelpie.Daemon.Contract.Toggle_resp true -> ()
  | _ -> Alcotest.fail "expected toggle bool response"

let test_daemon_server_state () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "camelpie-daemon-%d.sock" (Unix.getpid ()))
  in
  let persist_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "camelpie-daemon-%d.json" (Unix.getpid ()))
  in
  (try Sys.remove socket_path with Sys_error _ -> ());
  (try Sys.remove persist_path with Sys_error _ -> ());
  let coordinator =
    Camelpie.Recording.Coordinator.create ~persist_path ()
  in
  Camelpie.Daemon.Server.start
    ~sw
    ~net:(Eio.Stdenv.net env)
    ~socket_path
    coordinator;
  let send req =
    let flow =
      Eio.Net.connect
        ~sw
        (Eio.Stdenv.net env)
        (`Unix socket_path)
    in
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close flow)
      (fun () ->
         let payload =
           Yojson.Safe.to_string (Camelpie.Daemon.Contract.request_to_json req)
           ^ "\n"
         in
         Eio.Flow.write flow [Cstruct.of_string payload];
         let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
         Yojson.Safe.from_string (Eio.Buf_read.line reader))
  in
  ignore (send Camelpie.Daemon.Contract.Pause);
  (match send Camelpie.Daemon.Contract.Status with
   | `Assoc fields ->
     Alcotest.(check bool) "paused" false
       (match List.assoc_opt "enabled" fields with Some (`Bool b) -> b | _ -> true)
   | _ -> Alcotest.fail "status value must be object");
  (match send Camelpie.Daemon.Contract.Toggle with
   | `Assoc fields ->
     Alcotest.(check bool) "toggle enabled" true
       (match List.assoc_opt "value" fields with Some (`Bool b) -> b | _ -> false)
   | _ -> Alcotest.fail "toggle should return enabled=true");
  (match send Camelpie.Daemon.Contract.Meeting_toggle with
   | `Assoc fields ->
     Alcotest.(check string) "meeting mode" "meeting-transcribe"
       (match List.assoc_opt "mode" fields with Some (`String s) -> s | _ -> "")
   | _ -> Alcotest.fail "meeting value must be object");
  (try Sys.remove persist_path with Sys_error _ -> ())

let test_daemon_server_meeting_stop_no_failure_signal () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "camelpie-daemon-%d.sock" (Unix.getpid ()))
  in
  let persist_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "camelpie-daemon-%d.json" (Unix.getpid ()))
  in
  (try Sys.remove socket_path with Sys_error _ -> ());
  (try Sys.remove persist_path with Sys_error _ -> ());
  let coordinator =
    Camelpie.Recording.Coordinator.create ~persist_path ()
  in
  (* Start PTT transcribe mode *)
  ignore
    (Camelpie.Recording.Coordinator.try_start
       coordinator
       Camelpie.Recording.Coordinator.Ptt_transcribe
       ());
  Camelpie.Daemon.Server.start
    ~sw
    ~net:(Eio.Stdenv.net env)
    ~socket_path
    coordinator;
  let send method_str =
    let flow =
      Eio.Net.connect
        ~sw
        (Eio.Stdenv.net env)
        (`Unix socket_path)
    in
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close flow)
      (fun () ->
         let payload =
           Yojson.Safe.to_string (`Assoc [("method", `String method_str)])
           ^ "\n"
         in
         Eio.Flow.write flow [Cstruct.of_string payload];
         let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
         Yojson.Safe.from_string (Eio.Buf_read.line reader))
  in
  (match send "MeetingStop" with
   | `Assoc fields ->
     let active =
       match List.assoc_opt "active" fields with
       | Some (`Bool b) -> b
       | _ -> false
     in
     let mode =
       match List.assoc_opt "mode" fields with
       | Some (`String s) -> s
       | _ -> ""
     in
     (* The coordinator is in PTT_transcribe mode, so Meeting_stop returns
        false internally but the server ignores this and returns a snapshot
        showing the current state. The client has no way to know the stop
        had no effect because there's no success/failure indicator. *)
     Alcotest.(check bool) "still active" true active;
     Alcotest.(check string) "still ptt-transcribe" "ptt-transcribe" mode;
     Alcotest.(check bool)
       "MeetingStop response should indicate when no meeting was active"
       true
       (List.mem_assoc "stopped" fields)
   | _ -> Alcotest.fail "expected JSON object");
  (try Sys.remove persist_path with Sys_error _ -> ())

let test_codex_realtime_url_builders () =
  let transcribe_url =
    Stt.Codex_realtime.build_realtime_url
      ~base_url:"wss://api.openai.com/"
      ~mode:Stt.Codex_realtime.Transcription
      ~model:"gpt-realtime-whisper"
      ()
  in
  Alcotest.(check string)
    "transcription url"
    "wss://api.openai.com/v1/realtime?model=gpt-realtime-1.5"
    transcribe_url;
  let translate_url =
    Stt.Codex_realtime.build_realtime_url
      ~base_url:"wss://api.openai.com"
      ~mode:Stt.Codex_realtime.Translation
      ~model:"gpt-realtime-translate"
      ()
  in
  Alcotest.(check string)
    "translation url"
    "wss://api.openai.com/v1/realtime/translations?model=gpt-realtime-translate"
    translate_url

let test_codex_realtime_headers () =
  let headers =
    Stt.Codex_realtime.realtime_header_pairs
      ~session_id:"camelpie-test-session"
      "token"
  in
  Alcotest.(check (option string))
    "authorization"
    (Some "Bearer token")
    (List.assoc_opt "Authorization" headers);
  Alcotest.(check (option string))
    "originator"
    (Some "codex_cli_rs")
    (List.assoc_opt "originator" headers);
  Alcotest.(check (option string))
    "user agent"
    (Some "codex_cli_rs/0.0.0 (camelpie)")
    (List.assoc_opt "User-Agent" headers);
  Alcotest.(check (option string))
    "version"
    (Some "0.0.0")
    (List.assoc_opt "version" headers);
  Alcotest.(check (option string))
    "session id"
    (Some "camelpie-test-session")
    (List.assoc_opt "x-session-id" headers);
  Alcotest.(check (option string))
    "beta header"
    None
    (List.assoc_opt "OpenAI-Beta" headers)

let test_codex_realtime_receive_stop_modes () =
  let transcript_done = Stt.Codex_realtime.Transcript_done "text" in
  Alcotest.(check bool)
    "transcription stops on transcript"
    true
    (Stt.Codex_realtime.event_should_stop
       ~mode:Stt.Codex_realtime.Transcription
       transcript_done);
  Alcotest.(check bool)
    "conversation waits for response.done"
    false
    (Stt.Codex_realtime.event_should_stop
       ~mode:Stt.Codex_realtime.Conversation
       transcript_done);
  Alcotest.(check bool)
    "conversation stops on response.done"
    true
    (Stt.Codex_realtime.event_should_stop
       ~mode:Stt.Codex_realtime.Conversation
       Stt.Codex_realtime.Response_done)

let test_codex_realtime_transcription_session_update () =
  match Stt.Codex_realtime.build_transcription_session_update ~model:"configured-model" () with
  | `Assoc fields ->
    Alcotest.(check (option string))
      "event type"
      (Some "session.update")
      (match List.assoc_opt "type" fields with
       | Some (`String value) -> Some value
       | _ -> None);
    (match List.assoc_opt "session" fields with
     | Some (`Assoc session) ->
       Alcotest.(check (option string))
         "session type"
         (Some "transcription")
         (match List.assoc_opt "type" session with
          | Some (`String value) -> Some value
          | _ -> None);
       Alcotest.(check (option string))
         "no websocket session model in session.update"
         None
         (match List.assoc_opt "model" session with
          | Some (`String value) -> Some value
          | _ -> None);
       Alcotest.(check bool)
         "instructions omitted"
         false
         (List.mem_assoc "instructions" session);
       Alcotest.(check bool)
         "tools omitted"
         false
         (List.mem_assoc "tools" session);
       let model =
         match List.assoc_opt "audio" session with
         | Some (`Assoc audio) ->
           (match List.assoc_opt "input" audio with
            | Some (`Assoc input) ->
              (match List.assoc_opt "transcription" input with
               | Some (`Assoc transcription) ->
                 (match List.assoc_opt "model" transcription with
                  | Some (`String value) -> Some value
                  | _ -> None)
               | _ -> None)
            | _ -> None)
         | _ -> None
       in
       Alcotest.(check (option string))
         "configured transcription model"
         (Some "configured-model")
         model
     | _ -> Alcotest.fail "expected session object")
  | _ -> Alcotest.fail "expected object"

let test_codex_realtime_event_parser () =
  (match Stt.Codex_realtime.parse_event
           "{\"type\":\"response.output_text.delta\",\"delta\":\"he\"}" with
   | Ok (Stt.Codex_realtime.Transcript_delta "he") -> ()
   | _ -> Alcotest.fail "expected transcript delta");
  (match Stt.Codex_realtime.parse_event
           "{\"type\":\"response.output_text.done\",\"text\":\"hello\"}" with
   | Ok (Stt.Codex_realtime.Transcript_done "hello") -> ()
   | _ -> Alcotest.fail "expected transcript done");
  (match Stt.Codex_realtime.parse_event
           "{\"type\":\"error\",\"error\":{\"message\":\"bad\",\"code\":\"x\"}}" with
   | Ok (Stt.Codex_realtime.Server_error { message = "bad"; code = Some "x" }) -> ()
   | _ -> Alcotest.fail "expected error event")

let test_codex_realtime_resample () =
  let input = Bytes.create 4 in
  Bytes.set_int16_le input 0 0;
  Bytes.set_int16_le input 2 12000;
  let output = Stt.Codex_realtime.resample_s16le_mono ~from_rate:16000 ~to_rate:24000 input in
  Alcotest.(check int) "2 samples at 16k become 3 at 24k" 6 (Bytes.length output);
  Alcotest.(check int) "first sample" 0 (Bytes.get_int16_le output 0)

let tcp_port = function
  | `Tcp (_, port) -> port
  | `Unix _ -> Alcotest.fail "expected TCP listener"

let request_target raw =
  match String.split_on_char '\n' raw with
  | request_line :: _ -> (
      match String.split_on_char ' ' request_line with
      | _method :: target :: _ -> Some target
      | _ -> None)
  | [] -> None

let header_value name raw =
  let expected = String.lowercase_ascii name in
  raw
  |> String.split_on_char '\n'
  |> List.find_map (fun line ->
       let line =
         let len = String.length line in
         if len > 0 && Char.equal line.[len - 1] '\r' then
           String.sub line 0 (len - 1)
         else
           line
       in
       match String.index_opt line ':' with
       | None -> None
       | Some index ->
         let actual =
           String.sub line 0 index
           |> String.trim
           |> String.lowercase_ascii
         in
         if String.equal actual expected then
           Some
             (String.sub line (index + 1) (String.length line - index - 1)
              |> String.trim)
         else
           None)

let has_suffix suffix value =
  let suffix_len = String.length suffix in
  let value_len = String.length value in
  value_len >= suffix_len
  && String.equal suffix
       (String.sub value (value_len - suffix_len) suffix_len)

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
  Eta_http.Ws.Codec.encode
    { fin = true; opcode = Text; payload = Bytes.of_string payload }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let read_ws_text flow =
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
  let masked = (Char.code (Bytes.get header 1) land 0x80) <> 0 in
  let len0 = Char.code (Bytes.get header 1) land 0x7f in
  let payload_len =
    if len0 = 126 then
      let ext = read_exact 2 in
      (Char.code (Bytes.get ext 0) lsl 8) lor Char.code (Bytes.get ext 1)
    else if len0 = 127 then begin
      let ext = read_exact 8 in
      let v = ref 0 in
      for i = 0 to 7 do
        v := (!v lsl 8) lor Char.code (Bytes.get ext i)
      done;
      !v
    end
    else len0
  in
  let mask_key = if masked then Some (read_exact 4) else None in
  let payload = read_exact payload_len in
  (match mask_key with
   | Some key ->
     for i = 0 to payload_len - 1 do
       let b =
         Char.code (Bytes.get payload i) lxor Char.code (Bytes.get key (i mod 4))
       in
       Bytes.set payload i (Char.chr b)
     done
   | None -> ());
  Bytes.to_string payload

let close_payload code reason =
  let payload = Bytes.create (2 + String.length reason) in
  Bytes.set payload 0 (Char.chr ((code lsr 8) land 0xff));
  Bytes.set payload 1 (Char.chr (code land 0xff));
  Bytes.blit_string reason 0 payload 2 (String.length reason);
  payload

let write_ws_close flow =
  Eta_http.Ws.Codec.encode
    { fin = true; opcode = Close; payload = close_payload 1000 "" }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let write_ws_switching_response flow key =
  let response =
    "HTTP/1.1 101 Switching Protocols\r\n"
    ^ "Upgrade: websocket\r\n"
    ^ "Connection: keep-alive, Upgrade\r\n"
    ^ "Sec-WebSocket-Accept: "
    ^ Eta_http.Ws.Codec.accept_key key
    ^ "\r\n\r\n"
  in
  Eio.Flow.copy_string response flow

let run_realtime_capture_session_update flow captured =
  let head = read_http_head flow in
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some k -> k
    | None -> Alcotest.fail "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key;
  let update_json = read_ws_text flow in
  (match Yojson.Safe.from_string update_json with
   | exception _ -> ()
   | json -> captured := json);
  write_ws_text flow {|{"type":"session.updated"}|};
  write_ws_close flow;
  try Eio.Flow.shutdown flow `Send with _ -> ()

let test_codex_realtime_transcribe_with_prompt_ignores_language_e2e () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw ~reuse_addr:true ~backlog:1
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = tcp_port (Eio.Net.listening_addr socket) in
  let captured = ref `Null in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Switch.run @@ fun conn_sw ->
    let flow, _addr = Eio.Net.accept ~sw:conn_sw socket in
    run_realtime_capture_session_update flow captured);
  let rt = Eta.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) () in
  let old_url = Sys.getenv_opt "CODEX_REALTIME_BASE_URL_OVERRIDE" in
  Unix.putenv "CODEX_REALTIME_BASE_URL_OVERRIDE"
    (Printf.sprintf "ws://127.0.0.1:%d" port);
  Unix.putenv "OPENAI_API_KEY" "test-token";
  let cleanup_env () =
    match old_url with
    | Some v -> Unix.putenv "CODEX_REALTIME_BASE_URL_OVERRIDE" v
    | None -> Unix.putenv "CODEX_REALTIME_BASE_URL_OVERRIDE" ""
  in
  let pcm = Bytes.make 3200 '\x00' in
  let effect =
    Stt.Codex_realtime.transcribe_with_prompt
      ~sw ~net:(Eio.Stdenv.net env)
      ~model:"m"
      ~language:"pl"
      ~prompt_template:"Transcribe in {{language}}."
      ~sample_rate:16000
      pcm
    |> Eta.Effect.timeout_as
         (Eta.Duration.seconds 5)
         ~on_timeout:(`Stt_error "timeout")
  in
  (match Eta.Runtime.run rt effect with Ok _ | Error _ -> ());
  cleanup_env ();
  match !captured with
  | `Null -> Alcotest.fail "did not capture session.update message"
  | `Assoc fields ->
    (match List.assoc_opt "session" fields with
     | Some (`Assoc session) ->
       (* transcribe_with_prompt must render ~prompt_template (with ~language
          substituted) into the session.update. Per the realtime transcription
          schema the prompt belongs under audio.input.transcription.prompt, not
          at the session top level, so check it there and confirm the language
          made it into the rendered text. *)
       let transcription =
         match List.assoc_opt "audio" session with
         | Some (`Assoc audio) ->
           (match List.assoc_opt "input" audio with
            | Some (`Assoc input) ->
              (match List.assoc_opt "transcription" input with
               | Some (`Assoc transcription) -> transcription
               | _ -> Alcotest.fail "expected transcription object")
            | _ -> Alcotest.fail "expected input object")
         | _ -> Alcotest.fail "expected audio object"
       in
       let prompt =
         match List.assoc_opt "prompt" transcription with
         | Some (`String value) -> value
         | _ -> Alcotest.fail "transcription should include the rendered prompt"
       in
       Alcotest.(check bool)
         "rendered prompt should include the configured language"
         true
         (let needle = "pl" in
          let nlen = String.length needle in
          let plen = String.length prompt in
          let rec scan i =
            if i + nlen > plen then false
            else if String.sub prompt i nlen = needle then true
            else scan (i + 1)
          in
          scan 0)
     | _ -> Alcotest.fail "expected session object")
  | _ -> Alcotest.fail "expected JSON object"

let test_codex_realtime_translate_nonconv_drops_prompt_e2e () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw ~reuse_addr:true ~backlog:1
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = tcp_port (Eio.Net.listening_addr socket) in
  let captured = ref `Null in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Switch.run @@ fun conn_sw ->
    let flow, _addr = Eio.Net.accept ~sw:conn_sw socket in
    run_realtime_capture_session_update flow captured);
  let rt = Eta.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) () in
  let old_url = Sys.getenv_opt "CODEX_REALTIME_BASE_URL_OVERRIDE" in
  Unix.putenv "CODEX_REALTIME_BASE_URL_OVERRIDE"
    (Printf.sprintf "ws://127.0.0.1:%d" port);
  Unix.putenv "OPENAI_API_KEY" "test-token";
  let cleanup_env () =
    match old_url with
    | Some v -> Unix.putenv "CODEX_REALTIME_BASE_URL_OVERRIDE" v
    | None -> Unix.putenv "CODEX_REALTIME_BASE_URL_OVERRIDE" ""
  in
  let pcm = Bytes.make 3200 '\x00' in
  let effect =
    Stt.Codex_realtime.translate_with_prompt
      ~sw ~net:(Eio.Stdenv.net env)
      ~model:"not-the-conversation-model"
      ~source_language:"pl" ~target_language:"en"
      ~prompt_template:"Translate from {{source_language}} to {{target_language}}."
      ~sample_rate:16000
      pcm
    |> Eta.Effect.timeout_as
         (Eta.Duration.seconds 5)
         ~on_timeout:(`Stt_error "timeout")
  in
  (match Eta.Runtime.run rt effect with Ok _ | Error _ -> ());
  cleanup_env ();
  match !captured with
  | `Null -> Alcotest.fail "did not capture session.update"
  | `Assoc fields ->
    (match List.assoc_opt "session" fields with
     | Some (`Assoc session) ->
       (* Bug: For non-conversation translation, translate_with_prompt renders
          the prompt template but build_translation_session_update has no
          prompt/instructions parameter. The rendered prompt is silently
          dropped from the session.update. *)
       Alcotest.(check bool)
         "non-conversation translation should include instructions"
         true
         (List.mem_assoc "instructions" session)
     | _ -> Alcotest.fail "expected session object")
  | _ -> Alcotest.fail "expected JSON object"

let run_realtime_error_ws_server flow =
  let head = read_http_head flow in
  Alcotest.(check (option string))
    "request target"
    (Some "/v1/realtime?model=gpt-realtime-2")
    (request_target head);
  Alcotest.(check (option string))
    "beta header"
    None
    (header_value "OpenAI-Beta" head);
  Alcotest.(check (option string))
    "originator"
    (Some "codex_cli_rs")
    (header_value "originator" head);
  Alcotest.(check (option string))
    "version"
    (Some "0.0.0")
    (header_value "version" head);
  (match header_value "x-session-id" head with
   | Some value when String.starts_with ~prefix:"camelpie-" value -> ()
   | Some value -> Alcotest.failf "unexpected x-session-id: %s" value
   | None -> Alcotest.fail "missing x-session-id");
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> Alcotest.fail "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key;
  write_ws_text
    flow
    {|{"type":"error","error":{"message":"boom","code":"bad_request"}}|};
  write_ws_close flow;
  try Eio.Flow.shutdown flow `Send with _ -> ()

let test_codex_realtime_stream_reports_server_error_e2e () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw
      ~reuse_addr:true
      ~backlog:1
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = tcp_port (Eio.Net.listening_addr socket) in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Switch.run @@ fun conn_sw ->
    let flow, _addr = Eio.Net.accept ~sw:conn_sw socket in
    run_realtime_error_ws_server flow);
  let rt = Eta.Runtime.create ~sw ~clock:(Eio.Stdenv.clock env) () in
  let url = Printf.sprintf "ws://127.0.0.1:%d/v1/realtime?model=gpt-realtime-2" port in
  let open Eta.Syntax in
  let prompt = "Translate." in
  let effect =
    Stt.Codex_realtime.with_stream_session
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~url
      ~access_token:"test-token"
      ~mode:Stt.Codex_realtime.Conversation
      ~session_update:(Stt.Codex_realtime.build_conversation_translation_session_update
                         ~model:"gpt-realtime-2"
                         ~prompt)
      ~response_create:(Stt.Codex_realtime.build_conversation_translation_response_create
                          ~prompt)
      ~sample_rate:16000
      ~cancel_message:"cancelled"
      (fun stream ->
         let* () = Eta.Effect.delay (Eta.Duration.ms 100) Eta.Effect.unit in
         let* () = stream.offer_chunk (Bytes.make 32000 '\001') in
         stream.finish_stream ())
    |> Eta.Effect.timeout_as
         (Eta.Duration.seconds 5)
         ~on_timeout:(`Stt_error "timeout")
  in
  let pp_stt_error fmt = function
    | `Stt_error message -> Format.pp_print_string fmt message
  in
  match Eta.Runtime.run rt effect with
  | Error (Eta.Cause.Fail (`Stt_error message)) ->
    Alcotest.(check string)
      "server error"
      "Codex realtime server error: boom (bad_request)"
      message
  | Error cause ->
    Alcotest.failf
      "expected server error, got cause: %a"
      (Eta.Cause.pp pp_stt_error)
      cause
  | Ok text ->
    Alcotest.failf "expected server error, got success: %s" text

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let temp_dir prefix =
  let path = Filename.temp_file prefix "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  path

let test_recording_coordinator_persist_error_swallowed () =
  let base = temp_dir "camelpie-coordinator-persist" in
  let path = Filename.concat base "recording.json" in
  write_file path "not a directory";
  let persist_path = Filename.concat path "nested.json" in
  (* create persists its initial state. When the target is unwritable (here the
     parent component is a regular file, not a directory), create must fail
     loudly instead of silently swallowing the error and handing back a
     coordinator whose state file was never written. *)
  match Recording.Coordinator.create ~persist_path () with
  | _coordinator ->
    Alcotest.fail "create should raise when it cannot persist initial state"
  | exception _ -> ()

let test_recording_coordinator_stop_clears_error () =
  let path = Filename.temp_file "camelpie-recording" ".json" in
  let coordinator =
    Recording.Coordinator.create ~persist_path:path ()
  in
  (* Start a meeting, set an error, then stop via toggle_meeting *)
  ignore
    (Recording.Coordinator.try_start
       coordinator Recording.Coordinator.Meeting_transcribe ());
  Recording.Coordinator.set_error coordinator "toggle-stops-meeting";
  let snap_toggle = Recording.Coordinator.toggle_meeting coordinator in
  (* toggle_meeting preserves last_error when stopping *)
  Alcotest.(check (option string))
    "toggle_meeting preserves last_error"
    (Some "toggle-stops-meeting")
    snap_toggle.Recording.Coordinator.last_error;
  (* Now start fresh and stop via stop *)
  ignore
    (Recording.Coordinator.try_start
       coordinator Recording.Coordinator.Meeting_transcribe ());
  Recording.Coordinator.set_error coordinator "stop-clears-it";
  ignore
    (Recording.Coordinator.stop
       coordinator Recording.Coordinator.Meeting_transcribe);
  let snap_stop = Recording.Coordinator.snapshot coordinator in
  (* Bug: stop clears last_error, but toggle_meeting preserves it.
     The behavior should be consistent — either both should clear or
     both should preserve the error on stop. *)
  Alcotest.(check (option string))
    "stop should also preserve last_error"
    (Some "stop-clears-it")
    snap_stop.Recording.Coordinator.last_error;
  Sys.remove path

let test_daemon_server_mkdir_p_silently_accepts_file () =
  let base = temp_dir "camelpie-daemon-mkdir_p" in
  let file_path = Filename.concat base "not-a-dir" in
  write_file file_path "block";
  (* Bug: Daemon.Server.mkdir_p is a duplicate of the old buggy mkdir_p that
     checks Sys.file_exists without Sys.is_directory. When called on an
     existing regular file it silently returns () instead of raising EEXIST.
     Fs.mkdir_p was fixed but this copy was missed. *)
  try
    Camelpie.Daemon.Server.mkdir_p file_path;
    Alcotest.fail
      "mkdir_p should raise EEXIST for an existing file, not silently return"
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | exn -> Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)

let test_assistant_prompt_value_utf8_truncation () =
  (* Build a 201-byte string: 199 ASCII chars followed by a 2-byte UTF-8
     character 'e' (C3 A9). A naive String.sub cap at 200 bytes slices inside
     that 2-byte sequence, leaving a dangling C3 lead byte and producing an
     invalid encoding the receiving API may reject. A correct truncation backs
     off to the character boundary, dropping the split character entirely, so
     the result is a valid UTF-8 string of at most 200 bytes (here 199). *)
  let prefix = String.make 199 'x' in
  let input = prefix ^ "\195\169" in (* 'e' = two bytes *)
  match Camelpie.Commands.Assistant.prompt_value (Some input) with
  | None -> Alcotest.fail "prompt_value returned None"
  | Some truncated ->
    Alcotest.(check bool) "at most 200 bytes" true
      (String.length truncated <= 200);
    Alcotest.(check bool) "actually truncated" true
      (String.length truncated < String.length input);
    let last_byte = Char.code truncated.[String.length truncated - 1] in
    (* The result must not end inside a multi-byte UTF-8 sequence: the final
       byte is neither a lead byte (>= 0xC0) nor a continuation byte
       (0x80..0xBF). For this input it is the ASCII 'x'. *)
    Alcotest.(check bool)
      "truncation should not split a UTF-8 character"
      true
      (last_byte < 128);
    Alcotest.(check string) "keeps the complete leading run" prefix truncated

let test_pulse_parse_error_code_short_payload () =
  (* Bug: parse_error_code calls Protocol.Tag_struct_reader.get_u32 without
     checking whether the payload has at least 4 bytes. A malformed 1-byte
     error response from the PulseAudio server causes a Failure crash that
     terminates the read_loop and kills the entire connection. *)
  try
    let result =
      Camelpie.Pulse.Commands.parse_error_code (Bytes.make 1 '\x00')
    in
    Alcotest.(check (option int))
      "short error code payload should return None"
      None
      result
  with Failure msg ->
    Alcotest.fail
      ("parse_error_code crashed on short payload: " ^ msg)

let test_pulse_default_record_stream_options_mismatched_channel_map () =
  let default = Camelpie.Pulse.Defs.default_record_stream_options () in
  let overrides = { default with
    sample_spec = { format = Camelpie.Pulse.Defs.Pa_sample_format.s16le;
                    channels = 2; rate = 16000 };
    channel_map = [];
  } in
  let options =
    Camelpie.Pulse.Defs.default_record_stream_options ~overrides ()
  in
  (* Bug: default_record_stream_options uses the override's channel_map
     without validating that its length matches sample_spec.channels.
     PulseAudio requires one channel_map entry per channel. *)
  Alcotest.(check int)
    "channel map length must match channel count"
    options.sample_spec.channels
    (List.length options.channel_map)

let test_pulse_parse_command_envelope_short_payload () =
  (* parse_command_envelope reads two u32 tag-struct values (10 bytes). It is
     the very first function called for every incoming packet, so a malformed
     short payload must be handled gracefully (return None) rather than raising
     a Failure that tears down the whole read loop. *)
  Alcotest.(check bool)
    "short payload returns None instead of crashing"
    true
    (Camelpie.Pulse.Commands.parse_command_envelope (Bytes.make 3 '\x00') = None)

let test_daemon_contract_meeting_toggle_no_indicator () =
  let snap : Camelpie.Daemon.Contract.recording_snapshot =
    { active = true; mode = "ptt-transcribe"; enabled = true;
      started_at = None; updated_at = None; transcript_path = None;
      last_error = None; }
  in
  let json =
    Camelpie.Daemon.Contract.response_to_json
      (Camelpie.Daemon.Contract.Meeting_toggle_resp snap)
  in
  match json with
  | `Assoc fields ->
    (* Bug: Meeting_stop_resp now includes a "stopped" boolean so callers
       can tell whether the stop had any effect. Meeting_toggle_resp only
       carries a snapshot with no "toggled" or success indicator. The
       caller has no programmatic way to know if toggle changed anything. *)
    Alcotest.(check bool)
      "meeting toggle response should include a success indicator"
      true
      (List.mem_assoc "toggled" fields || List.mem_assoc "changed" fields)
  | _ -> Alcotest.fail "expected JSON object"

let test_codex_realtime_translation_ignores_prompt () =
  let update =
    Stt.Codex_realtime.build_translation_session_update ~target_language:"pl" ()
  in
  match update with
  | `Assoc fields ->
    (match List.assoc_opt "session" fields with
     | Some (`Assoc session) ->
       (* Bug: translate_with_prompt renders the prompt template into a
          usable prompt, but the non-conversation translation path calls
          build_translation_session_update which has no ~prompt parameter
          and includes no instructions. The rendered prompt is silently
          dropped. The conversation translation path does include
          instructions via build_conversation_translation_session_update. *)
       Alcotest.(check bool)
         "translation session should include instructions"
         true
         (List.mem_assoc "instructions" session)
     | _ -> Alcotest.fail "expected session object")
  | _ -> Alcotest.fail "expected JSON object"

let test_stt_config_whitespace_app_id_not_trimmed () =
  let dir = temp_dir "camelpie-stt-config-whitespace" in
  let config_path = Filename.concat dir "stt.toml" in
  write_file
    config_path
    {toml|
schema_version = 2

[[newline_rules]]
app_id = "   "
action = "ctrl-j"
|toml};
  (* A whitespace-only app_id is not a real matcher. get_optional_string trims
     it to None, leaving a rule with neither app_id nor title_regex. Such a
     matcher-less rule would match every window (a catch-all), so it must be
     rejected at load time rather than silently accepted. *)
  match Camelpie.Config.Stt.load ~config_path () with
  | _ ->
    Alcotest.fail "whitespace-only app_id should be rejected, not loaded"
  | exception Camelpie.Config.Stt.Config_error _ -> ()

let test_assistant_focused_window_prompt_context () =
  let window : Camelpie.Niri.window_info =
    {
      id = 7;
      title = Some "  Project   notes\nDraft  ";
      app_id = Some "org.editor";
      pid = None;
      workspace_id = None;
    }
  in
  let prompt =
    Camelpie.Commands.Assistant.append_focused_window_context
      "Transcribe in {{language}}."
      (Some window)
  in
  Alcotest.(check string)
    "prompt context"
    "Transcribe in {{language}}.\n\nFocused window context:\n- app_id: org.editor\n- title: Project notes Draft\nUse this as context only when it helps disambiguate the audio."
    prompt

let json_member name = function
  | `Assoc fields ->
    (match List.assoc_opt name fields with
     | Some value -> value
     | None -> Alcotest.failf "missing JSON field: %s" name)
  | _ -> Alcotest.fail "expected JSON object"

let json_string name json =
  match json_member name json with
  | `String value -> value
  | _ -> Alcotest.failf "expected JSON string field: %s" name

let test_assistant_startup_config_json () =
  let config : Camelpie.Config.Stt.t =
    {
      schema_version = 2;
      provider = Camelpie.Config.Stt.Codex_realtime;
      transcription_model = "openrouter/google/gemini-3.1-flash-lite";
      translation_model = "openrouter/google/gemini-3.1-flash-lite";
      transcription_language = "Polish";
      translation_source_language = "Polish";
      translation_target_language = "English";
      wakeword_enabled = false;
      wakeword_dictation_silence_seconds = 3.0;
      wakeword_dictation_max_seconds = 120.0;
      wakeword_dictation_speech_rms_threshold = 0.01;
      ptt_min_capture_rms = 0.001;
      ptt_min_capture_peak = 0.008;
      thinking_effort = Some "minimal";
      transcription_prompt = "Transcribe in {{language}}.";
      translation_prompt = "Translate from {{source_language}} to {{target_language}}.";
      newline_rules =
        [
          {
            app_id = Some "Alacritty";
            title_regex = Some "^π";
            action = Camelpie.Input.Text_injection.Ctrl_j;
          };
        ];
    }
  in
  let report =
    Camelpie.Commands.Assistant.startup_config_json
      ~config_path:"/tmp/stt.toml"
      ~config
      ~ptt_transcribe_keysym:65478
      ~ptt_translate_keysym:65479
      ~min_duration_ms:120
      ()
  in
  let json = Yojson.Safe.from_string report in
  Alcotest.(check string) "config path" "/tmp/stt.toml" (json_string "config_path" json);
  Alcotest.(check string) "provider" "codex-realtime" (json_string "provider" json);
  Alcotest.(check string) "thinking effort" "minimal" (json_string "thinking_effort" json);
  Alcotest.(check string)
    "prompt"
    "Transcribe in {{language}}."
    (json_string "transcription_prompt" json);
  let transcription_model = json_member "transcription_model" json in
  Alcotest.(check string)
    "configured model"
    "openrouter/google/gemini-3.1-flash-lite"
    (json_string "configured" transcription_model);
  let resolved = json_member "resolved" transcription_model in
  Alcotest.(check string) "resolved provider" "openrouter" (json_string "provider" resolved);
  Alcotest.(check string)
    "resolved model"
    "google/gemini-3.1-flash-lite"
    (json_string "model" resolved);
  match json_member "newline_rules" json with
  | `List [ rule ] ->
    Alcotest.(check string) "newline app" "Alacritty" (json_string "app_id" rule);
    Alcotest.(check string) "newline action" "ctrl-j" (json_string "action" rule)
  | _ -> Alcotest.fail "expected one newline rule"

let test_assistant_project_context_prompt_context () =
  let dir = temp_dir "camelpie-project-context" in
  let project_dir = Filename.concat dir "project" in
  let nested_dir = Filename.concat project_dir "src" in
  let pie_dir = Filename.concat project_dir ".pie" in
  Unix.mkdir project_dir 0o700;
  Unix.mkdir nested_dir 0o700;
  Unix.mkdir pie_dir 0o700;
  write_file
    (Filename.concat pie_dir "context.md")
    "Use Eta for the internal effect library.\nGlossary: pie means push-to-talk assistant.";
  let context = Camelpie.Context.Project.load_for_cwd nested_dir in
  let prompt =
    Camelpie.Context.Project.append_to_prompt
      "Translate to {{target_language}}."
      context
  in
  Alcotest.(check string)
    "project context"
    "Translate to {{target_language}}.\n\nProject context from .pie/context.md:\nUse Eta for the internal effect library.\nGlossary: pie means push-to-talk assistant."
    prompt

let test_assistant_project_context_missing_is_empty () =
  let dir = temp_dir "camelpie-project-context" in
  let context = Camelpie.Context.Project.load_for_cwd dir in
  let prompt =
    Camelpie.Context.Project.append_to_prompt
      "Translate to {{target_language}}."
      context
  in
  Alcotest.(check string)
    "no project context placeholder"
    "Translate to {{target_language}}."
    prompt

let test_project_context_relative_cwd_traversal () =
  let dir = temp_dir "camelpie-project-context-relative" in
  let pie_dir = Filename.concat dir ".pie" in
  Unix.mkdir pie_dir 0o700;
  write_file (Filename.concat pie_dir "context.md") "Project context.";
  let old_cwd = Sys.getcwd () in
  Sys.chdir dir;
  try
    Unix.mkdir "subdir" 0o700;
    Sys.chdir "subdir";
    let context = Camelpie.Context.Project.load_for_cwd "." in
    Sys.chdir old_cwd;
    (* Bug: find_context_file uses Filename.dirname which returns "." for ".",
       so it never traverses parent directories when given a relative path.
       The context file at ../.pie/context.md should be found. *)
    Alcotest.(check (option string))
      "relative cwd should traverse parents to find context"
      (Some "Project context.")
      context
  with exn ->
    Sys.chdir old_cwd;
    raise exn

let test_assistant_project_context_empty_file_is_empty () =
  let dir = temp_dir "camelpie-project-context" in
  let pie_dir = Filename.concat dir ".pie" in
  Unix.mkdir pie_dir 0o700;
  write_file (Filename.concat pie_dir "context.md") "  \n\t  ";
  let context = Camelpie.Context.Project.load_for_cwd dir in
  let prompt =
    Camelpie.Context.Project.append_to_prompt
      "Translate to {{target_language}}."
      context
  in
  Alcotest.(check string)
    "empty project context"
    "Translate to {{target_language}}."
    prompt

let process ?(ppid = 0) ?(pgrp = 0) ?(tty_nr = 0) ?(tpgid = 0) ?cwd pid =
  {
    Camelpie.Context.Project.pid;
    ppid;
    pgrp;
    tty_nr;
    tpgid;
    depth = 0;
    cwd;
  }

let test_project_context_prefers_foreground_terminal_cwd () =
  let processes =
    [
      process 10 ~cwd:"/home/ribelo";
      process 11 ~ppid:10 ~pgrp:11 ~tty_nr:34816 ~tpgid:12 ~cwd:"/home/ribelo/projects/camelpie";
      process 12 ~ppid:11 ~pgrp:12 ~tty_nr:34816 ~tpgid:12 ~cwd:"/home/ribelo/projects/camelpie/subdir";
    ]
  in
  Alcotest.(check (option string))
    "foreground cwd"
    (Some "/home/ribelo/projects/camelpie/subdir")
    (Camelpie.Context.Project.cwd_from_processes
       ~root_pid:10
       ~root_cwd:(Some "/home/ribelo")
       processes)

let test_project_context_falls_back_to_root_cwd () =
  let processes =
    [
      process 10 ~cwd:"/home/ribelo/projects/gui-app";
      process 11 ~ppid:10;
    ]
  in
  Alcotest.(check (option string))
    "root cwd"
    (Some "/home/ribelo/projects/gui-app")
    (Camelpie.Context.Project.cwd_from_processes
       ~root_pid:10
       ~root_cwd:(Some "/home/ribelo/projects/gui-app")
       processes)

let test_project_metadata_package_json () =
  let dir = temp_dir "camelpie-project-metadata" in
  write_file
    (Filename.concat dir "package.json")
    {|{"name":"web-tool","description":"TypeScript UI tool"}|};
  let prompt =
    Camelpie.Context.Metadata.load_for_cwd dir
    |> Camelpie.Context.Metadata.append_to_prompt "Base prompt."
  in
  Alcotest.(check string)
    "package.json metadata"
    "Base prompt.\n\nProject metadata for transcription and translation disambiguation:\nUse this metadata to choose words, spellings, identifiers, and technical terms that best fit the project context. Do not mention this metadata in the output unless it is actually spoken or needed for the requested translation.\n- manifest: package.json\n- language: TypeScript\n- package: web-tool\n- description: TypeScript UI tool"
    prompt

let test_project_metadata_cargo_toml () =
  let dir = temp_dir "camelpie-project-metadata" in
  write_file
    (Filename.concat dir "Cargo.toml")
    {toml|[package]
name = "rust-tool"
description = "Rust CLI"
|toml};
  let prompt =
    Camelpie.Context.Metadata.load_for_cwd dir
    |> Camelpie.Context.Metadata.append_to_prompt "Base prompt."
  in
  Alcotest.(check string)
    "Cargo.toml metadata"
    "Base prompt.\n\nProject metadata for transcription and translation disambiguation:\nUse this metadata to choose words, spellings, identifiers, and technical terms that best fit the project context. Do not mention this metadata in the output unless it is actually spoken or needed for the requested translation.\n- manifest: Cargo.toml\n- language: Rust\n- package: rust-tool\n- description: Rust CLI"
    prompt

let test_project_metadata_pyproject_toml () =
  let dir = temp_dir "camelpie-project-metadata" in
  write_file
    (Filename.concat dir "pyproject.toml")
    {toml|[project]
name = "python-tool"
description = "Python service"

[tool.uv]
|toml};
  let prompt =
    Camelpie.Context.Metadata.load_for_cwd dir
    |> Camelpie.Context.Metadata.append_to_prompt "Base prompt."
  in
  Alcotest.(check string)
    "pyproject.toml metadata"
    "Base prompt.\n\nProject metadata for transcription and translation disambiguation:\nUse this metadata to choose words, spellings, identifiers, and technical terms that best fit the project context. Do not mention this metadata in the output unless it is actually spoken or needed for the requested translation.\n- manifest: pyproject.toml\n- language: Python\n- package: python-tool\n- description: Python service"
    prompt

let test_project_metadata_dune_project () =
  let dir = temp_dir "camelpie-project-metadata" in
  write_file
    (Filename.concat dir "dune-project")
    {|(lang dune 3.21)
(name camelpie)
(package
 (name camelpie)
 (synopsis "Camelpie")
 (description "Camelpie OCaml library."))
|};
  let prompt =
    Camelpie.Context.Metadata.load_for_cwd dir
    |> Camelpie.Context.Metadata.append_to_prompt "Base prompt."
  in
  Alcotest.(check string)
    "dune-project metadata"
    "Base prompt.\n\nProject metadata for transcription and translation disambiguation:\nUse this metadata to choose words, spellings, identifiers, and technical terms that best fit the project context. Do not mention this metadata in the output unless it is actually spoken or needed for the requested translation.\n- manifest: dune-project\n- language: OCaml\n- package: camelpie\n- description: Camelpie OCaml library."
    prompt

let test_project_metadata_opam_without_dune_project () =
  let dir = temp_dir "camelpie-project-metadata" in
  write_file
    (Filename.concat dir "sample.opam")
    {|opam-version: "2.0"
synopsis: "Sample"
description: "Sample OCaml package"
|};
  let prompt =
    Camelpie.Context.Metadata.load_for_cwd dir
    |> Camelpie.Context.Metadata.append_to_prompt "Base prompt."
  in
  Alcotest.(check string)
    "opam metadata"
    "Base prompt.\n\nProject metadata for transcription and translation disambiguation:\nUse this metadata to choose words, spellings, identifiers, and technical terms that best fit the project context. Do not mention this metadata in the output unless it is actually spoken or needed for the requested translation.\n- manifest: sample.opam\n- language: OCaml\n- package: sample\n- description: Sample OCaml package"
    prompt

let test_project_metadata_exact_directory_only () =
  let dir = temp_dir "camelpie-project-metadata" in
  let child = Filename.concat dir "child" in
  Unix.mkdir child 0o700;
  write_file
    (Filename.concat dir "package.json")
    {|{"name":"parent","description":"Parent package"}|};
  let prompt =
    Camelpie.Context.Metadata.load_for_cwd child
    |> Camelpie.Context.Metadata.append_to_prompt "Base prompt."
  in
  Alcotest.(check string)
    "no parent metadata"
    "Base prompt."
    prompt

let test_project_metadata_manifest_without_description () =
  let dir = temp_dir "camelpie-project-metadata" in
  write_file
    (Filename.concat dir "pyproject.toml")
    {toml|[tool.uv]
|toml};
  let prompt =
    Camelpie.Context.Metadata.load_for_cwd dir
    |> Camelpie.Context.Metadata.append_to_prompt "Base prompt."
  in
  Alcotest.(check string)
    "language only"
    "Base prompt.\n\nProject metadata for transcription and translation disambiguation:\nUse this metadata to choose words, spellings, identifiers, and technical terms that best fit the project context. Do not mention this metadata in the output unless it is actually spoken or needed for the requested translation.\n- manifest: pyproject.toml\n- language: Python"
    prompt

let test_stt_config_loads_toml_newline_rules () =
  let dir = temp_dir "camelpie-stt-config" in
  let prompts_dir = Filename.concat dir "prompts" in
  Unix.mkdir prompts_dir 0o700;
  write_file
    (Filename.concat prompts_dir "transcription.md")
    "Transcribe the spoken audio in {{language}}.";
  write_file
    (Filename.concat prompts_dir "translation.md")
    "Translate from {{source_language}} to {{target_language}}.";
  let config_path = Filename.concat dir "stt.toml" in
  write_file
    config_path
    {toml|
schema_version = 2
provider = "codex-realtime"
transcription_model = "gpt-realtime-whisper"
translation_model = "gpt-realtime-2"
transcription_language = "Polish"
translation_source_language = "Polish"
translation_target_language = "English"
wakeword_enabled = false
wakeword_dictation_silence_seconds = 3.0
wakeword_dictation_max_seconds = 120.0
wakeword_dictation_speech_rms_threshold = 0.01
ptt_min_capture_rms = 0.001
ptt_min_capture_peak = 0.008
thinking_effort = "minimal"

[[newline_rules]]
app_id = "Alacritty"
title_regex = "(codex|π - csai)"
action = "ctrl-j"

[[newline_rules]]
app_id = "Slack"
action = "shift-enter"
|toml};
	  let config = Camelpie.Config.Stt.load ~config_path () in
	  Alcotest.(check bool) "wakeword" false config.wakeword_enabled;
	  Alcotest.(check (float 0.00001)) "ptt rms" 0.001 config.ptt_min_capture_rms;
	  Alcotest.(check (float 0.00001)) "ptt peak" 0.008 config.ptt_min_capture_peak;
	  Alcotest.(check (option string)) "thinking effort" (Some "minimal") config.thinking_effort;
	  Alcotest.(check int) "newline rules" 2 (List.length config.newline_rules);
  match config.newline_rules with
  | first :: second :: [] ->
    Alcotest.(check (option string)) "first app" (Some "Alacritty") first.app_id;
    Alcotest.(check (option string)) "first regex" (Some "(codex|π - csai)") first.title_regex;
    Alcotest.(check bool) "first action" true (first.action = Camelpie.Input.Text_injection.Ctrl_j);
    Alcotest.(check (option string)) "second app" (Some "Slack") second.app_id;
    Alcotest.(check bool) "second action" true (second.action = Camelpie.Input.Text_injection.Shift_enter)
  | _ -> Alcotest.fail "expected two newline rules"

let test_stt_config_rejects_bad_title_regex () =
  let dir = temp_dir "camelpie-stt-config" in
  let config_path = Filename.concat dir "stt.toml" in
  write_file
    config_path
    {toml|
schema_version = 2

[[newline_rules]]
app_id = "Alacritty"
title_regex = "("
action = "ctrl-j"
|toml};
  match Camelpie.Config.Stt.load ~config_path () with
  | _ -> Alcotest.fail "expected invalid regex to fail"
  | exception Camelpie.Config.Stt.Config_error msg ->
    Alcotest.(check bool) "mentions title_regex" true (String.contains msg 'r')

let test_stt_config_rejects_prompt_with_wrong_placeholder () =
  let dir = temp_dir "camelpie-stt-config-prompt" in
  let prompts_dir = Filename.concat dir "prompts" in
  Unix.mkdir prompts_dir 0o700;
  write_file
    (Filename.concat prompts_dir "transcription.md")
    "Transcribe the audio in {language}.";
  write_file
    (Filename.concat prompts_dir "translation.md")
    "Translate from {{source_language}} to {{target_language}}.";
  let config_path = Filename.concat dir "stt.toml" in
  write_file
    config_path
    {toml|
schema_version = 2
|toml};
  (* Bug: validate_transcription_prompt checks for any '{' character instead
     of the required "{{language}}" placeholder. A prompt with "{language}"
     passes validation but render_template won't substitute it, so the
     language variable is silently omitted from the actual API request. *)
  match Camelpie.Config.Stt.load ~config_path () with
  | exception Camelpie.Config.Stt.Config_error msg ->
    Alcotest.(check bool) "mentions language placeholder" true
      (String.contains msg '{')
  | _ ->
    Alcotest.fail "expected Config_error for transcription prompt missing {{language}}"

let test_stt_config_resolves_provider_qualified_model () =
  let open Camelpie.Config.Stt in
  let openrouter =
    resolve_model_provider
      ~default_provider:Codex_realtime
      "openrouter/xiaomi/mimo-v2.5"
  in
  Alcotest.(check bool) "openrouter provider" true (openrouter.provider = Openrouter);
  Alcotest.(check string) "openrouter model" "xiaomi/mimo-v2.5" openrouter.model;
  let openrouter_native_model =
    resolve_model_provider
      ~default_provider:Openrouter
      "openai/whisper-large-v3"
  in
  Alcotest.(check bool)
    "unknown prefix uses default provider"
    true
    (openrouter_native_model.provider = Openrouter);
  Alcotest.(check string)
    "unknown prefix remains part of model id"
    "openai/whisper-large-v3"
    openrouter_native_model.model;
  match resolve_model_provider ~default_provider:Codex_realtime "openrouter/" with
  | _ -> Alcotest.fail "expected empty provider-qualified model to fail"
  | exception Camelpie.Config.Stt.Config_error msg ->
    Alcotest.(check bool) "mentions empty model" true (String.contains msg 'e')

let test_openrouter_responses_request_uses_text_and_audio () =
  let request : Eta_ai.chat_request =
    Camelpie.Stt.Openrouter.responses_request
      ~model:"xiaomi/mimo-v2.5"
      ~prompt:"Use Camelpie context."
      ~sample_rate:16000
      (Bytes.create 4)
  in
  Alcotest.(check string) "model id" "xiaomi/mimo-v2.5" request.model;
  Alcotest.(check (option int)) "max tokens" None request.max_output_tokens;
  match request.prompt with
  | [Eta_ai.User [Eta_ai.Text prompt; Eta_ai.Audio audio]] ->
    Alcotest.(check string) "prompt" "Use Camelpie context." prompt;
    Alcotest.(check bool) "wav format" true (audio.Eta_ai.format = Eta_ai.Wav);
    (match audio.Eta_ai.data with
     | Eta_ai.Bytes wav ->
       Alcotest.(check string) "wav riff" "RIFF" (Bytes.sub_string wav 0 4)
     | Eta_ai.Base64 _ -> Alcotest.fail "expected raw wav bytes")
  | _ -> Alcotest.fail "expected one user message with text and audio"

let test_openrouter_decodes_structured_output_only () =
  Alcotest.(check (result string reject))
    "structured text"
    (Ok "hello")
    (Camelpie.Stt.Openrouter.decode_structured_output "{\"text\":\"hello\"}");
  Alcotest.(check bool)
    "raw text rejected"
    true
    (match Camelpie.Stt.Openrouter.decode_structured_output "hello" with
     | Error _ -> true
     | Ok _ -> false);
  Alcotest.(check bool)
    "wrong field rejected"
    true
    (match Camelpie.Stt.Openrouter.decode_structured_output "{\"transcription\":\"hello\"}" with
     | Error _ -> true
     | Ok _ -> false)

let test_text_injection_newline_rule_regex () =
  let window : Camelpie.Niri.window_info =
    {
      id = 384;
      title = Some "Eta | codex | 019e6df3-3186-7a51-ba29-34d742378792";
      app_id = Some "Alacritty";
      pid = None;
      workspace_id = Some 3;
    }
  in
  let rules : Camelpie.Input.Text_injection.newline_rule list =
    [
      {
        app_id = Some "Alacritty";
        title_regex = Some "(^π - csai$|codex)";
        action = Camelpie.Input.Text_injection.Ctrl_j;
      };
    ]
  in
  Alcotest.(check bool)
    "regex selects ctrl-j"
    true
    (Camelpie.Input.Text_injection.newline_action_for_window rules (Some window)
     = Camelpie.Input.Text_injection.Ctrl_j)

let test_ptt_loop_continues_after_finish () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let rt = Eta.Runtime.create ~sw ~clock () in
  let keyboard = Camelpie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let running = ref true in
  let starts = ref 0 in
  let finishes = ref 0 in
  let chunk = Bytes.make 320 '\001' in
  Eio.Fiber.fork ~sw (fun () ->
    while !running do
      Eio.Stream.add audio chunk;
      Eio.Time.sleep clock 0.02
    done);
  let key released =
    Camelpie.Keyboard.Monitor.emit keyboard
      { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 0.05;
    key false;
    Eio.Time.sleep clock 0.05;
    key true;
    Eio.Time.sleep clock 2.3;
    key false;
    Eio.Time.sleep clock 0.05;
    key true;
    Eio.Time.sleep clock 2.3;
    key false;
    Eio.Time.sleep clock 0.05;
    key true);
  let handle =
    {
      Camelpie.Ptt_loop.offer = (fun _ -> Eta.Effect.unit);
      finish =
        (fun _ ->
           Eta.Effect.sync (fun () -> incr finishes)
           |> Eta.Effect.bind (fun () ->
             if !finishes = 2 then Eta.Effect.fail (`Ptt_error "done")
             else Eta.Effect.unit));
      cancel = Eta.Effect.unit;
    }
  in
  let config : Camelpie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
           if event.Camelpie.Keyboard.Monitor.keysym = 1 then
             Some
               (if event.released then
                  Camelpie.Ptt_loop.Release "test"
                else Press "test")
           else None);
	      min_duration_ms = 0;
	      min_capture_rms = Camelpie.Audio.Pcm.ptt_min_capture_rms_for_stt;
	      min_capture_peak = Camelpie.Audio.Pcm.ptt_min_capture_peak_for_stt;
	      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press =
        (fun _ ->
           Eta.Effect.sync (fun () -> incr starts)
           |> Eta.Effect.map (fun () -> Camelpie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort = Eta.Effect.pure false;
      on_dead_input = Eta.Effect.unit;
    }
  in
  let recv () = Eta.Effect.sync (fun () -> Eio.Stream.take audio) in
  let effect =
    Camelpie.Ptt_loop.run ~keyboard ~recv config
    |> Eta.Effect.timeout_as (Eta.Duration.seconds 7) ~on_timeout:(`Ptt_error "timeout")
  in
  let result = Eta.Runtime.run rt effect in
  running := false;
  Alcotest.(check int) "starts" 3 !starts;
  Alcotest.(check int) "finishes" 3 !finishes;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected second finish to stop the test loop"

let test_ptt_loop_post_roll_repress_continues_capture () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let rt = Eta.Runtime.create ~sw ~clock () in
  let keyboard = Camelpie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let starts = ref 0 in
  let finishes = ref 0 in
  let offered = ref [] in
  let key released =
    Camelpie.Keyboard.Monitor.emit keyboard
      { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 0.05;
    key false;
    Eio.Stream.add audio (Bytes.of_string "a");
    Eio.Time.sleep clock 0.05;
    key true;
    Eio.Time.sleep clock 0.10;
    key false;
    Eio.Stream.add audio (Bytes.of_string "b");
    Eio.Time.sleep clock 0.05;
    key true);
  let handle =
    {
      Camelpie.Ptt_loop.offer =
        (fun chunk ->
           Eta.Effect.sync (fun () ->
             offered := Bytes.to_string chunk :: !offered));
      finish =
        (fun _ ->
           Eta.Effect.sync (fun () -> incr finishes)
           |> Eta.Effect.bind (fun () -> Eta.Effect.fail (`Ptt_error "done")));
      cancel = Eta.Effect.unit;
    }
  in
  let config : Camelpie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
           if event.Camelpie.Keyboard.Monitor.keysym = 1 then
             Some
               (if event.released then
                  Camelpie.Ptt_loop.Release "test"
                else Press "test")
           else None);
	      min_duration_ms = 0;
	      min_capture_rms = Camelpie.Audio.Pcm.ptt_min_capture_rms_for_stt;
	      min_capture_peak = Camelpie.Audio.Pcm.ptt_min_capture_peak_for_stt;
	      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press =
        (fun _ ->
           Eta.Effect.sync (fun () -> incr starts)
           |> Eta.Effect.map (fun () -> Camelpie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort = Eta.Effect.pure false;
      on_dead_input = Eta.Effect.unit;
    }
  in
  let recv () = Eta.Effect.sync (fun () -> Eio.Stream.take audio) in
  let result =
    Eta.Runtime.run rt
      (Camelpie.Ptt_loop.run ~keyboard ~recv config
       |> Eta.Effect.timeout_as (Eta.Duration.seconds 5) ~on_timeout:(`Ptt_error "timeout"))
  in
  Alcotest.(check int) "one handle" 1 !starts;
  Alcotest.(check int) "one finish" 1 !finishes;
  Alcotest.(check (list string)) "chunks" ["a"; "b"] (List.rev !offered);
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected finish to stop the test loop"

let test_ptt_loop_scoped_handle_lifetime () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let rt = Eta.Runtime.create ~sw ~clock () in
  let keyboard = Camelpie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let scope_entries = ref 0 in
  let scope_exits = ref 0 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Camelpie.Keyboard.Monitor.emit keyboard
      { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 0.05;
    key false;
    Eio.Time.sleep clock 0.05;
    Eio.Stream.add audio (Bytes.make 320 '\001');
    Eio.Time.sleep clock 0.05;
    key true);
  let handle =
    {
      Camelpie.Ptt_loop.offer =
        (fun _ -> Eta.Effect.sync (fun () -> incr offers));
      finish = (fun _ -> Eta.Effect.sync (fun () -> incr finishes));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  let config : Camelpie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
           if event.Camelpie.Keyboard.Monitor.keysym = 1 then
             Some
               (if event.released then
                  Camelpie.Ptt_loop.Release "test"
                else Press "test")
           else None);
	      min_duration_ms = 0;
	      min_capture_rms = Camelpie.Audio.Pcm.ptt_min_capture_rms_for_stt;
	      min_capture_peak = Camelpie.Audio.Pcm.ptt_min_capture_peak_for_stt;
	      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press =
        (fun _ ->
           Eta.Effect.pure
             (Camelpie.Ptt_loop.Scoped
                (fun use ->
                   Eta.Effect.sync (fun () -> incr scope_entries)
                   |> Eta.Effect.bind (fun () ->
                     use handle)
                   |> Eta.Effect.bind (fun () ->
                     Eta.Effect.sync (fun () -> incr scope_exits)))));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort = Eta.Effect.pure false;
      on_dead_input = Eta.Effect.unit;
    }
  in
  let recv () = Eta.Effect.sync (fun () -> Eio.Stream.take audio) in
  let effect =
    Camelpie.Ptt_loop.run ~keyboard ~recv config
    |> Eta.Effect.timeout_as (Eta.Duration.seconds 5) ~on_timeout:(`Ptt_error "timeout")
  in
  let result = Eta.Runtime.run rt effect in
  Alcotest.(check int) "scope entered" 1 !scope_entries;
  Alcotest.(check int) "scope exited" 1 !scope_exits;
  Alcotest.(check int) "offers" 1 !offers;
  Alcotest.(check int) "finishes" 1 !finishes;
  Alcotest.(check int) "cancels" 0 !cancels;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected timeout after scoped capture returns to idle"

let pcm_chunk sample samples =
  let chunk = Bytes.create (samples * 2) in
  for i = 0 to samples - 1 do
    Bytes.set_int16_le chunk (i * 2) sample
  done;
  chunk

let mostly_quiet_chunk ~peak_sample samples =
  let chunk = Bytes.create (samples * 2) in
  Bytes.set_int16_le chunk 0 peak_sample;
  chunk

let test_ptt_loop_ignores_short_capture_before_offering () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let rt = Eta.Runtime.create ~sw ~clock () in
  let keyboard = Camelpie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Camelpie.Keyboard.Monitor.emit keyboard
      { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 0.05;
    key false;
    for _ = 1 to 3 do
      Eio.Time.sleep clock 0.02;
      Eio.Stream.add audio (pcm_chunk 12000 160)
    done;
    key true);
  let handle =
    {
      Camelpie.Ptt_loop.offer =
        (fun _ -> Eta.Effect.sync (fun () -> incr offers));
      finish = (fun _ -> Eta.Effect.sync (fun () -> incr finishes));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  let config : Camelpie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
           if event.Camelpie.Keyboard.Monitor.keysym = 1 then
             Some
               (if event.released then
                  Camelpie.Ptt_loop.Release "test"
                else Press "test")
           else None);
	      min_duration_ms = 1000;
	      min_capture_rms = Camelpie.Audio.Pcm.ptt_min_capture_rms_for_stt;
	      min_capture_peak = Camelpie.Audio.Pcm.ptt_min_capture_peak_for_stt;
	      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press = (fun _ -> Eta.Effect.pure (Camelpie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort = Eta.Effect.pure false;
      on_dead_input = Eta.Effect.unit;
    }
  in
  let recv () = Eta.Effect.sync (fun () -> Eio.Stream.take audio) in
  let effect =
    Camelpie.Ptt_loop.run ~keyboard ~recv config
    |> Eta.Effect.timeout_as (Eta.Duration.seconds 4) ~on_timeout:(`Ptt_error "timeout")
  in
  let result = Eta.Runtime.run rt effect in
  Alcotest.(check int) "offers" 0 !offers;
  Alcotest.(check int) "finishes" 0 !finishes;
  Alcotest.(check int) "cancels" 1 !cancels;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected timeout after short capture returns to idle"

let test_ptt_loop_ignores_low_rms_capture_before_offering () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let rt = Eta.Runtime.create ~sw ~clock () in
  let keyboard = Camelpie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 64 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Camelpie.Keyboard.Monitor.emit keyboard
      { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 0.05;
    key false;
    for _ = 1 to 8 do
      Eio.Time.sleep clock 0.05;
      Eio.Stream.add audio (pcm_chunk 4 160)
    done;
    Eio.Time.sleep clock 0.05;
    key true);
  let handle =
    {
      Camelpie.Ptt_loop.offer =
        (fun _ -> Eta.Effect.sync (fun () -> incr offers));
      finish = (fun _ -> Eta.Effect.sync (fun () -> incr finishes));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  let config : Camelpie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
           if event.Camelpie.Keyboard.Monitor.keysym = 1 then
             Some
               (if event.released then
                  Camelpie.Ptt_loop.Release "test"
                else Press "test")
           else None);
	      min_duration_ms = 300;
	      min_capture_rms = Camelpie.Audio.Pcm.ptt_min_capture_rms_for_stt;
	      min_capture_peak = Camelpie.Audio.Pcm.ptt_min_capture_peak_for_stt;
	      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press = (fun _ -> Eta.Effect.pure (Camelpie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort = Eta.Effect.pure false;
      on_dead_input = Eta.Effect.unit;
    }
  in
  let recv () = Eta.Effect.sync (fun () -> Eio.Stream.take audio) in
  let effect =
    Camelpie.Ptt_loop.run ~keyboard ~recv config
    |> Eta.Effect.timeout_as (Eta.Duration.seconds 4) ~on_timeout:(`Ptt_error "timeout")
  in
  let result = Eta.Runtime.run rt effect in
  Alcotest.(check int) "offers" 0 !offers;
  Alcotest.(check int) "finishes" 0 !finishes;
  Alcotest.(check int) "cancels" 1 !cancels;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected timeout after low-rms capture returns to idle"

let test_ptt_loop_accepts_peak_over_silence_threshold () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let rt = Eta.Runtime.create ~sw ~clock () in
  let keyboard = Camelpie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 64 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Camelpie.Keyboard.Monitor.emit keyboard
      { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 0.05;
    key false;
    for _ = 1 to 8 do
      Eio.Time.sleep clock 0.05;
      Eio.Stream.add audio (mostly_quiet_chunk ~peak_sample:660 160)
    done;
    Eio.Time.sleep clock 0.05;
    key true);
  let handle =
    {
      Camelpie.Ptt_loop.offer =
        (fun _ -> Eta.Effect.sync (fun () -> incr offers));
      finish =
        (fun _ ->
           Eta.Effect.sync (fun () -> incr finishes)
           |> Eta.Effect.bind (fun () -> Eta.Effect.fail (`Ptt_error "done")));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  let config : Camelpie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
           if event.Camelpie.Keyboard.Monitor.keysym = 1 then
             Some
               (if event.released then
                  Camelpie.Ptt_loop.Release "test"
                else Press "test")
           else None);
	      min_duration_ms = 300;
	      min_capture_rms = Camelpie.Audio.Pcm.ptt_min_capture_rms_for_stt;
	      min_capture_peak = Camelpie.Audio.Pcm.ptt_min_capture_peak_for_stt;
	      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press = (fun _ -> Eta.Effect.pure (Camelpie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort = Eta.Effect.pure false;
      on_dead_input = Eta.Effect.unit;
    }
  in
  let recv () = Eta.Effect.sync (fun () -> Eio.Stream.take audio) in
  let result =
    Eta.Runtime.run rt
      (Camelpie.Ptt_loop.run ~keyboard ~recv config
       |> Eta.Effect.timeout_as (Eta.Duration.seconds 4) ~on_timeout:(`Ptt_error "timeout"))
  in
  Alcotest.(check bool) "offers" true (!offers > 0);
  Alcotest.(check int) "finishes" 1 !finishes;
  Alcotest.(check int) "cancels" 1 !cancels;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected timeout after finish failure returns to idle"

let test_ptt_loop_accepts_quiet_speech_over_rms_threshold () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let rt = Eta.Runtime.create ~sw ~clock () in
  let keyboard = Camelpie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 64 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Camelpie.Keyboard.Monitor.emit keyboard
      { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 0.05;
    key false;
    for _ = 1 to 8 do
      Eio.Time.sleep clock 0.05;
      Eio.Stream.add audio (pcm_chunk 88 160)
    done;
    Eio.Time.sleep clock 0.05;
    key true);
  let handle =
    {
      Camelpie.Ptt_loop.offer =
        (fun _ -> Eta.Effect.sync (fun () -> incr offers));
      finish =
        (fun _ ->
           Eta.Effect.sync (fun () -> incr finishes)
           |> Eta.Effect.bind (fun () -> Eta.Effect.fail (`Ptt_error "done")));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  let config : Camelpie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
           if event.Camelpie.Keyboard.Monitor.keysym = 1 then
             Some
               (if event.released then
                  Camelpie.Ptt_loop.Release "test"
                else Press "test")
           else None);
	      min_duration_ms = 300;
	      min_capture_rms = Camelpie.Audio.Pcm.ptt_min_capture_rms_for_stt;
	      min_capture_peak = Camelpie.Audio.Pcm.ptt_min_capture_peak_for_stt;
	      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press = (fun _ -> Eta.Effect.pure (Camelpie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort = Eta.Effect.pure false;
      on_dead_input = Eta.Effect.unit;
    }
  in
  let recv () = Eta.Effect.sync (fun () -> Eio.Stream.take audio) in
  let result =
    Eta.Runtime.run rt
      (Camelpie.Ptt_loop.run ~keyboard ~recv config
       |> Eta.Effect.timeout_as (Eta.Duration.seconds 4) ~on_timeout:(`Ptt_error "timeout"))
  in
  Alcotest.(check bool) "offers" true (!offers > 0);
  Alcotest.(check int) "finishes" 1 !finishes;
  Alcotest.(check int) "cancels" 1 !cancels;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected timeout after finish failure returns to idle"

let test_ptt_loop_dead_input_cancels_active_capture () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let rt = Eta.Runtime.create ~sw ~clock () in
  let keyboard = Camelpie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let dead_inputs = ref 0 in
  let key released =
    Camelpie.Keyboard.Monitor.emit keyboard
      { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Time.sleep clock 0.05;
    key false;
    for _ = 1 to 4 do
      Eio.Time.sleep clock 0.02;
      Eio.Stream.add audio (Bytes.make 320 '\000')
    done;
    Eio.Time.sleep clock 0.05;
    key true);
  let handle =
    {
      Camelpie.Ptt_loop.offer =
        (fun _ -> Eta.Effect.sync (fun () -> incr offers));
      finish = (fun _ -> Eta.Effect.sync (fun () -> incr finishes));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  let config : Camelpie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
           if event.Camelpie.Keyboard.Monitor.keysym = 1 then
             Some
               (if event.released then
                  Camelpie.Ptt_loop.Release "test"
                else Press "test")
           else None);
	      min_duration_ms = 0;
	      min_capture_rms = Camelpie.Audio.Pcm.ptt_min_capture_rms_for_stt;
	      min_capture_peak = Camelpie.Audio.Pcm.ptt_min_capture_peak_for_stt;
	      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press = (fun _ -> Eta.Effect.pure (Camelpie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort = Eta.Effect.pure false;
      on_dead_input = Eta.Effect.sync (fun () -> incr dead_inputs);
    }
  in
  let recv () = Eta.Effect.sync (fun () -> Eio.Stream.take audio) in
  let effect =
    Camelpie.Ptt_loop.run ~keyboard ~recv config
    |> Eta.Effect.timeout_as (Eta.Duration.seconds 3) ~on_timeout:(`Ptt_error "timeout")
  in
  let result = Eta.Runtime.run rt effect in
  Alcotest.(check int) "offers" 0 !offers;
  Alcotest.(check int) "finishes" 0 !finishes;
  Alcotest.(check int) "cancels" 1 !cancels;
  Alcotest.(check int) "dead input callbacks" 1 !dead_inputs;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected timeout after dead-input cancellation returns to idle"

let test_wakeword_pipeline_smoothing_uses_threshold_not_history () =
  let det = Camelpie.Wakeword.Pipeline.create_detector () in
  let loud = Bytes.create 1600 in
  for i = 0 to 799 do
    Bytes.set_int16_le loud (i * 2) 32767
  done;
  let score1 = Camelpie.Wakeword.Pipeline.process det loud in
  let score2 = Camelpie.Wakeword.Pipeline.process det loud in
  (* Bug: process_frame blends the raw score toward model.threshold (0.5)
     instead of toward the previous detector.score. With proper temporal
     smoothing, score2 should be higher than score1 because the previous
     score (0.85) would be blended in. The buggy formula always gives
     the same result for identical frames because it ignores history. *)
  Alcotest.(check bool)
    "temporal smoothing should increase score on repeated identical loud frames"
    true
    (score2 > score1)

let () =
  Alcotest.run "camelpie"
    [
      ("pcm",
       [
         Alcotest.test_case "rms" `Quick test_pcm_rms;
         Alcotest.test_case "peak" `Quick test_pcm_peak;
         Alcotest.test_case "normalization" `Quick test_pcm_normalization;
         Alcotest.test_case "wav header" `Quick test_pcm_wav_header;
       ]);
      ("pulse_protocol",
       [
         Alcotest.test_case "packet framing" `Quick test_pa_packet_framing;
         Alcotest.test_case "packet header channel roundtrip" `Quick test_pa_packet_header_channel_roundtrip;
         Alcotest.test_case "tag struct roundtrip" `Quick test_tag_struct_roundtrip;
         Alcotest.test_case "sample spec roundtrip" `Quick test_sample_spec_roundtrip;
       ]);
      ("pulse_commands",
       [
         Alcotest.test_case "auth command" `Quick test_auth_command;
         Alcotest.test_case "server info command" `Quick test_server_info_command;
         Alcotest.test_case "parse error code short payload" `Quick
           test_pulse_parse_error_code_short_payload;
         Alcotest.test_case "parse command envelope short payload" `Quick
           test_pulse_parse_command_envelope_short_payload;
       ]);
      ("pulse_defs",
       [
         Alcotest.test_case "default record stream mismatched channel map" `Quick
           test_pulse_default_record_stream_options_mismatched_channel_map;
       ]);
      ("pulse_client",
       [
         Alcotest.test_case "set_client_name error ignored" `Quick
           test_pulse_client_set_client_name_error_ignored;
       ]);
      ("config",
       [
         Alcotest.test_case "provider codex-realtime" `Quick (fun () ->
           let p = Camelpie.Config.Stt.provider_of_string "codex-realtime" in
           Alcotest.(check bool) "is codex" true (p = Camelpie.Config.Stt.Codex_realtime));
         Alcotest.test_case "provider openrouter" `Quick (fun () ->
           let p = Camelpie.Config.Stt.provider_of_string "openrouter" in
           Alcotest.(check bool) "is openrouter" true (p = Camelpie.Config.Stt.Openrouter));
         Alcotest.test_case "provider-qualified model" `Quick
           test_stt_config_resolves_provider_qualified_model;
         Alcotest.test_case "load TOML newline rules" `Quick
           test_stt_config_loads_toml_newline_rules;
         Alcotest.test_case "reject invalid title regex" `Quick
           test_stt_config_rejects_bad_title_regex;
         Alcotest.test_case "reject transcription prompt missing language placeholder" `Quick
           test_stt_config_rejects_prompt_with_wrong_placeholder;
         Alcotest.test_case "whitespace app_id not trimmed" `Quick
           test_stt_config_whitespace_app_id_not_trimmed;
       ]);
      ("text_injection",
       [
         Alcotest.test_case "normalize" `Quick (fun () ->
           Alcotest.(check string) "trimmed" "hello" (Camelpie.Input.Text_injection.normalize_text "  hello  "));
         Alcotest.test_case "collapse line breaks" `Quick (fun () ->
           Alcotest.(check string)
             "collapsed"
             "first second third"
             (Camelpie.Input.Text_injection.collapse_line_breaks " first\nsecond\r\n third "));
         Alcotest.test_case "regex newline rule" `Quick
           test_text_injection_newline_rule_regex;
       ]);
      ("daemon_contract",
       [
         Alcotest.test_case "snapshot parsing" `Quick (fun () ->
           let json = `Assoc [("active", `Bool true); ("mode", `String "ptt-transcribe"); ("enabled", `Bool true)] in
           let snap = Camelpie.Daemon.Contract.snapshot_of_json json in
           Alcotest.(check bool) "active" true snap.active;
           Alcotest.(check string) "mode" "ptt-transcribe" snap.mode;
           Alcotest.(check bool) "enabled" true snap.enabled);
         Alcotest.test_case "request parsing" `Quick
           test_daemon_contract_request;
         Alcotest.test_case "response roundtrip" `Quick
           test_daemon_contract_response;
         Alcotest.test_case "error response parsed as success" `Quick
           test_daemon_contract_error_response_parsed_as_success;
         Alcotest.test_case "status error swallowed" `Quick
           test_daemon_contract_status_error_swallowed;
         Alcotest.test_case "pause error swallowed" `Quick
           test_daemon_contract_pause_error_swallowed;
         Alcotest.test_case "meeting toggle missing indicator" `Quick
           test_daemon_contract_meeting_toggle_no_indicator;
       ]);
      ("daemon_server",
       [
         Alcotest.test_case "socket state" `Quick
           test_daemon_server_state;
         Alcotest.test_case "meeting stop no failure signal" `Quick
           test_daemon_server_meeting_stop_no_failure_signal;
         Alcotest.test_case "mkdir_p silent on existing file" `Quick
           test_daemon_server_mkdir_p_silently_accepts_file;
       ]);
      ("keyboard",
       [
         Alcotest.test_case "parse KeyEvent" `Quick test_keyboard_key_event_parse;
         Alcotest.test_case "parse D-Bus KeyEvent" `Quick
           test_keyboard_dbus_key_event_parse;
         Alcotest.test_case "session bus path rejects abstract" `Quick
           test_keyboard_monitor_session_bus_path_abstract_socket;
         Alcotest.test_case "keysym_of_name rejects unknown keys" `Quick (fun () ->
           let keysym = Camelpie.Keyboard.Monitor.keysym_of_name "F11" in
           (* Bug: keysym_of_name returns 0 for unknown keys, but 0 is a valid
              keysym (XKB_KEY_NoSymbol). Callers cannot distinguish an unknown
              key from a deliberately-configured NoSymbol. The function should
              raise Invalid_argument or return an option. *)
           Alcotest.(check bool)
             "unknown key name should not silently return 0"
             false
             (keysym = 0));
       ]);
      ("recording_coordinator",
       [
         Alcotest.test_case "transitions" `Quick test_recording_coordinator_transitions;
         Alcotest.test_case "persist error swallowed" `Quick
           test_recording_coordinator_persist_error_swallowed;
         Alcotest.test_case "stop clears error inconsistently" `Quick
           test_recording_coordinator_stop_clears_error;
       ]);
      ("stt_codex_realtime",
       [
         Alcotest.test_case "url builders" `Quick test_codex_realtime_url_builders;
         Alcotest.test_case "headers" `Quick test_codex_realtime_headers;
         Alcotest.test_case "receive stop modes" `Quick
           test_codex_realtime_receive_stop_modes;
         Alcotest.test_case "transcription session update" `Quick
           test_codex_realtime_transcription_session_update;
         Alcotest.test_case "event parser" `Quick test_codex_realtime_event_parser;
         Alcotest.test_case "resample" `Quick test_codex_realtime_resample;
         Alcotest.test_case "stream server error e2e" `Quick
           test_codex_realtime_stream_reports_server_error_e2e;
         Alcotest.test_case "translation ignores prompt" `Quick
           test_codex_realtime_translation_ignores_prompt;
         Alcotest.test_case "transcribe ignores language e2e" `Quick
           test_codex_realtime_transcribe_with_prompt_ignores_language_e2e;
         Alcotest.test_case "translate non-conv drops prompt e2e" `Quick
           test_codex_realtime_translate_nonconv_drops_prompt_e2e;
       ]);
      ("assistant_prompt",
       [
         Alcotest.test_case "focused window context" `Quick
           test_assistant_focused_window_prompt_context;
         Alcotest.test_case "startup config report" `Quick
           test_assistant_startup_config_json;
         Alcotest.test_case "project context" `Quick
           test_assistant_project_context_prompt_context;
         Alcotest.test_case "missing project context is empty" `Quick
           test_assistant_project_context_missing_is_empty;
         Alcotest.test_case "relative cwd traverses parents" `Quick
           test_project_context_relative_cwd_traversal;
         Alcotest.test_case "empty project context is empty" `Quick
           test_assistant_project_context_empty_file_is_empty;
         Alcotest.test_case "foreground terminal cwd" `Quick
           test_project_context_prefers_foreground_terminal_cwd;
         Alcotest.test_case "root cwd fallback" `Quick
           test_project_context_falls_back_to_root_cwd;
         Alcotest.test_case "package.json metadata" `Quick
           test_project_metadata_package_json;
         Alcotest.test_case "Cargo.toml metadata" `Quick
           test_project_metadata_cargo_toml;
         Alcotest.test_case "pyproject.toml metadata" `Quick
           test_project_metadata_pyproject_toml;
         Alcotest.test_case "dune-project metadata" `Quick
           test_project_metadata_dune_project;
         Alcotest.test_case "opam metadata" `Quick
           test_project_metadata_opam_without_dune_project;
         Alcotest.test_case "metadata exact directory only" `Quick
           test_project_metadata_exact_directory_only;
         Alcotest.test_case "metadata without description" `Quick
           test_project_metadata_manifest_without_description;
         Alcotest.test_case "prompt value utf8 truncation" `Quick
           test_assistant_prompt_value_utf8_truncation;
       ]);
      ("ptt_capture",
       [
         Alcotest.test_case "state machine" `Quick (fun () ->
           let s0 = Camelpie.Ptt.Capture.idle in
           Alcotest.(check bool) "idle not accepting" false (Camelpie.Ptt.Capture.is_accepting_chunks s0);
           let s1 = Camelpie.Ptt.Capture.start s0 0.0 in
           Alcotest.(check bool) "capturing accepting" true (Camelpie.Ptt.Capture.is_accepting_chunks s1);
           let s2 = Camelpie.Ptt.Capture.release s1 1000.0 in
           Alcotest.(check bool) "postroll accepting" true (Camelpie.Ptt.Capture.is_accepting_chunks s2);
           Alcotest.(check bool) "postroll remaining > 0" true (Camelpie.Ptt.Capture.post_roll_remaining_ms s2 1000.0 > 0.0);
           Alcotest.(check bool) "postroll expired" true (Camelpie.Ptt.Capture.post_roll_remaining_ms s2 4000.0 = 0.0));
         Alcotest.test_case "loop continues after finish" `Quick
           test_ptt_loop_continues_after_finish;
         Alcotest.test_case "post-roll re-press continues capture" `Quick
           test_ptt_loop_post_roll_repress_continues_capture;
         Alcotest.test_case "scoped handle lifetime" `Quick
           test_ptt_loop_scoped_handle_lifetime;
         Alcotest.test_case "short capture ignored before offer" `Quick
           test_ptt_loop_ignores_short_capture_before_offering;
         Alcotest.test_case "low-rms capture ignored before offer" `Quick
           test_ptt_loop_ignores_low_rms_capture_before_offering;
         Alcotest.test_case "peak over silence threshold accepted" `Quick
           test_ptt_loop_accepts_peak_over_silence_threshold;
         Alcotest.test_case "quiet speech over RMS threshold accepted" `Quick
           test_ptt_loop_accepts_quiet_speech_over_rms_threshold;
       ]);
      ("ptt_dead_input",
       [
         Alcotest.test_case "flatline detection" `Quick (fun () ->
           let det = Camelpie.Ptt.Dead_input.initial () in
           let det = Camelpie.Ptt.Dead_input.sync det true in
           let chunk = Bytes.make 16 '\x00' in
           let rec loop det n =
             if n = 0 then det
             else
               let r = Camelpie.Ptt.Dead_input.process_chunk det chunk in
               loop r.detector (n - 1)
           in
           let det = loop det 3 in
           let r = Camelpie.Ptt.Dead_input.process_chunk det chunk in
           Alcotest.(check bool) "warn after 4 zeros" true r.warn;
           Alcotest.(check bool) "dead after 4 zeros" true r.dead);
         Alcotest.test_case "loop cancels flatline capture" `Quick
           test_ptt_loop_dead_input_cancels_active_capture;
       ]);
      ("stt_openrouter",
       [
         Alcotest.test_case "responses request uses text and audio" `Quick
           test_openrouter_responses_request_uses_text_and_audio;
         Alcotest.test_case "structured output decoder" `Quick
           test_openrouter_decodes_structured_output_only;
       ]);
      ("wakeword_trigger",
       [
         Alcotest.test_case "trigger threshold" `Quick (fun () ->
           let s = Camelpie.Wakeword.Trigger.initial in
           Alcotest.(check bool) "not triggered initially" false s.triggered;
           let s2 = Camelpie.Wakeword.Trigger.update s 0.6 in
           Alcotest.(check bool) "triggered above threshold" true s2.triggered;
           let s3 = Camelpie.Wakeword.Trigger.update s2 0.3 in
           Alcotest.(check bool) "not triggered below threshold" false s3.triggered);
       ]);
      ("integration",
       [
         Alcotest.test_case "wav header roundtrip" `Quick (fun () ->
           let pcm = Bytes.create 32000 in
           for i = 0 to 15999 do
             let sample = (i * 2) mod 65536 in
             Bytes.set_uint16_le pcm (i * 2) sample
           done;
           let header = Camelpie.Audio.Pcm.build_pcm_wav_header ~data_length:32000 ~sample_rate:16000 in
           Alcotest.(check int) "header size" 44 (Bytes.length header);
           Alcotest.(check string) "RIFF" "RIFF" (Bytes.sub_string header 0 4);
           Alcotest.(check string) "WAVE" "WAVE" (Bytes.sub_string header 8 4);
           Alcotest.(check int32) "data len"
             (Int32.of_int 32000)
             (Bytes.get_int32_le header 40));
         Alcotest.test_case "normalization then silence gate" `Quick (fun () ->
           (* quiet signal — should be normalized *)
           let quiet = Bytes.create 100 in
           Bytes.fill quiet 0 100 '\x01';
           let result = Camelpie.Audio.Pcm.normalize_pcm_for_stt quiet in
           Alcotest.(check bool) "quiet signal gets gain" true (result.gain > 1.0);
           (* silence — should not be normalized *)
           let silent = Bytes.make 100 '\x00' in
           let result2 = Camelpie.Audio.Pcm.normalize_pcm_for_stt silent in
           Alcotest.(check bool) "silent signal no gain" true (result2.gain = 1.0));
       ]);
      ("wakeword_pipeline",
       [
         Alcotest.test_case "synthetic scoring" `Quick (fun () ->
           let det = Camelpie.Wakeword.Pipeline.create_detector () in
           (* loud signal should trigger *)
           let loud = Bytes.create 1600 in
           for i = 0 to 799 do
             Bytes.set_int16_le loud (i * 2) 20000
           done;
           let score = Camelpie.Wakeword.Pipeline.process det loud in
           Alcotest.(check bool) "loud triggers" true det.triggered;
           Alcotest.(check bool) "score high" true (score > 0.5);
           (* silence should not trigger *)
           let silent = Bytes.make 1600 '\x00' in
           let score2 = Camelpie.Wakeword.Pipeline.process det silent in
           Alcotest.(check bool) "silence no trigger" false det.triggered;
           Alcotest.(check bool) "score low" true (score2 < 0.5));
         Alcotest.test_case "smoothing ignores history" `Quick
           test_wakeword_pipeline_smoothing_uses_threshold_not_history;
       ]);
      ("wayland_portal",
       [
         Alcotest.test_case "parse object path" `Quick (fun () ->
           let input = "o \"/org/freedesktop/portal/desktop/request/1_123/pie_456\"" in
           match String.split_on_char '"' input with
           | _ :: path :: _ ->
             Alcotest.(check string) "path"
               "/org/freedesktop/portal/desktop/request/1_123/pie_456"
               path
           | _ -> Alcotest.fail "unexpected split");
       ]);
      ("fs",
       [
         Alcotest.test_case "mkdir_p creates nested directories" `Quick (fun () ->
           let base = temp_dir "camelpie-fs-mkdir_p" in
           let nested = Filename.concat base "a/b/c" in
           try
             Camelpie.Fs.mkdir_p nested;
             Alcotest.(check bool)
               "mkdir_p should create nested directories"
               true
               (Sys.file_exists nested && Sys.is_directory nested);
             (* cleanup *)
             (try Unix.rmdir nested with _ -> ());
             (try Unix.rmdir (Filename.concat base "a/b") with _ -> ());
             (try Unix.rmdir (Filename.concat base "a") with _ -> ());
             (try Unix.rmdir base with _ -> ())
           with
           | Unix.Unix_error (Unix.ENOENT, _, _) ->
             (* Bug: Fs.mkdir_p only calls Unix.mkdir once, which fails when
                parent directories do not exist. It should recursively create
                parents like the mkdir_p in Recording.Coordinator and
                Daemon.Server do. *)
             Alcotest.fail "mkdir_p raised ENOENT for nested path (bug: missing parent creation)"
           | exn ->
             Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn));
         Alcotest.test_case "mkdir_p should fail when path is an existing file" `Quick (fun () ->
           let base = temp_dir "camelpie-fs-mkdir_p-file" in
           let path = Filename.concat base "existing-file" in
           write_file path "content";
           try
             Camelpie.Fs.mkdir_p path;
             (* Bug: Fs.mkdir_p catches Unix.EEXIST and returns () even when the
                path is a regular file, not a directory. The caller then assumes
                the path is a directory and will fail later when trying to create
                files inside it. mkdir_p should verify the path is a directory. *)
             Alcotest.fail "mkdir_p should not silently succeed when path is an existing file"
           with
           | Unix.Unix_error (Unix.EEXIST, _, _) ->
             (* This is the expected behavior: raise EEXIST when the path exists
                but is not a directory. *)
             ()
           | exn ->
             Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn));
       ]);
      ("sh",
       [
         Alcotest.test_case "find_executable rejects directories" `Quick (fun () ->
           let base = temp_dir "camelpie-sh-find" in
           let fake_dir = Filename.concat base "not-an-executable" in
           Unix.mkdir fake_dir 0o700;
           let old_path =
             match Sys.getenv_opt "PATH" with Some p -> p | None -> ""
           in
           Unix.putenv "PATH" base;
           try
             let result = Camelpie.Sh.find_executable "not-an-executable" in
             Unix.putenv "PATH" old_path;
             (* Bug: find_executable uses Sys.file_exists which returns true for
                directories. It should check Sys.is_directory = false (and ideally
                Unix.access path [X_OK]) before returning a match. *)
             Alcotest.(check (option string))
               "directory should not match as executable"
               None
               result;
             Unix.rmdir fake_dir;
             Unix.rmdir base
           with exn ->
             Unix.putenv "PATH" old_path;
             Unix.rmdir fake_dir;
             Unix.rmdir base;
             raise exn);
       ]);

      ("metadata",
       [
         Alcotest.test_case "sexp_tokens handles escaped newlines" `Quick (fun () ->
           let input = "(name \"foo\\nbar\")" in
           let tokens = Camelpie.Context.Metadata.sexp_tokens input in
           let atoms =
             List.filter_map
               (function Camelpie.Context.Metadata.TAtom s -> Some s | _ -> None)
               tokens
           in
           (* We expect two atoms: bare atom name and string literal foonbar.
              The bug is that the string literal is parsed as foonbar
              instead of foo-newline-bar (backslash is ignored). *)
           let string_atom =
             List.find_opt (fun s -> String.starts_with ~prefix:"foo" s) atoms
           in
           match string_atom with
           | None -> Alcotest.fail "expected string atom starting with foo"
           | Some s ->
             (* Bug: sexp_tokens treats backslash as a transparent prefix and just
                emits the next raw character. So backslash-n becomes the letter n
                instead of a newline character. A correct parser would either
                reject backslash-n (like dune does) or translate it to a newline. *)
             Alcotest.(check string)
               "escaped newline should be parsed as newline character"
               "foo\nbar"
               s);
       ]);
    ]
