let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

open Pie
open Ox.Result.Syntax

let run_test f =
  Pie_test_runtime.run @@ fun env ->
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () -> f env)

let sync f = Ok (f ())

let rec consume_stream on_chunk pull =
  match pull () with
  | Ok (Some chunk) ->
      on_chunk chunk;
      consume_stream on_chunk pull
  | Ok None -> Ok ()
  | Error _ as error -> error

let par ~sw left right =
  let pending, resolve = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () -> Eio.Promise.resolve resolve (left ()));
  let second = right () in
  let* first = Eio.Promise.await pending in
  let+ second = second in
  (first, second)

let run_boundary f =
  match Ox.Scope.run (fun _ -> f ()) with
  | Ox.Exit.Success value -> Ok value
  | Ox.Exit.Failure cause -> Error cause

let run_ptt_boundary ~clock operation =
  run_boundary (fun () ->
      Ox.Timeout.timeout ~clock (Ox.Duration.seconds 8) (fun () ->
          Result.map_error
            (fun error -> (error :> [ Pie.Ptt_loop.run_error | `Timeout ]))
            (operation ())))

let test_logger env =
  Ox_observability.Logger.create ~service:"pie-test"
    ~clock:(Eio.Stdenv.clock env)
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~reporter:(Ox_observability.Logger.Reporter.records (fun _ -> ()))
    ()

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
  let header =
    Audio.Pcm.build_pcm_wav_header ~data_length:16000 ~sample_rate:16000
  in
  Alcotest.(check int) "wav header length" 44 (Bytes.length header);
  Alcotest.(check string) "RIFF marker" "RIFF" (Bytes.sub_string header 0 4);
  Alcotest.(check string) "WAVE marker" "WAVE" (Bytes.sub_string header 8 4)

let test_pa_packet_framing () =
  let payload = Bytes.of_string "hello" in
  let packet = Pulse.Protocol.frame_packet payload in
  Alcotest.(check int) "framed packet length" 25 (Bytes.length packet);
  let split = Pulse.Protocol.split_framed_packet packet in
  Alcotest.(check int) "payload length" 5 (Bytes.length split.payload);
  Alcotest.(check string)
    "payload content" "hello"
    (Bytes.to_string split.payload)

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
  Alcotest.(check int)
    "u32" 42
    (Pulse.Protocol.Tag_struct_reader.get_u32 reader);
  Alcotest.(check (option string))
    "string" (Some "test")
    (Pulse.Protocol.Tag_struct_reader.get_string reader);
  Alcotest.(check bool)
    "bool" true
    (Pulse.Protocol.Tag_struct_reader.get_bool reader);
  Alcotest.(check int) "u8" 7 (Pulse.Protocol.Tag_struct_reader.get_u8 reader);
  Alcotest.(check bool)
    "no remaining" false
    (Pulse.Protocol.Tag_struct_reader.has_remaining reader)

let test_sample_spec_roundtrip () =
  let spec =
    {
      Pulse.Defs.format = Pulse.Defs.Pa_sample_format.s16le;
      channels = 1;
      rate = 16000;
    }
  in
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
      Alcotest.(check bool)
        "auth packet bytes length > 24" true
        (Bytes.length pkt.bytes > 24)
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
  Fun.protect
    ~finally:(fun () -> restore_dbus_env old)
    (fun () ->
      match Pie.Keyboard.Monitor.session_bus_path ~caps () with
      | exception Pie.Keyboard.Monitor.Unsupported_input msg ->
          Alcotest.(check string)
            "abstract is not a filesystem path"
            "unix:abstract is not a filesystem path" msg
      | path ->
          Alcotest.failf "abstract must not become a filesystem path: %s" path)

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
  let owner = ":1.42" in
  let message : Keyboard.Monitor.dbus_message =
    {
      msg_type = 4;
      serial = 1;
      reply_serial = None;
      path = Some Keyboard.Monitor.a11y_path;
      interface = Some Keyboard.Monitor.a11y_interface;
      member = Some "KeyEvent";
      error_name = None;
      sender = Some owner;
      destination = None;
      body_signature = Some "buuuq";
      body;
    }
  in
  match Keyboard.Monitor.parse_key_event ~owner message with
  | Some event ->
      Alcotest.(check bool) "released" true event.released;
      Alcotest.(check int) "state" 2 event.state;
      Alcotest.(check int) "F10 keysym" 65479 event.keysym;
      Alcotest.(check int) "keycode" 68 event.keycode
  | None -> Alcotest.fail "expected D-Bus KeyEvent"

let test_recording_coordinator_transitions () =
  run_test @@ fun _ ->
  let path = Filename.temp_file "pie-recording" ".json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path:path () in
  let initial = Recording.Coordinator.snapshot coordinator in
  Alcotest.(check bool) "enabled" true initial.enabled;
  Alcotest.(check string) "idle" "idle" initial.mode;
  (match
     Recording.Coordinator.try_start coordinator
       Recording.Coordinator.Ptt_transcribe ()
   with
  | Recording.Coordinator.Started attempt ->
      Alcotest.(check bool)
        "started ptt-transcribe" true
        (Recording.Coordinator.attempt_mode attempt
        = Recording.Coordinator.Ptt_transcribe);
      (match
         Recording.Coordinator.try_start coordinator
           Recording.Coordinator.Ptt_translate ()
       with
      | Recording.Coordinator.Busy Recording.Coordinator.Ptt_transcribe -> ()
      | _ -> Alcotest.fail "expected busy ptt-transcribe");
      Recording.Coordinator.set_enabled coordinator false;
      ignore (Recording.Coordinator.release coordinator attempt)
  | _ -> Alcotest.fail "expected ptt-transcribe to start");
  (match
     Recording.Coordinator.try_start coordinator
       Recording.Coordinator.Ptt_translate ()
   with
  | Recording.Coordinator.Disabled -> ()
  | _ -> Alcotest.fail "expected disabled");
  Sys.remove path

let test_recording_coordinator_release_preserves_newer_same_mode_attempt () =
  run_test @@ fun _ ->
  let path = Filename.temp_file "pie-recording-identity" ".json" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let coordinator =
        Recording.Coordinator.create ~caps ~persist_path:path ()
      in
      let first =
        match
          Recording.Coordinator.try_start coordinator
            Recording.Coordinator.Ptt_transcribe ()
        with
        | Recording.Coordinator.Started attempt -> attempt
        | _ -> Alcotest.fail "expected the first attempt to be admitted"
      in
      Alcotest.(check bool)
        "owner release clears admission" true
        (Recording.Coordinator.release coordinator first);
      let second =
        match
          Recording.Coordinator.try_start coordinator
            Recording.Coordinator.Ptt_transcribe ()
        with
        | Recording.Coordinator.Started attempt -> attempt
        | _ -> Alcotest.fail "expected the second same-mode attempt"
      in
      Alcotest.(check bool)
        "stale release does not clear a newer same-mode attempt" false
        (Recording.Coordinator.release coordinator first);
      Alcotest.(check bool)
        "newer attempt still owns admission" true
        (Recording.Coordinator.snapshot coordinator)
          .Recording.Coordinator.active;
      Alcotest.(check bool)
        "owner release of the newer attempt succeeds" true
        (Recording.Coordinator.release coordinator second);
      Alcotest.(check bool)
        "idle after the owning release" false
        (Recording.Coordinator.snapshot coordinator)
          .Recording.Coordinator.active)

let test_daemon_contract_request () =
  let json =
    Pie.Assistant_service.Contract.request_to_json
      Pie.Assistant_service.Contract.Status
  in
  match Pie.Assistant_service.Contract.request_of_json json with
  | Ok request ->
      Alcotest.(check bool)
        "request" true
        (request = Pie.Assistant_service.Contract.Status)
  | Error msg -> Alcotest.fail msg

let test_daemon_contract_action_request () =
  let json =
    Pie.Assistant_service.Contract.request_to_json
      (Pie.Assistant_service.Contract.Action "ptt-start")
  in
  match Pie.Assistant_service.Contract.request_of_json json with
  | Ok (Pie.Assistant_service.Contract.Action action) ->
      Alcotest.(check string) "action" "ptt-start" action
  | Ok _ -> Alcotest.fail "expected Action request"
  | Error msg -> Alcotest.fail msg

let test_daemon_contract_action_response () =
  let snapshot : Pie.Assistant_service.Contract.recording_snapshot =
    {
      phase = Pie.Recording.Phase.Active Pie.Recording.Phase.Processing;
      processing = 0;
      active = true;
      mode = "ptt-transcribe";
      enabled = true;
      started_at = None;
      updated_at = Some "now";
      transcript_path = None;
      last_error = None;
      build_commit = None;
      build_time = None;
      reasoning_effort = None;
      response_schema = None;
    }
  in
  let response =
    Pie.Assistant_service.Contract.Action_resp
      {
        action = "ptt-start";
        accepted = true;
        message = Some "queued";
        snapshot;
      }
    |> Pie.Assistant_service.Contract.response_to_json
  in
  match
    Pie.Assistant_service.Contract.response_of_json
      (Pie.Assistant_service.Contract.Action "ptt-start") response
  with
  | Ok (Pie.Assistant_service.Contract.Action_resp result) ->
      Alcotest.(check bool) "accepted" true result.accepted;
      Alcotest.(check string)
        "message" "queued"
        (Option.value result.message ~default:"");
      Alcotest.(check string) "mode" "ptt-transcribe" result.snapshot.mode
  | _ -> Alcotest.fail "expected Action response"

let test_daemon_contract_unknown_method () =
  match
    Pie.Assistant_service.Contract.request_of_json
      (`Assoc [ ("method", `String "MeetingStart") ])
  with
  | Error msg ->
      Alcotest.(check string)
        "unknown method rejected" "unknown method: MeetingStart" msg
  | Ok _ -> Alcotest.fail "removed MeetingStart must not decode"

let test_daemon_contract_status_error_swallowed () =
  let error_json = `Assoc [ ("error", `String "server crashed") ] in
  let response =
    Pie.Assistant_service.Contract.response_of_json
      Pie.Assistant_service.Contract.Status error_json
  in
  (* Bug: response_of_json for Status treats any JSON object as a valid status
     snapshot. A server error like {"error": "..."} becomes a bogus
     Status_resp with active=false, enabled=false, mode="idle", so the client
     thinks the daemon is paused when it actually crashed. *)
  match response with
  | Ok (Pie.Assistant_service.Contract.Error_resp "server crashed") -> ()
  | Ok (Pie.Assistant_service.Contract.Status_resp _) ->
      Alcotest.fail "status error response was silently parsed as a snapshot"
  | Ok _ -> Alcotest.fail "expected Error_resp"
  | Error label -> Alcotest.failf "protocol error %s" label

let test_daemon_contract_pause_error_swallowed () =
  let error_json = `Assoc [ ("error", `String "server crashed") ] in
  let response =
    Pie.Assistant_service.Contract.response_of_json
      Pie.Assistant_service.Contract.Pause error_json
  in
  (* Bug: response_of_json for Pause/Resume always returns Pause_resp or
     Resume_resp regardless of the JSON content. A server error is therefore
     indistinguishable from a successful call. *)
  match response with
  | Ok (Pie.Assistant_service.Contract.Error_resp "server crashed") -> ()
  | Ok Pie.Assistant_service.Contract.Pause_resp ->
      Alcotest.fail "pause error response was silently parsed as success"
  | Ok _ -> Alcotest.fail "expected Error_resp"
  | Error label -> Alcotest.failf "protocol error %s" label

let pulse_command_packet packet_type tag body_bytes =
  let envelope_writer = Pulse.Protocol.Tag_struct_writer.create () in
  Pulse.Protocol.Tag_struct_writer.add_u32 envelope_writer packet_type;
  Pulse.Protocol.Tag_struct_writer.add_u32 envelope_writer tag;
  let envelope = Pulse.Protocol.Tag_struct_writer.finalize envelope_writer in
  let payload =
    Bytes.create (Bytes.length envelope + Bytes.length body_bytes)
  in
  Bytes.blit envelope 0 payload 0 (Bytes.length envelope);
  Bytes.blit body_bytes 0 payload (Bytes.length envelope)
    (Bytes.length body_bytes);
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

let pulse_write_response flow envelope packet_type body =
  pulse_command_packet packet_type envelope.Pulse.Commands.tag body
  |> Cstruct.of_bytes
  |> fun response -> Eio.Flow.write flow [ response ]

let pulse_u32 value =
  let writer = Pulse.Protocol.Tag_struct_writer.create () in
  Pulse.Protocol.Tag_struct_writer.add_u32 writer value;
  Pulse.Protocol.Tag_struct_writer.finalize writer

let pulse_server_handshake flow =
  let auth = pulse_read_command flow in
  pulse_write_response flow auth Pulse.Defs.Pa_command.reply
    (pulse_u32 Pulse.Defs.pa_native_protocol_version);
  let set_name = pulse_read_command flow in
  pulse_write_response flow set_name Pulse.Defs.Pa_command.reply Bytes.empty

let pulse_server_info_body () =
  let writer = Pulse.Protocol.Tag_struct_writer.create () in
  Pulse.Protocol.Tag_struct_writer.add_string writer (Some "test-server");
  Pulse.Protocol.Tag_struct_writer.add_string writer (Some "1.0");
  Pulse.Protocol.Tag_struct_writer.add_string writer (Some "tester");
  Pulse.Protocol.Tag_struct_writer.add_string writer (Some "localhost");
  Pulse.Protocol.Tag_struct_writer.add_sample_spec writer
    {
      Pulse.Defs.format = Pulse.Defs.Pa_sample_format.s16le;
      channels = 1;
      rate = 16000;
    };
  Pulse.Protocol.Tag_struct_writer.add_string writer None;
  Pulse.Protocol.Tag_struct_writer.add_string writer None;
  Pulse.Protocol.Tag_struct_writer.add_u32 writer 42;
  Pulse.Protocol.Tag_struct_writer.add_channel_map writer [ 0 ];
  Pulse.Protocol.Tag_struct_writer.finalize writer

let pulse_record_stream_body stream_index =
  let writer = Pulse.Protocol.Tag_struct_writer.create () in
  Pulse.Protocol.Tag_struct_writer.add_u32 writer stream_index;
  Pulse.Protocol.Tag_struct_writer.add_u32 writer 11;
  Pulse.Protocol.Tag_struct_writer.add_u32 writer 65536;
  Pulse.Protocol.Tag_struct_writer.add_u32 writer 320;
  Pulse.Protocol.Tag_struct_writer.add_sample_spec writer
    {
      Pulse.Defs.format = Pulse.Defs.Pa_sample_format.s16le;
      channels = 1;
      rate = 16000;
    };
  Pulse.Protocol.Tag_struct_writer.add_channel_map writer [ 0 ];
  Pulse.Protocol.Tag_struct_writer.add_u32 writer 1;
  Pulse.Protocol.Tag_struct_writer.add_string writer (Some "test-source");
  Pulse.Protocol.Tag_struct_writer.add_bool writer false;
  Pulse.Protocol.Tag_struct_writer.add_usec writer 0L;
  Pulse.Protocol.Tag_struct_writer.add_format_info writer 0 [];
  Pulse.Protocol.Tag_struct_writer.finalize writer

let pulse_test_server ~sw ~net name handler =
  let socket_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "pie-pulse-%s-%d.sock" name (Unix.getpid ()))
  in
  (try Sys.remove socket_path with Sys_error _ -> ());
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net (`Unix socket_path)
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run @@ fun conn_sw ->
      let flow, _ = Eio.Net.accept ~sw:conn_sw socket in
      handler flow);
  socket_path

(* pulse-l1qs, pulse-vm4c, pulse-qoto *)
let test_pulse_client_set_client_name_error_is_typed () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "pie-pulse-%d.sock" (Unix.getpid ()))
  in
  (try Sys.remove socket_path with Sys_error _ -> ());
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 (Eio.Stdenv.net env)
      (`Unix socket_path)
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run @@ fun conn_sw ->
      let flow, _addr = Eio.Net.accept ~sw:conn_sw socket in
      let auth_env = pulse_read_command flow in
      let auth_reply_body =
        let w = Pulse.Protocol.Tag_struct_writer.create () in
        Pulse.Protocol.Tag_struct_writer.add_u32 w
          Pulse.Defs.pa_native_protocol_version;
        Pulse.Protocol.Tag_struct_writer.finalize w
      in
      let auth_reply =
        pulse_command_packet Pulse.Defs.Pa_command.reply
          auth_env.Pulse.Commands.tag auth_reply_body
      in
      Eio.Flow.write flow [ Cstruct.of_bytes auth_reply ];
      let name_env = pulse_read_command flow in
      let err_body =
        let w = Pulse.Protocol.Tag_struct_writer.create () in
        Pulse.Protocol.Tag_struct_writer.add_u32 w 1;
        Pulse.Protocol.Tag_struct_writer.finalize w
      in
      let err_reply =
        pulse_command_packet Pulse.Defs.Pa_command.error
          name_env.Pulse.Commands.tag err_body
      in
      Eio.Flow.write flow [ Cstruct.of_bytes err_reply ];
      let buffer = Cstruct.create 1 in
      try ignore (Eio.Flow.single_read flow buffer) with End_of_file -> ());
  let operation () =
    Pulse.Client.with_client ~caps ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env) ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00')
      (fun _client -> Ok ())
  in
  match run_boundary operation with
  | Error cause
    when List.exists
           (function
             | `Pulse_error (Pulse.Client.Server_error { code = Some 1; _ }) ->
                 true
             | _ -> false)
           (Ox.Cause.failures cause) ->
      ()
  | Error cause ->
      Alcotest.failf "expected typed SET_CLIENT_NAME server error, got %a"
        (Ox.Cause.pp (fun formatter (`Pulse_error error) ->
             Pulse.Client.pp_error formatter error))
        cause
  | Ok () -> Alcotest.fail "expected SET_CLIENT_NAME rejection"

(* pulse-2ncz, pulse-9v2r *)
let test_pulse_default_request_timeout () =
  Alcotest.(check int)
    "default request timeout" 3_000
    (Ox.Duration.to_ms Pulse.Client.default_request_timeout);
  Alcotest.(check int)
    "record stream capacity" 64 Pulse.Client.record_stream_capacity

(* pulse-gu09, pulse-b7kt, pulse-6qr7 *)
let test_pulse_client_correlates_concurrent_replies () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "concurrent" (fun flow ->
        pulse_server_handshake flow;
        let first = pulse_read_command flow in
        let second = pulse_read_command flow in
        let respond envelope =
          if
            envelope.Pulse.Commands.packet_type
            = Pulse.Defs.Pa_command.get_server_info
          then
            pulse_write_response flow envelope Pulse.Defs.Pa_command.reply
              (pulse_server_info_body ())
          else if
            envelope.packet_type = Pulse.Defs.Pa_command.get_source_info_list
          then
            pulse_write_response flow envelope Pulse.Defs.Pa_command.reply
              Bytes.empty
          else
            Alcotest.failf "unexpected PulseAudio command %d"
              envelope.packet_type
        in
        respond second;
        respond first;
        let buffer = Cstruct.create 1 in
        try ignore (Eio.Flow.single_read flow buffer) with End_of_file -> ())
  in
  let operation () =
    Pulse.Client.with_client ~caps ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env) ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00')
      (fun client ->
        let pending, resolve = Eio.Promise.create () in
        Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.resolve resolve (Pulse.Client.get_server_info client));
        let sources = Pulse.Client.list_sources client in
        let* server = Eio.Promise.await pending in
        let+ sources = sources in
        (server, sources))
  in
  match run_boundary operation with
  | Ok (server, sources) ->
      Alcotest.(check (option string)) "server" (Some "test-server") server.name;
      Alcotest.(check int) "sources" 0 (List.length sources)
  | Error cause ->
      Alcotest.failf "concurrent PulseAudio commands failed: %a"
        (Ox.Cause.pp (fun formatter (`Pulse_error error) ->
             Pulse.Client.pp_error formatter error))
        cause

(* pulse-h303, pulse-wipy, pulse-ow5o, pulse-xqrb *)
let test_pulse_client_drops_late_reply_after_timeout () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "late" (fun flow ->
        pulse_server_handshake flow;
        let timed_out = pulse_read_command flow in
        Eio.Time.sleep clock 0.015;
        pulse_write_response flow timed_out Pulse.Defs.Pa_command.reply
          (pulse_server_info_body ());
        let next = pulse_read_command flow in
        pulse_write_response flow next Pulse.Defs.Pa_command.reply Bytes.empty;
        let buffer = Cstruct.create 1 in
        try ignore (Eio.Flow.single_read flow buffer) with End_of_file -> ())
  in
  let operation () =
    Pulse.Client.with_client ~caps ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env) ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00')
      ~request_timeout:(Ox.Duration.ms 10) (fun client ->
        let timed_out = Pulse.Client.get_server_info client in
        match timed_out with
        | Error (`Pulse_error (Pulse.Client.Timeout _)) ->
            Pulse.Client.list_sources client
        | Error (`Pulse_error error) ->
            Alcotest.failf "expected timeout, got %a" Pulse.Client.pp_error
              error
        | Ok _ -> Alcotest.fail "expected first command to time out")
  in
  match run_boundary operation with
  | Ok [] -> ()
  | Ok _ -> Alcotest.fail "expected empty source list"
  | Error cause ->
      Alcotest.failf "late reply corrupted the next command: %a"
        (Ox.Cause.pp (fun formatter (`Pulse_error error) ->
             Pulse.Client.pp_error formatter error))
        cause

(* pulse-33fc *)
let test_pulse_client_rejects_malformed_error_payload () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "malformed" (fun flow ->
        let auth = pulse_read_command flow in
        pulse_write_response flow auth Pulse.Defs.Pa_command.reply
          (pulse_u32 Pulse.Defs.pa_native_protocol_version);
        let set_name = pulse_read_command flow in
        pulse_write_response flow set_name Pulse.Defs.Pa_command.error
          (Bytes.of_string "x");
        let buffer = Cstruct.create 1 in
        try ignore (Eio.Flow.single_read flow buffer) with End_of_file -> ())
  in
  let operation () =
    Pulse.Client.with_client ~caps ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env) ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00') (fun _ ->
        Ok ())
  in
  match run_boundary operation with
  | Error cause
    when List.exists
           (function
             | `Pulse_error (Pulse.Client.Protocol_error _) -> true | _ -> false)
           (Ox.Cause.failures cause) ->
      ()
  | Error _ -> Alcotest.fail "expected typed protocol error"
  | Ok () -> Alcotest.fail "malformed command error was accepted"

(* pulse-if62, pulse-rla0, pulse-blrc *)
let test_pulse_reader_failure_fails_pending_command () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "reader-failure"
      (fun flow ->
        pulse_server_handshake flow;
        ignore (pulse_read_command flow);
        Eio.Flow.shutdown flow `All)
  in
  let operation () =
    Pulse.Client.with_client ~caps ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env) ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00')
      (fun client -> Pulse.Client.get_server_info client)
  in
  match run_boundary operation with
  | Error cause
    when List.exists
           (function
             | `Pulse_error (Pulse.Client.Disconnected _) -> true | _ -> false)
           (Ox.Cause.failures cause) ->
      ()
  | Error _ -> Alcotest.fail "expected typed reader disconnection"
  | Ok _ -> Alcotest.fail "pending command survived reader failure"

(* pulse-o36o *)
let test_pulse_scope_end_disconnects_pending_command () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let request_seen, resolve_request_seen = Eio.Promise.create () in
  let command_exit, resolve_command_exit = Eio.Promise.create () in
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "scope-end" (fun flow ->
        pulse_server_handshake flow;
        ignore (pulse_read_command flow);
        Eio.Promise.resolve resolve_request_seen ();
        let buffer = Cstruct.create 1 in
        try ignore (Eio.Flow.single_read flow buffer) with End_of_file -> ())
  in
  let operation () =
    Pulse.Client.with_client ~caps ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env) ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00')
      (fun client ->
        sync (fun () ->
            Eio.Fiber.fork ~sw (fun () ->
                let result =
                  run_boundary (fun () -> Pulse.Client.get_server_info client)
                in
                Eio.Promise.resolve resolve_command_exit result);
            Eio.Promise.await request_seen))
  in
  (match run_boundary operation with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "client scope failed while releasing normally");
  match Eio.Promise.await command_exit with
  | Error cause
    when List.exists
           (function
             | `Pulse_error (Pulse.Client.Disconnected _) -> true | _ -> false)
           (Ox.Cause.failures cause) ->
      ()
  | Error _ -> Alcotest.fail "expected typed scope-end disconnection"
  | Ok _ -> Alcotest.fail "pending command survived client scope release"

(* pulse-bc14, pulse-ngc5, pulse-9bc5, pulse-7ny7, pulse-bsa1 *)
let test_pulse_record_stream_drains_before_reader_failure () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "stream" (fun flow ->
        pulse_server_handshake flow;
        let create = pulse_read_command flow in
        let create_reply =
          pulse_command_packet Pulse.Defs.Pa_command.reply
            create.Pulse.Commands.tag
            (pulse_record_stream_body 7)
        in
        Eio.Flow.write flow
          [
            Cstruct.of_bytes create_reply;
            Cstruct.of_bytes
              (Pulse.Protocol.frame_packet ~channel:7 (Bytes.of_string "one"));
            Cstruct.of_bytes
              (Pulse.Protocol.frame_packet ~channel:7 (Bytes.of_string "two"));
          ];
        Eio.Flow.shutdown flow `All)
  in
  let seen = ref [] in
  let operation () =
    Pulse.Client.with_client ~caps ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env) ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00')
      (fun client ->
        Pulse.Client.with_record_stream client @@ fun _ stream ->
        consume_stream
          (fun chunk -> seen := Bytes.to_string chunk :: !seen)
          stream)
  in
  (match run_boundary operation with
  | Error cause
    when List.exists
           (function
             | `Pulse_error (Pulse.Client.Disconnected _) -> true | _ -> false)
           (Ox.Cause.failures cause) ->
      ()
  | Error cause ->
      Alcotest.failf "expected typed disconnection, got %a"
        (Ox.Cause.pp (fun formatter (`Pulse_error error) ->
             Pulse.Client.pp_error formatter error))
        cause
  | Ok () -> Alcotest.fail "reader failure unexpectedly succeeded");
  Alcotest.(check (list string))
    "buffered chunks" [ "one"; "two" ] (List.rev !seen)

let test_pulse_record_stream_preserves_pcm_chunk_bytes () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "pcm" (fun flow ->
        pulse_server_handshake flow;
        let create = pulse_read_command flow in
        let create_reply =
          pulse_command_packet Pulse.Defs.Pa_command.reply
            create.Pulse.Commands.tag
            (pulse_record_stream_body 7)
        in
        Eio.Flow.write flow [ Cstruct.of_bytes create_reply ];
        let pcm = Bytes.make 4 'A' in
        Eio.Flow.write flow
          [ Cstruct.of_bytes (Pulse.Protocol.frame_packet ~channel:7 pcm) ];
        Bytes.fill pcm 0 4 'B';
        Eio.Flow.write flow
          [ Cstruct.of_bytes (Pulse.Protocol.frame_packet ~channel:7 pcm) ];
        Eio.Flow.shutdown flow `All)
  in
  let received = ref [] in
  let operation () =
    Pulse.Client.with_client ~caps ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env) ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00')
      (fun client ->
        Pulse.Client.with_record_stream client @@ fun _ stream ->
        consume_stream (fun chunk -> received := chunk :: !received) stream)
  in
  (match run_boundary operation with
  | Error cause
    when List.exists
           (function
             | `Pulse_error (Pulse.Client.Disconnected _) -> true | _ -> false)
           (Ox.Cause.failures cause) ->
      ()
  | Error cause ->
      Alcotest.failf "expected typed disconnection, got %a"
        (Ox.Cause.pp (fun formatter (`Pulse_error error) ->
             Pulse.Client.pp_error formatter error))
        cause
  | Ok () -> Alcotest.fail "reader failure unexpectedly succeeded");
  let chunks = List.rev !received in
  Alcotest.(check int) "two pcm chunks" 2 (List.length chunks);
  let first = List.nth chunks 0 in
  let second = List.nth chunks 1 in
  Alcotest.(check string) "first pcm bytes" "AAAA" (Bytes.to_string first);
  Alcotest.(check string) "second pcm bytes" "BBBB" (Bytes.to_string second);
  Bytes.fill first 0 (Bytes.length first) 'Z';
  Alcotest.(check string)
    "second chunk isolated from first mutation" "BBBB" (Bytes.to_string second)

(* pulse-tg9e, pulse-4kja *)
let test_pulse_record_cleanup_preserves_body_result () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "cleanup" (fun flow ->
        pulse_server_handshake flow;
        let create = pulse_read_command flow in
        pulse_write_response flow create Pulse.Defs.Pa_command.reply
          (pulse_record_stream_body 8);
        let delete = pulse_read_command flow in
        pulse_write_response flow delete Pulse.Defs.Pa_command.error
          (pulse_u32 9);
        let buffer = Cstruct.create 1 in
        try ignore (Eio.Flow.single_read flow buffer) with End_of_file -> ())
  in
  let operation () =
    Pulse.Client.with_client ~caps ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env) ~socket_path
      ~cookie:(Bytes.make Pulse.Defs.pa_native_cookie_length '\x00')
      (fun client ->
        Pulse.Client.with_record_stream client (fun _ _ -> Ok "body"))
  in
  match run_boundary operation with
  | Ok value -> Alcotest.(check string) "body result" "body" value
  | Error cause ->
      Alcotest.failf "DELETE_RECORD_STREAM failure replaced body result: %a"
        (Ox.Cause.pp (fun formatter (`Pulse_error error) ->
             Pulse.Client.pp_error formatter error))
        cause

let test_daemon_contract_response () =
  let snap : Pie.Assistant_service.Contract.recording_snapshot =
    {
      phase = Pie.Recording.Phase.Active Pie.Recording.Phase.Processing;
      processing = 0;
      active = true;
      mode = "ptt-transcribe";
      enabled = true;
      started_at = None;
      updated_at = Some "now";
      transcript_path = None;
      last_error = None;
      build_commit = None;
      build_time = None;
      reasoning_effort = None;
      response_schema = None;
    }
  in
  let response =
    Pie.Assistant_service.Contract.response_to_json
      (Pie.Assistant_service.Contract.Status_resp snap)
  in
  (match
     Pie.Assistant_service.Contract.response_of_json
       Pie.Assistant_service.Contract.Status response
   with
  | Ok (Pie.Assistant_service.Contract.Status_resp decoded) ->
      Alcotest.(check bool) "active" true decoded.active;
      Alcotest.(check string) "mode" "ptt-transcribe" decoded.mode
  | _ -> Alcotest.fail "expected status response");
  let toggle_response =
    Pie.Assistant_service.Contract.response_to_json
      (Pie.Assistant_service.Contract.Toggle_resp true)
  in
  match
    Pie.Assistant_service.Contract.response_of_json
      Pie.Assistant_service.Contract.Toggle toggle_response
  with
  | Ok (Pie.Assistant_service.Contract.Toggle_resp true) -> ()
  | _ -> Alcotest.fail "expected toggle bool response"

(* ox_ipc refuses a socket whose directory other users can write (e.g. /tmp), so each
   daemon test binds inside a private 0700 directory. *)
let private_socket_dir =
  lazy (Filename.temp_dir ~perms:0o700 "pie-daemon-test" "")

let expect_listener_start = function
  | Ok () -> ()
  | Error _ -> Alcotest.fail "daemon listener did not start"

(* A runtime directory left 0755 (by Fs.mkdir_p ~caps under the service umask, or by the v1
   daemon) is Pie's own: the server tightens it to 0700 instead of failing with
   "unsafe runtime directory" (found at the eta-next cut-over, 2026-09-28). *)
let test_daemon_server_tightens_runtime_dir () =
  run_test @@ fun env ->
  let runtime =
    Filename.concat (Lazy.force private_socket_dir) "loose-runtime"
  in
  (try Unix.mkdir runtime 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Unix.chmod runtime 0o755;
  let socket_path = Filename.concat runtime "control.sock" in
  let persist_path = Filename.concat runtime "recording.json" in
  let coordinator = Pie.Recording.Coordinator.create ~caps ~persist_path () in
  (try
     Eio.Switch.run @@ fun sw ->
     expect_listener_start
       (Pie.Assistant_service.Server.start ~caps ~sw ~fs:(Eio.Stdenv.fs env)
          ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ~socket_path
          coordinator);
     Eio.Switch.fail sw Exit
   with Exit -> ());
  Alcotest.(check int)
    "runtime dir tightened" 0o700
    ((Unix.stat runtime).Unix.st_perm land 0o777)

let test_daemon_server_state () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    Filename.concat
      (Lazy.force private_socket_dir)
      (Printf.sprintf "pie-daemon-%d.sock" (Unix.getpid ()))
  in
  let persist_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "pie-daemon-%d.json" (Unix.getpid ()))
  in
  (try Sys.remove socket_path with Sys_error _ -> ());
  (try Sys.remove persist_path with Sys_error _ -> ());
  let coordinator = Pie.Recording.Coordinator.create ~caps ~persist_path () in
  let effort = ref "low" in
  expect_listener_start
    (Pie.Assistant_service.Server.start ~caps ~sw ~fs:(Eio.Stdenv.fs env)
       ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ~socket_path
       ~status_metadata:(fun () -> (Some !effort, Some "dictation/text-v1"))
       coordinator);
  let send req =
    let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close flow)
      (fun () ->
        let payload =
          Yojson.Safe.to_string
            (Pie.Assistant_service.Contract.request_to_json req)
          ^ "\n"
        in
        Eio.Flow.write flow [ Cstruct.of_string payload ];
        let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
        Yojson.Safe.from_string (Eio.Buf_read.line reader))
  in
  ignore (send Pie.Assistant_service.Contract.Pause);
  let check_metadata expected json =
    Alcotest.(check bool)
      "status configured effort" true
      (Yojson.Safe.Util.member "reasoningEffort" json = `String expected);
    Alcotest.(check bool)
      "status configured schema" true
      (Yojson.Safe.Util.member "responseSchema" json
      = `String "dictation/text-v1")
  in
  let status = send Pie.Assistant_service.Contract.Status in
  check_metadata "low" status;
  effort := "high";
  check_metadata "high" (send Pie.Assistant_service.Contract.Status);
  (match status with
  | `Assoc fields ->
      Alcotest.(check bool)
        "paused" false
        (match List.assoc_opt "enabled" fields with
        | Some (`Bool b) -> b
        | _ -> true)
  | _ -> Alcotest.fail "status value must be object");
  (match send Pie.Assistant_service.Contract.Toggle with
  | `Assoc fields ->
      Alcotest.(check bool)
        "toggle enabled" true
        (match List.assoc_opt "value" fields with
        | Some (`Bool b) -> b
        | _ -> false)
  | _ -> Alcotest.fail "toggle should return enabled=true");
  try Sys.remove persist_path with Sys_error _ -> ()

let test_daemon_server_action_handler () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    Filename.concat
      (Lazy.force private_socket_dir)
      (Printf.sprintf "pie-daemon-action-%d.sock" (Unix.getpid ()))
  in
  let persist_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "pie-daemon-action-%d.json" (Unix.getpid ()))
  in
  (try Sys.remove socket_path with Sys_error _ -> ());
  (try Sys.remove persist_path with Sys_error _ -> ());
  let coordinator = Pie.Recording.Coordinator.create ~caps ~persist_path () in
  let seen = ref [] in
  let action_handler action =
    seen := action :: !seen;
    {
      Pie.Assistant_service.Contract.action;
      accepted = true;
      message = Some "queued";
      snapshot =
        {
          phase = Pie.Recording.Phase.Ready;
          processing = 0;
          active = false;
          mode = "idle";
          enabled = true;
          started_at = None;
          updated_at = Some "now";
          transcript_path = None;
          last_error = None;
          build_commit = None;
          build_time = None;
          reasoning_effort = None;
          response_schema = None;
        };
    }
  in
  expect_listener_start
    (Pie.Assistant_service.Server.start ~caps ~sw ~fs:(Eio.Stdenv.fs env)
       ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ~socket_path
       ~action_handler coordinator);
  let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
  let response =
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close flow)
      (fun () ->
        let payload =
          Yojson.Safe.to_string
            (Pie.Assistant_service.Contract.request_to_json
               (Pie.Assistant_service.Contract.Action "ptt-start"))
          ^ "\n"
        in
        Eio.Flow.write flow [ Cstruct.of_string payload ];
        let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
        Yojson.Safe.from_string (Eio.Buf_read.line reader))
  in
  Alcotest.(check (list string))
    "handler called" [ "ptt-start" ] (List.rev !seen);
  (match response with
  | `Assoc fields ->
      Alcotest.(check bool)
        "accepted" true
        (match List.assoc_opt "accepted" fields with
        | Some (`Bool b) -> b
        | _ -> false);
      Alcotest.(check string)
        "message" "queued"
        (match List.assoc_opt "message" fields with
        | Some (`String s) -> s
        | _ -> "")
  | _ -> Alcotest.fail "expected action response object");
  try Sys.remove persist_path with Sys_error _ -> ()

let test_daemon_server_unknown_meeting_method () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    Filename.concat
      (Lazy.force private_socket_dir)
      (Printf.sprintf "pie-daemon-%d.sock" (Unix.getpid ()))
  in
  let persist_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "pie-daemon-%d.json" (Unix.getpid ()))
  in
  (try Sys.remove socket_path with Sys_error _ -> ());
  (try Sys.remove persist_path with Sys_error _ -> ());
  let coordinator = Pie.Recording.Coordinator.create ~caps ~persist_path () in
  ignore
    (Pie.Recording.Coordinator.try_start coordinator
       Pie.Recording.Coordinator.Ptt_transcribe ());
  expect_listener_start
    (Pie.Assistant_service.Server.start ~caps ~sw ~fs:(Eio.Stdenv.fs env)
       ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env) ~socket_path
       coordinator);
  let send method_str =
    let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
    Fun.protect
      ~finally:(fun () -> Eio.Flow.close flow)
      (fun () ->
        let payload =
          Yojson.Safe.to_string (`Assoc [ ("method", `String method_str) ])
          ^ "\n"
        in
        Eio.Flow.write flow [ Cstruct.of_string payload ];
        let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
        Yojson.Safe.from_string (Eio.Buf_read.line reader))
  in
  (match send "MeetingStop" with
  | `Assoc fields ->
      Alcotest.(check string)
        "unknown method rejected"
        Pie.Assistant_service.Server.err_malformed_request
        (match List.assoc_opt "error" fields with
        | Some (`String s) -> s
        | _ -> "");
      Alcotest.(check bool)
        "no admission change field" false
        (List.mem_assoc "active" fields)
  | _ -> Alcotest.fail "expected JSON error object");
  Alcotest.(check bool)
    "ptt admission unchanged" true
    (Pie.Recording.Coordinator.snapshot coordinator)
      .Pie.Recording.Coordinator.active;
  try Sys.remove persist_path with Sys_error _ -> ()

let test_codex_realtime_url_builders () =
  let transcribe_url =
    Stt.Codex_realtime.build_realtime_url ~caps
      ~base_url:"wss://api.openai.com/" ~mode:Stt.Codex_realtime.Transcription
      ~model:"gpt-live-transcribe" ()
  in
  Alcotest.(check string)
    "transcription url" "wss://api.openai.com/v1/realtime?intent=transcription"
    transcribe_url;
  let translate_url =
    Stt.Codex_realtime.build_realtime_url ~caps ~base_url:"wss://api.openai.com"
      ~mode:Stt.Codex_realtime.Transcription ~model:"gpt-live-transcribe" ()
  in
  Alcotest.(check string)
    "translation url" "wss://api.openai.com/v1/realtime?intent=transcription"
    translate_url

let test_codex_realtime_headers () =
  let headers =
    Stt.Codex_realtime.realtime_header_pairs ~session_id:"pie-test-session"
      "token"
  in
  Alcotest.(check (option string))
    "authorization" (Some "Bearer token")
    (List.assoc_opt "Authorization" headers);
  Alcotest.(check (option string))
    "originator" (Some "codex_cli_rs")
    (List.assoc_opt "originator" headers);
  Alcotest.(check (option string))
    "user agent" (Some "codex_cli_rs/0.0.0 (pie)")
    (List.assoc_opt "User-Agent" headers);
  Alcotest.(check (option string))
    "version" (Some "0.0.0")
    (List.assoc_opt "version" headers);
  Alcotest.(check (option string))
    "session id" (Some "pie-test-session")
    (List.assoc_opt "x-session-id" headers);
  Alcotest.(check (option string))
    "beta header" None
    (List.assoc_opt "OpenAI-Beta" headers)

let test_codex_realtime_receive_stop_modes () =
  Alcotest.(check bool)
    "transcription final stops" true
    (Stt.Codex_realtime.event_should_stop ~mode:Stt.Codex_realtime.Transcription
       (Stt.Codex_realtime.Transcript_final "text"));
  Alcotest.(check bool)
    "delta does not stop" false
    (Stt.Codex_realtime.event_should_stop ~mode:Stt.Codex_realtime.Transcription
       (Stt.Codex_realtime.Transcript_delta "partial"))

let test_codex_realtime_transcription_session_update () =
  match
    Stt.Codex_realtime.build_pass1_session_update ~model:"configured-model" ()
  with
  | `Assoc fields -> (
      Alcotest.(check (option string))
        "event type" (Some "session.update")
        (match List.assoc_opt "type" fields with
        | Some (`String value) -> Some value
        | _ -> None);
      match List.assoc_opt "session" fields with
      | Some (`Assoc session) ->
          Alcotest.(check (option string))
            "session type" (Some "transcription")
            (match List.assoc_opt "type" session with
            | Some (`String value) -> Some value
            | _ -> None);
          Alcotest.(check (option string))
            "no websocket session model in session.update" None
            (match List.assoc_opt "model" session with
            | Some (`String value) -> Some value
            | _ -> None);
          Alcotest.(check bool)
            "instructions omitted" false
            (List.mem_assoc "instructions" session);
          Alcotest.(check bool)
            "tools omitted" false
            (List.mem_assoc "tools" session);
          let model =
            match List.assoc_opt "audio" session with
            | Some (`Assoc audio) -> (
                match List.assoc_opt "input" audio with
                | Some (`Assoc input) -> (
                    match List.assoc_opt "transcription" input with
                    | Some (`Assoc transcription) -> (
                        match List.assoc_opt "model" transcription with
                        | Some (`String value) -> Some value
                        | _ -> None)
                    | _ -> None)
                | _ -> None)
            | _ -> None
          in
          Alcotest.(check (option string))
            "configured transcription model" (Some "configured-model") model
      | _ -> Alcotest.fail "expected session object")
  | _ -> Alcotest.fail "expected object"

let test_codex_realtime_event_parser () =
  (match
     Stt.Codex_realtime.parse_event
       "{\"type\":\"conversation.item.input_audio_transcription.delta\",\"delta\":\"he\"}"
   with
  | Ok (Stt.Codex_realtime.Transcript_delta "he") -> ()
  | _ -> Alcotest.fail "expected transcript delta");
  (match
     Stt.Codex_realtime.parse_event
       "{\"type\":\"conversation.item.input_audio_transcription.completed\",\"text\":\"hello\"}"
   with
  | Ok (Stt.Codex_realtime.Transcript_final "hello") -> ()
  | _ -> Alcotest.fail "expected transcript done");
  match
    Stt.Codex_realtime.parse_event
      "{\"type\":\"error\",\"error\":{\"message\":\"bad\",\"code\":\"x\"}}"
  with
  | Ok (Stt.Codex_realtime.Server_error { message = "bad"; code = Some "x" }) ->
      ()
  | _ -> Alcotest.fail "expected error event"

let test_codex_realtime_first_token_timer () =
  let timer = Stt.Codex_realtime.First_token_timer.create () in
  Stt.Codex_realtime.First_token_timer.start timer ~at_ms:1_000;
  Alcotest.(check (option int))
    "whitespace is not a token" None
    (Stt.Codex_realtime.First_token_timer.observe timer ~at_ms:1_040 "  ");
  Alcotest.(check (option int))
    "first token latency" (Some 125)
    (Stt.Codex_realtime.First_token_timer.observe timer ~at_ms:1_125 "hello");
  Alcotest.(check (option int))
    "emits once" None
    (Stt.Codex_realtime.First_token_timer.observe timer ~at_ms:1_180 " world")

let test_codex_realtime_resample () =
  let input = Bytes.create 4 in
  Bytes.set_int16_le input 0 0;
  Bytes.set_int16_le input 2 12000;
  let output =
    Stt.Codex_realtime.resample_s16le_mono ~from_rate:16000 ~to_rate:24000 input
  in
  Alcotest.(check int)
    "2 samples at 16k become 3 at 24k" 6 (Bytes.length output);
  Alcotest.(check int) "first sample" 0 (Bytes.get_int16_le output 0)

(* stttime-33v8, stttime-nby7, stttime-blym, stttime-pf5a *)
let test_speech_operation_deadline_policy () =
  Alcotest.(check int)
    "OAuth response idle timeout" 30_000
    (Ox.Duration.to_ms Stt.Codex_realtime.oauth_response_idle_timeout);
  Alcotest.(check int)
    "WebSocket establishment timeout" 10_000
    (Ox.Duration.to_ms Stt.Codex_realtime.websocket_establishment_timeout);
  Alcotest.(check int)
    "direct transcription timeout" 60_000
    (Ox.Duration.to_ms Stt.Openrouter.direct_transcription_timeout)

(* Both calls enter the real provider boundary. The server accepts a local socket
   but sends no response, while Ox_test advances the clock used by Ox_http. *)
let stalled_openrouter_call ~seconds call =
  Ox_test.run (fun env sw ->
      let socket =
        Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 env#net
          (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
      in
      let port =
        match Eio.Net.listening_addr socket with
        | `Tcp (_, port) -> port
        | `Unix _ -> Alcotest.fail "expected TCP listener"
      in
      let accepted, resolve_accepted = Eio.Promise.create () in
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Switch.run @@ fun conn_sw ->
          let _flow, _addr = Eio.Net.accept ~sw:conn_sw socket in
          Eio.Promise.resolve resolve_accepted ();
          Eio.Fiber.await_cancel ());
      let previous_url = Sys.getenv_opt "OPENROUTER_BASE_URL" in
      let previous_key = Sys.getenv_opt "OPENROUTER_API_KEY" in
      Unix.putenv "OPENROUTER_BASE_URL"
        (Printf.sprintf "http://127.0.0.1:%d" port);
      Unix.putenv "OPENROUTER_API_KEY" "pie-local-deadline-test";
      Fun.protect
        ~finally:(fun () ->
          Unix.putenv "OPENROUTER_BASE_URL"
            (Option.value previous_url ~default:"");
          Unix.putenv "OPENROUTER_API_KEY"
            (Option.value previous_key ~default:""))
        (fun () ->
          let completed, resolve_completed = Eio.Promise.create () in
          Eio.Fiber.fork ~sw (fun () ->
              Eio.Promise.resolve resolve_completed (call env sw));
          Eio.Promise.await accepted;
          Eio.Fiber.yield ();
          Ox_test.advance env#clock (Ox.Duration.seconds seconds);
          Eio.Promise.await completed))

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

let has_suffix suffix value =
  let suffix_len = String.length suffix in
  let value_len = String.length value in
  value_len >= suffix_len
  && String.equal suffix (String.sub value (value_len - suffix_len) suffix_len)

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
  let masked = Char.code (Bytes.get header 1) land 0x80 <> 0 in
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
          Char.code (Bytes.get payload i)
          lxor Char.code (Bytes.get key (i mod 4))
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
  Ox_http.Ws_codec.encode
    { fin = true; opcode = Close; payload = close_payload 1000 "" }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let drain_ws_until_client_eof ~clock flow =
  let buf = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read flow buf with
    | 0 -> ()
    | _ -> loop ()
    | exception End_of_file -> ()
  in
  Eio.Time.with_timeout_exn clock 4.0 loop

let write_ws_switching_response flow key =
  let response =
    "HTTP/1.1 101 Switching Protocols\r\n" ^ "Upgrade: websocket\r\n"
    ^ "Connection: keep-alive, Upgrade\r\n" ^ "Sec-WebSocket-Accept: "
    ^ Ox_http.Ws_codec.accept_key ~sha1:Ox_http.Ox_http_tls_openssl.sha1 key
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

let run_realtime_error_ws_server ~clock flow =
  let head = read_http_head flow in
  Alcotest.(check (option string))
    "request target" (Some "/v1/realtime?model=gpt-realtime-2")
    (request_target head);
  Alcotest.(check (option string))
    "beta header" None
    (header_value "OpenAI-Beta" head);
  Alcotest.(check (option string))
    "originator" (Some "codex_cli_rs")
    (header_value "originator" head);
  Alcotest.(check (option string))
    "version" (Some "0.0.0")
    (header_value "version" head);
  (match header_value "x-session-id" head with
  | Some value when String.starts_with ~prefix:"pie-" value -> ()
  | Some value -> Alcotest.failf "unexpected x-session-id: %s" value
  | None -> Alcotest.fail "missing x-session-id");
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> Alcotest.fail "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key;
  (* Let the client finish sending before delivering the provider error, so
     cancellation of the sender cannot mask the receiver result. *)
  Eio.Time.sleep clock 0.2;
  write_ws_text flow
    {|{"type":"error","error":{"message":"boom","code":"bad_request"}}|};
  drain_ws_until_client_eof ~clock flow

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

let require_store dir =
  match Recording.Retry_store.Active.open_store ~caps ~root:dir with
  | Ok store -> store
  | Error (`Retry_store msg) -> Alcotest.fail msg

let require_owner ~dir ~coordinator ~attempt mode =
  let store = require_store dir in
  match
    Assistant.Service.create_attempt_owner ~store ~coordinator ~attempt mode
  with
  | Ok owner -> owner
  | Error (`Ptt_error msg) -> Alcotest.fail msg

let peek_audio_bytes store =
  match Recording.Retry_store.Active.peek store with
  | Error (`Retry_store msg) -> Alcotest.fail msg
  | Ok None -> Alcotest.fail "expected retained audio"
  | Ok (Some (Recording.Retry_store.Active.Text _)) ->
      Alcotest.fail "expected Audio candidate"
  | Ok (Some (Recording.Retry_store.Active.Audio { generation; _ })) -> (
      match Recording.Retry_store.Active.load_audio store generation with
      | Ok bytes -> bytes
      | Error (`Retry_store msg) -> Alcotest.fail msg)

let peek_none store =
  match Recording.Retry_store.Active.peek store with
  | Ok None -> true
  | Ok (Some _) -> false
  | Error (`Retry_store msg) -> Alcotest.fail msg

let test_retry_store_retains_and_discards_recordings () =
  let dir = temp_dir "pie-retry-store" in
  let store = require_store dir in
  let writer =
    match
      Recording.Retry_store.Active.open_audio_writer store
        ~mode:Recording.Retry_store.Transcribe
    with
    | Ok writer -> writer
    | Error (`Retry_store msg) -> Alcotest.fail msg
  in
  (match
     Recording.Retry_store.Active.append_audio writer
       (Bytes.of_string "first-second")
   with
  | Ok () -> ()
  | Error (`Retry_store msg) -> Alcotest.fail msg);
  (match Recording.Retry_store.Active.close_writer writer with
  | Ok () -> ()
  | Error (`Retry_store msg) -> Alcotest.fail msg);
  let generation =
    match Recording.Retry_store.Active.publish_audio store writer with
    | Ok generation -> generation
    | Error (`Retry_store msg) -> Alcotest.fail msg
  in
  Alcotest.(check string)
    "captured bytes" "first-second"
    (Bytes.to_string (peek_audio_bytes store));
  (match Recording.Retry_store.Active.clear store generation with
  | Ok () -> ()
  | Error (`Retry_store msg) -> Alcotest.fail msg);
  Alcotest.(check bool) "clear after successful retry" true (peek_none store);
  let rec rm path =
    match Unix.lstat path with
    | { st_kind = Unix.S_DIR; _ } ->
        Array.iter
          (fun n -> if n <> "." && n <> ".." then rm (Filename.concat path n))
          (Sys.readdir path);
        Unix.rmdir path
    | _ -> Unix.unlink path
  in
  rm dir

let pp_ptt_error formatter = function
  | `Ptt_cause cause ->
      Ox.Cause.pp
        (fun formatter (`Ptt_error message) ->
          Format.pp_print_string formatter message)
        formatter cause
  | `Ptt_error message -> Format.pp_print_string formatter message
  | `Pulse_error error ->
      Format.pp_print_string formatter (Pulse.Client.error_message error)
  | `Timeout -> Format.pp_print_string formatter "timeout"

let cause_contains cause needle =
  let rendered = Format.asprintf "%a" (Ox.Cause.pp pp_ptt_error) cause in
  let rec loop i =
    i <= String.length rendered - String.length needle
    && (String.sub rendered i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let openrouter_clip_config () : Pie.Config.Stt.t =
  {
    provider = Pie.Config.Stt.Openrouter;
    transcription_model = "openrouter/google/gemini-3.1-flash-lite";
    translation_model = "openrouter/google/gemini-3.1-flash-lite";
    transcription_language = "Polish";
    translation_source_language = "Polish";
    translation_target_language = "English";
    ptt_release_grace_ms = 0;
    ptt_min_capture_rms = 0.001;
    ptt_min_capture_peak = 0.008;
    ptt_transcribe_keysym = 65516;
    ptt_translate_keysym = 65508;
    ptt_transcribe_shift = true;
    ptt_translate_shift = false;
    thinking_effort = Some "minimal";
    newline_rules = [];
    text_entry_rules = [];
    prompt_rules = [];
    pipeline = Two_pass;
    openrouter_provider_order = None;
    one_pass_timeout_seconds = 30.;
    streaming = false;
    correction = None;
    audio_retention_hours = 24.;
    bindings =
      [
        {
          keysym = 65516;
          shift = true;
          mode = Pie.Config.Stt.Transcribe;
          correct = false;
          prompt_named = false;
          prompt_file = "correction.md";
          prompt = "{{glossary}}";
        };
        {
          keysym = 65508;
          shift = false;
          mode = Pie.Config.Stt.Translate;
          correct = false;
          prompt_named = false;
          prompt_file = "correction-translate.md";
          prompt = "{{glossary}} {{source_language}} {{target_language}}";
        };
      ];
  }

let rec remove_dir dir =
  match Unix.lstat dir with
  | { st_kind = Unix.S_DIR; _ } ->
      Array.iter
        (fun name ->
          if name <> "." && name <> ".." then
            remove_dir (Filename.concat dir name))
        (Sys.readdir dir);
      Unix.rmdir dir
  | _ -> Unix.unlink dir

let settle_attempt owner () : (unit, Ptt_loop.error) result =
  match Assistant.Service.settle_attempt owner with
  | Ok () -> Ok ()
  | Error (`Ptt_error message) -> Error (`Ptt_error message)

let test_failed_processing_retains_captured_audio () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-failed-capture" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let attempt =
    match
      Recording.Coordinator.try_start coordinator
        Recording.Coordinator.Ptt_transcribe ()
    with
    | Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected the attempt to be admitted"
  in
  let owner =
    require_owner ~dir ~coordinator ~attempt Assistant.Service.Transcribe
  in
  let stt _config _mode _bytes = Error (`Ptt_error "STT unavailable") in
  let deliver _config _mode _target _text = Ok Pie.History.Pasted in
  let inner =
    Assistant.Service.make_clip_handle ~owner ~stt ~deliver
      ~config:(openrouter_clip_config ())
      Assistant.Service.Transcribe
  in
  let handle = Assistant.Service.attempt_handle owner inner in
  let pcm_bytes = Bytes.of_string "captured-audio" in
  let operation () =
    Assistant.Service.run_acquired_attempt ~sw ~attempt
      ~on_settle:(settle_attempt owner) (fun () ->
        let* () = handle.offer pcm_bytes in
        match handle.finish with
        | Ptt_loop.Buffered f -> f { Ptt_loop.duration_ms = 500; pcm_bytes }
        | Ptt_loop.Streaming _ -> Alcotest.fail "expected Buffered finish")
  in
  (match run_boundary operation with
  | Ok () -> Alcotest.fail "expected processing failure"
  | Error cause ->
      Alcotest.(check bool)
        "preserves STT unavailable" true
        (cause_contains cause "STT unavailable"));
  Alcotest.(check bool)
    "failed finish releases admission" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  let store = require_store dir in
  Alcotest.(check string)
    "retained published Audio" "captured-audio"
    (Bytes.to_string (peek_audio_bytes store));
  remove_dir dir

let test_setup_failure_releases_admission_and_preserves_error () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-setup-failure" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let attempt =
    match
      Recording.Coordinator.try_start coordinator
        Recording.Coordinator.Ptt_transcribe ()
    with
    | Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected the attempt to be admitted"
  in
  let owner =
    require_owner ~dir ~coordinator ~attempt Assistant.Service.Transcribe
  in
  let operation () =
    Assistant.Service.run_acquired_attempt ~sw ~attempt
      ~on_settle:(settle_attempt owner) (fun () ->
        Error (`Ptt_error "synthetic setup failure"))
  in
  (match run_boundary operation with
  | Ok () -> Alcotest.fail "setup failure was erased"
  | Error cause ->
      Alcotest.(check bool)
        "preserves the setup failure" true
        (cause_contains cause "synthetic setup failure"));
  Alcotest.(check bool)
    "setup failure releases admission" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  Alcotest.(check bool)
    "setup failure does not publish" true
    (peek_none (require_store dir));
  (match
     Recording.Coordinator.try_start coordinator
       Recording.Coordinator.Ptt_transcribe ()
   with
  | Recording.Coordinator.Started _ -> ()
  | _ -> Alcotest.fail "expected a subsequent attempt after setup failure");
  remove_dir dir

let test_settle_attempt_construction_does_not_run_duties () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-settle-blueprint" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let attempt =
    match
      Recording.Coordinator.try_start coordinator
        Recording.Coordinator.Ptt_transcribe ()
    with
    | Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected the attempt to be admitted"
  in
  let owner =
    require_owner ~dir ~coordinator ~attempt Assistant.Service.Transcribe
  in
  let store = require_store dir in
  let handle =
    Assistant.Service.attempt_handle owner
      {
        Ptt_loop.offer = (fun _ -> Ok ());
        finish = Ptt_loop.Buffered (fun _ -> Ok ());
        cancel = (fun () -> Ok ());
      }
  in
  let pcm_bytes = Bytes.of_string "captured-audio" in
  (match run_boundary (fun () -> handle.offer pcm_bytes) with
  | Ok () -> ()
  | Error cause ->
      Alcotest.failf "offer failed: %a" (Ox.Cause.pp pp_ptt_error) cause);
  Alcotest.(check bool)
    "unpublished draft is not a recovery candidate" true (peek_none store);
  Alcotest.(check bool)
    "admitted before cleanup" true
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  let abandoned () = Assistant.Service.settle_attempt owner in
  ignore abandoned;
  Alcotest.(check bool)
    "abandoned blueprint leaves admission" true
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  Alcotest.(check bool)
    "abandoned blueprint does not publish" true (peek_none store);
  (match run_boundary (fun () -> Assistant.Service.settle_attempt owner) with
  | Ok () -> ()
  | Error cause ->
      Alcotest.failf "executed cleanup failed: %a" (Ox.Cause.pp pp_ptt_error)
        cause);
  Alcotest.(check bool)
    "executed cleanup releases admission" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  Alcotest.(check bool)
    "executed cleanup does not publish abort" true (peek_none store);
  (match run_boundary (fun () -> Assistant.Service.settle_attempt owner) with
  | Ok () -> ()
  | Error cause ->
      Alcotest.failf "repeated cleanup failed: %a" (Ox.Cause.pp pp_ptt_error)
        cause);
  Alcotest.(check bool)
    "repeated cleanup stays idle" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  Alcotest.(check bool)
    "repeated cleanup still unpublished" true (peek_none store);
  remove_dir dir

let test_buffered_clip_cancellation_settles_after_offer () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let dir = temp_dir "pie-clip-cancel" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let observed = ref None in
  let offered, resolve_offered = Eio.Promise.create () in
  let pcm_bytes = Bytes.of_string "captured-audio" in
  let waiting () =
    let exit =
      Ox.Scope.run (fun _ ->
          let* press =
            Pie_test_runtime.Service.on_press ~caps ~sw
              ~net:(Eio.Stdenv.net env) ~clock ~logger:(test_logger env)
              ~stt:(fun _ _ _ -> Ok "synthetic transcript")
              ~deliver:(fun _ _ _ _ -> Ok Pie.History.Pasted)
              ~coordinator ~retry_dir:dir
              ~config:(openrouter_clip_config ())
              "transcribe"
          in
          match press with
          | Ptt_loop.Handle _ ->
              Error
                (`Ptt_error
                   "buffered clip attempt returned Handle without Scoped")
          | Ptt_loop.Skip -> Error (`Ptt_error "buffered clip attempt skipped")
          | Ptt_loop.Scoped with_handle ->
              with_handle (fun handle ->
                  let* () = handle.offer pcm_bytes in
                  Eio.Promise.resolve resolve_offered ();
                  let blocked, _ = Eio.Promise.create () in
                  Eio.Promise.await blocked))
    in
    observed := Some exit;
    match exit with
    | Ox.Exit.Success () -> Ok `Body
    | Ox.Exit.Failure _ -> Error (`Ptt_error "waiting body failed")
  in
  let operation () =
    Eio.Fiber.first waiting (fun () ->
        Eio.Promise.await offered;
        Ok `Stop)
  in
  (match run_boundary operation with
  | Error cause when cause_contains cause "harness timeout" ->
      Alcotest.fail "cancellation did not complete before harness timeout"
  | Error cause ->
      Alcotest.failf "cancellation race failed: %a" (Ox.Cause.pp pp_ptt_error)
        cause
  | Ok `Body -> Alcotest.fail "waiting body completed before cancellation"
  | Ok `Stop -> ());
  (match !observed with
  | None -> Alcotest.fail "cancelled body did not run on_exit"
  | Some (Ox.Exit.Success _) -> Alcotest.fail "cancelled body succeeded"
  | Some (Ox.Exit.Failure cause) ->
      Alcotest.(check bool)
        "cancellation stays interruption" true
        (Ox.Cause.is_interrupt_only cause));
  Alcotest.(check bool)
    "cancellation releases admission" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  (* dict-u5xj: the attempt was not paused or cancelled by the user, so the
     captured audio stays available for pie retry (a service stop is the same
     kind of cancellation). *)
  Alcotest.(check bool)
    "unrequested cancellation publishes the captured audio" false
    (peek_none (require_store dir));
  remove_dir dir

let test_buffered_clip_defect_settles_after_offer () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-clip-defect" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let pcm_bytes = Bytes.of_string "captured-audio" in
  let operation () =
    let* press =
      Pie_test_runtime.Service.on_press ~caps ~sw ~net:(Eio.Stdenv.net env)
        ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env)
        ~stt:(fun _ _ _ -> Ok "synthetic transcript")
        ~deliver:(fun _ _ _ _ -> Ok Pie.History.Pasted)
        ~coordinator ~retry_dir:dir
        ~config:(openrouter_clip_config ())
        "transcribe"
    in
    match press with
    | Ptt_loop.Handle _ ->
        Error
          (`Ptt_error "buffered clip attempt returned Handle without Scoped")
    | Ptt_loop.Skip -> Error (`Ptt_error "buffered clip attempt skipped")
    | Ptt_loop.Scoped with_handle ->
        with_handle (fun handle ->
            let* () = handle.offer pcm_bytes in
            sync (fun () -> failwith "synthetic defect"))
  in
  (match run_boundary operation with
  | Ok () -> Alcotest.fail "defect was erased"
  | Error cause ->
      Alcotest.(check bool)
        "defect is not interruption" false
        (Ox.Cause.is_interrupt_only cause);
      Alcotest.(check bool)
        "preserves synthetic defect" true
        (cause_contains cause "synthetic defect"));
  Alcotest.(check bool)
    "defect releases admission" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  (match Recording.Retry_store.Active.peek (require_store dir) with
  | Ok (Some (Recording.Retry_store.Active.Audio { generation; _ })) -> (
      match
        Recording.Retry_store.Active.load_audio (require_store dir) generation
      with
      | Ok retained ->
          Alcotest.(check bytes)
            "dict-u5xj defect retains exact audio" pcm_bytes retained
      | Error (`Retry_store message) -> Alcotest.fail message)
  | _ -> Alcotest.fail "dict-u5xj defect must retain an audio candidate");
  remove_dir dir

let test_on_press_construction_does_not_admit () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-on-press-construct" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  Alcotest.(check bool)
    "idle before construction" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  let blueprint () =
    Pie_test_runtime.Service.on_press ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env)
      ~stt:(fun _ _ _ -> Ok "synthetic transcript")
      ~deliver:(fun _ _ _ _ -> Ok Pie.History.Pasted)
      ~coordinator
      ~config:(openrouter_clip_config ())
      "transcribe"
  in
  ignore blueprint;
  Alcotest.(check bool)
    "constructing on_press does not admit without execution" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  remove_dir dir

let test_on_press_execution_admits () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-on-press-exec" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let blueprint () =
    Pie_test_runtime.Service.on_press ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env)
      ~stt:(fun _ _ _ -> Ok "synthetic transcript")
      ~deliver:(fun _ _ _ _ -> Ok Pie.History.Pasted)
      ~coordinator ~retry_dir:dir
      ~config:(openrouter_clip_config ())
      "transcribe"
  in
  Alcotest.(check bool)
    "idle before execution" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  (match run_boundary blueprint with
  | Error cause ->
      Alcotest.failf "on_press execution failed: %a" (Ox.Cause.pp pp_ptt_error)
        cause
  | Ok Ptt_loop.Skip -> Alcotest.fail "expected admission"
  | Ok (Ptt_loop.Handle _) ->
      Alcotest.fail "buffered clip attempt must remain Scoped"
  | Ok (Ptt_loop.Scoped _) -> ());
  Alcotest.(check bool)
    "execution admits" true
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  remove_dir dir

let test_on_press_busy_and_disabled_at_execution () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-on-press-busy" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let press () =
    Pie_test_runtime.Service.on_press ~caps ~sw ~net ~clock
      ~logger:(test_logger env)
      ~stt:(fun _ _ _ -> Ok "synthetic transcript")
      ~deliver:(fun _ _ _ _ -> Ok Pie.History.Pasted)
      ~coordinator ~retry_dir:dir
      ~config:(openrouter_clip_config ())
      "transcribe"
  in
  let first = press in
  let second = press in
  Alcotest.(check bool)
    "constructed pair leaves coordinator idle" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  (match run_boundary first with
  | Ok (Ptt_loop.Scoped _) -> ()
  | Ok _ -> Alcotest.fail "first execution should admit a Scoped handle"
  | Error cause ->
      Alcotest.failf "first execution failed: %a" (Ox.Cause.pp pp_ptt_error)
        cause);
  (match run_boundary second with
  | Ok Ptt_loop.Skip -> ()
  | Ok _ -> Alcotest.fail "second execution should see busy admission"
  | Error cause ->
      Alcotest.failf "busy check became an error: %a" (Ox.Cause.pp pp_ptt_error)
        cause);
  Alcotest.(check bool)
    "first attempt still owns admission" true
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  Recording.Coordinator.set_enabled coordinator false;
  let while_enabled = press in
  (match run_boundary while_enabled with
  | Ok Ptt_loop.Skip -> ()
  | Ok _ -> Alcotest.fail "disabled execution should skip"
  | Error cause ->
      Alcotest.failf "disabled check became an error: %a"
        (Ox.Cause.pp pp_ptt_error) cause);
  Recording.Coordinator.set_enabled coordinator false;
  let constructed_while_disabled = press in
  Recording.Coordinator.set_enabled coordinator true;
  (match Recording.Coordinator.current_attempt coordinator with
  | Some attempt -> ignore (Recording.Coordinator.release coordinator attempt)
  | None -> ());
  (match run_boundary constructed_while_disabled with
  | Ok (Ptt_loop.Scoped _) -> ()
  | Ok Ptt_loop.Skip ->
      Alcotest.fail "disabled snapshot must be read at execution"
  | Ok _ -> Alcotest.fail "expected Scoped after enabling"
  | Error cause ->
      Alcotest.failf "enabled execution failed: %a" (Ox.Cause.pp pp_ptt_error)
        cause);
  Alcotest.(check bool)
    "execution after enable admits" true
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  remove_dir dir

let test_on_press_constructor_failure_releases_without_clearing_other_attempt ()
    =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-on-press-ctor" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let config =
    { (openrouter_clip_config ()) with transcription_model = "openrouter/" }
  in
  let blueprint () =
    Pie_test_runtime.Service.on_press ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(test_logger env)
      ~stt:(fun _ _ _ -> Ok "synthetic transcript")
      ~deliver:(fun _ _ _ _ -> Ok Pie.History.Pasted)
      ~coordinator ~retry_dir:dir ~config "transcribe"
  in
  Alcotest.(check bool)
    "constructor blueprint is idle" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  (match run_boundary blueprint with
  | Ok Ptt_loop.Skip | Ok (Ptt_loop.Handle _) ->
      Alcotest.fail "empty model should not Skip/Handle"
  | Ok (Ptt_loop.Scoped with_handle) -> (
      Alcotest.(check bool)
        "on_press admits before deferred handle construction" true
        (Recording.Coordinator.snapshot coordinator)
          .Recording.Coordinator.active;
      match run_boundary (fun () -> with_handle (fun _ -> Ok ())) with
      | Ok _ -> Alcotest.fail "empty model should fail handle construction"
      | Error cause -> (
          match Ox.Cause.failures cause with
          | [ `Ptt_error "config:model" ] ->
              Alcotest.(check bool)
                "constructor failure is not interruption" false
                (Ox.Cause.is_interrupt_only cause)
          | _ ->
              Alcotest.failf "expected exact Fail config:model, got %a"
                (Ox.Cause.pp pp_ptt_error) cause))
  | Error cause -> (
      match Ox.Cause.failures cause with
      | [ `Ptt_error "config:model" ] ->
          Alcotest.(check bool)
            "constructor failure is not interruption" false
            (Ox.Cause.is_interrupt_only cause)
      | _ ->
          Alcotest.failf "expected exact Fail config:model, got %a"
            (Ox.Cause.pp pp_ptt_error) cause));
  Alcotest.(check bool)
    "constructor failure releases reservation" false
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  let other =
    match
      Recording.Coordinator.try_start coordinator
        Recording.Coordinator.Ptt_transcribe ()
    with
    | Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected a later attempt after constructor failure"
  in
  Alcotest.(check bool)
    "later same-mode attempt remains admitted" true
    (Recording.Coordinator.snapshot coordinator).Recording.Coordinator.active;
  Alcotest.(check bool)
    "failed construction did not clear the later attempt" true
    (Recording.Coordinator.attempt_mode other
    = Recording.Coordinator.Ptt_transcribe);
  remove_dir dir

let even_hold_pcm = Bytes.of_string "captured-audio"

let fake_provider_handle () : Ptt_loop.handle =
  {
    offer = (fun _ -> Ok ());
    finish = Ptt_loop.Buffered (fun _ -> Ok ());
    cancel = (fun () -> Ok ());
  }

let assert_busy coordinator =
  match
    Recording.Coordinator.try_start coordinator
      Recording.Coordinator.Ptt_transcribe ()
  with
  | Recording.Coordinator.Busy _ -> ()
  | Recording.Coordinator.Disabled ->
      Alcotest.fail "expected Busy, got Disabled"
  | Recording.Coordinator.Started _ ->
      Alcotest.fail "admission released before outer scope finalizer"

let test_attempt_handle_finish_holds_until_outer_settle () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-hold-finish" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let attempt =
    match
      Recording.Coordinator.try_start coordinator
        Recording.Coordinator.Ptt_transcribe ()
    with
    | Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected the attempt to be admitted"
  in
  let owner =
    require_owner ~dir ~coordinator ~attempt Assistant.Service.Transcribe
  in
  let entered, resolve_entered = Eio.Promise.create () in
  let held, resolve_held = Eio.Promise.create () in
  let handle =
    Assistant.Service.attempt_handle owner (fake_provider_handle ())
  in
  let body () =
    let* () = handle.offer even_hold_pcm in
    let* () =
      match handle.finish with
      | Ptt_loop.Buffered f ->
          f { Ptt_loop.duration_ms = 1; pcm_bytes = even_hold_pcm }
      | Ptt_loop.Streaming _ -> Alcotest.fail "expected Buffered finish"
    in
    let* () = sync (fun () -> Eio.Promise.resolve resolve_entered ()) in
    sync (fun () -> Eio.Promise.await held)
  in
  let checker () =
    sync (fun () ->
        Eio.Promise.await entered;
        assert_busy coordinator;
        Eio.Promise.resolve resolve_held ())
  in
  let operation () =
    par ~sw
      (fun () ->
        Assistant.Service.run_acquired_attempt ~sw ~attempt
          ~on_settle:(settle_attempt owner) body)
      checker
  in
  (match run_boundary operation with
  | Ok _ -> ()
  | Error cause ->
      Alcotest.failf "outer finish hold failed: %a" (Ox.Cause.pp pp_ptt_error)
        cause);
  (match
     Recording.Coordinator.try_start coordinator
       Recording.Coordinator.Ptt_transcribe ()
   with
  | Recording.Coordinator.Started _ -> ()
  | _ -> Alcotest.fail "expected admission free after outer settle");
  Alcotest.(check bool)
    "finish abort does not publish" true
    (peek_none (require_store dir));
  remove_dir dir

let test_attempt_handle_cancel_holds_until_outer_settle () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = temp_dir "pie-hold-cancel" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let attempt =
    match
      Recording.Coordinator.try_start coordinator
        Recording.Coordinator.Ptt_transcribe ()
    with
    | Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected the attempt to be admitted"
  in
  let owner =
    require_owner ~dir ~coordinator ~attempt Assistant.Service.Transcribe
  in
  let entered, resolve_entered = Eio.Promise.create () in
  let held, resolve_held = Eio.Promise.create () in
  let handle =
    Assistant.Service.attempt_handle owner (fake_provider_handle ())
  in
  let body () =
    let* () = handle.offer even_hold_pcm in
    let* () = handle.cancel () in
    let* () = sync (fun () -> Eio.Promise.resolve resolve_entered ()) in
    sync (fun () -> Eio.Promise.await held)
  in
  let checker () =
    sync (fun () ->
        Eio.Promise.await entered;
        assert_busy coordinator;
        Eio.Promise.resolve resolve_held ())
  in
  let operation () =
    par ~sw
      (fun () ->
        Assistant.Service.run_acquired_attempt ~sw ~attempt
          ~on_settle:(settle_attempt owner) body)
      checker
  in
  (match run_boundary operation with
  | Ok _ -> ()
  | Error cause ->
      Alcotest.failf "outer cancel hold failed: %a" (Ox.Cause.pp pp_ptt_error)
        cause);
  (match
     Recording.Coordinator.try_start coordinator
       Recording.Coordinator.Ptt_transcribe ()
   with
  | Recording.Coordinator.Started _ -> ()
  | _ -> Alcotest.fail "expected admission free after outer settle");
  Alcotest.(check bool)
    "cancel abort does not publish" true
    (peek_none (require_store dir));
  remove_dir dir

let test_recording_coordinator_persist_error_swallowed () =
  let base = temp_dir "pie-coordinator-persist" in
  let path = Filename.concat base "recording.json" in
  write_file path "not a directory";
  let persist_path = Filename.concat path "nested.json" in
  (* create persists its initial state. When the target is unwritable (here the
     parent component is a regular file, not a directory), create must fail
     loudly instead of silently swallowing the error and handing back a
     coordinator whose state file was never written. *)
  match Recording.Coordinator.create ~caps ~persist_path () with
  | _coordinator ->
      Alcotest.fail "create should raise when it cannot persist initial state"
  | exception _ -> ()

let test_recording_coordinator_stop_clears_error () =
  run_test @@ fun _ ->
  let path = Filename.temp_file "pie-recording" ".json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path:path () in
  let attempt =
    match
      Recording.Coordinator.try_start coordinator
        Recording.Coordinator.Ptt_transcribe ()
    with
    | Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected start"
  in
  Recording.Coordinator.set_error coordinator "stop-clears-it";
  ignore (Recording.Coordinator.release coordinator attempt);
  let snap_stop = Recording.Coordinator.snapshot coordinator in
  Alcotest.(check (option string))
    "identity release preserves last_error" (Some "stop-clears-it")
    snap_stop.Recording.Coordinator.last_error;
  Sys.remove path

let test_daemon_server_mkdir_p_silently_accepts_file () =
  let base = temp_dir "pie-daemon-mkdir_p" in
  let file_path = Filename.concat base "not-a-dir" in
  write_file file_path "block";
  (* Bug: Daemon.Server.mkdir_p is a duplicate of the old buggy mkdir_p that
     checks Sys.file_exists without Sys.is_directory. When called on an
     existing regular file it silently returns () instead of raising EEXIST.
     Fs.mkdir_p ~caps was fixed but this copy was missed. *)
  try
    Pie.Assistant_service.Server.mkdir_p ~caps file_path;
    Alcotest.fail
      "mkdir_p should raise EEXIST for an existing file, not silently return"
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()
  | exn -> Alcotest.failf "unexpected exception: %s" (Printexc.to_string exn)

let test_pulse_parse_error_code_short_payload () =
  (* Bug: parse_error_code calls Protocol.Tag_struct_reader.get_u32 without
     checking whether the payload has at least 4 bytes. A malformed 1-byte
     error response from the PulseAudio server causes a Failure crash that
     terminates the read_loop and kills the entire connection. *)
  try
    let result = Pie.Pulse.Commands.parse_error_code (Bytes.make 1 '\x00') in
    Alcotest.(check (option int))
      "short error code payload should return None" None result
  with Failure msg ->
    Alcotest.fail ("parse_error_code crashed on short payload: " ^ msg)

let test_pulse_default_record_stream_options_mismatched_channel_map () =
  let default = Pie.Pulse.Defs.default_record_stream_options () in
  let overrides =
    {
      default with
      sample_spec =
        {
          format = Pie.Pulse.Defs.Pa_sample_format.s16le;
          channels = 2;
          rate = 16000;
        };
      channel_map = [];
    }
  in
  let options = Pie.Pulse.Defs.default_record_stream_options ~overrides () in
  (* Bug: default_record_stream_options uses the override's channel_map
     without validating that its length matches sample_spec.channels.
     PulseAudio requires one channel_map entry per channel. *)
  Alcotest.(check int)
    "channel map length must match channel count" options.sample_spec.channels
    (List.length options.channel_map)

let test_pulse_parse_command_envelope_short_payload () =
  (* parse_command_envelope reads two u32 tag-struct values (10 bytes). It is
     the very first function called for every incoming packet, so a malformed
     short payload must be handled gracefully (return None) rather than raising
     a Failure that tears down the whole read loop. *)
  Alcotest.(check bool)
    "short payload returns None instead of crashing" true
    (Pie.Pulse.Commands.parse_command_envelope (Bytes.make 3 '\x00') = None)

let test_daemon_contract_pause_rejects_non_object () =
  match
    Pie.Assistant_service.Contract.response_of_json
      Pie.Assistant_service.Contract.Pause (`String "ok")
  with
  | Error label -> Alcotest.(check string) "not-object" "not-object" label
  | Ok _ -> Alcotest.fail "Pause scalar must be a protocol error"

let test_stt_config_whitespace_app_id_not_trimmed () =
  run_test @@ fun env ->
  let dir = temp_dir "pie-stt-config-whitespace" in
  let config_path = Filename.concat dir "stt.toml" in
  write_file config_path
    {toml|

[[newline_rules]]
app_id = "   "
action = "ctrl-j"
|toml};
  (* A whitespace-only app_id is not a real matcher. get_optional_string trims
     it to None, leaving a rule with neither app_id nor title_regex. Such a
     matcher-less rule would match every window (a catch-all), so it must be
     rejected at load time rather than silently accepted. *)
  match
    Pie.Config.Stt.load ~caps ~fs:(Eio.Stdenv.fs env)
      ~source:(`Explicit config_path) ()
  with
  | Error _ -> remove_dir dir
  | Ok _ ->
      Alcotest.fail "whitespace-only app_id should be rejected, not loaded"

let json_member name = function
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some value -> value
      | None -> Alcotest.failf "missing JSON field: %s" name)
  | _ -> Alcotest.fail "expected JSON object"

let json_string name json =
  match json_member name json with
  | `String value -> value
  | _ -> Alcotest.failf "expected JSON string field: %s" name

let test_assistant_startup_config_json () =
  let config : Pie.Config.Stt.t =
    {
      provider = Pie.Config.Stt.Codex_realtime;
      transcription_model = "openrouter/google/gemini-3.1-flash-lite";
      translation_model = "openrouter/google/gemini-3.1-flash-lite";
      transcription_language = "Polish";
      translation_source_language = "Polish";
      translation_target_language = "English";
      ptt_release_grace_ms = 0;
      ptt_min_capture_rms = 0.001;
      ptt_min_capture_peak = 0.008;
      ptt_transcribe_keysym = 65516;
      ptt_translate_keysym = 65508;
      ptt_transcribe_shift = true;
      ptt_translate_shift = false;
      thinking_effort = Some "minimal";
      newline_rules =
        [
          {
            app_id = Some "Alacritty";
            title_regex = Some "^π";
            action = Pie.Input.Text_injection.Ctrl_j;
          };
        ];
      text_entry_rules =
        [
          {
            app_id = Some "slack";
            title_regex = None;
            method_ = Pie.Input.Text_injection.Paste;
          };
        ];
      prompt_rules = [];
      pipeline = Two_pass;
      openrouter_provider_order = None;
      one_pass_timeout_seconds = 30.;
      streaming = false;
      correction =
        Some
          {
            model = "google/gemini-3.8-flash";
            reasoning_effort = Some "low";
            timeout_seconds = 6.5;
            temperature = 0.2;
          };
      audio_retention_hours = 24.;
      bindings =
        [
          {
            keysym = 65516;
            shift = true;
            mode = Pie.Config.Stt.Transcribe;
            correct = true;
            prompt_named = false;
            prompt_file = "correction.md";
            prompt = "{{glossary}}";
          };
          {
            keysym = 65508;
            shift = false;
            mode = Pie.Config.Stt.Translate;
            correct = true;
            prompt_named = true;
            prompt_file = "custom.md";
            prompt = "{{glossary}} {{source_language}} {{target_language}}";
          };
        ];
    }
  in
  let report =
    Pie.Assistant.Service.startup_config_json ~caps ~config_path:"/tmp/stt.toml"
      ~config ~ptt_transcribe_keysym:65478 ~ptt_translate_keysym:65479
      ~min_duration_ms:120 ()
  in
  let json = Yojson.Safe.from_string report in
  Alcotest.(check string)
    "config path" "/tmp/stt.toml"
    (json_string "config_path" json);
  Alcotest.(check string)
    "provider" "codex-realtime"
    (json_string "provider" json);
  Alcotest.(check string)
    "thinking effort" "minimal"
    (json_string "thinking_effort" json);
  (match json with
  | `Assoc fields ->
      Alcotest.(check bool)
        "no ambient transcription_prompt in report" false
        (List.mem_assoc "transcription_prompt" fields)
  | _ -> Alcotest.fail "expected object");
  let transcription_model = json_member "transcription_model" json in
  Alcotest.(check string)
    "configured model" "openrouter/google/gemini-3.1-flash-lite"
    (json_string "configured" transcription_model);
  let resolved = json_member "resolved" transcription_model in
  Alcotest.(check string)
    "resolved provider" "openrouter"
    (json_string "provider" resolved);
  Alcotest.(check string)
    "resolved model" "google/gemini-3.1-flash-lite"
    (json_string "model" resolved);
  (* dict-oagf dict-rtdw: startup config JSON carries the pipeline. *)
  Alcotest.(check string) "pipeline" "two-pass" (json_string "pipeline" json);
  Alcotest.(check bool)
    "provider order absent" true
    (json_member "openrouter_provider_order" json = `Null);
  Alcotest.(check bool)
    "streaming" true
    (json_member "streaming" json = `Bool false);
  Alcotest.(check string)
    "correction model" "google/gemini-3.8-flash"
    (json_string "correction_model" json);
  Alcotest.(check string)
    "correction effort" "low"
    (json_string "correction_reasoning_effort" json);
  Alcotest.(check bool)
    "correction timeout" true
    (json_member "correction_timeout_seconds" json = `Float 6.5);
  (match json_member "bindings" json with
  | `List [ transcribe; translate ] ->
      Alcotest.(check string)
        "transcribe mode" "transcribe"
        (json_string "mode" transcribe);
      Alcotest.(check string)
        "transcribe prompt file" "correction.md"
        (json_string "prompt_file" transcribe);
      Alcotest.(check bool)
        "transcribe correct" true
        (json_member "correct" transcribe = `Bool true);
      Alcotest.(check string)
        "translate mode" "translate"
        (json_string "mode" translate);
      Alcotest.(check string)
        "translate prompt file" "custom.md"
        (json_string "prompt_file" translate)
  | _ -> Alcotest.fail "expected two bindings");
  (match json_member "newline_rules" json with
  | `List [ rule ] ->
      Alcotest.(check string)
        "newline app" "Alacritty"
        (json_string "app_id" rule);
      Alcotest.(check string)
        "newline action" "ctrl-j"
        (json_string "action" rule)
  | _ -> Alcotest.fail "expected one newline rule");
  match json_member "text_entry_rules" json with
  | `List [ rule ] ->
      Alcotest.(check string) "entry app" "slack" (json_string "app_id" rule);
      Alcotest.(check string) "entry method" "paste" (json_string "method" rule)
  | _ -> Alcotest.fail "expected one text entry rule"

let load_stt_config env config_path =
  match
    Pie.Config.Stt.load ~caps ~fs:(Eio.Stdenv.fs env)
      ~source:(`Explicit config_path) ()
  with
  | Ok config -> config
  | Error (`Config msg) -> Alcotest.fail msg

let test_stt_config_loads_toml_newline_rules () =
  run_test @@ fun env ->
  let dir = temp_dir "pie-stt-config" in
  let config_path = Filename.concat dir "stt.toml" in
  write_file config_path
    {toml|
provider = "openrouter"
transcription_model = "google/gemini-3.8-flash"
translation_model = "google/gemini-3.8-flash"
transcription_language = "Polish"
translation_source_language = "Polish"
translation_target_language = "English"
ptt_min_capture_rms = 0.001
ptt_min_capture_peak = 0.008
ptt_transcribe_keysym = 65516
ptt_translate_keysym = 65508
ptt_transcribe_shift = true
ptt_translate_shift = false
thinking_effort = "minimal"

[[newline_rules]]
app_id = "Alacritty"
title_regex = "(codex|π - csai)"
action = "ctrl-j"

[[newline_rules]]
app_id = "Slack"
action = "shift-enter"

[[text_entry_rules]]
app_id = "slack"
method = "paste"
|toml};
  let config = load_stt_config env config_path in
  Alcotest.(check (float 0.00001)) "ptt rms" 0.001 config.ptt_min_capture_rms;
  Alcotest.(check (float 0.00001)) "ptt peak" 0.008 config.ptt_min_capture_peak;
  Alcotest.(check int)
    "ptt transcribe keysym" 65516 config.ptt_transcribe_keysym;
  Alcotest.(check int) "ptt translate keysym" 65508 config.ptt_translate_keysym;
  Alcotest.(check bool) "ptt transcribe shift" true config.ptt_transcribe_shift;
  Alcotest.(check bool) "ptt translate shift" false config.ptt_translate_shift;
  Alcotest.(check (option string))
    "thinking effort" (Some "minimal") config.thinking_effort;
  Alcotest.(check int) "newline rules" 2 (List.length config.newline_rules);
  Alcotest.(check int)
    "text entry rules" 1
    (List.length config.text_entry_rules);
  (match config.text_entry_rules with
  | [ rule ] ->
      Alcotest.(check (option string)) "entry app" (Some "slack") rule.app_id;
      Alcotest.(check bool)
        "entry method" true
        (rule.method_ = Pie.Input.Text_injection.Paste)
  | _ -> Alcotest.fail "expected one text entry rule");
  match config.newline_rules with
  | [ first; second ] ->
      Alcotest.(check (option string))
        "first app" (Some "Alacritty") first.app_id;
      Alcotest.(check (option string))
        "first regex" (Some "(codex|π - csai)") first.title_regex;
      Alcotest.(check bool)
        "first action" true
        (first.action = Pie.Input.Text_injection.Ctrl_j);
      Alcotest.(check (option string)) "second app" (Some "Slack") second.app_id;
      Alcotest.(check bool)
        "second action" true
        (second.action = Pie.Input.Text_injection.Shift_enter)
  | _ -> Alcotest.fail "expected two newline rules"

let test_stt_config_defaults_to_one_pass () =
  run_test @@ fun env ->
  let dir = temp_dir "pie-stt-config-default-model" in
  let config_path = Filename.concat dir "stt.toml" in
  write_file config_path "schema_version = 2\n";
  let config = load_stt_config env config_path in
  Alcotest.(check bool)
    "default one-pass" true
    (config.pipeline = Pie.Config.Stt.One_pass);
  Alcotest.(check bool)
    "default OpenRouter" true
    (config.provider = Pie.Config.Stt.Openrouter)

let test_assistant_hot_reloads_keysyms_and_releases_original_hold () =
  let binding keysym shift : Pie.Assistant.Service.ptt_binding =
    { keysym; shift }
  in
  let bindings = ref (binding 65478 false, binding 65479 false) in
  let recognize =
    Pie.Assistant.Service.make_live_recognizer
      ~load_bindings:(fun () -> !bindings)
      ()
  in
  let event keysym released : Pie.Keyboard.Monitor.key_event =
    { released; state = 0; keysym; unichar = 0; keycode = 42 }
  in
  let check expected actual =
    Alcotest.(check bool) "recognized event" true (actual = expected)
  in
  check (Some (Pie.Ptt_loop.Press "transcribe")) (recognize (event 65478 false));
  bindings := (binding 65516 false, binding 65508 false);
  check (Some (Pie.Ptt_loop.Press "transcribe")) (recognize (event 65478 false));
  check (Some (Pie.Ptt_loop.Release "transcribe"))
    (recognize (event 65478 true));
  Alcotest.(check bool)
    "old binding no longer starts capture" true
    (recognize (event 65478 false) = None);
  check (Some (Pie.Ptt_loop.Press "translate")) (recognize (event 65508 false))

let test_assistant_distinguishes_shift_right_control () =
  let binding keysym shift : Pie.Assistant.Service.ptt_binding =
    { keysym; shift }
  in
  let recognize =
    Pie.Assistant.Service.make_live_recognizer
      ~load_bindings:(fun () -> (binding 65508 true, binding 65508 false))
      ()
  in
  let event state keycode released : Pie.Keyboard.Monitor.key_event =
    { released; state; keysym = 65508; unichar = 0; keycode }
  in
  Alcotest.(check bool)
    "right control alone translates" true
    (recognize (event 0 42 false) = Some (Pie.Ptt_loop.Press "translate"));
  ignore (recognize (event 0 42 true));
  Alcotest.(check bool)
    "shift plus right control transcribes" true
    (recognize (event 1 43 false) = Some (Pie.Ptt_loop.Press "transcribe"))

let test_assistant_mod5_chords_latch () =
  run_test @@ fun _ ->
  let coordinator = Pie.Recording.Coordinator.create ~caps () in
  let latch_keycode = ref None in
  let binding keysym shift : Pie.Assistant.Service.ptt_binding =
    { keysym; shift }
  in
  let recognize =
    Pie.Assistant.Service.make_live_recognizer ~coordinator ~latch_keycode
      ~load_bindings:(fun () -> (binding 65508 true, binding 65508 false))
      ()
  in
  let event ~state ~keysym ~keycode ~released : Pie.Keyboard.Monitor.key_event =
    { released; state; keysym; unichar = 0; keycode }
  in
  Alcotest.(check bool)
    "Mod5 plus Right Control starts translation latch" true
    (recognize (event ~state:128 ~keysym:65508 ~keycode:42 ~released:false)
    = Some (Pie.Ptt_loop.Press "translate"));
  Alcotest.(check bool)
    "releasing Right Control does not stop translation latch" true
    (recognize (event ~state:4 ~keysym:65508 ~keycode:42 ~released:true) = None);
  let translate_attempt =
    match
      Pie.Recording.Coordinator.try_start coordinator
        Pie.Recording.Coordinator.Ptt_translate ()
    with
    | Pie.Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected translate admission"
  in
  Alcotest.(check bool)
    "second Mod5 plus Right Control stops translation latch" true
    (recognize (event ~state:128 ~keysym:65508 ~keycode:42 ~released:false)
    = Some (Pie.Ptt_loop.Release "translate"));
  ignore (Pie.Recording.Coordinator.release coordinator translate_attempt);
  Alcotest.(check bool)
    "Mod5 plus Right Shift starts transcription latch" true
    (recognize (event ~state:129 ~keysym:65506 ~keycode:43 ~released:false)
    = Some (Pie.Ptt_loop.Press "transcribe"));
  Alcotest.(check bool)
    "releasing Right Shift does not stop transcription latch" true
    (recognize (event ~state:128 ~keysym:65506 ~keycode:43 ~released:true)
    = None)

let test_assistant_rejects_colliding_effective_keysyms () =
  run_test @@ fun env ->
  let dir = temp_dir "pie-stt-config-keysyms" in
  let config_path = Filename.concat dir "stt.toml" in
  write_file config_path
    "ptt_transcribe_keysym = 10\nptt_translate_keysym = 20\n";
  let config = load_stt_config env config_path in
  match
    Pie.Assistant.Service.effective_bindings ~ptt_transcribe_keysym:20 config
  with
  | _ -> Alcotest.fail "effective CLI and config keysyms must not collide"
  | exception Pie.Config.Stt.Config_error _ -> ()

let test_stt_config_rejects_bad_title_regex () =
  run_test @@ fun env ->
  let dir = temp_dir "pie-stt-config" in
  let config_path = Filename.concat dir "stt.toml" in
  write_file config_path
    {toml|

[[newline_rules]]
app_id = "Alacritty"
title_regex = "("
action = "ctrl-j"
|toml};
  match
    Pie.Config.Stt.load ~caps ~fs:(Eio.Stdenv.fs env)
      ~source:(`Explicit config_path) ()
  with
  | Ok _ -> Alcotest.fail "expected invalid regex to fail"
  | Error (`Config msg) ->
      Alcotest.(check bool)
        "mentions title_regex" true (String.contains msg 'r')

let test_stt_config_resolves_provider_qualified_model () =
  let open Pie.Config.Stt in
  let openrouter =
    resolve_model_provider ~default_provider:Codex_realtime
      "openrouter/xiaomi/mimo-v2.5"
  in
  Alcotest.(check bool)
    "openrouter provider" true
    (openrouter.provider = Openrouter);
  Alcotest.(check string) "openrouter model" "xiaomi/mimo-v2.5" openrouter.model;
  let openrouter_native_model =
    resolve_model_provider ~default_provider:Openrouter
      "openai/whisper-large-v3"
  in
  Alcotest.(check bool)
    "unknown prefix uses default provider" true
    (openrouter_native_model.provider = Openrouter);
  Alcotest.(check string)
    "unknown prefix remains part of model id" "openai/whisper-large-v3"
    openrouter_native_model.model;
  match
    resolve_model_provider ~default_provider:Codex_realtime "openrouter/"
  with
  | _ -> Alcotest.fail "expected empty provider-qualified model to fail"
  | exception Pie.Config.Stt.Config_error msg ->
      Alcotest.(check bool)
        "mentions empty model" true (String.contains msg 'e')

let test_openrouter_responses_request_uses_text_and_audio () =
  let wav = Bytes.of_string "RIFF\000\000\000\000WAVE" in
  let request : Ox_ai_openrouter.Responses.request =
    {
      model = "xiaomi/mimo-v2.5";
      prompt = "Use Pie context.";
      wav;
      temperature = Some 0.0;
      text_format = None;
      reasoning_effort = None;
    }
  in
  let encoded = Ox_ai_openrouter.Responses.encode request in
  Alcotest.(check string)
    "model id" "xiaomi/mimo-v2.5"
    (json_string "model" encoded);
  Alcotest.(check bool)
    "max tokens absent" true
    (match encoded with
    | `Assoc fields -> not (List.mem_assoc "max_output_tokens" fields)
    | _ -> false);
  match json_member "input" encoded with
  | `List [ `Assoc fields ] -> (
      match List.assoc_opt "content" fields with
      | Some (`List [ text; audio ]) ->
          Alcotest.(check string)
            "prompt" "Use Pie context." (json_string "text" text);
          let input_audio = json_member "input_audio" audio in
          Alcotest.(check string)
            "wav format" "wav"
            (json_string "format" input_audio);
          Alcotest.(check string) "wav riff" "RIFF" (Bytes.sub_string wav 0 4);
          Alcotest.(check string)
            "wav encoded intact"
            (Pie.Stt.Openrouter.base64_encode wav)
            (json_string "data" input_audio)
      | _ -> Alcotest.fail "expected one text and audio input")
  | _ -> Alcotest.fail "expected one user message with text and audio"

let test_text_injection_newline_rule_regex () =
  let window : Pie.Niri.window_info =
    {
      id = 384;
      title = Some "Ox | codex | 019e6df3-3186-7a51-ba29-34d742378792";
      app_id = Some "Alacritty";
      pid = None;
      workspace_id = Some 3;
    }
  in
  let rules : Pie.Input.Text_injection.newline_rule list =
    [
      {
        app_id = Some "Alacritty";
        title_regex = Some "(^π - csai$|codex)";
        action = Pie.Input.Text_injection.Ctrl_j;
      };
    ]
  in
  Alcotest.(check bool)
    "regex selects ctrl-j" true
    (Pie.Input.Text_injection.newline_action_for_window rules (Some window)
    = Pie.Input.Text_injection.Ctrl_j)

let test_text_injection_entry_method_rules () =
  let window : Pie.Niri.window_info =
    {
      id = 12;
      title = Some "community (Channel) - Retailic - Slack";
      app_id = Some "slack";
      pid = None;
      workspace_id = Some 2;
    }
  in
  let rules : Pie.Input.Text_injection.text_entry_rule list =
    [
      {
        app_id = Some "Slack";
        title_regex = Some "Draft$";
        method_ = Pie.Input.Text_injection.Paste;
      };
      {
        app_id = Some "Slack";
        title_regex = None;
        method_ = Pie.Input.Text_injection.Paste;
      };
    ]
  in
  Alcotest.(check bool)
    "case-insensitive app_id and first matching rule" true
    (Pie.Input.Text_injection.entry_method_for_window rules (Some window)
    = Pie.Input.Text_injection.Paste);
  Alcotest.(check string)
    "paste applies collapse newline rule" "first second"
    (Pie.Input.Text_injection.prepare_text
       ~method_:Pie.Input.Text_injection.Paste
       ~newline_action:Pie.Input.Text_injection.Collapse_to_space
       " first\nsecond ")

let test_wayland_clipboard_paste_ignores_previous previous_types () =
  let base = temp_dir "pie-clipboard" in
  let old_path = Option.value ~default:"" (Sys.getenv_opt "PATH") in
  let old_session = Sys.getenv_opt "XDG_SESSION_TYPE" in
  let old_wayland = Sys.getenv_opt "WAYLAND_DISPLAY" in
  let old_desktop = Sys.getenv_opt "XDG_CURRENT_DESKTOP" in
  let old_niri = Sys.getenv_opt "NIRI_SOCKET" in
  let shell =
    Option.value ~default:"/bin/sh" (Pie.Sh.find_executable ~caps "sh")
  in
  let script name body =
    let path = Filename.concat base name in
    write_file path ("#!" ^ shell ^ "\nset -eu\n" ^ body);
    Unix.chmod path 0o700
  in
  script "wl-paste"
    {script|
printf '%s' "$PIE_CLIPBOARD_PREVIOUS_TYPES"
touch "$PIE_CLIPBOARD_TEST_DIR/inspected"
|script};
  script "wl-copy"
    {script|
printf '%s\n' "$*" > "$PIE_CLIPBOARD_TEST_DIR/copy-args"
cat > "$PIE_CLIPBOARD_TEST_DIR/copy-stdin"
while :; do sleep 0.05; done
|script};
  script "wtype"
    {script|
printf '%s\n' "$*" > "$PIE_CLIPBOARD_TEST_DIR/wtype-args"
|script};
  script "niri"
    {script|
printf '%s\n' '{"id":1,"title":"t","app_id":"app"}'
|script};
  Unix.putenv "PATH" (base ^ ":" ^ old_path);
  Unix.putenv "PIE_CLIPBOARD_TEST_DIR" base;
  Unix.putenv "PIE_CLIPBOARD_PREVIOUS_TYPES" previous_types;
  Unix.putenv "XDG_SESSION_TYPE" "wayland";
  Unix.putenv "WAYLAND_DISPLAY" "pie-fake";
  Unix.putenv "XDG_CURRENT_DESKTOP" "niri";
  Unix.putenv "NIRI_SOCKET" (Filename.concat base "niri.sock");
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PIE_CLIPBOARD_TEST_DIR" "";
      Unix.putenv "PIE_CLIPBOARD_PREVIOUS_TYPES" "";
      Unix.putenv "XDG_SESSION_TYPE" (Option.value old_session ~default:"");
      Unix.putenv "WAYLAND_DISPLAY" (Option.value old_wayland ~default:"");
      Unix.putenv "XDG_CURRENT_DESKTOP" (Option.value old_desktop ~default:"");
      Unix.putenv "NIRI_SOCKET" (Option.value old_niri ~default:""))
    (fun () ->
      run_test @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let owner =
        Pie.Wayland.Clipboard.create ~caps ~sw
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ()
      in
      let paste_rules =
        [
          {
            Pie.Input.Text_injection.app_id = None;
            title_regex = None;
            method_ = Paste;
          };
        ]
      in
      let operation () =
        Pie.Input.Text_injection.inject ~caps ~clipboard_owner:owner
          ~target_window:
            (Some
               {
                 Pie.Niri.id = 1;
                 app_id = Some "app";
                 title = Some "t";
                 pid = None;
                 workspace_id = None;
               })
          ~clock:(Eio.Stdenv.clock env) ~text_entry_rules:paste_rules
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          "first\nsecond"
      in
      match run_boundary operation with
      | Ok result ->
          Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
          Alcotest.(check bool)
            "previous offer not inspected" false
            (Sys.file_exists (Filename.concat base "inspected"));
          Alcotest.(check string)
            "offered prepared transcript" "first second"
            (In_channel.with_open_bin
               (Filename.concat base "copy-stdin")
               In_channel.input_all);
          Alcotest.(check string)
            "foreground offer without a single MIME type" "--foreground\n"
            (In_channel.with_open_bin
               (Filename.concat base "copy-args")
               In_channel.input_all);
          Alcotest.(check string)
            "one ctrl-v" "-M ctrl -k v -m ctrl\n"
            (In_channel.with_open_bin
               (Filename.concat base "wtype-args")
               In_channel.input_all);
          Alcotest.(check bool)
            "paste key attempted" true
            (result.outcome = Pie.Input.Text_injection.Clipboard_key_attempted)
      | Error cause ->
          let pp_error ppf = function
            | `Clipboard_error message
            | `Wtype_error message
            | `Injection_error message ->
                Format.pp_print_string ppf message
            | _ -> Format.pp_print_string ppf "error"
          in
          Alcotest.failf "clipboard paste failed: %a" (Ox.Cause.pp pp_error)
            cause)

let test_wayland_clipboard_paste_reports_offer_failure () =
  let base = temp_dir "pie-clipboard-failure" in
  let old_path = Option.value ~default:"" (Sys.getenv_opt "PATH") in
  let old_session = Sys.getenv_opt "XDG_SESSION_TYPE" in
  let old_wayland = Sys.getenv_opt "WAYLAND_DISPLAY" in
  let old_desktop = Sys.getenv_opt "XDG_CURRENT_DESKTOP" in
  let old_niri = Sys.getenv_opt "NIRI_SOCKET" in
  let shell =
    Option.value ~default:"/bin/sh" (Pie.Sh.find_executable ~caps "sh")
  in
  let script name body =
    let path = Filename.concat base name in
    write_file path ("#!" ^ shell ^ "\nset -eu\n" ^ body);
    Unix.chmod path 0o700
  in
  script "wtype"
    {script|
printf '%s\n' "$*" > "$PIE_CLIPBOARD_TEST_DIR/wtype-args"
|script};
  script "niri"
    {script|
printf '%s\n' '{"id":1,"title":"t","app_id":"app"}'
|script};
  Unix.putenv "PATH" (base ^ ":" ^ old_path);
  Unix.putenv "PIE_CLIPBOARD_TEST_DIR" base;
  Unix.putenv "XDG_SESSION_TYPE" "wayland";
  Unix.putenv "WAYLAND_DISPLAY" "pie-fake";
  Unix.putenv "XDG_CURRENT_DESKTOP" "niri";
  Unix.putenv "NIRI_SOCKET" (Filename.concat base "niri.sock");
  Fun.protect
    ~finally:(fun () ->
      Unix.putenv "PATH" old_path;
      Unix.putenv "PIE_CLIPBOARD_TEST_DIR" "";
      Unix.putenv "XDG_SESSION_TYPE" (Option.value old_session ~default:"");
      Unix.putenv "WAYLAND_DISPLAY" (Option.value old_wayland ~default:"");
      Unix.putenv "XDG_CURRENT_DESKTOP" (Option.value old_desktop ~default:"");
      Unix.putenv "NIRI_SOCKET" (Option.value old_niri ~default:""))
    (fun () ->
      run_test @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      let owner =
        Pie.Wayland.Clipboard.create ~caps ~sw
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          ~copy_program:(Filename.concat base "missing-wl-copy")
          ()
      in
      let paste_rules =
        [
          {
            Pie.Input.Text_injection.app_id = None;
            title_regex = None;
            method_ = Paste;
          };
        ]
      in
      let operation () =
        Pie.Input.Text_injection.inject ~caps ~clipboard_owner:owner
          ~target_window:
            (Some
               {
                 Pie.Niri.id = 1;
                 app_id = Some "app";
                 title = Some "t";
                 pid = None;
                 workspace_id = None;
               })
          ~clock:(Eio.Stdenv.clock env) ~text_entry_rules:paste_rules
          ~process_mgr:(Eio.Stdenv.process_mgr env)
          "first\nsecond"
      in
      match run_boundary operation with
      | Ok _ -> Alcotest.fail "a failed offer was reported as a paste"
      | Error _ ->
          Alcotest.(check bool)
            "no key dispatch without a live offer" false
            (Sys.file_exists (Filename.concat base "wtype-args")))

let test_ptt_loop_continues_after_finish () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
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
    Pie.Keyboard.Monitor.emit keyboard
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
      Pie.Ptt_loop.offer = (fun _ -> Ok ());
      finish =
        Ptt_loop.Buffered
          (fun _ ->
            let* () = sync (fun () -> incr finishes) in
            if !finishes = 2 then Error (`Ptt_error "done") else Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
          if event.Pie.Keyboard.Monitor.keysym = 1 then
            Some
              (if event.released then Pie.Ptt_loop.Release "test"
               else Press "test")
          else None);
      min_duration_ms = 0;
      capture_thresholds =
        (fun () ->
          ( Pie.Audio.Pcm.ptt_min_capture_rms_for_stt,
            Pie.Audio.Pcm.ptt_min_capture_peak_for_stt ));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press =
        (fun _ ->
          sync (fun () -> incr starts)
          |> Result.map (fun () -> Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  let operation () =
    Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~keyboard
      ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
      config
  in
  let result = run_ptt_boundary ~clock:(Eio.Stdenv.clock env) operation in
  running := false;
  Alcotest.(check int) "starts" 3 !starts;
  Alcotest.(check int) "finishes" 3 !finishes;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected second finish to stop the test loop"

let test_ptt_loop_control_stream_triggers_capture () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let starts = ref 0 in
  let finishes = ref 0 in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep clock 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Press "test");
      Eio.Stream.add audio (Bytes.make 320 '\001');
      Eio.Time.sleep clock 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Release "test"));
  let handle =
    {
      Pie.Ptt_loop.offer = (fun _ -> Ok ());
      finish =
        Ptt_loop.Buffered
          (fun _ ->
            let* () = sync (fun () -> incr finishes) in
            Error (`Ptt_error "done"));
      cancel = (fun () -> Ok ());
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize = (fun _ -> None);
      min_duration_ms = 0;
      capture_thresholds =
        (fun () ->
          ( Pie.Audio.Pcm.ptt_min_capture_rms_for_stt,
            Pie.Audio.Pcm.ptt_min_capture_peak_for_stt ));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press =
        (fun _ ->
          sync (fun () -> incr starts)
          |> Result.map (fun () -> Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~control ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int) "starts" 1 !starts;
  Alcotest.(check int) "finishes" 1 !finishes;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected finish to stop the test loop"

let test_ptt_loop_post_roll_repress_continues_capture () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let starts = ref 0 in
  let finishes = ref 0 in
  let offered = ref [] in
  let key released =
    Pie.Keyboard.Monitor.emit keyboard
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
      Pie.Ptt_loop.offer =
        (fun chunk ->
          sync (fun () -> offered := Bytes.to_string chunk :: !offered));
      finish =
        Ptt_loop.Buffered
          (fun _ ->
            let* () = sync (fun () -> incr finishes) in
            Error (`Ptt_error "done"));
      cancel = (fun () -> Ok ());
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
          if event.Pie.Keyboard.Monitor.keysym = 1 then
            Some
              (if event.released then Pie.Ptt_loop.Release "test"
               else Press "test")
          else None);
      min_duration_ms = 0;
      capture_thresholds =
        (fun () ->
          ( Pie.Audio.Pcm.ptt_min_capture_rms_for_stt,
            Pie.Audio.Pcm.ptt_min_capture_peak_for_stt ));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press =
        (fun _ ->
          sync (fun () -> incr starts)
          |> Result.map (fun () -> Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int) "one handle" 1 !starts;
  Alcotest.(check int) "one finish" 1 !finishes;
  Alcotest.(check (list string)) "chunks" [ "a"; "b" ] (List.rev !offered);
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected finish to stop the test loop"

let test_ptt_loop_scoped_handle_lifetime () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let scope_entries = ref 0 in
  let scope_exits = ref 0 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Pie.Keyboard.Monitor.emit keyboard
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
      Pie.Ptt_loop.offer = (fun _ -> sync (fun () -> incr offers));
      finish = Ptt_loop.Buffered (fun _ -> sync (fun () -> incr finishes));
      cancel = (fun () -> Ok (incr cancels));
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
          if event.Pie.Keyboard.Monitor.keysym = 1 then
            Some
              (if event.released then Pie.Ptt_loop.Release "test"
               else Press "test")
          else None);
      min_duration_ms = 0;
      capture_thresholds =
        (fun () ->
          ( Pie.Audio.Pcm.ptt_min_capture_rms_for_stt,
            Pie.Audio.Pcm.ptt_min_capture_peak_for_stt ));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press =
        (fun _ ->
          Ok
            (Pie.Ptt_loop.Scoped
               (fun use ->
                 let* () = sync (fun () -> incr scope_entries) in
                 let* () = use handle in
                 sync (fun () -> incr scope_exits))));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  let operation () =
    Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~keyboard
      ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
      config
  in
  let result = run_ptt_boundary ~clock:(Eio.Stdenv.clock env) operation in
  Alcotest.(check int) "scope entered" 1 !scope_entries;
  Alcotest.(check int) "scope exited" 1 !scope_exits;
  Alcotest.(check int) "offers" 1 !offers;
  Alcotest.(check int) "finishes" 1 !finishes;
  Alcotest.(check int) "cancels" 0 !cancels;
  match result with
  | Error _ -> ()
  | Ok () ->
      Alcotest.fail "expected timeout after scoped capture returns to idle"

let test_ptt_loop_rejects_busy_press_and_accepts_fresh_cycle () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let started, resolve_started = Eio.Promise.create () in
  let offered, resolve_offered = Eio.Promise.create () in
  let finishing, resolve_finishing = Eio.Promise.create () in
  let resume, resolve_resume = Eio.Promise.create () in
  let first_finished, resolve_first_finished = Eio.Promise.create () in
  let offered_once = ref false in
  let calls = ref 0 in
  let finishes = ref 0 in
  let admitted = ref None in
  let dir = temp_dir "pie-ptt-busy-finish" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let chunk = Bytes.make 320 (Char.chr 1) in
  let release_admitted () =
    match !admitted with
    | None -> ()
    | Some attempt ->
        ignore (Recording.Coordinator.release coordinator attempt);
        admitted := None
  in
  let handle : Pie.Ptt_loop.handle =
    {
      offer =
        (fun _ ->
          sync (fun () ->
              if not !offered_once then (
                offered_once := true;
                Eio.Promise.resolve resolve_offered ())));
      finish =
        Ptt_loop.Buffered
          (fun _ ->
            sync (fun () ->
                incr finishes;
                if !finishes = 1 then (
                  Eio.Promise.resolve resolve_finishing ();
                  Eio.Promise.await resume;
                  Eio.Promise.resolve resolve_first_finished ());
                release_admitted ()));
      cancel = (fun () -> Ok (release_admitted ()));
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize = (fun _ -> None);
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press =
        (fun _ ->
          sync (fun () ->
              incr calls;
              match
                Recording.Coordinator.try_start coordinator
                  Recording.Coordinator.Ptt_transcribe ()
              with
              | Recording.Coordinator.Started attempt ->
                  admitted := Some attempt;
                  if !calls = 1 then Eio.Promise.resolve resolve_started ();
                  Pie.Ptt_loop.Handle handle
              | _ -> Pie.Ptt_loop.Skip));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> Recording.Coordinator.is_active coordinator);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.await started;
      Eio.Stream.add audio chunk;
      Eio.Promise.await offered;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe");
      Eio.Promise.await finishing;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe");
      Alcotest.(check int)
        "busy press is not admitted while finish is gated" 1 !calls;
      Eio.Promise.resolve resolve_resume ();
      Eio.Promise.await first_finished;
      Eio.Time.sleep clock 0.1;
      Alcotest.(check int)
        "busy cycle is not replayed after settlement" 1 !calls;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Stream.add audio chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe"));
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~control ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int) "fresh cycle after settlement is admitted" 2 !calls;
  Alcotest.(check int) "both admitted cycles finished" 2 !finishes;
  remove_dir dir;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let test_ptt_loop_scoped_setup_failure_allows_subsequent_press () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let second_started, resolve_second_started = Eio.Promise.create () in
  let calls = ref 0 in
  let finishes = ref 0 in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let chunk = Bytes.make 320 (Char.chr 1) in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Ok ());
      finish = Ptt_loop.Buffered (fun _ -> sync (fun () -> incr finishes));
      cancel = (fun () -> Ok ());
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize = (fun _ -> None);
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press =
        (fun _ ->
          sync (fun () ->
              incr calls;
              if !calls = 1 then
                Pie.Ptt_loop.Scoped
                  (fun _use -> Error (`Ptt_error "synthetic setup failure"))
              else (
                Eio.Promise.resolve resolve_second_started ();
                Pie.Ptt_loop.Handle handle)));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
  Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await second_started;
      Eio.Stream.add audio chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe"));
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~control ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int)
    "setup failure does not consume the subsequent press" 2 !calls;
  Alcotest.(check int) "subsequent press completed capture" 1 !finishes;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let test_ptt_loop_press_during_gated_on_press_is_not_replayed () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let entered, resolve_entered = Eio.Promise.create () in
  let admit, resolve_admit = Eio.Promise.create () in
  let offered, resolve_offered = Eio.Promise.create () in
  let finishing, resolve_finishing = Eio.Promise.create () in
  let resume, resolve_resume = Eio.Promise.create () in
  let first_finished, resolve_first_finished = Eio.Promise.create () in
  let offered_once = ref false in
  let calls = ref 0 in
  let finishes = ref 0 in
  let admitted = ref None in
  let dir = temp_dir "pie-gated-on-press" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let chunk = Bytes.make 320 (Char.chr 1) in
  let handle : Pie.Ptt_loop.handle =
    {
      offer =
        (fun _ ->
          sync (fun () ->
              if not !offered_once then (
                offered_once := true;
                Eio.Promise.resolve resolve_offered ())));
      finish =
        Ptt_loop.Buffered
          (fun _ ->
            sync (fun () ->
                incr finishes;
                if !finishes = 1 then (
                  Eio.Promise.resolve resolve_finishing ();
                  Eio.Promise.await resume;
                  Eio.Promise.resolve resolve_first_finished ())));
      cancel = (fun () -> Ok ());
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize = (fun _ -> None);
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press =
        (fun _ ->
          sync (fun () ->
              incr calls;
              match
                Recording.Coordinator.try_start coordinator
                  Recording.Coordinator.Ptt_transcribe ()
              with
              | Recording.Coordinator.Started attempt ->
                  admitted := Some attempt;
                  Eio.Promise.resolve resolve_entered ();
                  Eio.Promise.await admit;
                  Pie.Ptt_loop.Handle handle
              | _ -> Pie.Ptt_loop.Skip));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> Recording.Coordinator.is_active coordinator);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.await entered;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.resolve resolve_admit ();
      Eio.Stream.add audio chunk;
      Eio.Promise.await offered;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe");
      Eio.Promise.await finishing;
      (match !admitted with
      | Some attempt ->
          ignore (Recording.Coordinator.release coordinator attempt)
      | None -> ());
      Eio.Promise.resolve resolve_resume ();
      Eio.Promise.await first_finished;
      Eio.Time.sleep clock 0.1);
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~control ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int)
    "press during gated on_press is not a later attempt" 1 !calls;
  Alcotest.(check int) "one admitted capture finished" 1 !finishes;
  Array.iter
    (fun name ->
      try Sys.remove (Filename.concat dir name) with Sys_error _ -> ())
    (Sys.readdir dir);
  Unix.rmdir dir;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let emit_test_key keyboard released =
  Pie.Keyboard.Monitor.emit keyboard
    { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }

let recognize_test_key event =
  if event.Pie.Keyboard.Monitor.keysym = 1 then
    Some
      (if event.released then Pie.Ptt_loop.Release "test"
       else Pie.Ptt_loop.Press "test")
  else None

let test_ptt_loop_retry_admission_rejects_busy_control_cycle () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ready, resolve_ready = Eio.Promise.create () in
  let stamped, resolve_stamped = Eio.Promise.create () in
  let arm_stamp = Atomic.make false in
  let calls = ref 0 in
  let finishes = ref 0 in
  let dir = temp_dir "pie-ptt-retry-control" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let retry =
    match
      Recording.Coordinator.try_start coordinator
        Recording.Coordinator.Ptt_transcribe ()
    with
    | Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected retry to occupy admission"
  in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let chunk = Bytes.make 320 (Char.chr 1) in
  let config : Pie.Ptt_loop.config =
    {
      recognize = (fun _ -> None);
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready =
        (fun () -> sync (fun () -> Eio.Promise.resolve resolve_ready ()));
      on_press =
        (fun _ ->
          sync (fun () ->
              incr calls;
              match
                Recording.Coordinator.try_start coordinator
                  Recording.Coordinator.Ptt_transcribe ()
              with
              | Recording.Coordinator.Started attempt ->
                  Pie.Ptt_loop.Handle
                    {
                      offer = (fun _ -> Ok ());
                      finish =
                        Ptt_loop.Buffered
                          (fun _ ->
                            sync (fun () ->
                                incr finishes;
                                ignore
                                  (Recording.Coordinator.release coordinator
                                     attempt)));
                      cancel =
                        (fun () ->
                          sync (fun () ->
                              ignore
                                (Recording.Coordinator.release coordinator
                                   attempt)));
                    }
              | _ -> Pie.Ptt_loop.Skip));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active =
        (fun () ->
          let active = Recording.Coordinator.is_active coordinator in
          if Atomic.get arm_stamp && active then (
            Atomic.set arm_stamp false;
            Eio.Promise.resolve resolve_stamped ());
          active);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await ready;
      Atomic.set arm_stamp true;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.await stamped;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe");
      ignore (Recording.Coordinator.release coordinator retry);
      Alcotest.(check int)
        "retry-occupied cycle is not admitted after retry release" 0 !calls;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Stream.add audio chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe"));
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~control ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int) "fresh cycle after retry release is admitted" 1 !calls;
  Alcotest.(check int) "fresh cycle finished" 1 !finishes;
  remove_dir dir;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let test_ptt_loop_retry_admission_rejects_busy_keyboard_cycle () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ready, resolve_ready = Eio.Promise.create () in
  let stamped, resolve_stamped = Eio.Promise.create () in
  let arm_stamp = Atomic.make false in
  let calls = ref 0 in
  let finishes = ref 0 in
  let dir = temp_dir "pie-ptt-retry-keyboard" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let retry =
    match
      Recording.Coordinator.try_start coordinator
        Recording.Coordinator.Ptt_transcribe ()
    with
    | Recording.Coordinator.Started attempt -> attempt
    | _ -> Alcotest.fail "expected retry to occupy admission"
  in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let chunk = Bytes.make 320 (Char.chr 1) in
  let config : Pie.Ptt_loop.config =
    {
      recognize = recognize_test_key;
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready =
        (fun () -> sync (fun () -> Eio.Promise.resolve resolve_ready ()));
      on_press =
        (fun _ ->
          sync (fun () ->
              incr calls;
              match
                Recording.Coordinator.try_start coordinator
                  Recording.Coordinator.Ptt_transcribe ()
              with
              | Recording.Coordinator.Started attempt ->
                  Pie.Ptt_loop.Handle
                    {
                      offer = (fun _ -> Ok ());
                      finish =
                        Ptt_loop.Buffered
                          (fun _ ->
                            sync (fun () ->
                                incr finishes;
                                ignore
                                  (Recording.Coordinator.release coordinator
                                     attempt)));
                      cancel =
                        (fun () ->
                          sync (fun () ->
                              ignore
                                (Recording.Coordinator.release coordinator
                                   attempt)));
                    }
              | _ -> Pie.Ptt_loop.Skip));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active =
        (fun () ->
          let active = Recording.Coordinator.is_active coordinator in
          if Atomic.get arm_stamp && active then (
            Atomic.set arm_stamp false;
            Eio.Promise.resolve resolve_stamped ());
          active);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await ready;
      Atomic.set arm_stamp true;
      emit_test_key keyboard false;
      Eio.Promise.await stamped;
      emit_test_key keyboard true;
      ignore (Recording.Coordinator.release coordinator retry);
      Alcotest.(check int)
        "retry-occupied keyboard cycle is not admitted after retry release" 0
        !calls;
      emit_test_key keyboard false;
      Eio.Stream.add audio chunk;
      emit_test_key keyboard true);
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int)
    "fresh keyboard cycle after retry release is admitted" 1 !calls;
  Alcotest.(check int) "fresh keyboard cycle finished" 1 !finishes;
  remove_dir dir;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let test_ptt_loop_failed_on_press_does_not_replay_busy_control_cycle () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ready, resolve_ready = Eio.Promise.create () in
  let entered, resolve_entered = Eio.Promise.create () in
  let proceed, resolve_proceed = Eio.Promise.create () in
  let failed, resolve_failed = Eio.Promise.create () in
  let stamped, resolve_stamped = Eio.Promise.create () in
  let arm_stamp = Atomic.make false in
  let calls = ref 0 in
  let finishes = ref 0 in
  let dir = temp_dir "pie-ptt-fail-control" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let chunk = Bytes.make 320 (Char.chr 1) in
  let config : Pie.Ptt_loop.config =
    {
      recognize = (fun _ -> None);
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready =
        (fun () -> sync (fun () -> Eio.Promise.resolve resolve_ready ()));
      on_press =
        (fun _ ->
          incr calls;
          if !calls = 1 then
            let* disposition =
              sync (fun () ->
                  match
                    Recording.Coordinator.try_start coordinator
                      Recording.Coordinator.Ptt_transcribe ()
                  with
                  | Recording.Coordinator.Started attempt ->
                      Eio.Promise.resolve resolve_entered ();
                      Eio.Promise.await proceed;
                      ignore (Recording.Coordinator.release coordinator attempt);
                      Eio.Promise.resolve resolve_failed ();
                      `Fail
                  | _ -> `Skip)
            in
            match disposition with
            | `Skip -> Ok Pie.Ptt_loop.Skip
            | `Fail -> Error (`Ptt_error "setup failed before handle")
          else
            sync (fun () ->
                match
                  Recording.Coordinator.try_start coordinator
                    Recording.Coordinator.Ptt_transcribe ()
                with
                | Recording.Coordinator.Started attempt ->
                    Pie.Ptt_loop.Handle
                      {
                        offer = (fun _ -> Ok ());
                        finish =
                          Ptt_loop.Buffered
                            (fun _ ->
                              sync (fun () ->
                                  incr finishes;
                                  ignore
                                    (Recording.Coordinator.release coordinator
                                       attempt)));
                        cancel =
                          (fun () ->
                            sync (fun () ->
                                ignore
                                  (Recording.Coordinator.release coordinator
                                     attempt)));
                      }
                | _ -> Pie.Ptt_loop.Skip));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active =
        (fun () ->
          let active = Recording.Coordinator.is_active coordinator in
          if Atomic.get arm_stamp && active then (
            Atomic.set arm_stamp false;
            Eio.Promise.resolve resolve_stamped ());
          active);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await ready;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.await entered;
      Atomic.set arm_stamp true;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.await stamped;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe");
      Eio.Promise.resolve resolve_proceed ();
      Eio.Promise.await failed;
      Alcotest.(check int)
        "busy cycle during failed on_press is not replayed" 1 !calls;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Stream.add audio chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe"));
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~control ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int) "fresh cycle after failed on_press is admitted" 2 !calls;
  Alcotest.(check int) "fresh cycle finished" 1 !finishes;
  remove_dir dir;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let test_ptt_loop_failed_on_press_does_not_replay_busy_keyboard_cycle () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ready, resolve_ready = Eio.Promise.create () in
  let entered, resolve_entered = Eio.Promise.create () in
  let proceed, resolve_proceed = Eio.Promise.create () in
  let failed, resolve_failed = Eio.Promise.create () in
  let stamped, resolve_stamped = Eio.Promise.create () in
  let arm_stamp = Atomic.make false in
  let calls = ref 0 in
  let finishes = ref 0 in
  let dir = temp_dir "pie-ptt-fail-keyboard" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let chunk = Bytes.make 320 (Char.chr 1) in
  let config : Pie.Ptt_loop.config =
    {
      recognize = recognize_test_key;
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready =
        (fun () -> sync (fun () -> Eio.Promise.resolve resolve_ready ()));
      on_press =
        (fun _ ->
          incr calls;
          if !calls = 1 then
            let* disposition =
              sync (fun () ->
                  match
                    Recording.Coordinator.try_start coordinator
                      Recording.Coordinator.Ptt_transcribe ()
                  with
                  | Recording.Coordinator.Started attempt ->
                      Eio.Promise.resolve resolve_entered ();
                      Eio.Promise.await proceed;
                      ignore (Recording.Coordinator.release coordinator attempt);
                      Eio.Promise.resolve resolve_failed ();
                      `Fail
                  | _ -> `Skip)
            in
            match disposition with
            | `Skip -> Ok Pie.Ptt_loop.Skip
            | `Fail -> Error (`Ptt_error "setup failed before handle")
          else
            sync (fun () ->
                match
                  Recording.Coordinator.try_start coordinator
                    Recording.Coordinator.Ptt_transcribe ()
                with
                | Recording.Coordinator.Started attempt ->
                    Pie.Ptt_loop.Handle
                      {
                        offer = (fun _ -> Ok ());
                        finish =
                          Ptt_loop.Buffered
                            (fun _ ->
                              sync (fun () ->
                                  incr finishes;
                                  ignore
                                    (Recording.Coordinator.release coordinator
                                       attempt)));
                        cancel =
                          (fun () ->
                            sync (fun () ->
                                ignore
                                  (Recording.Coordinator.release coordinator
                                     attempt)));
                      }
                | _ -> Pie.Ptt_loop.Skip));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active =
        (fun () ->
          let active = Recording.Coordinator.is_active coordinator in
          if Atomic.get arm_stamp && active then (
            Atomic.set arm_stamp false;
            Eio.Promise.resolve resolve_stamped ());
          active);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await ready;
      emit_test_key keyboard false;
      Eio.Promise.await entered;
      Atomic.set arm_stamp true;
      emit_test_key keyboard false;
      Eio.Promise.await stamped;
      emit_test_key keyboard true;
      Eio.Promise.resolve resolve_proceed ();
      Eio.Promise.await failed;
      Alcotest.(check int)
        "busy keyboard cycle during failed on_press is not replayed" 1 !calls;
      emit_test_key keyboard false;
      Eio.Stream.add audio chunk;
      emit_test_key keyboard true);
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int)
    "fresh keyboard cycle after failed on_press is admitted" 2 !calls;
  Alcotest.(check int) "fresh keyboard cycle finished" 1 !finishes;
  remove_dir dir;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let ptt_flood_sentinel = Bytes.make 1 '\255'

let ptt_audio_with_flood_ingress_ack audio resolve_ingress =
  let signaled = Atomic.make false in
  let rec next () =
    let chunk = Eio.Stream.take audio in
    if Bytes.length chunk = 1 && Bytes.get chunk 0 = '\255' then (
      if Atomic.compare_and_set signaled false true then
        Eio.Promise.resolve resolve_ingress ();
      next ())
    else Ok (Some chunk)
  in
  next

let test_ptt_loop_processing_keeps_input_live_during_finish () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ready, resolve_ready = Eio.Promise.create () in
  let started, resolve_started = Eio.Promise.create () in
  let finishing, resolve_finishing = Eio.Promise.create () in
  let resume, resolve_resume = Eio.Promise.create () in
  let flood_ingressed, resolve_flood_ingressed = Eio.Promise.create () in
  let control_ingressed, resolve_control_ingressed = Eio.Promise.create () in
  let control_ack_phase = Atomic.make false in
  let control_ingress = Atomic.make 0 in
  let expected_busy_ingress = 32 in
  let calls = ref 0 in
  let finishes = ref 0 in
  let offered = ref [] in
  let dir = temp_dir "pie-ptt-processing-live" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 2048 in
  let capture_chunk = Bytes.make 320 (Char.chr 1) in
  let flood_chunk = Bytes.make 4 (Char.chr 2) in
  let fresh_chunk = Bytes.make 320 (Char.chr 3) in
  let config : Pie.Ptt_loop.config =
    {
      recognize = recognize_test_key;
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready =
        (fun () -> sync (fun () -> Eio.Promise.resolve resolve_ready ()));
      on_press =
        (fun _ ->
          sync (fun () ->
              incr calls;
              match
                Recording.Coordinator.try_start coordinator
                  Recording.Coordinator.Ptt_transcribe ()
              with
              | Recording.Coordinator.Started attempt ->
                  if !calls = 1 then Eio.Promise.resolve resolve_started ();
                  Pie.Ptt_loop.Handle
                    {
                      offer =
                        (fun chunk ->
                          sync (fun () ->
                              offered := Bytes.get chunk 0 :: !offered));
                      finish =
                        Ptt_loop.Buffered
                          (fun _ ->
                            sync (fun () ->
                                incr finishes;
                                if !finishes = 1 then (
                                  Eio.Promise.resolve resolve_finishing ();
                                  Eio.Promise.await resume);
                                ignore
                                  (Recording.Coordinator.release coordinator
                                     attempt)));
                      cancel =
                        (fun () ->
                          sync (fun () ->
                              ignore
                                (Recording.Coordinator.release coordinator
                                   attempt)));
                    }
              | _ -> Pie.Ptt_loop.Skip));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active =
        (fun () ->
          let active = Recording.Coordinator.is_active coordinator in
          (if Atomic.get control_ack_phase then
             let n = Atomic.fetch_and_add control_ingress 1 + 1 in
             if n = expected_busy_ingress && active then
               Eio.Promise.resolve resolve_control_ingressed ());
          active);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await ready;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.await started;
      Eio.Stream.add audio capture_chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe");
      Eio.Promise.await finishing;
      for _ = 1 to 1100 do
        Eio.Stream.add audio flood_chunk
      done;
      Eio.Stream.add audio ptt_flood_sentinel;
      Eio.Promise.await flood_ingressed;
      Atomic.set control_ack_phase true;
      for _ = 1 to 8 do
        Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
        emit_test_key keyboard false;
        Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe");
        emit_test_key keyboard true
      done;
      Eio.Promise.await control_ingressed;
      Atomic.set control_ack_phase false;
      Eio.Promise.resolve resolve_resume ();
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Stream.add audio fresh_chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe"));
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~control ~keyboard
          ~audio:
            (ptt_audio_with_flood_ingress_ack audio resolve_flood_ingressed)
          config)
  in
  Alcotest.(check int) "no extra on_press during gated finish" 2 !calls;
  Alcotest.(check int) "two finishes" 2 !finishes;
  Alcotest.(check bool)
    "flood audio was not offered" true
    (not (List.mem '\002' !offered));
  Alcotest.(check bool)
    "fresh capture offered only post-finish audio" true
    (List.mem '\003' !offered);
  remove_dir dir;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let test_ptt_loop_processing_preserves_fresh_press_pending () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ready, resolve_ready = Eio.Promise.create () in
  let started, resolve_started = Eio.Promise.create () in
  let finishing, resolve_finishing = Eio.Promise.create () in
  let resume, resolve_resume = Eio.Promise.create () in
  let stamped, resolve_stamped = Eio.Promise.create () in
  let arm_stamp = Atomic.make false in
  let calls = ref 0 in
  let finishes = ref 0 in
  let dir = temp_dir "pie-ptt-processing-pending" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let admitted = ref None in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let chunk = Bytes.make 320 (Char.chr 1) in
  let config : Pie.Ptt_loop.config =
    {
      recognize = (fun _ -> None);
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready =
        (fun () -> sync (fun () -> Eio.Promise.resolve resolve_ready ()));
      on_press =
        (fun _ ->
          sync (fun () ->
              incr calls;
              match
                Recording.Coordinator.try_start coordinator
                  Recording.Coordinator.Ptt_transcribe ()
              with
              | Recording.Coordinator.Started attempt ->
                  admitted := Some attempt;
                  if !calls = 1 then Eio.Promise.resolve resolve_started ();
                  Pie.Ptt_loop.Handle
                    {
                      offer = (fun _ -> Ok ());
                      finish =
                        Ptt_loop.Buffered
                          (fun _ ->
                            sync (fun () ->
                                incr finishes;
                                if !finishes = 1 then (
                                  Eio.Promise.resolve resolve_finishing ();
                                  Eio.Promise.await resume);
                                (match !admitted with
                                | Some current ->
                                    ignore
                                      (Recording.Coordinator.release coordinator
                                         current)
                                | None -> ());
                                admitted := None));
                      cancel =
                        (fun () ->
                          sync (fun () ->
                              match !admitted with
                              | Some current ->
                                  ignore
                                    (Recording.Coordinator.release coordinator
                                       current)
                              | None -> ()));
                    }
              | _ -> Pie.Ptt_loop.Skip));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active =
        (fun () ->
          let active = Recording.Coordinator.is_active coordinator in
          if Atomic.get arm_stamp && not active then (
            Atomic.set arm_stamp false;
            Eio.Promise.resolve resolve_stamped ());
          active);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await ready;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.await started;
      Eio.Stream.add audio chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe");
      Eio.Promise.await finishing;
      (match !admitted with
      | Some attempt ->
          ignore (Recording.Coordinator.release coordinator attempt);
          admitted := None
      | None -> ());
      Atomic.set arm_stamp true;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.await stamped;
      Eio.Promise.resolve resolve_resume ();
      Eio.Stream.add audio chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe"));
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~control ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check int)
    "fresh press during finish handoff is admitted once" 2 !calls;
  Alcotest.(check int) "handoff cycle finished" 2 !finishes;
  remove_dir dir;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let test_ptt_loop_processing_join_preserves_finish_failure () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let ready, resolve_ready = Eio.Promise.create () in
  let started, resolve_started = Eio.Promise.create () in
  let finishing, resolve_finishing = Eio.Promise.create () in
  let resume, resolve_resume = Eio.Promise.create () in
  let flood_ingressed, resolve_flood_ingressed = Eio.Promise.create () in
  let calls = ref 0 in
  let finishes = ref 0 in
  let offered = ref [] in
  let dir = temp_dir "pie-ptt-processing-fail" in
  let persist_path = Filename.concat dir "recording-state.json" in
  let coordinator = Recording.Coordinator.create ~caps ~persist_path () in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 2048 in
  let capture_chunk = Bytes.make 320 (Char.chr 1) in
  let flood_chunk = Bytes.make 4 (Char.chr 2) in
  let fresh_chunk = Bytes.make 320 (Char.chr 3) in
  let config : Pie.Ptt_loop.config =
    {
      recognize = (fun _ -> None);
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0., 0.));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready =
        (fun () -> sync (fun () -> Eio.Promise.resolve resolve_ready ()));
      on_press =
        (fun _ ->
          sync (fun () ->
              incr calls;
              match
                Recording.Coordinator.try_start coordinator
                  Recording.Coordinator.Ptt_transcribe ()
              with
              | Recording.Coordinator.Started attempt ->
                  if !calls = 1 then Eio.Promise.resolve resolve_started ();
                  Pie.Ptt_loop.Handle
                    {
                      offer =
                        (fun chunk ->
                          sync (fun () ->
                              offered := Bytes.get chunk 0 :: !offered));
                      finish =
                        Ptt_loop.Buffered
                          (fun _ ->
                            let* () =
                              sync (fun () ->
                                  incr finishes;
                                  if !finishes = 1 then (
                                    Eio.Promise.resolve resolve_finishing ();
                                    Eio.Promise.await resume);
                                  ignore
                                    (Recording.Coordinator.release coordinator
                                       attempt))
                            in
                            if !finishes = 1 then
                              Error
                                (`Ptt_error "provider failed during processing")
                            else Ok ());
                      cancel =
                        (fun () ->
                          sync (fun () ->
                              ignore
                                (Recording.Coordinator.release coordinator
                                   attempt)));
                    }
              | _ -> Pie.Ptt_loop.Skip));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> Recording.Coordinator.is_active coordinator);
    }
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await ready;
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Promise.await started;
      Eio.Stream.add audio capture_chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe");
      Eio.Promise.await finishing;
      for _ = 1 to 1100 do
        Eio.Stream.add audio flood_chunk
      done;
      Eio.Stream.add audio ptt_flood_sentinel;
      Eio.Promise.await flood_ingressed;
      Eio.Promise.resolve resolve_resume ();
      Eio.Stream.add control (Pie.Ptt_loop.Press "transcribe");
      Eio.Stream.add audio fresh_chunk;
      Eio.Stream.add control (Pie.Ptt_loop.Release "transcribe"));
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~control ~keyboard
          ~audio:
            (ptt_audio_with_flood_ingress_ack audio resolve_flood_ingressed)
          config)
  in
  Alcotest.(check int) "failed finish did not extra-admit" 2 !calls;
  Alcotest.(check bool)
    "failed processing did not offer flood audio" true
    (not (List.mem '\002' !offered));
  Alcotest.(check bool)
    "next capture did not receive old flood audio" true
    (List.mem '\003' !offered);
  remove_dir dir;
  match result with
  | Ok () -> Alcotest.fail "expected the loop to settle by timeout"
  | Error cause ->
      Alcotest.(check bool)
        "loop ended by local timeout" true
        (cause_contains cause "timeout")

let dump_exit = function
  | None -> "None"
  | Some (Ox.Exit.Success _) -> "Ok"
  | Some (Ox.Exit.Failure cause) ->
      Printf.sprintf "Error(interrupt_only=%b cause=%s)"
        (Ox.Cause.is_interrupt_only cause)
        (Format.asprintf "%a" (Ox.Cause.pp pp_ptt_error) cause)

let assert_with_background_clean_cancel ~label result body_exit bg_exit =
  let mismatch () =
    Alcotest.failf "%s markers body=%s bg=%s result=%s" label
      (dump_exit body_exit) (dump_exit bg_exit) (dump_exit (Some result))
  in
  (match body_exit with Some (Ox.Exit.Success _) -> () | _ -> mismatch ());
  (match result with
  | Ox.Exit.Success () -> ()
  | Ox.Exit.Failure _ -> mismatch ());
  match bg_exit with
  | Some (Ox.Exit.Failure cause) when Ox.Cause.is_interrupt_only cause -> ()
  | _ -> mismatch ()

let run_background_cancel ~bg_exit ~body_exit ~background ~body =
  let record marker action =
    let exit =
      Ox.Scope.run (fun _ ->
          action ();
          Ok ())
    in
    marker := Some exit;
    match exit with
    | Ox.Exit.Success () -> Ok ()
    | Ox.Exit.Failure _ -> Error (`Ptt_error "operation interrupted")
  in
  Ox.Scope.run (fun _ ->
      Eio.Fiber.first
        (fun () ->
          match record bg_exit background with
          | Ok () -> Error (`Ptt_error "background finished unexpectedly")
          | Error _ as error -> error)
        (fun () -> record body_exit body))

let test_with_background_cancels_sync_stream_take () =
  run_test @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let empty = Eio.Stream.create 1 in
  let bg_exit = ref None in
  let body_exit = ref None in
  let body_started, resolve_body_started = Eio.Promise.create () in
  let resume, resolve_resume = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await body_started;
      Eio.Promise.resolve resolve_resume ());
  let result =
    run_background_cancel ~bg_exit ~body_exit
      ~background:(fun () -> ignore (Eio.Stream.take empty))
      ~body:(fun () ->
        Eio.Promise.resolve resolve_body_started ();
        Eio.Promise.await resume)
  in
  assert_with_background_clean_cancel ~label:"sync-take" result !body_exit
    !bg_exit

let test_with_background_cancels_never () =
  run_test @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let bg_exit = ref None in
  let body_exit = ref None in
  let body_started, resolve_body_started = Eio.Promise.create () in
  let resume, resolve_resume = Eio.Promise.create () in
  let never, _resolve_never = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.await body_started;
      Eio.Promise.resolve resolve_resume ());
  let result =
    run_background_cancel ~bg_exit ~body_exit
      ~background:(fun () -> ignore (Eio.Promise.await never))
      ~body:(fun () ->
        Eio.Promise.resolve resolve_body_started ();
        Eio.Promise.await resume)
  in
  assert_with_background_clean_cancel ~label:"never" result !body_exit !bg_exit

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
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Pie.Keyboard.Monitor.emit keyboard
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
      Pie.Ptt_loop.offer = (fun _ -> sync (fun () -> incr offers));
      finish = Ptt_loop.Buffered (fun _ -> sync (fun () -> incr finishes));
      cancel = (fun () -> Ok (incr cancels));
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
          if event.Pie.Keyboard.Monitor.keysym = 1 then
            Some
              (if event.released then Pie.Ptt_loop.Release "test"
               else Press "test")
          else None);
      min_duration_ms = 1000;
      capture_thresholds =
        (fun () ->
          ( Pie.Audio.Pcm.ptt_min_capture_rms_for_stt,
            Pie.Audio.Pcm.ptt_min_capture_peak_for_stt ));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press = (fun _ -> Ok (Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  let operation () =
    Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~keyboard
      ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
      config
  in
  let result = run_ptt_boundary ~clock:(Eio.Stdenv.clock env) operation in
  Alcotest.(check int) "offers" 0 !offers;
  Alcotest.(check int) "finishes" 0 !finishes;
  Alcotest.(check int) "cancels" 1 !cancels;
  match result with
  | Error _ -> ()
  | Ok () ->
      Alcotest.fail "expected timeout after short capture returns to idle"

let test_ptt_loop_ignores_low_rms_capture_before_offering () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 64 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Pie.Keyboard.Monitor.emit keyboard
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
      Pie.Ptt_loop.offer = (fun _ -> sync (fun () -> incr offers));
      finish = Ptt_loop.Buffered (fun _ -> sync (fun () -> incr finishes));
      cancel = (fun () -> Ok (incr cancels));
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
          if event.Pie.Keyboard.Monitor.keysym = 1 then
            Some
              (if event.released then Pie.Ptt_loop.Release "test"
               else Press "test")
          else None);
      min_duration_ms = 300;
      capture_thresholds =
        (fun () ->
          ( Pie.Audio.Pcm.ptt_min_capture_rms_for_stt,
            Pie.Audio.Pcm.ptt_min_capture_peak_for_stt ));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press = (fun _ -> Ok (Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  let operation () =
    Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~keyboard
      ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
      config
  in
  let result = run_ptt_boundary ~clock:(Eio.Stdenv.clock env) operation in
  Alcotest.(check int) "offers" 0 !offers;
  Alcotest.(check int) "finishes" 0 !finishes;
  Alcotest.(check int) "cancels" 1 !cancels;
  match result with
  | Error _ -> ()
  | Ok () ->
      Alcotest.fail "expected timeout after low-rms capture returns to idle"

let test_ptt_loop_accepts_peak_over_silence_threshold () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 64 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Pie.Keyboard.Monitor.emit keyboard
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
      Pie.Ptt_loop.offer = (fun _ -> sync (fun () -> incr offers));
      finish =
        Ptt_loop.Buffered
          (fun _ ->
            let* () = sync (fun () -> incr finishes) in
            Error (`Ptt_error "done"));
      cancel = (fun () -> Ok (incr cancels));
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
          if event.Pie.Keyboard.Monitor.keysym = 1 then
            Some
              (if event.released then Pie.Ptt_loop.Release "test"
               else Press "test")
          else None);
      min_duration_ms = 300;
      capture_thresholds =
        (fun () ->
          ( Pie.Audio.Pcm.ptt_min_capture_rms_for_stt,
            Pie.Audio.Pcm.ptt_min_capture_peak_for_stt ));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press = (fun _ -> Ok (Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check bool) "offers" true (!offers > 0);
  Alcotest.(check int) "finishes" 1 !finishes;
  Alcotest.(check int) "cancels" 1 !cancels;
  match result with
  | Error _ -> ()
  | Ok () ->
      Alcotest.fail "expected timeout after finish failure returns to idle"

let test_ptt_loop_accepts_quiet_speech_over_rms_threshold () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 64 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let key released =
    Pie.Keyboard.Monitor.emit keyboard
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
      Pie.Ptt_loop.offer = (fun _ -> sync (fun () -> incr offers));
      finish =
        Ptt_loop.Buffered
          (fun _ ->
            let* () = sync (fun () -> incr finishes) in
            Error (`Ptt_error "done"));
      cancel = (fun () -> Ok (incr cancels));
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
          if event.Pie.Keyboard.Monitor.keysym = 1 then
            Some
              (if event.released then Pie.Ptt_loop.Release "test"
               else Press "test")
          else None);
      min_duration_ms = 300;
      capture_thresholds =
        (fun () ->
          ( Pie.Audio.Pcm.ptt_min_capture_rms_for_stt,
            Pie.Audio.Pcm.ptt_min_capture_peak_for_stt ));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press = (fun _ -> Ok (Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  let result =
    run_ptt_boundary ~clock:(Eio.Stdenv.clock env) (fun () ->
        Pie.Ptt_loop.run ~sw ~logger:(test_logger env)
          ~mono_clock:(Eio.Stdenv.mono_clock env)
          ~keyboard
          ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
          config)
  in
  Alcotest.(check bool) "offers" true (!offers > 0);
  Alcotest.(check int) "finishes" 1 !finishes;
  Alcotest.(check int) "cancels" 1 !cancels;
  match result with
  | Error _ -> ()
  | Ok () ->
      Alcotest.fail "expected timeout after finish failure returns to idle"

let test_ptt_loop_dead_input_settles_after_release ?(recover = false) () =
  run_test @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let offers = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let dead_inputs = ref 0 in
  let ignored = ref [] in
  let key released =
    Pie.Keyboard.Monitor.emit keyboard
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
      Alcotest.(check int) "no cancellation before release" 0 !cancels;
      Alcotest.(check int)
        "no ignored decision before release" 0 (List.length !ignored);
      if recover then (
        Eio.Stream.add audio (pcm_chunk 2048 160);
        Eio.Time.sleep clock 0.05);
      key true);
  let handle =
    {
      Pie.Ptt_loop.offer = (fun _ -> sync (fun () -> incr offers));
      finish =
        Ptt_loop.Streaming
          (fun ~duration_ms:_ -> sync (fun () -> incr finishes));
      cancel = (fun () -> Ok (incr cancels));
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize =
        (fun event ->
          if event.Pie.Keyboard.Monitor.keysym = 1 then
            Some
              (if event.released then Pie.Ptt_loop.Release "test"
               else Press "test")
          else None);
      min_duration_ms = 0;
      capture_thresholds =
        (fun () ->
          ( Pie.Audio.Pcm.ptt_min_capture_rms_for_stt,
            Pie.Audio.Pcm.ptt_min_capture_peak_for_stt ));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press = (fun _ -> Ok (Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> sync (fun () -> incr dead_inputs));
      admission_active = (fun () -> false);
    }
  in
  let operation () =
    Pie.Ptt_loop.run
      ~on_ignored_capture:(fun capture -> ignored := capture :: !ignored)
      ~sw ~logger:(test_logger env)
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~keyboard
      ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
      config
  in
  let result = run_ptt_boundary ~clock:(Eio.Stdenv.clock env) operation in
  Alcotest.(check int) "offers" (if recover then 5 else 0) !offers;
  Alcotest.(check int) "finishes" (if recover then 1 else 0) !finishes;
  Alcotest.(check int) "cancels" (if recover then 0 else 1) !cancels;
  Alcotest.(check int) "dead input callbacks" 1 !dead_inputs;
  Alcotest.(check int)
    "one ignored capture after release"
    (if recover then 0 else 1)
    (List.length !ignored);
  match result with
  | Error _ -> ()
  | Ok () ->
      Alcotest.fail
        "expected timeout after dead-input cancellation returns to idle"

let process_pp_err formatter = function
  | `Invalid_config message | `Stt_error message ->
      Format.pp_print_string formatter message
  | _ -> Format.pp_print_string formatter "error"

let run_process_door ?on_switch program =
  Ox_test.run (fun env _ ->
      let errors = Buffer.create 80 in
      let journal = Ox_observability.Logger.Memory.create () in
      let config : unit Ox_app.Config.t =
        {
          of_yojson = (fun _ -> Ok ());
          yojson_of_t = (fun () -> `Assoc []);
          env = (fun ~getenv:_ () -> ());
        }
      in
      let code =
        Ox_app.run_for_test_with_args ?on_switch
          ~service_args:(Cmdliner.Term.const ()) ~env
          ~journal:(Ox_observability.Logger.Memory.reporter journal)
          ~name:"pie" ~config ~pp_err:process_pp_err
          ~getenv:(fun _ -> None)
          ~read:(fun _ -> Ok "")
          ~stderr:(fun text -> Buffer.add_string errors text)
          ~stdout:(fun _ -> ())
          ~argv:[| "pie" |]
          (fun door_env _source () () sw -> program door_env sw)
      in
      ( code,
        Buffer.contents errors,
        Ox_observability.Logger.Memory.records journal
        |> List.map Ox_observability.Logger.to_string
        |> String.concat "\n" ))

let test_process_success_exit () =
  let code, _, _ = run_process_door (fun _ _ -> Ok ()) in
  Alcotest.(check int) "pieeta-94ds" 0 code

let test_process_typed_failure_exit () =
  let code, stderr, _ =
    run_process_door (fun _ _ -> Error (`Stt_error "typed boom"))
  in
  Alcotest.(check int) "pieeta-np5l" 1 code;
  Alcotest.(check bool)
    "renders typed failure" true
    (String.contains stderr 't'
    &&
    let needle = "typed boom" in
    let rec contains i =
      i + String.length needle <= String.length stderr
      && (String.sub stderr i (String.length needle) = needle
         || contains (i + 1))
    in
    contains 0)

let test_process_interrupt_exit () =
  let id = Ox.Interrupt_id.create () in
  let code, _, _ =
    run_process_door
      ~on_switch:(fun sw ->
        Eio.Fiber.fork_daemon ~sw (fun () ->
            Eio.Fiber.yield ();
            Ox_app.request_stop ~sw id;
            `Stop_daemon))
      (fun _ sw -> Ox_app.serve ~sw ())
  in
  Alcotest.(check int) "owned signal stop exits cleanly" 0 code

let test_process_logfmt_stderr () =
  let code, _, journal =
    run_process_door (fun env _ ->
        Ox_observability.Logger.info env.Ox_app.Env.logger "process door ready";
        Ok ())
  in
  Alcotest.(check int) "log emitted by successful Door" 0 code;
  Alcotest.(check bool)
    "journal contains process door ready" true
    (let needle = "process door ready" in
     let rec contains i =
       i + String.length needle <= String.length journal
       && (String.sub journal i (String.length needle) = needle
          || contains (i + 1))
     in
     contains 0)

let test_process_otel_disabled () =
  let code, _, _ = run_process_door (fun _ _ -> Ok ()) in
  Alcotest.(check int) "Door runs with no OTEL exporter" 0 code

let () =
  Alcotest.run "pie"
    [
      ( "process_door",
        [
          Alcotest.test_case "success exit 0" `Quick test_process_success_exit;
          Alcotest.test_case "typed failure exit 1" `Quick
            test_process_typed_failure_exit;
          Alcotest.test_case "requested stop exit 0" `Quick
            test_process_interrupt_exit;
          Alcotest.test_case "JSON journal records" `Quick
            test_process_logfmt_stderr;
          Alcotest.test_case "OTEL_SDK_DISABLED still runs" `Quick
            test_process_otel_disabled;
        ] );
      ( "pcm",
        [
          Alcotest.test_case "rms" `Quick test_pcm_rms;
          Alcotest.test_case "peak" `Quick test_pcm_peak;
          Alcotest.test_case "normalization" `Quick test_pcm_normalization;
          Alcotest.test_case "wav header" `Quick test_pcm_wav_header;
        ] );
      ( "pulse_protocol",
        [
          Alcotest.test_case "packet framing" `Quick test_pa_packet_framing;
          Alcotest.test_case "packet header channel roundtrip" `Quick
            test_pa_packet_header_channel_roundtrip;
          Alcotest.test_case "tag struct roundtrip" `Quick
            test_tag_struct_roundtrip;
          Alcotest.test_case "sample spec roundtrip" `Quick
            test_sample_spec_roundtrip;
        ] );
      ( "pulse_commands",
        [
          Alcotest.test_case "auth command" `Quick test_auth_command;
          Alcotest.test_case "server info command" `Quick
            test_server_info_command;
          Alcotest.test_case "parse error code short payload" `Quick
            test_pulse_parse_error_code_short_payload;
          Alcotest.test_case "parse command envelope short payload" `Quick
            test_pulse_parse_command_envelope_short_payload;
        ] );
      ( "pulse_defs",
        [
          Alcotest.test_case "default record stream mismatched channel map"
            `Quick
            test_pulse_default_record_stream_options_mismatched_channel_map;
        ] );
      ( "pulse_client",
        [
          Alcotest.test_case "set_client_name error is typed" `Quick
            test_pulse_client_set_client_name_error_is_typed;
          Alcotest.test_case "default request timeout" `Quick
            test_pulse_default_request_timeout;
          Alcotest.test_case "concurrent tagged replies" `Quick
            test_pulse_client_correlates_concurrent_replies;
          Alcotest.test_case "late reply after timeout" `Quick
            test_pulse_client_drops_late_reply_after_timeout;
          Alcotest.test_case "malformed command error" `Quick
            test_pulse_client_rejects_malformed_error_payload;
          Alcotest.test_case "reader failure fails pending command" `Quick
            test_pulse_reader_failure_fails_pending_command;
          Alcotest.test_case "scope end disconnects pending command" `Quick
            test_pulse_scope_end_disconnects_pending_command;
          Alcotest.test_case "record stream drains on failure" `Quick
            test_pulse_record_stream_drains_before_reader_failure;
          Alcotest.test_case "record stream preserves pcm chunk bytes" `Quick
            test_pulse_record_stream_preserves_pcm_chunk_bytes;
          Alcotest.test_case "record cleanup preserves body" `Quick
            test_pulse_record_cleanup_preserves_body_result;
        ] );
      ( "config",
        [
          Alcotest.test_case "provider codex-realtime" `Quick (fun () ->
              let p = Pie.Config.Stt.provider_of_string "codex-realtime" in
              Alcotest.(check bool)
                "is codex" true
                (p = Pie.Config.Stt.Codex_realtime));
          Alcotest.test_case "provider openrouter" `Quick (fun () ->
              let p = Pie.Config.Stt.provider_of_string "openrouter" in
              Alcotest.(check bool)
                "is openrouter" true
                (p = Pie.Config.Stt.Openrouter));
          Alcotest.test_case "provider-qualified model" `Quick
            test_stt_config_resolves_provider_qualified_model;
          Alcotest.test_case "load TOML newline rules" `Quick
            test_stt_config_loads_toml_newline_rules;
          Alcotest.test_case "default GPT Realtime 2.1 models" `Quick
            test_stt_config_defaults_to_one_pass;
          Alcotest.test_case "reject invalid title regex" `Quick
            test_stt_config_rejects_bad_title_regex;
          Alcotest.test_case "whitespace app_id not trimmed" `Quick
            test_stt_config_whitespace_app_id_not_trimmed;
        ] );
      ( "text_injection",
        [
          Alcotest.test_case "normalize" `Quick (fun () ->
              Alcotest.(check string)
                "trimmed" "hello"
                (Pie.Input.Text_injection.normalize_text "  hello  "));
          Alcotest.test_case "collapse line breaks" `Quick (fun () ->
              Alcotest.(check string)
                "collapsed" "first second third"
                (Pie.Input.Text_injection.collapse_line_breaks
                   " first\nsecond\r\n third "));
          Alcotest.test_case "regex newline rule" `Quick
            test_text_injection_newline_rule_regex;
          Alcotest.test_case "per-app entry method" `Quick
            test_text_injection_entry_method_rules;
          Alcotest.test_case "paste ignores image-only previous offer" `Quick
            (test_wayland_clipboard_paste_ignores_previous
               "image/png\nimage/webp\n");
          Alcotest.test_case "paste ignores text previous offer" `Quick
            (test_wayland_clipboard_paste_ignores_previous
               "text/plain;charset=utf-8\n");
          Alcotest.test_case "paste ignores empty previous offer" `Quick
            (test_wayland_clipboard_paste_ignores_previous "");
          Alcotest.test_case "paste ignores arbitrary previous offer" `Quick
            (test_wayland_clipboard_paste_ignores_previous
               "application/octet-stream\n");
          Alcotest.test_case "paste reports a failed offer" `Quick
            test_wayland_clipboard_paste_reports_offer_failure;
        ] );
      ( "daemon_contract",
        [
          Alcotest.test_case "snapshot parsing" `Quick (fun () ->
              let json =
                `Assoc
                  [
                    ("active", `Bool true);
                    ("mode", `String "ptt-transcribe");
                    ("enabled", `Bool true);
                  ]
              in
              match Pie.Assistant_service.Contract.snapshot_of_json json with
              | Ok snap ->
                  Alcotest.(check bool) "active" true snap.active;
                  Alcotest.(check string) "mode" "ptt-transcribe" snap.mode;
                  Alcotest.(check bool) "enabled" true snap.enabled
              | Error label -> Alcotest.failf "snapshot protocol error %s" label);
          Alcotest.test_case "request parsing" `Quick
            test_daemon_contract_request;
          Alcotest.test_case "action request parsing" `Quick
            test_daemon_contract_action_request;
          Alcotest.test_case "action response parsing" `Quick
            test_daemon_contract_action_response;
          Alcotest.test_case "response roundtrip" `Quick
            test_daemon_contract_response;
          Alcotest.test_case "unknown meeting method rejected" `Quick
            test_daemon_contract_unknown_method;
          Alcotest.test_case "status error swallowed" `Quick
            test_daemon_contract_status_error_swallowed;
          Alcotest.test_case "pause error swallowed" `Quick
            test_daemon_contract_pause_error_swallowed;
          Alcotest.test_case "pause scalar rejected" `Quick
            test_daemon_contract_pause_rejects_non_object;
        ] );
      ( "daemon_server",
        [
          Alcotest.test_case "socket state" `Quick test_daemon_server_state;
          Alcotest.test_case "loose runtime dir tightened" `Quick
            test_daemon_server_tightens_runtime_dir;
          Alcotest.test_case "action handler" `Quick
            test_daemon_server_action_handler;
          Alcotest.test_case "unknown meeting method no admission" `Quick
            test_daemon_server_unknown_meeting_method;
          Alcotest.test_case "mkdir_p silent on existing file" `Quick
            test_daemon_server_mkdir_p_silently_accepts_file;
        ] );
      ( "keyboard",
        [
          Alcotest.test_case "parse D-Bus KeyEvent" `Quick
            test_keyboard_dbus_key_event_parse;
          Alcotest.test_case "session bus path rejects abstract" `Quick
            test_keyboard_monitor_session_bus_path_abstract_socket;
          Alcotest.test_case "keysym_of_name rejects unknown keys" `Quick
            (fun () ->
              let keysym = Pie.Keyboard.Monitor.keysym_of_name "F11" in
              (* Bug: keysym_of_name returns 0 for unknown keys, but 0 is a valid
              keysym (XKB_KEY_NoSymbol). Callers cannot distinguish an unknown
              key from a deliberately-configured NoSymbol. The function should
              raise Invalid_argument or return an option. *)
              Alcotest.(check bool)
                "unknown key name should not silently return 0" false
                (keysym = 0));
        ] );
      ( "retry_store",
        [
          Alcotest.test_case "retains failed recording until success" `Quick
            test_retry_store_retains_and_discards_recordings;
          Alcotest.test_case "processing failure retains captured audio" `Quick
            test_failed_processing_retains_captured_audio;
          Alcotest.test_case "setup failure releases admission" `Quick
            test_setup_failure_releases_admission_and_preserves_error;
          Alcotest.test_case "settle_attempt duties run only on execution"
            `Quick test_settle_attempt_construction_does_not_run_duties;
          Alcotest.test_case "buffered clip cancellation settles after offer"
            `Quick test_buffered_clip_cancellation_settles_after_offer;
          Alcotest.test_case "buffered clip defect settles after offer" `Quick
            test_buffered_clip_defect_settles_after_offer;
          Alcotest.test_case "on_press construction does not admit" `Quick
            test_on_press_construction_does_not_admit;
          Alcotest.test_case "on_press execution admits" `Quick
            test_on_press_execution_admits;
          Alcotest.test_case "on_press busy and disabled at execution" `Quick
            test_on_press_busy_and_disabled_at_execution;
          Alcotest.test_case
            "on_press constructor failure releases without clearing other"
            `Quick
            test_on_press_constructor_failure_releases_without_clearing_other_attempt;
          Alcotest.test_case
            "attempt_handle finish holds admission until outer settle" `Quick
            test_attempt_handle_finish_holds_until_outer_settle;
          Alcotest.test_case
            "attempt_handle cancel holds admission until outer settle" `Quick
            test_attempt_handle_cancel_holds_until_outer_settle;
        ] );
      ( "recording_coordinator",
        [
          Alcotest.test_case "transitions" `Quick
            test_recording_coordinator_transitions;
          Alcotest.test_case "stale release preserves newer same-mode attempt"
            `Quick
            test_recording_coordinator_release_preserves_newer_same_mode_attempt;
          Alcotest.test_case "persist error swallowed" `Quick
            test_recording_coordinator_persist_error_swallowed;
          Alcotest.test_case "stop clears error inconsistently" `Quick
            test_recording_coordinator_stop_clears_error;
        ] );
      ( "stt_codex_realtime",
        [
          Alcotest.test_case "url builders" `Quick
            test_codex_realtime_url_builders;
          Alcotest.test_case "headers" `Quick test_codex_realtime_headers;
          Alcotest.test_case "receive stop modes" `Quick
            test_codex_realtime_receive_stop_modes;
          Alcotest.test_case "transcription session update" `Quick
            test_codex_realtime_transcription_session_update;
          Alcotest.test_case "event parser" `Quick
            test_codex_realtime_event_parser;
          Alcotest.test_case "first token timer" `Quick
            test_codex_realtime_first_token_timer;
          Alcotest.test_case "resample" `Quick test_codex_realtime_resample;
          Alcotest.test_case "speech operation deadline policy" `Quick
            test_speech_operation_deadline_policy;
        ] );
      ( "assistant_prompt",
        [
          Alcotest.test_case "startup config report" `Quick
            test_assistant_startup_config_json;
          Alcotest.test_case "hot reloads PTT keysyms" `Quick
            test_assistant_hot_reloads_keysyms_and_releases_original_hold;
          Alcotest.test_case "distinguishes Shift plus Right Control" `Quick
            test_assistant_distinguishes_shift_right_control;
          Alcotest.test_case "Mod5 chords latch" `Quick
            test_assistant_mod5_chords_latch;
          Alcotest.test_case "rejects colliding effective PTT keysyms" `Quick
            test_assistant_rejects_colliding_effective_keysyms;
        ] );
      ( "ptt_capture",
        [
          Alcotest.test_case "state machine" `Quick (fun () ->
              let s0 = Pie.Ptt.Capture.idle in
              Alcotest.(check bool)
                "idle not accepting" false
                (Pie.Ptt.Capture.is_accepting_chunks s0);
              let s1 = Pie.Ptt.Capture.start s0 0.0 in
              Alcotest.(check bool)
                "capturing accepting" true
                (Pie.Ptt.Capture.is_accepting_chunks s1);
              let s2 = Pie.Ptt.Capture.release s1 1000.0 in
              Alcotest.(check bool)
                "postroll accepting" true
                (Pie.Ptt.Capture.is_accepting_chunks s2);
              Alcotest.(check bool)
                "postroll remaining > 0" true
                (Pie.Ptt.Capture.post_roll_remaining_ms s2 1000.0 > 0.0);
              Alcotest.(check bool)
                "postroll expired" true
                (Pie.Ptt.Capture.post_roll_remaining_ms s2 4000.0 = 0.0));
          Alcotest.test_case "with_background cancels sync stream take" `Quick
            test_with_background_cancels_sync_stream_take;
          Alcotest.test_case "with_background cancels never" `Quick
            test_with_background_cancels_never;
          Alcotest.test_case "loop continues after finish" `Quick
            test_ptt_loop_continues_after_finish;
          Alcotest.test_case "control stream triggers capture" `Quick
            test_ptt_loop_control_stream_triggers_capture;
          Alcotest.test_case "post-roll re-press continues capture" `Quick
            test_ptt_loop_post_roll_repress_continues_capture;
          Alcotest.test_case "scoped handle lifetime" `Quick
            test_ptt_loop_scoped_handle_lifetime;
          Alcotest.test_case "busy finish cycle is not replayed" `Quick
            test_ptt_loop_rejects_busy_press_and_accepts_fresh_cycle;
          Alcotest.test_case "scoped setup failure allows a later press" `Quick
            test_ptt_loop_scoped_setup_failure_allows_subsequent_press;
          Alcotest.test_case "press during gated on_press is not replayed"
            `Quick test_ptt_loop_press_during_gated_on_press_is_not_replayed;
          Alcotest.test_case "retry admission rejects busy control cycle" `Quick
            test_ptt_loop_retry_admission_rejects_busy_control_cycle;
          Alcotest.test_case "retry admission rejects busy keyboard cycle"
            `Quick test_ptt_loop_retry_admission_rejects_busy_keyboard_cycle;
          Alcotest.test_case "failed on_press does not replay busy control"
            `Quick
            test_ptt_loop_failed_on_press_does_not_replay_busy_control_cycle;
          Alcotest.test_case "failed on_press does not replay busy keyboard"
            `Quick
            test_ptt_loop_failed_on_press_does_not_replay_busy_keyboard_cycle;
          Alcotest.test_case "processing keeps input live during finish" `Quick
            test_ptt_loop_processing_keeps_input_live_during_finish;
          Alcotest.test_case "processing preserves fresh press pending" `Quick
            test_ptt_loop_processing_preserves_fresh_press_pending;
          Alcotest.test_case "processing join preserves finish failure" `Quick
            test_ptt_loop_processing_join_preserves_finish_failure;
          Alcotest.test_case "short capture ignored before offer" `Quick
            test_ptt_loop_ignores_short_capture_before_offering;
          Alcotest.test_case "low-rms capture ignored before offer" `Quick
            test_ptt_loop_ignores_low_rms_capture_before_offering;
          Alcotest.test_case "peak over silence threshold accepted" `Quick
            test_ptt_loop_accepts_peak_over_silence_threshold;
          Alcotest.test_case "quiet speech over RMS threshold accepted" `Quick
            test_ptt_loop_accepts_quiet_speech_over_rms_threshold;
        ] );
      ( "ptt_dead_input",
        [
          Alcotest.test_case "flatline detection" `Quick (fun () ->
              let det = Pie.Ptt.Dead_input.initial () in
              let det = Pie.Ptt.Dead_input.sync det true in
              let chunk = Bytes.make 16 '\x00' in
              let rec loop det n =
                if n = 0 then det
                else
                  let r = Pie.Ptt.Dead_input.process_chunk det chunk in
                  loop r.detector (n - 1)
              in
              let det = loop det 3 in
              let r = Pie.Ptt.Dead_input.process_chunk det chunk in
              Alcotest.(check bool) "warn after 4 zeros" true r.warn;
              Alcotest.(check bool) "dead after 4 zeros" true r.dead);
          Alcotest.test_case "dead input settles after release" `Quick
            (fun () -> test_ptt_loop_dead_input_settles_after_release ());
          Alcotest.test_case "speech after flatline is offered and accepted"
            `Quick (fun () ->
              test_ptt_loop_dead_input_settles_after_release ~recover:true ());
        ] );
      ( "stt_openrouter",
        [
          Alcotest.test_case "responses request uses text and audio" `Quick
            test_openrouter_responses_request_uses_text_and_audio;
        ] );
      ( "integration",
        [
          Alcotest.test_case "wav header roundtrip" `Quick (fun () ->
              let pcm = Bytes.create 32000 in
              for i = 0 to 15999 do
                let sample = i * 2 mod 65536 in
                Bytes.set_uint16_le pcm (i * 2) sample
              done;
              let header =
                Pie.Audio.Pcm.build_pcm_wav_header ~data_length:32000
                  ~sample_rate:16000
              in
              Alcotest.(check int) "header size" 44 (Bytes.length header);
              Alcotest.(check string)
                "RIFF" "RIFF"
                (Bytes.sub_string header 0 4);
              Alcotest.(check string)
                "WAVE" "WAVE"
                (Bytes.sub_string header 8 4);
              Alcotest.(check int32)
                "data len" (Int32.of_int 32000)
                (Bytes.get_int32_le header 40));
          Alcotest.test_case "normalization then silence gate" `Quick (fun () ->
              (* quiet signal — should be normalized *)
              let quiet = Bytes.create 100 in
              Bytes.fill quiet 0 100 '\x01';
              let result = Pie.Audio.Pcm.normalize_pcm_for_stt quiet in
              Alcotest.(check bool)
                "quiet signal gets gain" true (result.gain > 1.0);
              (* silence — should not be normalized *)
              let silent = Bytes.make 100 '\x00' in
              let result2 = Pie.Audio.Pcm.normalize_pcm_for_stt silent in
              Alcotest.(check bool)
                "silent signal no gain" true (result2.gain = 1.0));
        ] );
      ( "wayland_portal",
        [
          Alcotest.test_case "parse object path" `Quick (fun () ->
              let input =
                "o \"/org/freedesktop/portal/desktop/request/1_123/pie_456\""
              in
              match String.split_on_char '"' input with
              | _ :: path :: _ ->
                  Alcotest.(check string)
                    "path"
                    "/org/freedesktop/portal/desktop/request/1_123/pie_456" path
              | _ -> Alcotest.fail "unexpected split");
        ] );
      ( "fs",
        [
          Alcotest.test_case "mkdir_p creates nested directories" `Quick
            (fun () ->
              let base = temp_dir "pie-fs-mkdir_p" in
              let nested = Filename.concat base "a/b/c" in
              try
                Pie.Fs.mkdir_p ~caps nested;
                Alcotest.(check bool)
                  "mkdir_p should create nested directories" true
                  (Sys.file_exists nested && Sys.is_directory nested);
                (* cleanup *)
                (try Unix.rmdir nested with _ -> ());
                (try Unix.rmdir (Filename.concat base "a/b") with _ -> ());
                (try Unix.rmdir (Filename.concat base "a") with _ -> ());
                try Unix.rmdir base with _ -> ()
              with
              | Unix.Unix_error (Unix.ENOENT, _, _) ->
                  (* Bug: Fs.mkdir_p ~caps only calls Unix.mkdir once, which fails when
                parent directories do not exist. It should recursively create
                parents like the mkdir_p in Recording.Coordinator and
                Daemon.Server do. *)
                  Alcotest.fail
                    "mkdir_p raised ENOENT for nested path (bug: missing \
                     parent creation)"
              | exn ->
                  Alcotest.failf "unexpected exception: %s"
                    (Printexc.to_string exn));
          Alcotest.test_case "mkdir_p should fail when path is an existing file"
            `Quick (fun () ->
              let base = temp_dir "pie-fs-mkdir_p-file" in
              let path = Filename.concat base "existing-file" in
              write_file path "content";
              try
                Pie.Fs.mkdir_p ~caps path;
                (* Bug: Fs.mkdir_p ~caps catches Unix.EEXIST and returns () even when the
                path is a regular file, not a directory. The caller then assumes
                the path is a directory and will fail later when trying to create
                files inside it. mkdir_p should verify the path is a directory. *)
                Alcotest.fail
                  "mkdir_p should not silently succeed when path is an \
                   existing file"
              with
              | Unix.Unix_error (Unix.EEXIST, _, _) ->
                  (* This is the expected behavior: raise EEXIST when the path exists
                but is not a directory. *)
                  ()
              | exn ->
                  Alcotest.failf "unexpected exception: %s"
                    (Printexc.to_string exn));
        ] );
      ( "sh",
        [
          Alcotest.test_case "find_executable rejects directories" `Quick
            (fun () ->
              let base = temp_dir "pie-sh-find" in
              let fake_dir = Filename.concat base "not-an-executable" in
              Unix.mkdir fake_dir 0o700;
              let old_path = Option.value ~default:"" (Sys.getenv_opt "PATH") in
              Unix.putenv "PATH" base;
              try
                let result = Pie.Sh.find_executable ~caps "not-an-executable" in
                Unix.putenv "PATH" old_path;
                (* Bug: find_executable uses Sys.file_exists which returns true for
                directories. It should check Sys.is_directory = false (and ideally
                Unix.access path [X_OK]) before returning a match. *)
                Alcotest.(check (option string))
                  "directory should not match as executable" None result;
                Unix.rmdir fake_dir;
                Unix.rmdir base
              with exn ->
                Unix.putenv "PATH" old_path;
                Unix.rmdir fake_dir;
                Unix.rmdir base;
                raise exn);
        ] );
    ]
