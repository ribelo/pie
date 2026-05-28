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
    "wss://api.openai.com/v1/realtime?model=gpt-realtime-whisper"
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

let test_assistant_focused_window_prompt_context () =
  let window : Camelpie.Niri.window_info =
    {
      id = 7;
      title = Some "  Project   notes\nDraft  ";
      app_id = Some "org.editor";
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

let test_text_injection_newline_rule_regex () =
  let window : Camelpie.Niri.window_info =
    {
      id = 384;
      title = Some "Eta | codex | 019e6df3-3186-7a51-ba29-34d742378792";
      app_id = Some "Alacritty";
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
    key true);
  let handle =
    {
      Camelpie.Ptt_loop.offer = (fun _ -> Eta.Effect.unit);
      finish =
        (fun _ ->
           Eta.Effect.sync (fun () -> incr finishes)
           |> Eta.Effect.bind (fun () ->
             if !finishes >= 2 then Eta.Effect.fail (`Ptt_error "done")
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
  Alcotest.(check int) "starts" 2 !starts;
  Alcotest.(check int) "finishes" 2 !finishes;
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
  Alcotest.(check int) "cancels" 0 !cancels;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected finish to stop the test loop"

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
  Alcotest.(check int) "cancels" 0 !cancels;
  match result with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected finish to stop the test loop"

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
       ]);
      ("config",
       [
         Alcotest.test_case "provider codex-realtime" `Quick (fun () ->
           let p = Camelpie.Config.Stt.provider_of_string "codex-realtime" in
           Alcotest.(check bool) "is codex" true (p = Camelpie.Config.Stt.Codex_realtime));
         Alcotest.test_case "provider openrouter" `Quick (fun () ->
           let p = Camelpie.Config.Stt.provider_of_string "openrouter" in
           Alcotest.(check bool) "is openrouter" true (p = Camelpie.Config.Stt.Openrouter));
         Alcotest.test_case "load TOML newline rules" `Quick
           test_stt_config_loads_toml_newline_rules;
         Alcotest.test_case "reject invalid title regex" `Quick
           test_stt_config_rejects_bad_title_regex;
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
       ]);
      ("daemon_server",
       [
         Alcotest.test_case "socket state" `Quick
           test_daemon_server_state;
       ]);
      ("keyboard",
       [
         Alcotest.test_case "parse KeyEvent" `Quick test_keyboard_key_event_parse;
         Alcotest.test_case "parse D-Bus KeyEvent" `Quick
           test_keyboard_dbus_key_event_parse;
       ]);
      ("recording_coordinator",
       [
         Alcotest.test_case "transitions" `Quick test_recording_coordinator_transitions;
       ]);
      ("stt_codex_realtime",
       [
         Alcotest.test_case "url builders" `Quick test_codex_realtime_url_builders;
         Alcotest.test_case "event parser" `Quick test_codex_realtime_event_parser;
         Alcotest.test_case "resample" `Quick test_codex_realtime_resample;
       ]);
      ("assistant_prompt",
       [
         Alcotest.test_case "focused window context" `Quick
           test_assistant_focused_window_prompt_context;
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
         Alcotest.test_case "module loads" `Quick (fun () ->
           Alcotest.(check bool) "module ok" true true);
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
    ]
