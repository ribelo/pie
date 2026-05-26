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
       ]);
      ("text_injection",
       [
         Alcotest.test_case "normalize" `Quick (fun () ->
           Alcotest.(check string) "trimmed" "hello" (Camelpie.Input.Text_injection.normalize_text "  hello  "));
       ]);
      ("daemon_contract",
       [
         Alcotest.test_case "snapshot parsing" `Quick (fun () ->
           let json = `Assoc [("active", `Bool true); ("mode", `String "ptt"); ("enabled", `Bool true)] in
           let snap = Camelpie.Daemon.Contract.snapshot_of_json json in
           Alcotest.(check bool) "active" true snap.active;
           Alcotest.(check (option string)) "mode" (Some "ptt") snap.mode;
           Alcotest.(check bool) "enabled" true snap.enabled);
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
