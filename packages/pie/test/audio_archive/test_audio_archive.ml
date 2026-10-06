(* dict-1y3i dict-agvw dict-g3ap *)

let pcm =
  let bytes = Bytes.create 8 in
  Bytes.set_int16_le bytes 0 1000;
  Bytes.set_int16_le bytes 2 (-1000);
  Bytes.set_int16_le bytes 4 2000;
  Bytes.set_int16_le bytes 6 (-2000);
  bytes

let perm path = (Unix.lstat path).Unix.st_perm land 0o777

let rec rm_r ~(caps : < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >) path
    =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } ->
      Array.iter
        (fun name ->
          if name <> "." && name <> ".." then
            rm_r ~caps (Filename.concat path name))
        (Ox_caps.Cap_sys.readdir ~caps path);
      Ox_caps.Cap_unix.rmdir ~caps path
  | _ -> Ox_caps.Cap_unix.unlink ~caps path

let with_root
    ~(caps :
       < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >) f
    =
  let root = Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-audio" "" in
  Ox_caps.Cap_sys.remove ~caps root;
  Ox_caps.Cap_unix.mkdir ~caps root 0o700;
  Fun.protect ~finally:(fun () -> rm_r ~caps root) (fun () -> f root)

let with_env ~(caps : < Ox_caps.Cap.env ; .. >) pairs f =
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

let write_file ~(caps : < Ox_caps.Cap.fs_write ; .. >) path contents =
  let channel = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file ~(caps : < Ox_caps.Cap.fs_read ; .. >) path =
  let channel = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let fail_archive = function
  | Error message -> Alcotest.fail message
  | Ok value -> value

let contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let install_ffmpeg ~(caps : < Ox_caps.Cap.fs_write ; .. >) ~root ~body =
  let bin = Filename.concat root "bin" in
  Ox_caps.Cap_unix.mkdir ~caps bin 0o700;
  let path = Filename.concat bin "ffmpeg" in
  write_file ~caps path body;
  Ox_caps.Cap_unix.chmod ~caps path 0o755;
  bin

let store
    ~(caps :
       < Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >)
    ~env ~dir ~id =
  Eio_main.run @@ fun env_eio ->
  ignore env;
  Pie.Recording.Audio_archive.store ~caps
    ~process_mgr:(Eio.Stdenv.process_mgr env_eio)
    ~dir ~id pcm

(* dict-x6e9 *)
let test_response_evidence
    ~(caps :
       < Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_root ~caps (fun root ->
      let dir = Filename.concat root "audio" in
      let body = Bytes.of_string "{\"private\":\"complete provider body\"}\n" in
      let store first =
        fail_archive
          (Pie.Recording.Audio_archive.store_response ~caps ~dir
             ~id:"attempt-response" ~first body)
      in
      let first = store true and final = store false in
      Alcotest.(check string)
        "first name" "attempt-response.response-1.json"
        (Filename.basename first);
      Alcotest.(check string)
        "final name" "attempt-response.response.json" (Filename.basename final);
      List.iter
        (fun path ->
          Alcotest.(check int) "private response" 0o600 (perm path);
          Alcotest.(check string)
            "whole response" (Bytes.to_string body) (read_file ~caps path))
        [ first; final ];
      let now = Ox_caps.Cap_unix.gettimeofday ~caps () in
      List.iter
        (fun path ->
          Unix.utimes path (now -. (169. *. 3600.)) (now -. (169. *. 3600.)))
        [ first; final ];
      Alcotest.(check int)
        "response retention matches audio" 2
        (fail_archive
           (Pie.Recording.Audio_archive.prune ~caps ~dir ~retention_hours:168.
              ~now));
      List.iter
        (fun path ->
          Alcotest.(check bool)
            "expired response gone" false (Sys.file_exists path))
        [ first; final ];
      match
        Pie.Recording.Audio_archive.store_response ~caps ~dir ~id:"../escape"
          ~first:false body
      with
      | Error _ -> ()
      | Ok _ -> Alcotest.fail "unsafe response id accepted")

(* dict-4c5v: actual encoder, probe and decode, not a shell fixture. *)
let test_real_mp3 ~(caps : < Ox_caps.Cap.env ; .. >) () =
  Eio_main.run @@ fun env ->
  let process_mgr = Eio.Stdenv.process_mgr env in
  let source = Bytes.create 32000 in
  for i = 0 to 15999 do
    Bytes.set_int16_le source (i * 2)
      (int_of_float (6000. *. sin (float_of_int i *. 0.1728)))
  done;
  let mp3 =
    fail_archive
      (Pie.Recording.Audio_archive.encode_mp3 ~caps ~process_mgr source)
  in
  let mp3 = Bytes.to_string mp3 in
  Alcotest.(check bool) "no ID3v2" false (String.starts_with ~prefix:"ID3" mp3);
  List.iter
    (fun marker ->
      Alcotest.(check bool) ("no " ^ marker) false (contains mp3 marker))
    [ "Xing"; "Info" ];
  let errors = Buffer.create 64 in
  let probe =
    Pie.Sh.capture_output ~process_mgr ~max_size:65_536
      ~stdin:(Eio.Flow.string_source mp3)
      ~stderr:(Eio.Flow.buffer_sink errors)
      [
        "ffprobe";
        "-v";
        "error";
        "-show_entries";
        "stream=sample_rate,channels,bit_rate";
        "-of";
        "json";
        "pipe:0";
      ]
  in
  let open Yojson.Safe.Util in
  let stream =
    Yojson.Safe.from_string probe |> member "streams" |> to_list |> List.hd
  in
  Alcotest.(check string)
    "32 kbit/s" "32000"
    (stream |> member "bit_rate" |> to_string);
  Alcotest.(check string)
    "16 kHz" "16000"
    (stream |> member "sample_rate" |> to_string);
  Alcotest.(check int) "mono" 1 (stream |> member "channels" |> to_int);
  let ffmpeg = Option.get (Pie.Recording.Audio_archive.resolve_ffmpeg ~caps) in
  let decoded =
    Pie.Sh.capture_output ~process_mgr
      ~max_size:Pie.Dictation_limits.capture_pcm_bytes ~executable:ffmpeg
      ~stdin:(Eio.Flow.string_source mp3)
      ~stderr:(Eio.Flow.buffer_sink errors)
      [
        ffmpeg;
        "-v";
        "error";
        "-f";
        "mp3";
        "-i";
        "pipe:0";
        "-f";
        "s16le";
        "-ar";
        "16000";
        "-ac";
        "1";
        "pipe:1";
      ]
  in
  Alcotest.(check bool)
    "decodes whole source" true
    (String.length decoded >= Bytes.length source)

let test_wav_fallback_ignores_path
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_root ~caps (fun root ->
      let dir = Filename.concat root "audio" in
      let log = Filename.concat root "ffmpeg.log" in
      let bin =
        install_ffmpeg ~caps ~root
          ~body:
            "#!/bin/sh\n\
             printf '%s\\n' \"$@\" >> \"$PIE_TEST_FFMPEG_LOG\"\n\
             exit 0\n"
      in
      let path =
        with_env ~caps
          [
            ("PIE_FFMPEG", Filename.concat root "missing-ffmpeg");
            ( "PATH",
              bin ^ ":"
              ^ Option.value
                  (Ox_caps.Cap_sys.getenv_opt ~caps "PATH")
                  ~default:"" );
            ("PIE_TEST_FFMPEG_LOG", log);
          ]
          (fun () -> fail_archive (store ~caps ~env:() ~dir ~id:"attempt-1"))
      in
      Alcotest.(check string)
        "wav path"
        (Filename.concat dir "attempt-1.wav")
        path;
      Alcotest.(check bool) "wav exists" true (Sys.file_exists path);
      Alcotest.(check int) "file 0600" 0o600 (perm path);
      Alcotest.(check int) "dir 0700" 0o700 (perm dir);
      Alcotest.(check bool)
        "path ffmpeg was not run" false (Sys.file_exists log);
      Eio_main.run @@ fun env ->
      let loaded =
        fail_archive
          (Pie.Recording.Audio_archive.load ~caps
             ~process_mgr:(Eio.Stdenv.process_mgr env)
             ~path)
      in
      Alcotest.(check string)
        "pcm round trip" (Bytes.to_string pcm) (Bytes.to_string loaded))

let test_opus_fake_ffmpeg_on_path
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_root ~caps (fun root ->
      let dir = Filename.concat root "audio" in
      let log = Filename.concat root "ffmpeg.log" in
      let bin =
        install_ffmpeg ~caps ~root
          ~body:
            ("#!/bin/sh\nprintf '%s\\n' \"$@\" >> \"$PIE_TEST_FFMPEG_LOG\"\n"
           ^ "out=\nfor arg in \"$@\"; do out=\"$arg\"; done\n"
           ^ "printf OPUS > \"$out\"\nexit 0\n")
      in
      let path =
        with_env ~caps
          [
            ("PIE_FFMPEG", "");
            ( "PATH",
              bin ^ ":"
              ^ Option.value
                  (Ox_caps.Cap_sys.getenv_opt ~caps "PATH")
                  ~default:"" );
            ("PIE_TEST_FFMPEG_LOG", log);
          ]
          (fun () -> fail_archive (store ~caps ~env:() ~dir ~id:"attempt-2"))
      in
      Alcotest.(check string)
        "opus path"
        (Filename.concat dir "attempt-2.opus")
        path;
      Alcotest.(check string) "marker" "OPUS" (read_file ~caps path);
      Alcotest.(check int) "file 0600" 0o600 (perm path);
      let args = read_file ~caps log in
      List.iter
        (fun needle -> Alcotest.(check bool) needle true (contains args needle))
        [ "s16le"; "16000"; "libopus"; "24k"; "voip"; "pipe:0"; "ogg" ])

let test_encoder_failure_writes_wav
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_root ~caps (fun root ->
      let dir = Filename.concat root "audio" in
      let bin = install_ffmpeg ~caps ~root ~body:"#!/bin/sh\nexit 1\n" in
      let path =
        with_env ~caps
          [ ("PIE_FFMPEG", Filename.concat bin "ffmpeg"); ("PATH", bin) ]
          (fun () -> fail_archive (store ~caps ~env:() ~dir ~id:"attempt-3"))
      in
      Alcotest.(check bool)
        "wav fallback" true
        (Filename.check_suffix path ".wav");
      Alcotest.(check bool)
        "no opus left" false
        (Sys.file_exists (Filename.concat dir "attempt-3.opus")))

let touch ~(caps : < Ox_caps.Cap.fs_write ; .. >) path =
  let channel = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  close_out channel

let age ~(caps : < Ox_caps.Cap.time ; .. >) path seconds =
  let stamp = Ox_caps.Cap_unix.gettimeofday ~caps () -. seconds in
  Unix.utimes path stamp stamp

let test_prune
    ~(caps :
       < Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_root ~caps (fun root ->
      let dir = Filename.concat root "audio" in
      Ox_caps.Cap_unix.mkdir ~caps dir 0o700;
      let old = Filename.concat dir "old-attempt.opus" in
      let fresh = Filename.concat dir "fresh-attempt.wav" in
      let notes = Filename.concat dir "notes.txt" in
      let partial = Filename.concat dir "old-attempt.opus.1.partial" in
      let outside = Filename.concat root "outside" in
      let link = Filename.concat dir "linked-attempt.opus" in
      touch ~caps old;
      touch ~caps fresh;
      touch ~caps notes;
      touch ~caps partial;
      write_file ~caps outside "keep";
      age ~caps outside (48. *. 3600.);
      Ox_caps.Cap_unix.symlink ~caps outside link;
      age ~caps old (48. *. 3600.);
      age ~caps notes (48. *. 3600.);
      age ~caps partial (48. *. 3600.);
      let removed =
        fail_archive
          (Pie.Recording.Audio_archive.prune ~caps ~dir ~retention_hours:24.
             ~now:(Ox_caps.Cap_unix.gettimeofday ~caps ()))
      in
      Alcotest.(check int) "one expired file" 1 removed;
      Alcotest.(check bool) "old opus gone" false (Sys.file_exists old);
      Alcotest.(check bool) "fresh wav kept" true (Sys.file_exists fresh);
      Alcotest.(check bool) "notes kept" true (Sys.file_exists notes);
      Alcotest.(check bool) "partial kept" true (Sys.file_exists partial);
      Alcotest.(check bool) "symlink kept" true (Sys.file_exists link);
      Alcotest.(check string) "target kept" "keep" (read_file ~caps outside);
      let kept =
        fail_archive
          (Pie.Recording.Audio_archive.prune ~caps ~dir ~retention_hours:0.
             ~now:(Ox_caps.Cap_unix.gettimeofday ~caps ()))
      in
      Alcotest.(check int) "zero retention deletes nothing" 0 kept;
      Alcotest.(check bool) "fresh still there" true (Sys.file_exists fresh))

let test_locate_errors
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_root ~caps (fun root ->
      let dir = Filename.concat root "audio" in
      Ox_caps.Cap_unix.mkdir ~caps dir 0o700;
      with_env ~caps
        [ ("PIE_FFMPEG", Filename.concat root "missing-ffmpeg") ]
        (fun () ->
          (match
             Pie.Recording.Audio_archive.locate ~caps ~dir ~id:"../escape"
           with
          | Error message ->
              Alcotest.(check bool)
                "unknown id" true
                (contains message "unknown archive id")
          | Ok _ -> Alcotest.fail "expected unknown id");
          (match
             Pie.Recording.Audio_archive.locate ~caps ~dir ~id:"missing"
           with
          | Error message ->
              Alcotest.(check bool)
                "missing" true
                (contains message "no archived audio")
          | Ok _ -> Alcotest.fail "expected missing file");
          touch ~caps (Filename.concat dir "only-opus.opus");
          match
            Pie.Recording.Audio_archive.locate ~caps ~dir ~id:"only-opus"
          with
          | Error message ->
              Alcotest.(check bool)
                "no decoder" true
                (contains message "no decoder")
          | Ok _ -> Alcotest.fail "expected no decoder"))

let write_config ~(caps : < Ox_caps.Cap.fs_write ; .. >) root body =
  let path = Filename.concat root "stt.toml" in
  write_file ~caps path body;
  path

let load ~(caps : < Ox_caps.Cap.env ; .. >) path =
  Eio_main.run @@ fun env ->
  Pie.Config.Stt.load ~caps ~fs:(Eio.Stdenv.fs env) ~source:(`Explicit path) ()

let test_retention_config
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_root ~caps (fun root ->
      let loaded body =
        match load ~caps (write_config ~caps root body) with
        | Error (`Config message) -> Alcotest.fail message
        | Ok config -> config
      in
      Alcotest.(check (float 0.))
        "default seven days" 168.
        (loaded "provider = \"openrouter\"\n").audio_retention_hours;
      Alcotest.(check (float 0.))
        "zero" 0. (loaded "audio_retention_hours = 0\n").audio_retention_hours;
      Alcotest.(check (float 0.001))
        "fraction" 12.5
        (loaded "audio_retention_hours = 12.5\n").audio_retention_hours;
      (match
         load ~caps (write_config ~caps root "audio_retention_hours = -1\n")
       with
      | Error (`Config message) ->
          Alcotest.(check bool)
            "negative" true
            (contains message "non-negative")
      | Ok _ -> Alcotest.fail "expected negative rejection");
      match
        load ~caps (write_config ~caps root "audio_retention_hours = \"day\"\n")
      with
      | Error (`Config message) ->
          Alcotest.(check bool) "type" true (contains message "number")
      | Ok _ -> Alcotest.fail "expected type rejection")

let transcribe_keysym = 65478

let loud_pcm =
  let bytes = Bytes.create 640 in
  for i = 0 to 319 do
    Bytes.set_int16_le bytes (i * 2) 20000
  done;
  bytes

let key ~released : Pie.Keyboard.Monitor.key_event =
  { released; state = 0; keysym = transcribe_keysym; unichar = 0; keycode = 75 }

let wait_until ?(steps = 160) ~clock pred =
  let rec loop n =
    if pred () then ()
    else if n <= 0 then Alcotest.fail "deadline"
    else (
      Eio.Time.sleep clock 0.05;
      loop (n - 1))
  in
  loop steps

let service_binding keysym mode : Pie.Config.Stt.binding =
  {
    keysym;
    shift = false;
    mode;
    correct = false;
    prompt_named = false;
    prompt_file = "correction.md";
    prompt = "{{glossary}}";
  }

let service_config ~retention () : Pie.Config.Stt.t =
  {
    provider = Openrouter;
    transcription_model = "openrouter/archive";
    translation_model = "openrouter/archive";
    transcription_language = "English";
    translation_source_language = "English";
    translation_target_language = "English";
    ptt_release_grace_ms = 0;
    ptt_min_capture_rms = 0.;
    ptt_min_capture_peak = 0.;
    ptt_transcribe_keysym = transcribe_keysym;
    ptt_translate_keysym = 65479;
    ptt_transcribe_shift = false;
    ptt_translate_shift = false;
    thinking_effort = None;
    newline_rules = [];
    text_entry_rules = [];
    prompt_rules = [];
    pipeline = Two_pass;
    openrouter_provider_order = None;
    one_pass_timeout_seconds = 30.;
    streaming = false;
    correction = None;
    audio_retention_hours = retention;
    bindings =
      [
        service_binding transcribe_keysym Transcribe;
        service_binding 65479 Translate;
      ];
  }

let archive_names ~(caps : < Ox_caps.Cap.fs_read ; .. >) dir =
  if not (Sys.file_exists dir) then []
  else
    Array.to_list (Ox_caps.Cap_sys.readdir ~caps dir)
    |> List.filter (fun name ->
        Filename.check_suffix name ".wav" || Filename.check_suffix name ".opus")

let draft_ready ~(caps : < Ox_caps.Cap.fs_read ; .. >) retry_dir =
  let active = Filename.concat retry_dir "active" in
  Sys.file_exists active
  && Array.exists
       (fun name ->
         Sys.file_exists
           (Filename.concat (Filename.concat active name) "draft.pcm"))
       (try Ox_caps.Cap_sys.readdir ~caps active with Sys_error _ -> [||])

let rpc ~net ~sw socket_path body =
  let socket = Eio.Net.connect ~sw net (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> Eio.Flow.close socket)
    (fun () ->
      Eio.Flow.copy_string (body ^ "\n") socket;
      Eio.Buf_read.of_flow ~max_size:65536 socket |> Eio.Buf_read.line)

let run_attempt_reported
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; .. >) ~report ~env ~dir ~retention ~pass1 ~deliver ~during =
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let audio_dir = Filename.concat dir "audio" in
  let history_path = Filename.concat dir "history.jsonl" in
  let retry_dir = Filename.concat dir "recordings" in
  let socket_path = Filename.concat dir "control.sock" in
  Ox_caps.Cap_unix.mkdir ~caps retry_dir 0o700;
  let config = service_config ~retention () in
  let stamp () = [ Some (1, 1, 0, 0.) ] in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio_q = Eio.Stream.create 16 in
  let audio () =
    match Eio.Stream.take audio_q with
    | `Chunk bytes -> Ok (Some bytes)
    | `Stop -> Ok None
  in
  let logger =
    Ox_observability.Logger.create ~service:"audio-archive-test" ~clock
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~reporter:(Ox_observability.Logger.Reporter.writer report)
      ()
  in
  let next_id = ref 0 in
  let pipeline : Pie.Assistant.Dictation.dependencies =
    {
      resolve_glossary =
        (fun () ->
          ( { Pie.Glossary.text = ""; paths = []; warnings = [] },
            Some { Pie.History.id = Some 42; app_id = Some "mpv"; title = None }
          ));
      pass1 = (fun _ _ _ pcm -> pass1 pcm);
      pass2 =
        (fun ~on_response:_ _ _ _ text ->
          Pie.Stt.Correction.of_result (Ok text));
      store_response = (fun ~id:_ ~first:_ _ -> Ok "/audio/response.json");
      append_transcribed =
        (fun event ->
          Pie.History.append_transcribed ~caps ~path:history_path event);
      append_finished =
        (fun event ->
          Pie.History.append_finished ~caps ~path:history_path event);
      warn = ignore;
      report_notification = (fun _ _ _ -> ());
      one_pass =
        (fun ~on_response:_ _ _ _ _ ->
          Pie.Stt.Correction.of_result (Error Pie.Stt.Correction.Blank));
      fresh_id =
        (fun () ->
          incr next_id;
          if !next_id = 1 then "live-attempt"
          else Printf.sprintf "live-attempt-%d" !next_id);
      now = (fun () -> Ox_caps.Cap_unix.gettimeofday ~caps ());
      log_pass = (fun _ ~model:_ ~duration_ms:_ -> ());
    }
  in
  Eio.Switch.run @@ fun sw ->
  let workflow () =
    Pie_test_runtime.Service.run_with_inputs ~caps ~sw ~net
      ~fs:(Eio.Stdenv.fs env) ~clock ~logger
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~inputs:{ keyboard; audio; source_name = Some "test-source" }
      ~config_control:{ stamp; load = (fun _ -> (config, stamp ())) }
      ~stt:(fun _ _ _ -> Ok "unused")
      ~deliver ~socket_path ~retry_dir ~audio_dir ~history_path ~pipeline
      ~initial_config:config ~min_duration_ms:0 ()
  in
  match
    Eio.Fiber.first workflow (fun () ->
        during ~clock ~net ~sw ~keyboard ~audio_q ~retry_dir ~socket_path
          ~audio_dir ~history_path;
        Ok ())
  with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "service workflow failed"

let run_attempt ~caps ~env ~dir ~retention ~pass1 ~deliver ~during =
  run_attempt_reported ~caps ~report:ignore ~env ~dir ~retention ~pass1 ~deliver
    ~during

let offer ~(caps : < Ox_caps.Cap.fs_read ; .. >) ~clock ~audio_q ~retry_dir =
  wait_until ~clock (fun () ->
      Eio.Stream.add audio_q (`Chunk loud_pcm);
      draft_ready ~caps retry_dir)

let test_archive_after_success
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  Pie_test_runtime.run ~caps @@ fun env ->
  with_root ~caps (fun dir ->
      with_env ~caps
        [ ("PIE_FFMPEG", Filename.concat dir "missing-ffmpeg") ]
        (fun () ->
          let delivered = ref [] in
          run_attempt ~caps ~env ~dir ~retention:24.
            ~pass1:(fun _ -> Ok "raw words")
            ~deliver:(fun _ _ _ text ->
              delivered := text :: !delivered;
              Ok Pie.History.Pasted)
            ~during:(fun
                ~clock
                ~net:_
                ~sw:_
                ~keyboard
                ~audio_q
                ~retry_dir
                ~socket_path:_
                ~audio_dir
                ~history_path
              ->
              Eio.Time.sleep clock 0.2;
              Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
              offer ~caps ~clock ~audio_q ~retry_dir;
              Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
              wait_until ~clock (fun () -> !delivered <> []);
              wait_until ~clock (fun () ->
                  archive_names ~caps audio_dir = [ "live-attempt.wav" ]);
              wait_until ~clock (fun () ->
                  match Pie.History.read ~caps ~path:history_path with
                  | Ok [ entry ] ->
                      entry.audio_path
                      = Some (Filename.concat audio_dir "live-attempt.wav")
                  | _ -> false);
              Alcotest.(check (list string))
                "delivered" [ "raw words" ] (List.rev !delivered))))

let test_archive_after_failure
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  Pie_test_runtime.run ~caps @@ fun env ->
  with_root ~caps (fun dir ->
      with_env ~caps
        [ ("PIE_FFMPEG", Filename.concat dir "missing-ffmpeg") ]
        (fun () ->
          run_attempt ~caps ~env ~dir ~retention:24.
            ~pass1:(fun _ -> Error (`Ptt_error "stt:unavailable"))
            ~deliver:(fun _ _ _ _ -> Alcotest.fail "delivery after failure")
            ~during:(fun
                ~clock
                ~net:_
                ~sw:_
                ~keyboard
                ~audio_q
                ~retry_dir
                ~socket_path:_
                ~audio_dir
                ~history_path:_
              ->
              Eio.Time.sleep clock 0.2;
              Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
              offer ~caps ~clock ~audio_q ~retry_dir;
              Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
              wait_until ~clock (fun () ->
                  archive_names ~caps audio_dir = [ "live-attempt.wav" ]))))

let test_no_archive_after_cancel
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  Pie_test_runtime.run ~caps @@ fun env ->
  with_root ~caps (fun dir ->
      with_env ~caps
        [ ("PIE_FFMPEG", Filename.concat dir "missing-ffmpeg") ]
        (fun () ->
          run_attempt ~caps ~env ~dir ~retention:24.
            ~pass1:(fun _ -> Alcotest.fail "pass 1 after cancel")
            ~deliver:(fun _ _ _ _ -> Alcotest.fail "delivery after cancel")
            ~during:(fun
                ~clock
                ~net
                ~sw
                ~keyboard
                ~audio_q
                ~retry_dir
                ~socket_path
                ~audio_dir
                ~history_path:_
              ->
              wait_until ~clock (fun () -> Sys.file_exists socket_path);
              Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
              offer ~caps ~clock ~audio_q ~retry_dir;
              ignore
                (rpc ~net ~sw socket_path
                   "{\"method\":\"Action\",\"action\":\"pause\"}");
              Eio.Time.sleep clock 1.0;
              Alcotest.(check (list string))
                "no archive after pause" []
                (archive_names ~caps audio_dir))))

let test_retention_zero_archives_nothing
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  Pie_test_runtime.run ~caps @@ fun env ->
  with_root ~caps (fun dir ->
      let delivered = ref false in
      run_attempt ~caps ~env ~dir ~retention:0.
        ~pass1:(fun _ -> Ok "kept")
        ~deliver:(fun _ _ _ _ ->
          delivered := true;
          Ok Pie.History.Pasted)
        ~during:(fun
            ~clock
            ~net:_
            ~sw:_
            ~keyboard
            ~audio_q
            ~retry_dir
            ~socket_path:_
            ~audio_dir
            ~history_path:_
          ->
          Eio.Time.sleep clock 0.2;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
          offer ~caps ~clock ~audio_q ~retry_dir;
          Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
          wait_until ~clock (fun () -> !delivered);
          Eio.Time.sleep clock 0.5;
          Alcotest.(check (list string))
            "retention 0 writes nothing" []
            (archive_names ~caps audio_dir)))

let write_private_wav ~(caps : < Ox_caps.Cap.fs_write ; .. >) dir id pcm =
  Ox_caps.Cap_unix.mkdir ~caps dir 0o700;
  let path = Filename.concat dir (id ^ ".wav") in
  let header =
    Pie.Audio.Pcm.build_pcm_wav_header ~data_length:(Bytes.length pcm)
      ~sample_rate:16000
  in
  let channel =
    Ox_caps.Cap_stdlib.open_out_gen ~caps
      [ Open_creat; Open_wronly; Open_trunc ]
      0o600 path
  in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () ->
      output_bytes channel header;
      output_bytes channel pcm);
  path

let json_bool line key =
  match Yojson.Safe.from_string line with
  | `Assoc fields -> (
      match List.assoc_opt key fields with
      | Some (`Bool value) -> value
      | _ -> false)
  | _ -> false

let json_message line =
  match Yojson.Safe.from_string line with
  | `Assoc fields -> (
      match List.assoc_opt "message" fields with
      | Some (`String value) -> value
      | _ -> "")
  | _ -> ""

let test_overlapping_archives
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  Pie_test_runtime.run ~caps @@ fun env ->
  with_root ~caps (fun dir ->
      with_env ~caps
        [ ("PIE_FFMPEG", Filename.concat dir "missing-ffmpeg") ]
        (fun () ->
          let first, release_first = Eio.Promise.create () in
          let calls = ref 0 and delivered = ref [] in
          run_attempt ~caps ~env ~dir ~retention:24.
            ~pass1:(fun _ ->
              incr calls;
              let n = !calls in
              if n = 1 then Eio.Promise.await first;
              Ok (string_of_int n))
            ~deliver:(fun _ _ _ text ->
              delivered := !delivered @ [ text ];
              Ok Pie.History.Pasted)
            ~during:(fun
                ~clock
                ~net:_
                ~sw:_
                ~keyboard
                ~audio_q
                ~retry_dir
                ~socket_path:_
                ~audio_dir
                ~history_path
              ->
              Eio.Time.sleep clock 0.2;
              for n = 1 to 2 do
                Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
                offer ~caps ~clock ~audio_q ~retry_dir;
                Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
                wait_until ~clock (fun () -> !calls = n)
              done;
              Alcotest.(check (list string))
                "later archive does not bypass delivery FIFO" [] !delivered;
              Eio.Promise.resolve release_first ();
              wait_until ~clock (fun () -> List.length !delivered = 2);
              wait_until ~clock (fun () ->
                  match Pie.History.read ~caps ~path:history_path with
                  | Ok entries ->
                      List.length entries = 2
                      && List.for_all
                           (fun (e : Pie.History.entry) ->
                             e.outcome = Some Delivered && e.audio_path <> None)
                           entries
                  | _ -> false);
              Alcotest.(check (list string))
                "independent audio files"
                [ "live-attempt-2.wav"; "live-attempt.wav" ]
                (List.sort String.compare (archive_names ~caps audio_dir));
              let entries =
                match Pie.History.read ~caps ~path:history_path with
                | Ok entries -> entries
                | Error _ -> Alcotest.fail "history read failed"
              in
              let records =
                List.map
                  (fun (e : Pie.History.entry) ->
                    (e.id, (e.raw_text, e.audio_path)))
                  entries
                |> List.sort compare
              in
              Alcotest.(
                check
                  (list (pair string (pair (option string) (option string)))))
                "attempt IDs keep their own text and archive"
                [
                  ( "live-attempt",
                    ( Some "1",
                      Some (Filename.concat audio_dir "live-attempt.wav") ) );
                  ( "live-attempt-2",
                    ( Some "2",
                      Some (Filename.concat audio_dir "live-attempt-2.wav") ) );
                ]
                records)))

let test_retry_by_id
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  Pie_test_runtime.run ~caps @@ fun env ->
  with_root ~caps (fun dir ->
      let clip = Bytes.of_string "archive-pcm!!" in
      let audio_dir = Filename.concat dir "audio" in
      ignore (write_private_wav ~caps audio_dir "clip-1" clip);
      let seen = ref None and delivered = ref false in
      run_attempt ~caps ~env ~dir ~retention:24.
        ~pass1:(fun pcm ->
          seen := Some (Bytes.to_string pcm);
          Ok "from-archive")
        ~deliver:(fun _ _ target _ ->
          Alcotest.(check bool)
            "dict-d4jm archived retry clipboard-only" true (target = None);
          delivered := true;
          Ok Pie.History.Clipboard)
        ~during:(fun
            ~clock
            ~net
            ~sw
            ~keyboard:_
            ~audio_q:_
            ~retry_dir:_
            ~socket_path
            ~audio_dir:_
            ~history_path:_
          ->
          wait_until ~clock (fun () -> Sys.file_exists socket_path);
          let missing =
            rpc ~net ~sw socket_path
              "{\"method\":\"Action\",\"action\":\"retry missing-id\"}"
          in
          Alcotest.(check bool)
            "unknown rejected" false
            (json_bool missing "accepted");
          Alcotest.(check bool)
            "missing message" true
            (contains (json_message missing) "no archived audio");
          let queued =
            rpc ~net ~sw socket_path
              "{\"method\":\"Action\",\"action\":\"retry clip-1\"}"
          in
          Alcotest.(check bool) "queued" true (json_bool queued "accepted");
          wait_until ~clock (fun () -> !delivered);
          Alcotest.(check (option string))
            "archived PCM reached provider"
            (Some (Bytes.to_string clip))
            !seen))

(* pcapslint-t4bn: the service is cancelled only after its archive process starts. *)
let test_archive_worker_cancellation ~caps () =
  Pie_test_runtime.run ~caps @@ fun env ->
  with_root ~caps (fun dir ->
      let entered = Filename.concat dir "encoder-entered" in
      let bin =
        install_ffmpeg ~caps ~root:dir
          ~body:
            (Printf.sprintf "#!/bin/sh\n: > %s\nexec sleep 30\n"
               (Filename.quote entered))
      in
      with_env ~caps
        [ ("PIE_FFMPEG", Filename.concat bin "ffmpeg") ]
        (fun () ->
          let warnings = ref [] in
          let delivered = ref false in
          let audio_dir = Filename.concat dir "audio" in
          run_attempt_reported ~caps ~env ~dir ~retention:24.
            ~report:(fun line ->
              if contains line "audio archive failed" then
                warnings := line :: !warnings)
            ~pass1:(fun _ -> Ok "raw words")
            ~deliver:(fun _ _ _ _ ->
              delivered := true;
              Ok Pie.History.Pasted)
            ~during:(fun
                ~clock
                ~net:_
                ~sw:_
                ~keyboard
                ~audio_q
                ~retry_dir
                ~socket_path:_
                ~audio_dir:_
                ~history_path:_
              ->
              Eio.Time.sleep clock 0.2;
              Pie.Keyboard.Monitor.emit keyboard (key ~released:false);
              offer ~caps ~clock ~audio_q ~retry_dir;
              Pie.Keyboard.Monitor.emit keyboard (key ~released:true);
              wait_until ~clock (fun () ->
                  !delivered && Sys.file_exists entered));
          Alcotest.(check bool)
            "encoder was running before real cancellation" true
            (Sys.file_exists entered);
          Alcotest.(check (list string))
            "cancellation is not an archive warning" [] !warnings;
          Alcotest.(check (list string))
            "cancelled encoder published no archive" []
            (archive_names ~caps audio_dir)))

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "audio_archive"
    [
      ( "archive",
        [
          Alcotest.test_case "response evidence dict-x6e9" `Quick
            (test_response_evidence ~caps);
          Alcotest.test_case "real MP3 dict-4c5v" `Quick (test_real_mp3 ~caps);
          Alcotest.test_case
            "wav fallback when PIE_FFMPEG is missing dict-agvw dict-1y3i" `Quick
            (test_wav_fallback_ignores_path ~caps);
          Alcotest.test_case "opus via fake ffmpeg on PATH dict-agvw" `Quick
            (test_opus_fake_ffmpeg_on_path ~caps);
          Alcotest.test_case "encoder failure writes wav dict-agvw" `Quick
            (test_encoder_failure_writes_wav ~caps);
          Alcotest.test_case
            "retention prune skips links and zero dict-g3ap dict-1y3i" `Quick
            (test_prune ~caps);
          Alcotest.test_case "locate unknown missing and no decoder dict-z1ax"
            `Quick (test_locate_errors ~caps);
          Alcotest.test_case "audio_retention_hours validation dict-1y3i" `Quick
            (test_retention_config ~caps);
          Alcotest.test_case
            "archive after success dict-1y3i dict-agvw dict-z1ax" `Quick
            (test_archive_after_success ~caps);
          Alcotest.test_case "archive after failure dict-1y3i dict-agvw" `Quick
            (test_archive_after_failure ~caps);
          Alcotest.test_case "no archive after cancel dict-1y3i" `Quick
            (test_no_archive_after_cancel ~caps);
          Alcotest.test_case
            "pcapslint-t4bn archive worker propagates cancellation" `Quick
            (test_archive_worker_cancellation ~caps);
          Alcotest.test_case "retention 0 archives nothing dict-1y3i" `Quick
            (test_retention_zero_archives_nothing ~caps);
          Alcotest.test_case
            "dict-q7we overlap preserves archive and history IDs" `Quick
            (test_overlapping_archives ~caps);
          Alcotest.test_case "retry by history id dict-z1ax dict-ynad" `Quick
            (test_retry_by_id ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
