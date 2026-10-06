(* Live, non-delivering replay through the production retry adapter.
   Prints only duration, outcome and retry metadata, never provider text.
   PIE_LIVE_EVIDENCE_DIR optionally retains bodies in a private scratch archive. *)

let read_file ~(caps : < Ox_caps.Cap.fs_read ; .. >) path =
  let input = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr input)
    (fun () -> really_input_string input (in_channel_length input))

let wav_bytes pcm =
  let header =
    Pie.Audio.Pcm.build_pcm_wav_header ~data_length:(Bytes.length pcm)
      ~sample_rate:16000
  in
  Bytes.cat header pcm

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exit
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; .. >) =
  let argc = Array.length Sys.argv in
  if argc < 5 || argc > 7 then (
    Ox_caps.Cap_stdlib.prerr_endline ~caps
      "usage: live_one_pass AUDIO_FILE pcm|wav|mp3 PROMPT_FILE GLOSSARY_FILE \
       [MODEL] [PROVIDER]";
    Ox_caps.Cap_stdlib.exit ~caps 64);
  let audio = Bytes.of_string (read_file ~caps Sys.argv.(1)) in
  let format = Sys.argv.(2) in
  let prompt_template = read_file ~caps Sys.argv.(3) in
  let glossary = read_file ~caps Sys.argv.(4) in
  let model = if argc > 5 then Sys.argv.(5) else "google/gemini-3.8-flash" in
  let provider_order =
    if argc > 6 then [ Sys.argv.(6) ] else [ "Google AI Studio" ]
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  let upload =
    match format with
    | "wav" -> Pie.Stt.Correction.Wav audio
    | "pcm" -> (
        let wav = wav_bytes audio in
        match
          Pie.Recording.Audio_archive.encode_mp3 ~caps ~process_mgr audio
        with
        | Ok mp3 -> Pie.Stt.Correction.Mp3 { mp3; wav }
        | Error _ -> Pie.Stt.Correction.Wav wav)
    | "mp3" ->
        let ffmpeg =
          match Pie.Recording.Audio_archive.resolve_ffmpeg ~caps with
          | Some path -> path
          | None -> failwith "MP3 replay needs ffmpeg for WAV fallback"
        in
        let pcm =
          Pie.Sh.capture_output ~process_mgr
            ~max_size:Pie.Dictation_limits.capture_pcm_bytes ~executable:ffmpeg
            ~stdin:(Eio.Flow.string_source (Bytes.to_string audio))
            ~stderr:(Pie.Sh.discarded_stderr ())
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
        Pie.Stt.Correction.Mp3
          { mp3 = audio; wav = wav_bytes (Bytes.of_string pcm) }
    | _ ->
        Ox_caps.Cap_stdlib.prerr_endline ~caps "format must be pcm, wav or mp3";
        Ox_caps.Cap_stdlib.exit ~caps 64
  in
  let logger =
    Ox_observability.Logger.create ~service:"pie-live-one-pass" ~clock
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~reporter:
        (Ox_observability.Logger.Reporter.journal (Eio.Stdenv.stderr env))
      ()
  in
  let on_response ~caps ~first body =
    match Ox_caps.Cap_sys.getenv_opt ~caps "PIE_LIVE_EVIDENCE_DIR" with
    | None -> ()
    | Some dir -> (
        match
          Pie.Recording.Audio_archive.store_response ~caps ~dir
            ~id:"live-one-pass" ~first body
        with
        | Ok _ -> ()
        | Error _ ->
            Ox_caps.Cap_stdlib.prerr_endline ~caps
              "private replay evidence write failed")
  in
  let mono_clock = Eio.Stdenv.mono_clock env in
  let started = Eio.Time.Mono.now mono_clock in
  let report =
    Pie.Stt.Correction.run_with_retry ~caps ~sw ~net:(Eio.Stdenv.net env) ~clock
      ~logger ~model ~prompt_template ~transcript:Pie.Stt.Correction.audio_cue
      ~upload ~provider_order ~glossary ~language:"Polish"
      ~source_language:"Polish" ~target_language:"English"
      ~reasoning_effort:"low" ~on_response:(on_response ~caps)
      ~timeout:(Ox.Duration.seconds 120) ()
  in
  let ms =
    int_of_float
      (Mtime.Span.to_float_ns
         (Mtime.span started (Eio.Time.Mono.now mono_clock))
      /. 1e6)
  in
  (match report.retry with
  | None -> Ox_caps.Cap_stdlib.printf ~caps "ms=%d retry=false " ms
  | Some retry ->
      Ox_caps.Cap_stdlib.printf ~caps
        "ms=%d retry=true first=%s format=%s effort=%s " ms
        retry.first_diagnostic
        (Option.value retry.audio_format ~default:"text")
        (Option.value retry.reasoning_effort ~default:"default"));
  match report.result with
  | Ok text ->
      Ox_caps.Cap_stdlib.printf ~caps "outcome=ok bytes=%d\n%!"
        (String.length text)
  | Error failure ->
      Ox_caps.Cap_stdlib.printf ~caps "outcome=%s\n%!"
        (Pie.Stt.Correction.diagnostic failure);
      Ox_caps.Cap_stdlib.exit ~caps 1

let () = main ~caps:(Ox_test.powerbox ())
