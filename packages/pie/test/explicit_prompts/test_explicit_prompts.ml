(* PIE017: configured prompts reach production_stt; ambient sentinels do not. *)

let has_suffix suffix s =
  let sl = String.length suffix in
  let n = String.length s in
  n >= sl && String.sub s (n - sl) sl = suffix

let contains needle hay =
  let n = String.length needle in
  let h = String.length hay in
  let rec loop i =
    i <= h - n && (String.sub hay i n = needle || loop (i + 1))
  in
  loop 0

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      Array.iter
        (fun name ->
          if name <> "." && name <> ".." then rm_rf (Filename.concat path name))
        (Sys.readdir path);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let pcm =
  let b = Bytes.create 640 in
  for i = 0 to 319 do
    Bytes.set_int16_le b (i * 2) 20000
  done;
  b

let config ~transcription_prompt ~translation_prompt ~transcription_model
    ~translation_model ~source_language ~target_language : Pie.Config.Stt.t =
  {
    provider = Pie.Config.Stt.Openrouter;
    transcription_model;
    translation_model;
    transcription_language = source_language;
    translation_source_language = source_language;
    translation_target_language = target_language;
    ptt_min_capture_rms = 0.0;
    ptt_min_capture_peak = 0.0;
    ptt_transcribe_keysym = 1;
    ptt_translate_keysym = 2;
    ptt_transcribe_shift = false;
    ptt_translate_shift = false;
    thinking_effort = None;
    transcription_prompt;
    translation_prompt;
    newline_rules = [];
    text_entry_rules = [];
  }

let completed_body text =
  let inner = Yojson.Safe.to_string (`Assoc [ ("text", `String text) ]) in
  Yojson.Safe.to_string
    (`Assoc
       [
         ("status", `String "completed");
         ( "output",
           `List
             [
               `Assoc
                 [
                   ("type", `String "message");
                   ( "content",
                     `List
                       [
                         `Assoc
                           [
                             ("type", `String "output_text");
                             ("text", `String inner);
                           ];
                       ] );
                 ];
             ] );
       ])

let read_http_request flow =
  let scratch = Cstruct.create 1 in
  let buffer = Buffer.create 4096 in
  let rec headers () =
    match Eio.Flow.single_read flow scratch with
    | 0 -> raise End_of_file
    | _ ->
        Buffer.add_char buffer (Cstruct.get_char scratch 0);
        let contents = Buffer.contents buffer in
        if has_suffix "\r\n\r\n" contents then contents else headers ()
  in
  let head = headers () in
  let content_length =
    let lower = String.lowercase_ascii head in
    List.find_map
      (fun line ->
        if String.starts_with ~prefix:"content-length:" line then
          int_of_string_opt
            (String.trim (String.sub line 15 (String.length line - 15)))
        else None)
      (String.split_on_char '\n' lower)
  in
  let body =
    match content_length with
    | None | Some 0 -> ""
    | Some n ->
        let buf = Cstruct.create n in
        let rec loop off =
          if off < n then
            match Eio.Flow.single_read flow (Cstruct.sub buf off (n - off)) with
            | 0 -> raise End_of_file
            | k -> loop (off + k)
        in
        loop 0;
        Cstruct.to_string buf
  in
  body

let write_http_json flow body =
  let response =
    Printf.sprintf
      "HTTP/1.1 200 OK\r\n\
       Content-Type: application/json\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n\
       %s"
      (String.length body) body
  in
  Eio.Flow.copy_string response flow

let niri_script =
  "#!/bin/sh\n"
  ^ "printf niri-recognition-lookup\\n >> \"${PIE_EXPLICIT_NIRI_LOG:?}\"\n"
  ^ "printf '%s\\n' \
     '{\"id\":1,\"title\":\"SENTINEL_TITLE\",\"app_id\":\"SentinelApp\",\"pid\":1}'\n"
  ^ "exit 0\n"

let pp_err ppf (`Ptt_error message) = Format.pp_print_string ppf message

let absent_sentinels body =
  List.iter
    (fun needle ->
      Alcotest.(check bool) ("absent " ^ needle) false (contains needle body))
    [ "SENTINEL_TITLE"; "SENTINEL_MANIFEST"; "SENTINEL_OUTSIDE"; "SentinelApp" ]

let run_production ~mode ~cfg ~check_body () =
  let sandbox = Filename.temp_file "pie-explicit" "" in
  Sys.remove sandbox;
  Unix.mkdir sandbox 0o700;
  Fun.protect
    ~finally:(fun () -> rm_rf sandbox)
    (fun () ->
      let bin = Filename.concat sandbox "bin" in
      let pie_dir = Filename.concat sandbox ".pie" in
      let outside = Filename.concat sandbox "outside.txt" in
      Unix.mkdir bin 0o700;
      Unix.mkdir pie_dir 0o700;
      write_file outside "SENTINEL_OUTSIDE\n";
      write_file
        (Filename.concat sandbox "package.json")
        {|{"name":"SENTINEL_MANIFEST"}|};
      Unix.symlink outside (Filename.concat pie_dir "context-link.md");
      Unix.mkfifo (Filename.concat pie_dir "context.md") 0o600;
      write_file (Filename.concat bin "niri") niri_script;
      Unix.chmod (Filename.concat bin "niri") 0o700;
      let niri_log = Filename.concat sandbox "niri.log" in
      write_file niri_log "";
      let old_path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
      let old_cwd = Sys.getcwd () in
      Unix.putenv "PATH" (bin ^ ":" ^ old_path);
      Unix.putenv "PIE_EXPLICIT_NIRI_LOG" niri_log;
      Unix.putenv "OPENROUTER_API_KEY" "synthetic-explicit-key";
      Sys.chdir sandbox;
      let captured = ref "" in
      Fun.protect
        ~finally:(fun () ->
          Sys.chdir old_cwd;
          Unix.putenv "PATH" old_path)
        (fun () ->
          Eio_main.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let net = Eio.Stdenv.net env in
          let process_mgr = Eio.Stdenv.process_mgr env in
          let clock = Eio.Stdenv.clock env in
          let logger =
            Eta_observability.Logger.create ~service:"pie-explicit-prompts-test"
              ~clock
              ~mono_clock:(Eio.Stdenv.mono_clock env)
              ~reporter:
                (Eta_observability.Logger.Reporter.records (fun _ -> ()))
              ()
          in
          let socket =
            Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net
              (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
          in
          let port =
            match Eio.Net.listening_addr socket with
            | `Tcp (_, port) -> port
            | `Unix _ -> Alcotest.fail "expected TCP"
          in
          Unix.putenv "OPENROUTER_BASE_URL"
            (Printf.sprintf "http://127.0.0.1:%d" port);
          Eio.Fiber.fork ~sw (fun () ->
              Eio.Switch.run (fun conn_sw ->
                  let flow, _ = Eio.Net.accept ~sw:conn_sw socket in
                  captured := read_http_request flow;
                  write_http_json flow (completed_body "ok")));
          let result =
            Pie.Assistant.Service.production_stt ~sw ~net ~clock ~logger
              ~process_mgr cfg mode pcm
          in
          (match result with
          | Ok _ -> ()
          | Error cause ->
              Alcotest.failf "production_stt failed: %a" pp_err cause);
          check_body !captured;
          let log =
            match open_in_bin niri_log with
            | exception Sys_error _ -> ""
            | ic ->
                Fun.protect
                  ~finally:(fun () -> close_in_noerr ic)
                  (fun () -> really_input_string ic (in_channel_length ic))
          in
          Alcotest.(check string) "no recognition niri lookup" "" log))

let test_configured_prompt_reaches_backend () =
  let cfg =
    config ~transcription_prompt:"EXPLICIT_INSTRUCTION_TOKEN {{language}}"
      ~translation_prompt:"TRANSLATION_MUST_NOT_APPEAR {{target_language}}"
      ~transcription_model:"openrouter/explicit-model"
      ~translation_model:"openrouter/translation-must-not-appear"
      ~source_language:"Klingon" ~target_language:"Zulu"
  in
  run_production ~mode:Pie.Assistant.Service.Transcribe ~cfg
    ~check_body:(fun body ->
      Alcotest.(check bool)
        "explicit instruction" true
        (contains "EXPLICIT_INSTRUCTION_TOKEN" body);
      Alcotest.(check bool) "language" true (contains "Klingon" body);
      Alcotest.(check bool) "model" true (contains "explicit-model" body);
      Alcotest.(check bool)
        "translation prompt unused" false
        (contains "TRANSLATION_MUST_NOT_APPEAR" body);
      Alcotest.(check bool)
        "translation model unused" false
        (contains "translation-must-not-appear" body);
      absent_sentinels body)
    ()

let test_translation_prompt_reaches_backend () =
  let cfg =
    config ~transcription_prompt:"TRANSCRIPTION_MUST_NOT_APPEAR {{language}}"
      ~translation_prompt:
        "EXPLICIT_TRANSLATION_TOKEN from {{source_language}} to \
         {{target_language}}"
      ~transcription_model:"openrouter/transcribe-must-not-appear"
      ~translation_model:"openrouter/explicit-translation-model"
      ~source_language:"Klingon" ~target_language:"Zulu"
  in
  run_production ~mode:Pie.Assistant.Service.Translate ~cfg
    ~check_body:(fun body ->
      Alcotest.(check bool)
        "explicit translation instruction" true
        (contains "EXPLICIT_TRANSLATION_TOKEN" body);
      Alcotest.(check bool) "source language" true (contains "Klingon" body);
      Alcotest.(check bool) "target language" true (contains "Zulu" body);
      Alcotest.(check bool)
        "translation model" true
        (contains "explicit-translation-model" body);
      Alcotest.(check bool)
        "transcription prompt unused" false
        (contains "TRANSCRIPTION_MUST_NOT_APPEAR" body);
      Alcotest.(check bool)
        "transcription model unused" false
        (contains "transcribe-must-not-appear" body);
      Alcotest.(check bool)
        "unsubstituted target placeholder" false
        (contains "{{target_language}}" body);
      Alcotest.(check bool)
        "unsubstituted source placeholder" false
        (contains "{{source_language}}" body);
      absent_sentinels body)
    ()

let () =
  Alcotest.run "explicit_prompts"
    [
      ( "production_stt",
        [
          Alcotest.test_case
            "configured prompt reaches backend; sentinels excluded" `Quick
            test_configured_prompt_reaches_backend;
          Alcotest.test_case
            "translation prompt and target reach backend; transcription unused"
            `Quick test_translation_prompt_reaches_backend;
        ] );
    ]
