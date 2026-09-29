let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

module Config = Pie.Config.Stt
module Door = Ox_app.Config

let with_sandbox f =
  let dir = Filename.temp_file "pie-config-boundary" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  let rec remove path =
    match (Unix.lstat path).Unix.st_kind with
    | Unix.S_DIR ->
        Array.iter
          (fun name -> remove (Filename.concat path name))
          (Sys.readdir path);
        Unix.rmdir path
    | _ -> Unix.unlink path
  in
  Fun.protect ~finally:(fun () -> remove dir) (fun () -> f dir)

let write path body =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc body)

let load env path =
  Config.load ~caps ~fs:(Eio.Stdenv.fs env) ~source:(`Explicit path) ()

let expect_ok = function
  | Ok config -> config
  | Error (`Config message) ->
      Alcotest.failf "unexpected config error: %s" message

let expect_error ~needle = function
  | Error (`Config message) ->
      let n = String.length needle in
      let found = ref false in
      for i = 0 to String.length message - n do
        if String.sub message i n = needle then found := true
      done;
      Alcotest.(check bool) ("error contains " ^ needle) true !found
  | Ok _ -> Alcotest.failf "expected config error containing %s" needle

let test_required_source () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  expect_error ~needle:"no such file" (load env path);
  expect_error ~needle:"no such file"
    (Config.load ~caps ~fs:(Eio.Stdenv.fs env) ~source:(`Default path) ())

let test_filename_and_env_path () =
  let path =
    Door.path_with ~file_name:"stt.toml" ~name:"pie" ~config:None
      ~getenv:(function
      | "XDG_CONFIG_HOME" -> Some "/tmp/pie-test-xdg"
      | _ -> None)
  in
  Alcotest.(check string)
    "stt.toml retained" "/tmp/pie-test-xdg/pie/stt.toml" path;
  let chosen =
    Door.resolve_with ~file_name:"stt.toml" ~name:"pie"
      ~config:(Some "/tmp/custom.toml") ~getenv:(fun _ -> None)
  in
  Alcotest.(check bool)
    "operator path required" true
    (chosen = `Explicit "/tmp/custom.toml")

let test_defaults_and_models () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path
    "provider = \"openrouter\"\n\
     translation_model = \"codex-realtime/gpt-realtime-2.1\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check bool) "provider" true (config.provider = Config.Openrouter);
  let model =
    Config.resolve_model_provider ~default_provider:config.provider
      config.translation_model
  in
  Alcotest.(check bool)
    "model override" true
    (model.provider = Config.Codex_realtime);
  Alcotest.(check string) "model" "gpt-realtime-2.1" model.model

let test_invalid_settings () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  List.iter
    (fun (text, needle) ->
      write path text;
      expect_error ~needle (load env path))
    [
      ("provider = \"unsupported\"\n", "unknown provider");
      ("wakeword_enabled = true\n", "unsupported wakeword");
      ("ptt_min_capture_peak = 1.5\n", "finite value");
      ("ptt_transcribe_keysym = 0\n", "must be positive");
      ("ptt_transcribe_keysym = 65479\n", "bindings must differ");
      ("[[newline_rules]]\naction = \"collapse\"\n", "app_id or title_regex");
      ("thinking_effort = \"ultra\"\n", "thinking_effort");
    ]

let test_rules () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path
    "[[newline_rules]]\n\
     app_id = \"org.test\"\n\
     action = \"shift-enter\"\n\
     [[text_entry_rules]]\n\
     app_id = \"org.test\"\n\
     method = \"paste\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check int) "newline rule" 1 (List.length config.newline_rules);
  Alcotest.(check int) "entry rule" 1 (List.length config.text_entry_rules)

let test_prompt_resolution () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path "provider = \"codex-realtime\"\n";
  let prompts = Filename.concat dir "prompts" in
  Unix.mkdir prompts 0o700;
  write
    (Filename.concat prompts "transcription.md")
    "Transcribe in {{language}}. CUSTOM";
  write
    (Filename.concat prompts "translation.md")
    "Translate {{source_language}} to {{target_language}}. CUSTOM";
  let config = expect_ok (load env path) in
  Alcotest.(check string)
    "relative transcription prompt" "Transcribe in {{language}}. CUSTOM"
    config.transcription_prompt;
  Alcotest.(check string)
    "relative translation prompt"
    "Translate {{source_language}} to {{target_language}}. CUSTOM"
    config.translation_prompt;
  write (Filename.concat prompts "translation.md") "missing placeholders";
  expect_error ~needle:"prompt" (load env path)

let test_nonregular_rejected () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  Unix.mkdir path 0o700;
  expect_error ~needle:"not a regular file" (load env path);
  Unix.rmdir path;
  let target = Filename.concat dir "target.toml" in
  write target "";
  Unix.symlink target path;
  ignore (expect_ok (load env path))

let test_byte_limits () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path (String.make (Config.max_config_file_bytes + 1) ' ');
  expect_error ~needle:"byte limit" (load env path);
  write path "";
  let prompts = Filename.concat dir "prompts" in
  Unix.mkdir prompts 0o700;
  write
    (Filename.concat prompts "transcription.md")
    (String.make (Config.max_config_file_bytes + 1) 'x');
  expect_error ~needle:"byte limit" (load env path)

let test_legacy_json_hint () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write (Filename.concat dir "stt.json") "{}";
  expect_error ~needle:"migrate legacy JSON" (load env path)

let test_door_spec () =
  let parse = Config.door_spec.of_yojson in
  let config =
    expect_ok (Result.map_error (fun msg -> `Config msg) (parse (`Assoc [])))
  in
  let json = Config.door_spec.yojson_of_t config in
  Alcotest.(check bool)
    "spec serializes provider" true
    (Yojson.Safe.Util.member "provider" json = `String "codex-realtime");
  let recoded =
    expect_ok (Result.map_error (fun msg -> `Config msg) (parse json))
  in
  Alcotest.(check bool) "spec roundtrip" true (config = recoded);
  match parse (`Assoc [ ("provider", `Int 1) ]) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "invalid provider accepted"

let () =
  Alcotest.run "config_boundary"
    [
      ( "door",
        [
          Alcotest.test_case "required default and explicit paths" `Quick
            test_required_source;
          Alcotest.test_case "stt.toml path and explicit source" `Quick
            test_filename_and_env_path;
          Alcotest.test_case "defaults and model overrides" `Quick
            test_defaults_and_models;
          Alcotest.test_case "invalid settings" `Quick test_invalid_settings;
          Alcotest.test_case "typed rule arrays" `Quick test_rules;
          Alcotest.test_case "relative prompt paths and validation" `Quick
            test_prompt_resolution;
          Alcotest.test_case "regular-file boundary" `Quick
            test_nonregular_rejected;
          Alcotest.test_case "1 MiB config and prompt caps" `Quick
            test_byte_limits;
          Alcotest.test_case "legacy filename guidance" `Quick
            test_legacy_json_hint;
          Alcotest.test_case "Door spec codec" `Quick test_door_spec;
        ] );
    ]
