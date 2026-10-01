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

let transcribe_binding config =
  match
    List.find_opt
      (fun (binding : Config.binding) -> binding.mode = Config.Transcribe)
      config.Config.bindings
  with
  | Some binding -> binding
  | None -> Alcotest.fail "missing transcribe binding"

let translate_binding config =
  match
    List.find_opt
      (fun (binding : Config.binding) -> binding.mode = Config.Translate)
      config.Config.bindings
  with
  | Some binding -> binding
  | None -> Alcotest.fail "missing translate binding"

let contains needle hay =
  let n = String.length needle in
  let h = String.length hay in
  let rec loop i =
    i <= h - n && (n = 0 || String.sub hay i n = needle || loop (i + 1))
  in
  loop 0

let test_pass1_provider_prefix () =
  (* dict-m2xr *)
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path "transcription_model = \"codex-realtime/gpt-live-transcribe\"\n";
  let config = expect_ok (load env path) in
  let resolved =
    Config.resolve_model_provider ~default_provider:config.provider
      config.transcription_model
  in
  Alcotest.(check bool)
    "codex provider" true
    (resolved.provider = Config.Codex_realtime);
  Alcotest.(check string) "codex model" "gpt-live-transcribe" resolved.model;
  write path
    "provider = \"codex-realtime\"\n\
     transcription_model = \"openrouter/microsoft/mai-transcribe-2\"\n";
  let config = expect_ok (load env path) in
  let resolved =
    Config.resolve_model_provider ~default_provider:config.provider
      config.transcription_model
  in
  Alcotest.(check bool)
    "openrouter provider" true
    (resolved.provider = Config.Openrouter);
  Alcotest.(check string)
    "openrouter model" "microsoft/mai-transcribe-2" resolved.model

let test_streaming_switch () =
  (* dict-q6xd dict-i28t *)
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path "";
  let config = expect_ok (load env path) in
  Alcotest.(check bool) "codex streams by default" true config.streaming;
  write path "provider = \"openrouter\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check bool) "openrouter buffers by default" false config.streaming;
  write path "streaming = false\n";
  let config = expect_ok (load env path) in
  Alcotest.(check bool) "explicit buffered" false config.streaming;
  write path
    "provider = \"openrouter\"\n\
     transcription_model = \"codex-realtime/gpt-live-transcribe\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check bool)
    "default follows resolved pass-1 provider" true config.streaming;
  List.iter
    (fun text ->
      write path text;
      expect_error ~needle:"openrouter" (load env path))
    [
      "provider = \"openrouter\"\nstreaming = true\n";
      "pipeline = \"legacy\"\nprovider = \"openrouter\"\nstreaming = true\n";
      "transcription_model = \"openrouter/microsoft/mai-transcribe-2\"\n\
       streaming = true\n";
    ]

let test_correction_settings () =
  (* dict-l4zp *)
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path
    "pipeline = \"two-pass\"\n\
     correction_model = \"openrouter/google/gemini-3.8-flash\"\n\
     correction_reasoning_effort = \"low\"\n\
     correction_timeout_seconds = 3\n\
     correction_temperature = 0\n";
  let config = expect_ok (load env path) in
  (match config.correction with
  | None -> Alcotest.fail "expected correction"
  | Some correction ->
      Alcotest.(check string)
        "resolved model" "google/gemini-3.8-flash" correction.model;
      Alcotest.(check (option string))
        "effort" (Some "low") correction.reasoning_effort;
      Alcotest.(check (float 0.00001)) "timeout" 3. correction.timeout_seconds;
      Alcotest.(check (float 0.00001)) "temperature" 0. correction.temperature);
  write path
    "pipeline = \"two-pass\"\n\
     correction_model = \"google/gemini-3.8-flash\"\n\
     correction_reasoning_effort = \"none\"\n\
     correction_temperature = 2\n";
  let config = expect_ok (load env path) in
  (match config.correction with
  | None -> Alcotest.fail "expected bare-model correction"
  | Some correction ->
      Alcotest.(check string)
        "bare model stays openrouter id" "google/gemini-3.8-flash"
        correction.model;
      Alcotest.(check (option string))
        "none is kept" (Some "none") correction.reasoning_effort;
      Alcotest.(check (float 0.00001))
        "default timeout" Config.default_correction_timeout_seconds
        correction.timeout_seconds;
      Alcotest.(check (float 0.00001))
        "upper temperature" 2. correction.temperature);
  List.iter
    (fun (text, needle) ->
      write path text;
      expect_error ~needle (load env path))
    [
      ( "correction_model = \"codex-realtime/gpt-realtime-2.1\"\n",
        "codex-realtime" );
      ("correction_model = \"\"\n", "empty");
      ("correction_reasoning_effort = \"ultra\"\n", "correction_reasoning");
      ("correction_timeout_seconds = 0\n", "positive");
      ("correction_timeout_seconds = -1\n", "positive");
      ("correction_temperature = 2.1\n", "0.0-2.0");
      ("correction_temperature = -0.1\n", "0.0-2.0");
      ("pipeline = \"yes\"\n", "pipeline");
      ("two_pass = true\n", "replaced");
    ]

let test_legacy_keys_become_bindings () =
  (* dict-ezcr *)
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path
    "ptt_transcribe_keysym = 11\n\
     ptt_transcribe_shift = true\n\
     ptt_translate_keysym = 12\n\
     ptt_translate_shift = true\n";
  let config = expect_ok (load env path) in
  Alcotest.(check int) "bindings" 2 (List.length config.bindings);
  let transcribe = transcribe_binding config in
  let translate = translate_binding config in
  Alcotest.(check int) "transcribe key" 11 transcribe.keysym;
  Alcotest.(check bool) "transcribe shift" true transcribe.shift;
  Alcotest.(check bool) "transcribe correct" false transcribe.correct;
  Alcotest.(check int) "translate key" 12 translate.keysym;
  Alcotest.(check bool) "translate shift" true translate.shift;
  Alcotest.(check bool)
    "lookup" true
    (Config.find_binding config ~keysym:11 ~shift:true = Some transcribe)

let test_bindings () =
  (* dict-wrbh *)
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  let prompts = Filename.concat dir "prompts" in
  Unix.mkdir prompts 0o700;
  write (Filename.concat prompts "raw.md") "Raw {{glossary}}";
  write path
    "pipeline = \"two-pass\"\n\
     correction_model = \"google/gemini-3.8-flash\"\n\
     ptt_transcribe_keysym = 99\n\
     ptt_translate_keysym = 98\n\
     [[bindings]]\n\
     keysym = 11\n\
     shift = true\n\
     mode = \"transcribe\"\n\
     correct = false\n\
     prompt = \"raw.md\"\n\
     [[bindings]]\n\
     keysym = 11\n\
     mode = \"translate\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check int) "authoritative count" 2 (List.length config.bindings);
  Alcotest.(check int)
    "legacy transcribe follows first binding" 11 config.ptt_transcribe_keysym;
  Alcotest.(check bool) "legacy shift" true config.ptt_transcribe_shift;
  Alcotest.(check int)
    "legacy translate follows first translate" 11 config.ptt_translate_keysym;
  let transcribe = transcribe_binding config in
  Alcotest.(check bool) "explicit correct" false transcribe.correct;
  Alcotest.(check bool) "named prompt" true transcribe.prompt_named;
  Alcotest.(check string) "named text" "Raw {{glossary}}" transcribe.prompt;
  let translate = translate_binding config in
  Alcotest.(check bool)
    "correct defaults true when a model is set" true translate.correct;
  write path
    "ptt_translate_keysym = 98\n\
     [[bindings]]\n\
     keysym = 11\n\
     mode = \"transcribe\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check int) "one binding" 1 (List.length config.bindings);
  Alcotest.(check int)
    "missing translate keeps built-in default" 65479 config.ptt_translate_keysym;
  Alcotest.(check bool)
    "file translate key ignored" true
    (config.ptt_translate_keysym <> 98);
  List.iter
    (fun (text, needle) ->
      write path text;
      expect_error ~needle (load env path))
    [
      ( "[[bindings]]\n\
         keysym = 11\n\
         mode = \"transcribe\"\n\
         [[bindings]]\n\
         keysym = 11\n\
         mode = \"translate\"\n",
        "duplicate" );
      ("[[bindings]]\nkeysym = 0\nmode = \"transcribe\"\n", "positive");
      ("[[bindings]]\nmode = \"transcribe\"\n", "required");
      ("[[bindings]]\nkeysym = 11\n", "mode");
      ("[[bindings]]\nkeysym = 11\nmode = \"nope\"\n", "transcribe or translate");
      ( "[[bindings]]\n\
         keysym = 11\n\
         mode = \"transcribe\"\n\
         prompt = \"../secret.md\"\n",
        "file name" );
      ( "[[bindings]]\nkeysym = 11\nmode = \"transcribe\"\nprompt = \"\"\n",
        "empty" );
      ( "[[bindings]]\nkeysym = 11\nmode = \"transcribe\"\nprompt = \"..\"\n",
        "file name" );
      ("bindings = \"no\"\n", "array of tables");
      ( "pipeline = \"two-pass\"\n\
         [[bindings]]\n\
         keysym = 11\n\
         mode = \"translate\"\n",
        "correction_model" );
      ( "pipeline = \"two-pass\"\n\
         [[bindings]]\n\
         keysym = 11\n\
         mode = \"transcribe\"\n\
         correct = true\n",
        "correction_model" );
      ("pipeline = \"two-pass\"\n", "correction_model");
    ];
  write path
    "pipeline = \"two-pass\"\n\
     [[bindings]]\n\
     keysym = 11\n\
     mode = \"transcribe\"\n\
     correct = false\n";
  let config = expect_ok (load env path) in
  Alcotest.(check bool)
    "raw transcribe needs no model" false (transcribe_binding config).correct;
  write path
    "pipeline = \"legacy\"\n[[bindings]]\nkeysym = 11\nmode = \"translate\"\n";
  ignore (expect_ok (load env path))

let test_pass2_prompts () =
  (* dict-g52m dict-4faf *)
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path
    "pipeline = \"two-pass\"\ncorrection_model = \"google/gemini-3.8-flash\"\n";
  let config = expect_ok (load env path) in
  let transcribe = transcribe_binding config in
  let translate = translate_binding config in
  Alcotest.(check string)
    "embedded correction" Config.default_correction_prompt transcribe.prompt;
  Alcotest.(check string)
    "embedded translation" Config.default_correction_translate_prompt
    translate.prompt;
  Alcotest.(check string)
    "default transcribe file" Config.default_correction_prompt_file
    transcribe.prompt_file;
  Alcotest.(check bool)
    "shipped correction has glossary" true
    (contains "{{glossary}}" Config.default_correction_prompt);
  Alcotest.(check bool)
    "shipped correction does not claim context is appended" false
    (contains "appended" Config.default_correction_prompt
    || contains "project context" Config.default_correction_prompt);
  Alcotest.(check bool)
    "shipped translation placeholders" true
    (contains "{{glossary}}" Config.default_correction_translate_prompt
    && contains "{{source_language}}" Config.default_correction_translate_prompt
    && contains "{{target_language}}" Config.default_correction_translate_prompt
    );
  let prompts = Filename.concat dir "prompts" in
  Unix.mkdir prompts 0o700;
  write
    (Filename.concat prompts "correction.md")
    "Custom glossary {{glossary}}.";
  write
    (Filename.concat prompts "correction-translate.md")
    "From {{source_language}} to {{target_language}} {{glossary}}";
  let config = expect_ok (load env path) in
  Alcotest.(check string)
    "file overrides embed" "Custom glossary {{glossary}}."
    (transcribe_binding config).prompt;
  write (Filename.concat prompts "correction.md") "no placeholders";
  expect_error ~needle:"{{glossary}}" (load env path);
  write
    (Filename.concat prompts "correction.md")
    "Custom glossary {{glossary}}.";
  write (Filename.concat prompts "named.md") "Named {{glossary}}";
  write path
    "pipeline = \"two-pass\"\n\
     correction_model = \"google/gemini-3.8-flash\"\n\
     [[bindings]]\n\
     keysym = 11\n\
     mode = \"transcribe\"\n\
     prompt = \"named.md\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check string)
    "named prompt" "Named {{glossary}}" (transcribe_binding config).prompt;
  write path
    "pipeline = \"two-pass\"\n\
     correction_model = \"google/gemini-3.8-flash\"\n\
     [[bindings]]\n\
     keysym = 11\n\
     mode = \"transcribe\"\n\
     prompt = \"missing.md\"\n";
  expect_error ~needle:"no such file" (load env path);
  write
    (Filename.concat prompts "correction-translate.md")
    "missing a placeholder {{glossary}}";
  write path
    "pipeline = \"two-pass\"\ncorrection_model = \"google/gemini-3.8-flash\"\n";
  expect_error ~needle:"{{source_language}}" (load env path)

let test_two_pass_off_is_legacy () =
  (* dict-26tm *)
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  let prompts = Filename.concat dir "prompts" in
  Unix.mkdir prompts 0o700;
  write
    (Filename.concat prompts "transcription.md")
    "Transcribe in {{language}}. LEGACY";
  write
    (Filename.concat prompts "translation.md")
    "Translate {{source_language}} to {{target_language}}. LEGACY";
  write
    (Filename.concat prompts "correction.md")
    "invalid pass-2 prompt that must not be read";
  write path "provider = \"codex-realtime\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check string)
    "pipeline default" "legacy"
    (Config.string_of_pipeline config.pipeline);
  Alcotest.(check string)
    "legacy model" "gpt-realtime-2.1" config.transcription_model;
  Alcotest.(check string)
    "legacy transcription prompt" "Transcribe in {{language}}. LEGACY"
    config.transcription_prompt;
  Alcotest.(check string)
    "legacy translation prompt"
    "Translate {{source_language}} to {{target_language}}. LEGACY"
    config.translation_prompt;
  Alcotest.(check int) "legacy key" 65478 config.ptt_transcribe_keysym;
  Alcotest.(check bool)
    "invalid correction.md ignored" true
    ((transcribe_binding config).prompt = Config.default_correction_prompt);
  Unix.unlink (Filename.concat prompts "transcription.md");
  Unix.unlink (Filename.concat prompts "translation.md");
  write path "";
  let config = expect_ok (load env path) in
  Alcotest.(check string)
    "inline transcription default"
    "Transcribe the following audio in {{language}}."
    config.transcription_prompt

let test_status_json_fields () =
  (* dict-oagf *)
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path
    "pipeline = \"two-pass\"\n\
     streaming = true\n\
     correction_model = \"google/gemini-3.8-flash\"\n\
     correction_reasoning_effort = \"minimal\"\n\
     correction_timeout_seconds = 4\n\
     [[bindings]]\n\
     keysym = 11\n\
     shift = true\n\
     mode = \"transcribe\"\n\
     correct = true\n\
     prompt = \"correction.md\"\n";
  let prompts = Filename.concat dir "prompts" in
  Unix.mkdir prompts 0o700;
  write (Filename.concat prompts "correction.md") "Keep {{glossary}}";
  let config = expect_ok (load env path) in
  let json = Config.door_spec.yojson_of_t config in
  let member name =
    match Yojson.Safe.Util.member name json with
    | `Null -> Alcotest.failf "missing %s" name
    | value -> value
  in
  Alcotest.(check bool) "pipeline" true (member "pipeline" = `String "two-pass");
  Alcotest.(check bool)
    "provider order absent" true
    (Yojson.Safe.Util.member "openrouter_provider_order" json = `Null);
  Alcotest.(check bool) "streaming" true (member "streaming" = `Bool true);
  Alcotest.(check bool)
    "model" true
    (member "correction_model" = `String "google/gemini-3.8-flash");
  Alcotest.(check bool)
    "effort" true
    (member "correction_reasoning_effort" = `String "minimal");
  Alcotest.(check bool)
    "timeout" true
    (member "correction_timeout_seconds" = `Float 4.);
  match member "bindings" with
  | `List [ binding ] ->
      Alcotest.(check bool)
        "keysym" true
        (Yojson.Safe.Util.member "keysym" binding = `Int 11);
      Alcotest.(check bool)
        "shift" true
        (Yojson.Safe.Util.member "shift" binding = `Bool true);
      Alcotest.(check bool)
        "mode" true
        (Yojson.Safe.Util.member "mode" binding = `String "transcribe");
      Alcotest.(check bool)
        "correct" true
        (Yojson.Safe.Util.member "correct" binding = `Bool true);
      Alcotest.(check bool)
        "prompt file" true
        (Yojson.Safe.Util.member "prompt_file" binding = `String "correction.md")
  | _ -> Alcotest.fail "expected one binding"

let test_one_pass_config () =
  (* dict-rtdw dict-gi5u dict-4y4r dict-dq3o *)
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  let prompts = Filename.concat dir "prompts" in
  Unix.mkdir prompts 0o700;
  write path
    "pipeline = \"one-pass\"\n\
     transcription_model = \"openrouter/google/gemini-3.8-flash\"\n\
     translation_model = \"openrouter/google/gemini-3.8-flash\"\n\
     thinking_effort = \"low\"\n\
     openrouter_provider_order = [\"Google AI Studio\", \"Google\"]\n\
     one_pass_timeout_seconds = 45\n\
     [[bindings]]\n\
     keysym = 11\n\
     shift = true\n\
     mode = \"transcribe\"\n\
     correct = true\n\
     [[bindings]]\n\
     keysym = 11\n\
     mode = \"transcribe\"\n\
     correct = false\n\
     [[bindings]]\n\
     keysym = 12\n\
     mode = \"translate\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check string)
    "pipeline" "one-pass"
    (Config.string_of_pipeline config.pipeline);
  Alcotest.(check (option (list string)))
    "provider order"
    (Some [ "Google AI Studio"; "Google" ])
    config.openrouter_provider_order;
  Alcotest.(check (float 0.00001)) "timeout" 45. config.one_pass_timeout_seconds;
  Alcotest.(check (option string)) "effort" (Some "low") config.thinking_effort;
  let tidy = List.find (fun (b : Config.binding) -> b.shift) config.bindings in
  let raw =
    List.find
      (fun (b : Config.binding) -> (not b.shift) && b.mode = Config.Transcribe)
      config.bindings
  in
  let translate = translate_binding config in
  Alcotest.(check string)
    "tidy file" Config.default_one_pass_prompt_file tidy.prompt_file;
  Alcotest.(check string)
    "tidy prompt" Config.default_one_pass_prompt tidy.prompt;
  Alcotest.(check string)
    "verbatim file" Config.default_one_pass_verbatim_prompt_file raw.prompt_file;
  Alcotest.(check string)
    "verbatim prompt" Config.default_one_pass_verbatim_prompt raw.prompt;
  Alcotest.(check string)
    "translate file" Config.default_one_pass_translate_prompt_file
    translate.prompt_file;
  Alcotest.(check string)
    "translate prompt" Config.default_one_pass_translate_prompt translate.prompt;
  Alcotest.(check bool)
    "verbatim has glossary" true
    (contains "{{glossary}}" Config.default_one_pass_verbatim_prompt);
  write (Filename.concat prompts "one-pass-verbatim.md") "no placeholders";
  expect_error ~needle:"{{glossary}}" (load env path);
  write
    (Filename.concat prompts "named.md")
    "Named {{glossary}} {{source_language}} {{target_language}}";
  write path
    "pipeline = \"one-pass\"\n\
     transcription_model = \"openrouter/google/gemini-3.8-flash\"\n\
     translation_model = \"openrouter/google/gemini-3.8-flash\"\n\
     [[bindings]]\n\
     keysym = 11\n\
     mode = \"transcribe\"\n\
     correct = false\n\
     prompt = \"named.md\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check string)
    "named override"
    "Named {{glossary}} {{source_language}} {{target_language}}"
    (transcribe_binding config).prompt;
  Unix.unlink (Filename.concat prompts "one-pass-verbatim.md");
  write path
    "pipeline = \"one-pass\"\n\
     transcription_model = \"openrouter/google/gemini-3.8-flash\"\n\
     translation_model = \"openrouter/google/gemini-3.8-flash\"\n";
  let config = expect_ok (load env path) in
  Alcotest.(check (float 0.00001))
    "default timeout" Config.default_one_pass_timeout_seconds
    config.one_pass_timeout_seconds;
  Alcotest.(check bool)
    "order absent" true
    (config.openrouter_provider_order = None);
  let json = Config.door_spec.yojson_of_t config in
  Alcotest.(check bool)
    "status pipeline" true
    (Yojson.Safe.Util.member "pipeline" json = `String "one-pass");
  List.iter
    (fun (text, needle) ->
      write path text;
      expect_error ~needle (load env path))
    [
      ( "pipeline = \"one-pass\"\n\
         transcription_model = \"codex-realtime/gpt-live-transcribe\"\n",
        "codex-realtime" );
      ( "pipeline = \"one-pass\"\n\
         transcription_model = \"openrouter/google/gemini-3.8-flash\"\n\
         translation_model = \"gpt-realtime-2.1\"\n\
         provider = \"codex-realtime\"\n",
        "translation_model" );
      ( "pipeline = \"one-pass\"\n\
         transcription_model = \"openrouter/google/gemini-3.8-flash\"\n\
         translation_model = \"openrouter/google/gemini-3.8-flash\"\n\
         streaming = true\n",
        "one-pass" );
      ("openrouter_provider_order = \"Google\"\n", "array of strings");
      ("openrouter_provider_order = [\"\"]\n", "must not be empty");
      ("one_pass_timeout_seconds = 0\n", "positive");
      ("pipeline = \"both\"\n", "pipeline");
    ]

let load_with_warnings env path =
  let warnings = ref [] in
  let warn message = warnings := message :: !warnings in
  let result =
    Config.load ~caps ~fs:(Eio.Stdenv.fs env) ~source:(`Explicit path) ~warn ()
  in
  (result, List.rev !warnings)

let editor_window : Pie.Niri.window_info =
  {
    id = 42;
    title = Some "Editor";
    app_id = Some "org.test";
    pid = None;
    workspace_id = None;
  }

let assert_paste_for window =
  let pasted = ref false in
  match
    Pie.Input.Text_injection.deliver
      ~copy:(fun _text -> Ok ())
      ~paste:(fun () ->
        pasted := true;
        Ok ())
      ~newline:(fun _action -> Ok ())
      ~focused_window:(fun () -> Ok (Some window))
      ~target_window:(Some window) ~newline_rules:[] "hello"
  with
  | Ok (_, Pie.Input.Text_injection.Clipboard_key_attempted) ->
      Alcotest.(check bool) "paste callback ran" true !pasted
  | Ok _ -> Alcotest.fail "deliver copied without pasting"
  | Error _ -> Alcotest.fail "deliver failed"

let test_text_entry_paste () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path "[[text_entry_rules]]\napp_id = \"org.test\"\nmethod = \"paste\"\n";
  let config, warnings = load_with_warnings env path in
  let config = expect_ok config in
  Alcotest.(check int) "no warning" 0 (List.length warnings);
  match config.text_entry_rules with
  | [ rule ] -> Alcotest.(check bool) "paste" true (rule.method_ = Config.Paste)
  | _ -> Alcotest.fail "expected one text entry rule"

let test_text_entry_type_pastes () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path
    "[[text_entry_rules]]\n\
     app_id = \"org.test\"\n\
     title_regex = \"Editor\"\n\
     method = \"type\"\n";
  let config, warnings = load_with_warnings env path in
  let config = expect_ok config in
  let expected =
    "text_entry_rules[0] (app_id=\"org.test\", title_regex=\"Editor\"): method \
     \"type\" is no longer supported; the rule pastes"
  in
  (match warnings with
  | [ warning ] -> Alcotest.(check string) "warning" expected warning
  | _ -> Alcotest.failf "expected one warning, got %d" (List.length warnings));
  match config.text_entry_rules with
  | [ rule ] ->
      Alcotest.(check bool) "loaded as paste" true (rule.method_ = Config.Paste);
      Alcotest.(check bool)
        "window selects paste" true
        (Pie.Input.Text_injection.entry_method_for_window
           config.text_entry_rules (Some editor_window)
        = Config.Paste);
      assert_paste_for editor_window
  | _ -> Alcotest.fail "expected one text entry rule"

let test_text_entry_type_title_only () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path
    "[[text_entry_rules]]\ntitle_regex = \"Editor\"\nmethod = \"type\"\n";
  let config, warnings = load_with_warnings env path in
  let config = expect_ok config in
  let expected =
    "text_entry_rules[0] (title_regex=\"Editor\"): method \"type\" is no \
     longer supported; the rule pastes"
  in
  (match warnings with
  | [ warning ] -> Alcotest.(check string) "title warning" expected warning
  | _ -> Alcotest.failf "expected one warning, got %d" (List.length warnings));
  match config.text_entry_rules with
  | [ rule ] -> Alcotest.(check bool) "paste" true (rule.method_ = Config.Paste)
  | _ -> Alcotest.fail "expected one text entry rule"

let test_text_entry_unknown_method () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path "[[text_entry_rules]]\napp_id = \"org.test\"\nmethod = \"nope\"\n";
  let result, warnings = load_with_warnings env path in
  expect_error ~needle:"unknown text entry method" result;
  Alcotest.(check int) "no warning on reject" 0 (List.length warnings)

let test_text_entry_rules_absent () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  let path = Filename.concat dir "stt.toml" in
  write path "provider = \"openrouter\"\n";
  let config, warnings = load_with_warnings env path in
  let config = expect_ok config in
  Alcotest.(check int) "no rules" 0 (List.length config.text_entry_rules);
  Alcotest.(check int) "no warning" 0 (List.length warnings)

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
          Alcotest.test_case "dict-m2xr pass-1 provider prefix" `Quick
            test_pass1_provider_prefix;
          Alcotest.test_case "dict-q6xd dict-i28t streaming switch" `Quick
            test_streaming_switch;
          Alcotest.test_case "dict-l4zp correction settings" `Quick
            test_correction_settings;
          Alcotest.test_case "dict-ezcr legacy keys become bindings" `Quick
            test_legacy_keys_become_bindings;
          Alcotest.test_case "dict-wrbh bindings" `Quick test_bindings;
          Alcotest.test_case "dict-g52m dict-4faf pass-2 prompts" `Quick
            test_pass2_prompts;
          Alcotest.test_case "dict-26tm two_pass off is legacy" `Quick
            test_two_pass_off_is_legacy;
          Alcotest.test_case "dict-oagf status JSON fields" `Quick
            test_status_json_fields;
          Alcotest.test_case "dict-rtdw/gi5u/4y4r/dq3o one-pass config" `Quick
            test_one_pass_config;
          Alcotest.test_case "text entry paste" `Quick test_text_entry_paste;
          Alcotest.test_case "retired type method pastes" `Quick
            test_text_entry_type_pastes;
          Alcotest.test_case "retired type method names title" `Quick
            test_text_entry_type_title_only;
          Alcotest.test_case "unknown text entry method" `Quick
            test_text_entry_unknown_method;
          Alcotest.test_case "absent text entry rules" `Quick
            test_text_entry_rules_absent;
        ] );
    ]
