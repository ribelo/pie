module D = Pie.Assistant.Dictation
module H = Pie.History
module C = Pie.Config.Stt

let base_config ?(provider = Pie.Config.Stt.Openrouter)
    ?(model = "openrouter/owner-a") () : Pie.Config.Stt.t =
  {
    provider;
    transcription_model = model;
    translation_model = model;
    transcription_language = "English";
    translation_source_language = "English";
    translation_target_language = "English";
    ptt_min_capture_rms = 0.0;
    ptt_min_capture_peak = 0.0;
    ptt_transcribe_keysym = 65478;
    ptt_translate_keysym = 65479;
    ptt_transcribe_shift = false;
    ptt_translate_shift = false;
    thinking_effort = None;
    transcription_prompt = "Transcribe in {{language}}.";
    translation_prompt =
      "Translate from {{source_language}} to {{target_language}}.";
    newline_rules = [];
    text_entry_rules = [];
    pipeline = Legacy;
    openrouter_provider_order = None;
    one_pass_timeout_seconds = 30.;
    streaming =
      (match provider with
      | Pie.Config.Stt.Codex_realtime -> true
      | Pie.Config.Stt.Openrouter -> false);
    correction = None;
    audio_retention_hours = 24.;
    bindings =
      [
        {
          keysym = 65478;
          shift = false;
          mode = Pie.Config.Stt.Transcribe;
          correct = false;
          prompt_named = false;
          prompt_file = "correction.md";
          prompt = "{{glossary}}";
        };
        {
          keysym = 65479;
          shift = false;
          mode = Pie.Config.Stt.Translate;
          correct = false;
          prompt_named = false;
          prompt_file = "correction-translate.md";
          prompt = "{{glossary}} {{source_language}} {{target_language}}";
        };
      ];
  }

let binding ?(mode = C.Transcribe) ?(correct = true) ?(keysym = 65478) () :
    C.binding =
  {
    keysym;
    shift = false;
    mode;
    correct;
    prompt_named = false;
    prompt_file = "binding.md";
    prompt = "{{glossary}} {{source_language}} {{target_language}}";
  }

let config binding =
  {
    (base_config ()) with
    pipeline = Two_pass;
    openrouter_provider_order = None;
    one_pass_timeout_seconds = 30.;
    bindings = [ binding ];
    translation_model = "openrouter/WRONG-TRANSLATION";
    transcription_language = "Polish";
    translation_source_language = "Polish";
    translation_target_language = "English";
    correction =
      Some
        {
          model = "openrouter/corrector";
          reasoning_effort = Some "high";
          timeout_seconds = 4.5;
          temperature = 0.3;
        };
  }

type fixture = {
  mutable trace : string list;
  mutable raw : H.transcribed list;
  mutable finished : H.finished list;
  mutable delivered : string list;
  mutable texts : (string * bool) list;
  mutable audios : int;
  mutable warnings : int;
  mutable notices : int;
  mutable resolutions : int;
  mutable passes : (D.pass * string * int) list;
  mutable time : float;
}

let fixture () =
  {
    trace = [];
    raw = [];
    finished = [];
    delivered = [];
    texts = [];
    audios = 0;
    warnings = 0;
    notices = 0;
    resolutions = 0;
    passes = [];
    time = 10.;
  }

let push f name = f.trace <- f.trace @ [ name ]

let dependencies ?(history_error = false) ?(pass1 = Ok "  surowy tekst  ")
    ?(pass2 = Ok "  corrected  ")
    ?(one_pass = fun _ _ _ _ -> Alcotest.fail "two-pass must not call one-pass")
    f : D.dependencies =
  let history_result () =
    if history_error then Error (`History "read-only") else Ok ()
  in
  {
    resolve_glossary =
      (fun () ->
        f.resolutions <- f.resolutions + 1;
        ( {
            Pie.Glossary.text = "PROJECT TERM";
            paths = [ "/global"; "/project" ];
            warnings = [];
            cwd = Some "/project";
          },
          Some { H.app_id = Some "editor"; title = Some "press title" } ));
    pass1 =
      (fun cfg b glossary pcm ->
        push f "pass1";
        f.time <- f.time +. 0.1;
        Alcotest.(check string)
          "pass1 transcription model" "openrouter/owner-a"
          cfg.C.transcription_model;
        Alcotest.(check string)
          "press glossary" "PROJECT TERM" glossary.Pie.Glossary.text;
        Alcotest.(check string)
          "source language" "Polish"
          (match b.C.mode with
          | C.Transcribe -> cfg.transcription_language
          | Translate -> cfg.translation_source_language);
        Alcotest.(check string) "full PCM" "PCM" (Bytes.to_string pcm);
        pass1);
    pass2 =
      (fun cfg b glossary raw ->
        push f "pass2";
        f.time <- f.time +. 0.2;
        Alcotest.(check string) "same raw" "  surowy tekst  " raw;
        Alcotest.(check string)
          "same glossary" "PROJECT TERM" glossary.Pie.Glossary.text;
        Alcotest.(check string)
          "chosen binding prompt"
          "{{glossary}} {{source_language}} {{target_language}}" b.C.prompt;
        (match cfg.C.correction with
        | None -> Alcotest.fail "missing correction"
        | Some c ->
            Alcotest.(check (option string))
              "reasoning" (Some "high") c.reasoning_effort;
            Alcotest.(check (float 0.001)) "timeout" 4.5 c.timeout_seconds;
            Alcotest.(check (float 0.001)) "temperature" 0.3 c.temperature);
        pass2);
    one_pass =
      (fun cfg b glossary pcm ->
        push f "one-pass";
        f.time <- f.time +. 0.25;
        Alcotest.(check string) "full PCM" "PCM" (Bytes.to_string pcm);
        Alcotest.(check string)
          "press glossary" "PROJECT TERM" glossary.Pie.Glossary.text;
        one_pass cfg b glossary pcm);
    append_transcribed =
      (fun raw ->
        push f "raw-history";
        f.raw <- f.raw @ [ raw ];
        history_result ());
    append_finished =
      (fun finished ->
        push f "finished-history";
        f.finished <- f.finished @ [ finished ];
        history_result ());
    warn = (fun _ -> f.warnings <- f.warnings + 1);
    notify_correction_failure = (fun _ -> f.notices <- f.notices + 1);
    notify_request_failure = (fun _ -> f.notices <- f.notices + 1);
    fresh_id = (fun () -> "attempt-id");
    now = (fun () -> f.time);
    log_pass =
      (fun pass ~model ~duration_ms ->
        f.passes <- f.passes @ [ (pass, model, duration_ms) ]);
  }

let execute ?(delivery_error = false) f t =
  D.run t
    ~recognize:(fun () -> D.recognize t (Bytes.of_string "PCM"))
    ~deliver:(fun text ->
      push f "deliver";
      f.delivered <- f.delivered @ [ text ];
      if delivery_error then Error (`Ptt_error "dispatch") else Ok ())
    ~on_audio_failure:(fun () ->
      f.audios <- f.audios + 1;
      Ok ())
    ~on_text_failure:(fun text ~dispatch_uncertain ->
      f.texts <- [ (text, dispatch_uncertain) ];
      Ok ())
    ~on_success:(fun () -> Ok ())

let ok = function Ok () -> () | Error _ -> Alcotest.fail "expected success"

let failed = function
  | Error _ -> ()
  | Ok () -> Alcotest.fail "expected failure"

let outcome f expected =
  match f.finished with
  | [ event ] ->
      Alcotest.(check string)
        "outcome"
        (H.string_of_outcome expected)
        (H.string_of_outcome event.H.outcome)
  | _ -> Alcotest.fail "one finished event required"

let make ?history_error ?pass1 ?pass2 ?mode ?correct () =
  let f = fixture () in
  let b = binding ?mode ?correct () in
  let t =
    D.create
      (dependencies ?history_error ?pass1 ?pass2 f)
      ~config:(config b) ~binding:b
  in
  (f, t)

let test_success mode () =
  let f, t = make ~mode () in
  ok (execute f t);
  outcome f H.Delivered;
  Alcotest.(check (list string))
    "ordered effects"
    [ "pass1"; "raw-history"; "pass2"; "deliver"; "finished-history" ]
    f.trace;
  Alcotest.(check (list string)) "trimmed final" [ "corrected" ] f.delivered;
  Alcotest.(check int) "one resolution" 1 f.resolutions;
  (match f.raw with
  | [ r ] ->
      Alcotest.(check string)
        "raw bytes retained" "  surowy tekst  " r.H.raw_text;
      Alcotest.(check (list string))
        "paths" [ "/global"; "/project" ] r.glossary_paths;
      Alcotest.(check string) "model" "openrouter/owner-a" r.pass1_model;
      Alcotest.(check bool)
        "focus" true
        (r.window
        = Some { H.app_id = Some "editor"; title = Some "press title" })
  | _ -> Alcotest.fail "one transcribed event required");
  match f.passes with
  | [ (D.Pass1, m1, d1); (D.Pass2, m2, d2) ] ->
      Alcotest.(check string) "pass1 latency model" "openrouter/owner-a" m1;
      Alcotest.(check string) "pass2 latency model" "openrouter/corrector" m2;
      Alcotest.(check bool)
        "latencies" true
        (d1 >= 99 && d1 <= 101 && d2 >= 199 && d2 <= 201)
  | _ -> Alcotest.fail "two pass latency records required"

let test_uncorrected () =
  let f, t = make ~correct:false () in
  ok (execute f t);
  Alcotest.(check (list string))
    "raw delivery" [ "  surowy tekst  " ] f.delivered;
  Alcotest.(check bool) "no pass2" false (List.mem "pass2" f.trace);
  outcome f H.Delivered

let test_correction_failure mode failure () =
  let f, t = make ~mode ~pass2:(Error failure) () in
  (match mode with
  | C.Transcribe ->
      ok (execute f t);
      outcome f H.Delivered_raw_after_correction_failure;
      Alcotest.(check (list string))
        "raw fallback" [ "  surowy tekst  " ] f.delivered
  | C.Translate ->
      failed (execute f t);
      outcome f H.Correction_failed;
      Alcotest.(check (list string)) "no delivery" [] f.delivered;
      Alcotest.(check bool)
        "certain raw recovery" true
        (f.texts = [ ("  surowy tekst  ", false) ]));
  Alcotest.(check int) "notification" 1 f.notices;
  Alcotest.(check int) "raw before failure" 1 (List.length f.raw)

let test_invalid_correction mode text () =
  let f, t = make ~mode ~pass2:(Ok text) () in
  match mode with
  | C.Transcribe ->
      ok (execute f t);
      outcome f H.Delivered_raw_after_correction_failure
  | Translate ->
      failed (execute f t);
      outcome f H.Correction_failed

let test_bad_pass1 value () =
  let f, t = make ~pass1:value () in
  failed (execute f t);
  outcome f H.Failed;
  Alcotest.(check int) "audio recovery" 1 f.audios;
  Alcotest.(check int) "no raw history" 0 (List.length f.raw);
  Alcotest.(check bool) "no pass2" false (List.mem "pass2" f.trace)

let test_history_warning () =
  let f, t = make ~history_error:true () in
  ok (execute f t);
  outcome f H.Delivered;
  Alcotest.(check int) "two warnings" 2 f.warnings;
  Alcotest.(check (list string))
    "delivery unaffected" [ "corrected" ] f.delivered

let test_delivery_failure () =
  let f, t = make () in
  failed (execute ~delivery_error:true f t);
  outcome f H.Delivery_failed;
  Alcotest.(check bool)
    "uncertain final recovery" true
    (f.texts = [ ("corrected", true) ])

let test_unfinished () =
  let f, t = make () in
  D.fail_unfinished t;
  D.fail_unfinished t;
  outcome f H.Failed

let test_stream_fallback_latency () =
  let f, t = make ~correct:false () in
  D.begin_pass1 t;
  f.time <- f.time +. 1.;
  ok (execute f t);
  match f.passes with
  | [ (D.Pass1, _, ms) ] ->
      Alcotest.(check bool) "includes attempted stream" true (ms >= 1099)
  | _ -> Alcotest.fail "one pass required"

let test_n_bindings () =
  let bindings =
    [
      binding ~correct:false ();
      binding ~keysym:65480 ();
      binding ~keysym:65481 ~mode:C.Translate ();
    ]
  in
  let current = ref bindings in
  let recognize =
    Pie.Assistant.Service.make_live_recognizer
      ~load_config_bindings:(fun () -> !current)
      ~load_bindings:(fun () ->
        ( { Pie.Assistant.Service.keysym = 65478; shift = false },
          { keysym = 65479; shift = false } ))
      ()
  in
  let event keysym released =
    {
      Pie.Keyboard.Monitor.released;
      state = 0;
      keysym;
      unichar = 0;
      keycode = keysym;
    }
  in
  List.iter
    (fun b ->
      (match recognize (event b.C.keysym false) with
      | Some (Pie.Ptt_loop.Press_binding selected) ->
          Alcotest.(check bool) "exact binding" true (selected = b)
      | _ -> Alcotest.fail "binding press");
      current := [];
      (match recognize (event b.keysym true) with
      | Some (Pie.Ptt_loop.Release mode) ->
          Alcotest.(check string)
            "press-time release" (C.string_of_mode b.mode) mode
      | _ -> Alcotest.fail "release");
      current := bindings)
    bindings

let test_cli_preserves_bindings () =
  let bindings =
    [
      binding ();
      binding ~keysym:65480 ~correct:false ();
      binding ~keysym:65481 ~mode:C.Translate ();
    ]
  in
  let config = { (base_config ()) with bindings } in
  let actual =
    Pie.Assistant.Service.with_cli_overrides ~ptt_transcribe_keysym:65490
      ~ptt_translate_keysym:65491 config
  in
  let expected =
    [
      { (List.nth bindings 0) with keysym = 65490 };
      List.nth bindings 1;
      { (List.nth bindings 2) with keysym = 65491 };
    ]
  in
  Alcotest.(check bool)
    "only first per mode changed" true
    (actual.bindings = expected)

let test_cli_collision () =
  let config =
    {
      (base_config ()) with
      bindings = [ binding (); binding ~keysym:65480 () ];
    }
  in
  match
    Pie.Assistant.Service.with_cli_overrides ~ptt_transcribe_keysym:65480 config
  with
  | _ -> Alcotest.fail "collision must be rejected"
  | exception C.Config_error _ -> ()

let test_mod5_binding () =
  let first = binding ~correct:false () in
  let recognize =
    Pie.Assistant.Service.make_live_recognizer
      ~load_config_bindings:(fun () -> [ first; binding ~keysym:65480 () ])
      ~load_bindings:(fun () ->
        ( { Pie.Assistant.Service.keysym = 65478; shift = false },
          { keysym = 65479; shift = false } ))
      ()
  in
  let event =
    {
      Pie.Keyboard.Monitor.released = false;
      state = 128;
      keysym = 65506;
      unichar = 0;
      keycode = 62;
    }
  in
  match recognize event with
  | Some (Pie.Ptt_loop.Press_binding actual) ->
      Alcotest.(check bool)
        "Mod5 chooses first current transcribe" true (actual = first)
  | _ -> Alcotest.fail "synthetic binding press"

let make_one_pass ?(mode = C.Transcribe) ?(correct = true)
    ?(prompt = C.default_one_pass_prompt) ?(result = Ok "heard text") () =
  let f = fixture () in
  let b =
    {
      (binding ~mode ~correct ()) with
      prompt;
      prompt_file =
        (if mode = C.Translate then C.default_one_pass_translate_prompt_file
         else if correct then C.default_one_pass_prompt_file
         else C.default_one_pass_verbatim_prompt_file);
    }
  in
  let cfg =
    {
      (config b) with
      pipeline = One_pass;
      transcription_model = "openrouter/google/gemini-3.8-flash";
      translation_model = "openrouter/google/gemini-translate";
    }
  in
  let seen = ref "" in
  let t =
    D.create
      (dependencies
         ~one_pass:(fun _cfg binding _glossary _pcm ->
           seen := binding.C.prompt;
           result)
         f)
      ~config:cfg ~binding:b
  in
  (f, t, seen)

let test_one_pass_success mode () =
  let f, t, _ = make_one_pass ~mode () in
  ok (execute f t);
  outcome f H.Delivered;
  Alcotest.(check (list string))
    "one request then deliver"
    [ "one-pass"; "raw-history"; "deliver"; "finished-history" ]
    f.trace;
  Alcotest.(check (list string)) "delivered output" [ "heard text" ] f.delivered;
  Alcotest.(check bool) "pass 2 not run" false (List.mem "pass2" f.trace);
  Alcotest.(check bool) "pass 1 not run" false (List.mem "pass1" f.trace);
  let expected_model =
    match mode with
    | C.Transcribe -> "openrouter/google/gemini-3.8-flash"
    | Translate -> "openrouter/google/gemini-translate"
  in
  (match f.raw with
  | [ event ] ->
      Alcotest.(check string)
        "raw is the one-pass output" "heard text" event.H.raw_text;
      Alcotest.(check string)
        "pass-1 model is the one-pass model" expected_model event.pass1_model
  | _ -> Alcotest.fail "one transcribed event required");
  (match f.finished with
  | [ event ] ->
      Alcotest.(check (option string))
        "final" (Some "heard text") event.H.final_text;
      Alcotest.(check (option string)) "no pass 2 model" None event.pass2_model
  | _ -> Alcotest.fail "one finished event required");
  match f.passes with
  | [ (D.One_pass, model, ms) ] ->
      Alcotest.(check string) "logged model" expected_model model;
      Alcotest.(check bool) "logged latency" true (ms >= 249 && ms <= 251)
  | _ -> Alcotest.fail "one-pass latency required"

let test_one_pass_failure () =
  let f, t, _ = make_one_pass ~result:(Error Pie.Stt.Correction.Timeout) () in
  failed (execute f t);
  outcome f H.Failed;
  Alcotest.(check int) "audio retained" 1 f.audios;
  Alcotest.(check int) "notified" 1 f.notices;
  Alcotest.(check (list string)) "nothing typed" [] f.delivered;
  Alcotest.(check int) "no raw history" 0 (List.length f.raw);
  match f.finished with
  | [ event ] ->
      Alcotest.(check (option string))
        "diagnostic" (Some "timeout") event.H.diagnostic
  | _ -> Alcotest.fail "one finished event required"

let test_one_pass_verbatim_prompt () =
  let prompt = C.default_one_pass_verbatim_prompt in
  let f, t, seen = make_one_pass ~correct:false ~prompt () in
  ok (execute f t);
  Alcotest.(check string) "verbatim prompt" prompt !seen;
  Alcotest.(check bool) "no pass 2" false (List.mem "pass2" f.trace);
  outcome f H.Delivered

let () =
  let case name f = Alcotest.test_case name `Quick f in
  let failures =
    [
      ("provider", Pie.Stt.Correction.Provider Pie.Stt.Correction.Unavailable);
      ("timeout", Pie.Stt.Correction.Timeout);
      ("blank", Pie.Stt.Correction.Blank);
      ("oversize", Pie.Stt.Correction.Over_budget);
    ]
  in
  let variants =
    List.concat_map
      (fun (name, failure) ->
        [
          case
            ("dict-bpuh/fk8i transcribe " ^ name)
            (test_correction_failure C.Transcribe failure);
          case
            ("dict-swyn/fk8i translate " ^ name)
            (test_correction_failure C.Translate failure);
        ])
      failures
  in
  Alcotest.run "dictation-pipeline"
    [
      ( "stories",
        [
          case "dict-dozi/3dqh/s0jv/6frx/oagf corrected transcribe"
            (test_success C.Transcribe);
          case "dict-dl3q/q6xd/vwco translate pass1 source"
            (test_success C.Translate);
          case "dict-wrbh correction disabled" test_uncorrected;
          case "dict-6frx history warns only" test_history_warning;
          case "dict-bpuh rejected blank result"
            (test_invalid_correction C.Transcribe " ");
          case "dict-bpuh translate rejected oversized result"
            (test_invalid_correction C.Translate
               (String.make (Pie.Dictation_limits.result_utf8_bytes + 1) 'x'));
          case "dict-u5xj pass1 provider error"
            (test_bad_pass1 (Error (`Ptt_error "provider")));
          case "dict-u5xj pass1 blank" (test_bad_pass1 (Ok " "));
          case "dict-u5xj pass1 oversized"
            (test_bad_pass1
               (Ok
                  (String.make (Pie.Dictation_limits.result_utf8_bytes + 1) 'x')));
          case "dict-6frx delivery failed" test_delivery_failure;
          case "dict-6frx unfinished settles once" test_unfinished;
          case "dict-oj7u/oagf attempted stream included"
            test_stream_fallback_latency;
          case "dict-wrbh N bindings and reload-safe release" test_n_bindings;
          case "dict-wrbh CLI preserves later bindings"
            test_cli_preserves_bindings;
          case "dict-wrbh CLI collision rejected" test_cli_collision;
          case "dict-wrbh Mod5 uses first current binding" test_mod5_binding;
          case "dict-dq3o/msdh/oagf one-pass success"
            (test_one_pass_success C.Transcribe);
          case "dict-dq3o/msdh one-pass translate model"
            (test_one_pass_success C.Translate);
          case "dict-pna4 one-pass failure keeps audio" test_one_pass_failure;
          case "dict-gi5u correct=false uses verbatim prompt"
            test_one_pass_verbatim_prompt;
        ]
        @ variants );
    ]
