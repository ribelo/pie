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
    prompt_rules = [];
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
            Pie.Glossary.text = "GLOBAL TERM";
            paths = [ "/global" ];
            warnings = [];
          },
          Some
            {
              H.id = Some 42;
              app_id = Some "editor";
              title = Some "press title";
            } ));
    pass1 =
      (fun cfg b glossary pcm ->
        push f "pass1";
        f.time <- f.time +. 0.1;
        Alcotest.(check string)
          "pass1 transcription model" "openrouter/owner-a"
          cfg.C.transcription_model;
        Alcotest.(check string)
          "press glossary" "GLOBAL TERM" glossary.Pie.Glossary.text;
        Alcotest.(check string)
          "source language" "Polish"
          (match b.C.mode with
          | C.Transcribe -> cfg.transcription_language
          | Translate -> cfg.translation_source_language);
        Alcotest.(check string) "full PCM" "PCM" (Bytes.to_string pcm);
        pass1);
    pass2 =
      (fun ~on_response:_ cfg b glossary raw ->
        push f "pass2";
        f.time <- f.time +. 0.2;
        Alcotest.(check string) "same raw" "  surowy tekst  " raw;
        Alcotest.(check string)
          "same glossary" "GLOBAL TERM" glossary.Pie.Glossary.text;
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
        Pie.Stt.Correction.of_result pass2);
    one_pass =
      (fun ~on_response:_ cfg b glossary pcm ->
        push f "one-pass";
        f.time <- f.time +. 0.25;
        Alcotest.(check string) "full PCM" "PCM" (Bytes.to_string pcm);
        Alcotest.(check string)
          "press glossary" "GLOBAL TERM" glossary.Pie.Glossary.text;
        Pie.Stt.Correction.of_result (one_pass cfg b glossary pcm));
    store_response = (fun ~id:_ ~first:_ _ -> Ok "/audio/response.json");
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
      if delivery_error then Error (`Ptt_error "dispatch") else Ok H.Pasted)
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
      Alcotest.(check (list string)) "paths" [ "/global" ] r.glossary_paths;
      Alcotest.(check string) "model" "openrouter/owner-a" r.pass1_model;
      Alcotest.(check bool)
        "focus" true
        (r.window
        = Some
            {
              H.id = Some 42;
              app_id = Some "editor";
              title = Some "press title";
            })
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

(* dict-6wq3 dict-x6e9: shared evidence seam for live attempts and retry. *)
let test_response_history ~one_pass ~archive_error ~retention () =
  let f = fixture () in
  let b = binding () in
  let cfg =
    {
      (config b) with
      pipeline = (if one_pass then C.One_pass else C.Two_pass);
      audio_retention_hours = retention;
    }
  in
  let report ~on_response _ _ _ _ =
    on_response ~first:true (Bytes.of_string "first private response");
    on_response ~first:false (Bytes.of_string "final private response");
    {
      Pie.Stt.Correction.result = Ok "recovered";
      retry =
        Some
          {
            first_diagnostic = "unavailable";
            final_diagnostic = "stt:ok";
            audio_format = (if one_pass then Some "wav" else None);
            reasoning_effort = Some "medium";
          };
    }
  in
  let writes = ref [] in
  let deps =
    {
      (dependencies f) with
      one_pass = report;
      pass2 = report;
      store_response =
        (fun ~id ~first body ->
          Alcotest.(check string) "attempt id" "attempt-id" id;
          writes := (first, Bytes.to_string body) :: !writes;
          if archive_error then Error "response directory unavailable"
          else
            Ok
              (if first then "/audio/attempt-id.response-1.json"
               else "/audio/attempt-id.response.json"));
    }
  in
  let t = D.create deps ~config:cfg ~binding:b in
  ok (execute f t);
  Alcotest.(check (list string))
    "only recovered text delivered" [ "recovered" ] f.delivered;
  let retained = retention > 0. in
  Alcotest.(check int)
    "response writes"
    (if retained then 2 else 0)
    (List.length !writes);
  Alcotest.(check int)
    "archive failure only warns"
    (if retained && archive_error then 2 else 0)
    f.warnings;
  Alcotest.(check int) "no failure notification" 0 f.notices;
  match f.finished with
  | [ event ] ->
      Alcotest.(check (option string))
        "first response path"
        (if retained && not archive_error then
           Some "/audio/attempt-id.response-1.json"
         else None)
        event.H.first_response_path;
      Alcotest.(check (option string))
        "final response path"
        (if retained && not archive_error then
           Some "/audio/attempt-id.response.json"
         else None)
        event.H.response_path;
      let retry = Option.get event.H.retry in
      Alcotest.(check string)
        "first diagnostic" "unavailable" retry.first_diagnostic;
      Alcotest.(check string) "final diagnostic" "stt:ok" retry.final_diagnostic;
      Alcotest.(check (option string))
        "effort" (Some "medium") retry.reasoning_effort;
      Alcotest.(check (option string))
        "actual final effort" (Some "medium") event.reasoning_effort;
      Alcotest.(check (option string))
        "schema" (Some "dictation/text-v1") event.response_schema
  | _ -> Alcotest.fail "missing finished retry event"

let test_target_delivery ?(pipeline = C.Two_pass) focus expected_paste () =
  let f = fixture () in
  let binding = binding ~correct:false () in
  let config = { (config binding) with pipeline } in
  let attempt =
    D.create
      (dependencies ~one_pass:(fun _ _ _ _ -> Ok "surowy tekst") f)
      ~config ~binding
  in
  let copies = ref [] and keys = ref 0 and notices = ref [] in
  let target =
    Some
      {
        Pie.Niri.id = 42;
        app_id = Some "editor";
        title = Some "press title";
        pid = None;
        workspace_id = None;
      }
  in
  let result =
    D.run attempt
      ~recognize:(fun () -> D.recognize attempt (Bytes.of_string "PCM"))
      ~deliver:(fun text ->
        let delivery =
          Pie.Input.Text_injection.deliver ~target_window:target
            ~focused_window:(fun () -> focus)
            ~newline_rules:[]
            ~copy:(fun text ->
              copies := text :: !copies;
              Ok ())
            ~paste:(fun () ->
              incr keys;
              Ok ())
            ~newline:(fun _ -> Ok ())
            text
        in
        Result.map_error (fun _ -> `Ptt_error "fake delivery") delivery
        |> Result.map (fun (_, outcome) ->
            Pie.Assistant.Service.report_delivery
              ~target_window:(D.window attempt) outcome
              ~notify:(fun title message ->
                notices := (title, message) :: !notices)))
      ~on_audio_failure:(fun () -> Alcotest.fail "audio fallback")
      ~on_text_failure:(fun _ ~dispatch_uncertain:_ ->
        Alcotest.fail "text fallback")
      ~on_success:(fun () -> Ok ())
  in
  Alcotest.(check bool) "delivered" true (Result.is_ok result);
  Alcotest.(check int) "keys" (if expected_paste then 1 else 0) !keys;
  Alcotest.(check int)
    "one notification only for clipboard"
    (if expected_paste then 0 else 1)
    (List.length !notices);
  Alcotest.(check (list string))
    "copy recognized text" [ "surowy tekst" ] !copies;
  (match !notices with
  | [] -> ()
  | [ ("Pie", message) ] ->
      Alcotest.(check string)
        "press title, no transcript"
        "Pie: text for press title is in the clipboard" message
  | _ -> Alcotest.fail "wrong notices");
  (match f.finished with
  | [ event ] ->
      Alcotest.(check bool)
        "history delivered" true
        (event.outcome = H.Delivered);
      Alcotest.(check bool)
        "history destination" true
        (event.delivery
        = Some (if expected_paste then H.Pasted else H.Clipboard))
  | _ -> Alcotest.fail "one finished history event required");
  match f.raw with
  | [ event ] ->
      Alcotest.(check (option int))
        "key-press window id" (Some 42)
        (Option.bind event.window (fun window -> window.H.id))
  | _ -> Alcotest.fail "one raw history event required"

let test_prompt_frozen_at_press () =
  let f = fixture () in
  let b = binding () in
  let rule : C.prompt_rule =
    {
      app_id = Some "EDITOR";
      title_regex = Some "^press";
      mode = Some C.Transcribe;
      prompt_file = "neutral.md";
      prompt = "Neutral {{glossary}}";
    }
  in
  let cfg =
    {
      (config b) with
      pipeline = C.One_pass;
      thinking_effort = Some "low";
      prompt_rules = [ rule ];
    }
  in
  let focus =
    ref
      (Some
         { H.id = Some 42; app_id = Some "editor"; title = Some "press title" })
  in
  let deps =
    {
      (dependencies
         ~one_pass:(fun _ chosen _ _ ->
           Alcotest.(check string)
             "request uses press prompt" "neutral.md" chosen.C.prompt_file;
           Alcotest.(check string)
             "request uses loaded content" rule.prompt chosen.prompt;
           Ok "heard text")
         f)
      with
      resolve_glossary =
        (fun () ->
          f.resolutions <- f.resolutions + 1;
          ( { Pie.Glossary.text = "GLOBAL TERM"; paths = []; warnings = [] },
            !focus ));
    }
  in
  let attempt = D.create deps ~config:cfg ~binding:b in
  focus :=
    Some
      { H.id = Some 43; app_id = Some "slack"; title = Some "delivery title" };
  ok (execute f attempt);
  Alcotest.(check int) "focus captured exactly once" 1 f.resolutions;
  match f.finished with
  | [ event ] ->
      Alcotest.(check (option string))
        "chosen prompt in history" (Some "neutral.md") event.H.prompt_file;
      Alcotest.(check (option string))
        "configured first effort" (Some "low") event.reasoning_effort;
      Alcotest.(check (option string))
        "schema identifier" (Some "dictation/text-v1") event.response_schema
  | _ -> Alcotest.fail "one finished event required"

let test_notification_title () =
  Alcotest.(check string)
    "markup and controls escaped"
    "Pie: text for &lt;Editor&gt; &amp; inbox is in the clipboard"
    (Pie.Desktop.Notification.clipboard_message (Some "<Editor> & inbox\n\x1b"))

let test_unknown_target_title () =
  let notices = ref [] in
  let target_window =
    Some { H.id = None; app_id = Some "terminal"; title = Some "Project" }
  in
  let result =
    Pie.Assistant.Service.report_delivery ~target_window
      ~notify:(fun title message -> notices := (title, message) :: !notices)
      Pie.Input.Text_injection.Clipboard_only
  in
  Alcotest.(check bool)
    "unknown id is delivered to clipboard" true (result = H.Clipboard);
  Alcotest.(check (list (pair string string)))
    "one notice keeps original title"
    [ ("Pie", "Pie: text for Project is in the clipboard") ]
    !notices

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
          case "dict-c3nt h8rd prompt frozen at press"
            test_prompt_frozen_at_press;
          case "dict-x6e9 one-pass response history"
            (test_response_history ~one_pass:true ~archive_error:false
               ~retention:168.);
          case "dict-x6e9 pass-2 response history"
            (test_response_history ~one_pass:false ~archive_error:false
               ~retention:168.);
          case "dict-x6e9 archive failure warns only"
            (test_response_history ~one_pass:true ~archive_error:true
               ~retention:168.);
          case "dict-x6e9 disabled retention"
            (test_response_history ~one_pass:true ~archive_error:false
               ~retention:0.);
          case "dict-snot one-pass unchanged id pastes"
            (test_target_delivery ~pipeline:C.One_pass
               (Ok
                  (Some
                     {
                       Pie.Niri.id = 42;
                       app_id = None;
                       title = Some "changed";
                       pid = None;
                       workspace_id = None;
                     }))
               true);
          case "dict-ib9n one-pass changed id stays clipboard-only"
            (test_target_delivery ~pipeline:C.One_pass
               (Ok
                  (Some
                     {
                       Pie.Niri.id = 43;
                       app_id = Some "terminal";
                       title = Some "Project";
                       pid = None;
                       workspace_id = None;
                     }))
               false);
          case "dict-snot target id unchanged pastes"
            (test_target_delivery
               (Ok
                  (Some
                     {
                       Pie.Niri.id = 42;
                       app_id = None;
                       title = Some "changed title";
                       pid = None;
                       workspace_id = None;
                     }))
               true);
          case "dict-ib9n changed target id delivers to clipboard"
            (test_target_delivery
               (Ok
                  (Some
                     {
                       Pie.Niri.id = 43;
                       app_id = Some "editor";
                       title = Some "press title";
                       pid = None;
                       workspace_id = None;
                     }))
               false);
          case "no focus delivers to clipboard"
            (test_target_delivery (Ok None) false);
          case "niri unavailable delivers to clipboard"
            (test_target_delivery (Error (`Niri_error "unavailable")) false);
          case "clipboard notification sanitizes title" test_notification_title;
          case "dict-ib9n unknown target id keeps title"
            test_unknown_target_title;
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
