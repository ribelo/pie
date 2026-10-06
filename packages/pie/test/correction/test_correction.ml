let monotonic_seconds () =
  Mtime.Span.to_float_ns (Mtime_clock.elapsed ()) /. 1e9

(* Pass-2 correction adapter against a local Responses fixture.
   dict-ldo9 dict-l4zp dict-g52m dict-fk8i dict-bpuh dict-swyn *)

let case_env = "PIE_CORRECTION_CASE"
let transcript_sentinel = "TRANSCRIPT_SENTINEL"
let prompt_sentinel = "PROMPT_SENTINEL"
let glossary_sentinel = "GLOSSARY_SENTINEL"
let refusal_sentinel = "REFUSAL_SENTINEL"
let provider_sentinel = "PROVIDER_SENTINEL"
let tool_sentinel = "TOOL_SENTINEL"

let leaks =
  [
    transcript_sentinel;
    prompt_sentinel;
    glossary_sentinel;
    refusal_sentinel;
    provider_sentinel;
    tool_sentinel;
  ]

let has_suffix suffix s =
  let sl = String.length suffix in
  let n = String.length s in
  n >= sl && String.sub s (n - sl) sl = suffix

let contains needle s =
  let n = String.length needle in
  let rec loop i =
    i <= String.length s - n && (String.sub s i n = needle || loop (i + 1))
  in
  n = 0 || loop 0

let read_http_request flow =
  let scratch = Cstruct.create 1 in
  let buffer = Buffer.create 512 in
  let rec headers () =
    match Eio.Flow.single_read flow scratch with
    | 0 -> raise End_of_file
    | _ ->
        Buffer.add_char buffer (Cstruct.get_char scratch 0);
        let contents = Buffer.contents buffer in
        if has_suffix "\r\n\r\n" contents then contents else headers ()
    | exception End_of_file -> raise End_of_file
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
            | exception End_of_file -> raise End_of_file
        in
        loop 0;
        Cstruct.to_string buf
  in
  (head, body)

let write_http flow status body =
  let response =
    Printf.sprintf
      "HTTP/1.1 %d OK\r\n\
       Content-Type: application/json\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n\
       %s"
      status (String.length body) body
  in
  Eio.Flow.copy_string response flow

let output_text text =
  `Assoc
    [
      ("type", `String "message");
      ( "content",
        `List
          [ `Assoc [ ("type", `String "output_text"); ("text", `String text) ] ]
      );
    ]

let completed_raw text =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("status", `String "completed"); ("output", `List [ output_text text ]);
       ])

let answer_json text = Yojson.Safe.to_string (`Assoc [ ("text", `String text) ])
let completed text = completed_raw (answer_json text)

let status_body status text =
  Yojson.Safe.to_string
    (`Assoc
       [ ("status", `String status); ("output", `List [ output_text text ]) ])

let refusal_body =
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
                             ("type", `String "refusal");
                             ("refusal", `String refusal_sentinel);
                           ];
                       ] );
                 ];
             ] );
       ])

(* Gemini through OpenRouter adds a reasoning item after the message when
   reasoning.effort is set. *)
let reasoning_body text =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("status", `String "completed");
         ( "output",
           `List
             [
               output_text (answer_json text);
               `Assoc [ ("type", `String "reasoning"); ("summary", `List []) ];
             ] );
       ])

let tool_body =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("status", `String "completed");
         ( "output",
           `List
             [
               `Assoc
                 [
                   ("type", `String "function_call");
                   ("name", `String tool_sentinel);
                   ("call_id", `String "call_1");
                   ("arguments", `String transcript_sentinel);
                 ];
               output_text "should-not-deliver";
             ] );
       ])

let failed_body =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("status", `String "failed");
         ("output", `List []);
         ( "error",
           `Assoc
             [
               ( "message",
                 `String (provider_sentinel ^ " " ^ transcript_sentinel) );
             ] );
       ])

type reply = Json of string | Http of int * string | Hang | Close

let logger env =
  Ox_observability.Logger.create ~service:"pie-correction-test"
    ~clock:(Eio.Stdenv.clock env)
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~reporter:
      (Ox_observability.Logger.Memory.reporter
         (Ox_observability.Logger.Memory.create ()))
    ()

let with_fixture ~(caps : < Ox_caps.Cap.env ; .. >) ~reply ?(extra_replies = [])
    ?(on_request = fun _ _ -> ()) f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let hits = ref 0 in
  let captured = ref None in
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> Alcotest.fail "expected TCP listener"
  in
  Ox_caps.Cap_unix.putenv ~caps "OPENROUTER_BASE_URL"
    (Printf.sprintf "http://127.0.0.1:%d" port);
  (* The server fiber finishes itself. A nested switch that only ends when the
     outer switch is cancelled can stick on accept, which hung the missing-key
     case. *)
  Eio.Fiber.fork ~sw (fun () ->
      List.iter
        (fun reply ->
          Eio.Switch.run (fun conn_sw ->
              match
                Eio.Time.with_timeout clock 2.0 (fun () ->
                    Ok (Eio.Net.accept ~sw:conn_sw socket))
              with
              | Error `Timeout -> ()
              | Ok (flow, _) -> (
                  incr hits;
                  let head, body = read_http_request flow in
                  captured := Some (head, body);
                  on_request head body;
                  match reply with
                  | Json payload -> write_http flow 200 payload
                  | Http (status, payload) -> write_http flow status payload
                  | Close -> ()
                  | Hang -> (
                      let scratch = Cstruct.create 1 in
                      (* The client closes on timeout; that read is end-of-file,
                     not a test failure. *)
                      try
                        (* The fixture waits for client closure; the callback success value is not an assertion. *)
                        let _closure_result =
                          Eio.Time.with_timeout clock 2.0 (fun () ->
                              ignore (Eio.Flow.single_read flow scratch);
                              Ok ())
                        in
                        ()
                      with End_of_file | Eio.Io _ -> ()))))
        (reply :: extra_replies));
  Eio.Fiber.yield ();
  let result = f ~sw ~net ~clock ~logger:(logger env) in
  (result, !hits, !captured)

let no_leak diagnostic =
  List.iter
    (fun needle ->
      if contains needle diagnostic then
        Alcotest.failf "diagnostic leaked %s" needle)
    leaks

let expect_failure expected result =
  match result with
  | Ok text ->
      Alcotest.failf "expected failure, delivered %d bytes" (String.length text)
  | Error failure ->
      let diagnostic = Pie.Stt.Correction.diagnostic failure in
      no_leak diagnostic;
      if failure <> expected then
        Alcotest.failf "expected %s, got %s"
          (Pie.Stt.Correction.diagnostic expected)
          diagnostic;
      Alcotest.(check string)
        "diagnostic"
        (Pie.Stt.Correction.diagnostic expected)
        diagnostic

let call ~(caps : < Ox_caps.Cap.env ; .. >) ~sw ~net ~clock ~logger ?temperature
    ?reasoning_effort ?audio ?audio_format ?provider_order ?output_language
    ?(prompt_template = prompt_sentinel ^ " {{glossary}}")
    ?(glossary = glossary_sentinel) ?(language = "pl") ?(source_language = "pl")
    ?(target_language = "en") ?(transcript = transcript_sentinel)
    ?(timeout = Ox.Duration.seconds 5) () =
  Pie.Stt.Correction.run ~caps ~sw ~net ~clock ~logger ~model:"test-model"
    ~prompt_template ~transcript ~glossary ~language ~source_language
    ~target_language ?audio ?audio_format ?provider_order ?temperature
    ?reasoning_effort ?output_language ~timeout ()

let fields = function
  | `Assoc items -> items
  | json ->
      Alcotest.failf "expected object, got %s" (Yojson.Safe.to_string json)

let field name json =
  match List.assoc_opt name (fields json) with
  | Some value -> value
  | None -> Alcotest.failf "missing field %s" name

let string_field name json =
  match field name json with
  | `String value -> value
  | other ->
      Alcotest.failf "%s not a string: %s" name (Yojson.Safe.to_string other)

let float_field name json =
  match field name json with
  | `Float value -> value
  | `Int value -> float_of_int value
  | other ->
      Alcotest.failf "%s not a number: %s" name (Yojson.Safe.to_string other)

let assert_request ?(output_language = "pl") ~temperature ~reasoning head body =
  let request_line =
    match String.index_opt head '\r' with
    | None -> String.sub head 0 (min 120 (String.length head))
    | Some i -> String.sub head 0 (min 120 i)
  in
  if not (String.starts_with ~prefix:"POST /api/v1/responses " head) then
    Alcotest.failf "request line: %s" request_line;
  let lower = String.lowercase_ascii head in
  Alcotest.(check bool)
    "json content type" true
    (contains "content-type: application/json" lower);
  Alcotest.(check bool)
    "bearer authorization" true
    (contains "authorization: bearer " lower);
  let json = Yojson.Safe.from_string body in
  let format = field "format" (field "text" json) in
  Alcotest.(check string)
    "final-answer schema" "json_schema"
    (string_field "type" format);
  Alcotest.(check bool)
    "strict answer schema" true
    (field "strict" format = `Bool true);
  Alcotest.(check bool)
    "no extra output fields" true
    (field "additionalProperties" (field "schema" format) = `Bool false);
  Alcotest.(check string) "model" "test-model" (string_field "model" json);
  let schema = field "schema" format in
  let text_property = field "text" (field "properties" schema) in
  Alcotest.(check string)
    "answer framing dict-7k2q"
    (Printf.sprintf
       "The final message to type, in %s, exactly as the instructions require. \
        Nothing else."
       output_language)
    (string_field "description" text_property);
  Alcotest.(check int)
    "only text property" 1
    (List.length (fields (field "properties" schema)));
  Alcotest.(check bool)
    "text remains required" true
    (field "required" schema = `List [ `String "text" ]);
  let expected_instructions =
    prompt_sentinel ^ "\n" ^ glossary_sentinel
    ^ "\npl from pl to en\n{{transcript}}"
  in
  Alcotest.(check string)
    "instructions" expected_instructions
    (string_field "instructions" json);
  Alcotest.(check bool)
    "transcript stays out of instructions" false
    (contains transcript_sentinel (string_field "instructions" json));
  let input =
    match field "input" json with
    | `List [ message ] -> message
    | _ -> Alcotest.fail "input was not one message"
  in
  Alcotest.(check string) "role" "user" (string_field "role" input);
  let part =
    match field "content" input with
    | `List [ part ] -> part
    | _ -> Alcotest.fail "user content was not one part"
  in
  Alcotest.(check string) "part type" "input_text" (string_field "type" part);
  Alcotest.(check string)
    "transcript" transcript_sentinel (string_field "text" part);
  Alcotest.(check bool)
    "temperature" true
    (abs_float (float_field "temperature" json -. temperature) < 1e-6);
  (match field "stream" json with
  | `Bool false -> ()
  | other -> Alcotest.failf "stream: %s" (Yojson.Safe.to_string other));
  Alcotest.(check bool)
    "provider omitted" true
    (List.assoc_opt "provider" (fields json) = None);
  match reasoning with
  | None ->
      Alcotest.(check bool)
        "reasoning omitted" true
        (List.assoc_opt "reasoning" (fields json) = None)
  | Some effort ->
      Alcotest.(check string)
        "reasoning effort" effort
        (string_field "effort" (field "reasoning" json));
      Alcotest.(check bool)
        "reasoning excluded" true
        (field "exclude" (field "reasoning" json) = `Bool true)

let child_request ~(caps : < Ox_caps.Cap.env ; .. >) ?output_language () =
  let result, hits, captured =
    with_fixture ~caps
      ~reply:(Json (completed "done"))
      (fun ~sw ~net ~clock ~logger ->
        call ~caps ~sw ~net ~clock ~logger ?output_language ~temperature:0.4
          ~reasoning_effort:"low"
          ~prompt_template:
            (prompt_sentinel
           ^ "\n\
              {{glossary}}\n\
              {{language}} from {{source_language}} to {{target_language}}\n\
              {{transcript}}")
          ())
  in
  Alcotest.(check int) "one request" 1 hits;
  (match result with
  | Ok "done" -> ()
  | Ok text -> Alcotest.failf "unexpected text %s" text
  | Error failure ->
      Alcotest.failf "request failed: %s"
        (Pie.Stt.Correction.diagnostic failure));
  match captured with
  | None -> Alcotest.fail "fixture saw no request"
  | Some (head, body) ->
      assert_request ?output_language ~temperature:0.4 ~reasoning:(Some "low")
        head body

let child_reasoning_item ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let result, _hits, _captured =
    with_fixture ~caps
      ~reply:(Json (reasoning_body "hello"))
      (fun ~sw ~net ~clock ~logger ->
        call ~caps ~sw ~net ~clock ~logger
          ~prompt_template:(prompt_sentinel ^ " {{glossary}}")
          ())
  in
  match result with
  | Ok text -> Alcotest.(check string) "text delivered" "hello" text
  | Error failure ->
      Alcotest.failf "reasoning item must not reject: %s"
        (Pie.Stt.Correction.diagnostic failure)

(* dict-dq3o/dict-ldo9: delivery consumes the final-text envelope, not the
   model's free-form message (the real incident contained repeated analysis). *)
let child_answer_envelope ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let answer =
    "# Plan\n\n\
     - debugging Eio\n\
     - \u{017c}aden commit\n\n\
     Kod: `/* comment */`. URL: https://example.test/a"
  in
  let envelope = Yojson.Safe.to_string (`Assoc [ ("text", `String answer) ]) in
  let result, _, _ =
    with_fixture ~caps
      ~reply:(Json (completed_raw envelope))
      (fun ~sw ~net ~clock ~logger -> call ~caps ~sw ~net ~clock ~logger ())
  in
  match result with
  | Ok got -> Alcotest.(check string) "only the Markdown answer" answer got
  | Error failure ->
      Alcotest.failf "envelope failed: %s"
        (Pie.Stt.Correction.diagnostic failure)

let child_invalid_envelopes ~(caps : < Ox_caps.Cap.env ; .. >) () =
  List.iter
    (fun raw ->
      let result, _, _ =
        with_fixture ~caps
          ~reply:(Json (completed_raw raw))
          (fun ~sw ~net ~clock ~logger -> call ~caps ~sw ~net ~clock ~logger ())
      in
      expect_failure
        (Pie.Stt.Correction.Provider Pie.Stt.Correction.Invalid_response) result)
    [
      "plain but plausible transcript";
      "{\"text\":\"truncated";
      "[]";
      "{\"text\":null}";
      "{\"text\":1}";
      "{\"answer\":\"wrong field\"}";
      "{\"text\":\"final\",\"analysis\":\"Wait, let's think\"}";
      "{\"text\":\"first\",\"text\":\"second\"}";
      "Analysis before {\"text\":\"final\"}";
      "{\"text\":\"final\"} Analysis after";
      "/* model analysis */ {\"text\":\"final\"}";
      "// model analysis\n{\"text\":\"final\"}";
      "{\"text\":\"final\"} /* analysis */";
      "{\"text\":/* analysis */\"final\"}";
      "{\"text\":\"unescaped\nnewline\"}";
    ]

let child_unframed_analysis ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let result, _, _ =
    with_fixture ~caps
      ~reply:
        (Json
           (completed_raw
              "The speaker says: ... Wait, let's listen again. Final text."))
      (fun ~sw ~net ~clock ~logger -> call ~caps ~sw ~net ~clock ~logger ())
  in
  expect_failure
    (Pie.Stt.Correction.Provider Pie.Stt.Correction.Invalid_response) result

let choice_error_body =
  "{\"choices\":[{\"finish_reason\":\"error\",\"native_finish_reason\":\"OTHER\",\"error\":{\"code\":500,\"message\":\"PROVIDER_SENTINEL\",\"metadata\":{\"error_type\":\"unmapped\"}},\"message\":{\"role\":\"assistant\",\"content\":\"truncated \
   made-up content\"}}],\"usage\":{\"total_tokens\":0}}"

let child_choice_error ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let result, hits, _ =
    with_fixture ~caps ~reply:(Json choice_error_body)
      (fun ~sw ~net ~clock ~logger -> call ~caps ~sw ~net ~clock ~logger ())
  in
  expect_failure (Pie.Stt.Correction.Provider Pie.Stt.Correction.Unavailable)
    result;
  Alcotest.(check int) "single exchange" 1 hits

(* dict-6wq3 dict-x6e9 *)
let child_retry ~(caps : < Ox_caps.Cap.env ; .. >) ?(effort = "low")
    ?(first_diagnostic = "unavailable") ?(wav = false) ~first ~second ~success
    ~audio () =
  let requests = ref [] and evidence = ref [] in
  let report, hits, _ =
    with_fixture ~caps ~reply:first
      ~extra_replies:[ second; Json (completed "third-forbidden") ]
      ~on_request:(fun _ body -> requests := body :: !requests)
      (fun ~sw ~net ~clock ~logger ->
        Pie.Stt.Correction.run_with_retry ~caps ~sw ~net ~clock ~logger
          ~model:"test-model" ~prompt_template:prompt_sentinel
          ~transcript:transcript_sentinel ~reasoning_effort:effort
          ?upload:
            (if audio then
               Some
                 (if wav then Pie.Stt.Correction.Wav (Bytes.of_string "WAV")
                  else
                    Pie.Stt.Correction.Mp3
                      {
                        mp3 = Bytes.of_string "MP3";
                        wav = Bytes.of_string "WAV";
                      })
             else None)
          ~on_response:(fun ~first body ->
            evidence := (first, Bytes.to_string body) :: !evidence)
          ~timeout:(Ox.Duration.seconds 1) ())
  in
  Alcotest.(check int) "exactly two requests, never a third" 2 hits;
  if success then
    Alcotest.(check (result string string))
      "delivered WAV result" (Ok "WAV recovery")
      (Result.map_error Pie.Stt.Correction.diagnostic report.result)
  else
    expect_failure (Pie.Stt.Correction.Provider Pie.Stt.Correction.Unavailable)
      report.result;
  let retry = Option.get report.retry in
  Alcotest.(check (option string))
    "retry effort metadata"
    (Some
       (if List.mem effort [ "medium"; "high"; "xhigh" ] then effort
        else "medium"))
    retry.reasoning_effort;
  Alcotest.(check (option string))
    "retry format metadata"
    (if audio then Some "wav" else None)
    retry.audio_format;
  Alcotest.(check string)
    "request retry effort"
    (Option.get retry.reasoning_effort)
    (string_field "effort"
       (field "reasoning" (Yojson.Safe.from_string (List.hd !requests))));
  no_leak retry.first_diagnostic;
  no_leak retry.final_diagnostic;
  Alcotest.(check string)
    "first failure" first_diagnostic retry.first_diagnostic;
  Alcotest.(check string)
    "final diagnostic"
    (if success then "stt:ok" else "unavailable")
    retry.final_diagnostic;
  let payload = function
    | Json text | Http (_, text) -> text
    | Hang | Close -> ""
  in
  Alcotest.(check (list (pair bool string)))
    "both complete private bodies"
    (List.filter
       (fun (_, body) -> body <> "")
       [ (true, payload first); (false, payload second) ])
    (List.rev !evidence);
  if audio then (
    let requests = List.rev !requests in
    Alcotest.(check bool)
      "first format" true
      (contains
         ("\"format\":\"" ^ (if wav then "wav" else "mp3") ^ "\"")
         (List.nth requests 0));
    Alcotest.(check bool)
      "retry WAV" true
      (contains "\"format\":\"wav\"" (List.nth requests 1));
    Alcotest.(check bool)
      "WAV bytes" true
      (contains
         (Pie.Stt.Openrouter.base64_encode (Bytes.of_string "WAV"))
         (List.nth requests 1)))
  else
    let content body = Yojson.Safe.from_string body |> field "input" in
    Alcotest.(check bool)
      "text and prompt unchanged" true
      (content (List.nth !requests 0) = content (List.nth !requests 1))

let child_retry_success ~(caps : < Ox_caps.Cap.env ; .. >) ?(cancelled = false)
    () =
  let evidence = ref [] in
  let report, hits, _ =
    with_fixture ~caps
      ~reply:
        (Json
           (if cancelled then
              status_body "cancelled" (answer_json "never deliver")
            else completed "first success"))
      ~extra_replies:[ Json choice_error_body ]
      (fun ~sw ~net ~clock ~logger ->
        Pie.Stt.Correction.run_with_retry ~caps ~sw ~net ~clock ~logger
          ~model:"test-model" ~prompt_template:prompt_sentinel
          ~transcript:transcript_sentinel
          ~on_response:(fun ~first body ->
            evidence := (first, Bytes.to_string body) :: !evidence)
          ~timeout:(Ox.Duration.seconds 1) ())
  in
  Alcotest.(check int) "success or cancellation never retries" 1 hits;
  Alcotest.(check (result string string))
    "terminal result"
    (if cancelled then Error "cancelled" else Ok "first success")
    (Result.map_error Pie.Stt.Correction.diagnostic report.result);
  Alcotest.(check bool) "no retry metadata" true (report.retry = None);
  Alcotest.(check (list bool))
    "final evidence only" [ false ] (List.map fst !evidence)

let child_retry_cancel ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let evidence = ref [] in
  let result, hits, _ =
    with_fixture ~caps ~reply:(Json choice_error_body)
      ~extra_replies:[ Hang; Json (completed "third-forbidden") ]
      (fun ~sw ~net ~clock ~logger ->
        Eio.Time.with_timeout clock 0.1 (fun () ->
            Ok
              (Pie.Stt.Correction.run_with_retry ~caps ~sw ~net ~clock ~logger
                 ~model:"test-model" ~prompt_template:prompt_sentinel
                 ~transcript:transcript_sentinel
                 ~on_response:(fun ~first body ->
                   evidence := (first, Bytes.to_string body) :: !evidence)
                 ~timeout:(Ox.Duration.seconds 1) ())))
  in
  Alcotest.(check bool) "cancellation propagates" true (result = Error `Timeout);
  Alcotest.(check int) "cancelled retry has no third call" 2 hits;
  Alcotest.(check (list (pair bool string)))
    "first evidence kept before cancellation"
    [ (true, choice_error_body) ]
    (List.rev !evidence)

let child_success ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let result, hits, captured =
    with_fixture ~caps
      ~reply:(Json (completed "  hello\nworld  "))
      (fun ~sw ~net ~clock ~logger ->
        call ~caps ~sw ~net ~clock ~logger
          ~prompt_template:(prompt_sentinel ^ " {{glossary}}")
          ())
  in
  Alcotest.(check int) "one request" 1 hits;
  (match result with
  | Ok text -> Alcotest.(check string) "trimmed" "hello\nworld" text
  | Error failure ->
      Alcotest.failf "expected success: %s"
        (Pie.Stt.Correction.diagnostic failure));
  match captured with
  | None -> Alcotest.fail "fixture saw no request"
  | Some (head, body) ->
      let json = Yojson.Safe.from_string body in
      Alcotest.(check string)
        "rendered instructions"
        (prompt_sentinel ^ " " ^ glossary_sentinel)
        (string_field "instructions" json);
      Alcotest.(check bool)
        "default temperature" true
        (abs_float (float_field "temperature" json -. 0.2) < 1e-6);
      Alcotest.(check bool)
        "reasoning omitted" true
        (List.assoc_opt "reasoning" (fields json) = None);
      if not (String.starts_with ~prefix:"POST /api/v1/responses " head) then
        Alcotest.fail "request line was not POST /api/v1/responses"

let child_blank ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let result, _, _ =
    with_fixture ~caps
      ~reply:(Json (completed " \n\t "))
      (fun ~sw ~net ~clock ~logger -> call ~caps ~sw ~net ~clock ~logger ())
  in
  expect_failure Pie.Stt.Correction.Blank result

let child_at_budget ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let text = String.make Pie.Dictation_limits.result_utf8_bytes 'y' in
  let result, _, _ =
    with_fixture ~caps
      ~reply:(Json (completed text))
      (fun ~sw ~net ~clock ~logger -> call ~caps ~sw ~net ~clock ~logger ())
  in
  match result with
  | Ok got ->
      Alcotest.(check int)
        "budget length" Pie.Dictation_limits.result_utf8_bytes
        (String.length got)
  | Error failure ->
      Alcotest.failf "text at the budget was rejected: %s"
        (Pie.Stt.Correction.diagnostic failure)

let child_oversize ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let text = String.make (Pie.Dictation_limits.result_utf8_bytes + 1) 'x' in
  let result, _, _ =
    with_fixture ~caps
      ~reply:(Json (completed text))
      (fun ~sw ~net ~clock ~logger -> call ~caps ~sw ~net ~clock ~logger ())
  in
  expect_failure Pie.Stt.Correction.Over_budget result

let child_reject ~(caps : < Ox_caps.Cap.env ; .. >) reply expected () =
  let result, _, _ =
    with_fixture ~caps ~reply (fun ~sw ~net ~clock ~logger ->
        call ~caps ~sw ~net ~clock ~logger ())
  in
  expect_failure expected result

let child_timeout ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  let elapsed = ref 0. in
  let result, hits, captured =
    with_fixture ~caps ~reply:Hang (fun ~sw ~net ~clock ~logger ->
        let started = monotonic_seconds () in
        let result =
          call ~caps ~sw ~net ~clock ~logger ~timeout:(Ox.Duration.ms 300) ()
        in
        elapsed := monotonic_seconds () -. started;
        result)
  in
  if hits <> 1 then
    Alcotest.failf "fixture hits %d, result %s" hits
      (match result with
      | Ok _ -> "ok"
      | Error failure -> Pie.Stt.Correction.diagnostic failure);
  (match captured with
  | None -> Alcotest.fail "timeout fixture saw no request"
  | Some _ -> ());
  expect_failure Pie.Stt.Correction.Timeout result;
  if !elapsed > 2.0 then Alcotest.failf "timeout took %.2fs" !elapsed

let child_missing_key ~(caps : < Ox_caps.Cap.env ; .. >) () =
  (* A closed port, not a listening socket: a mistaken call fails as
     unavailable instead of hanging on accept. *)
  Ox_caps.Cap_unix.putenv ~caps "OPENROUTER_API_KEY" "";
  Ox_caps.Cap_unix.putenv ~caps "OPENROUTER_BASE_URL" "http://127.0.0.1:1";
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    call ~caps ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
      ~logger:(logger env) ()
  in
  expect_failure (Pie.Stt.Correction.Provider Missing_api_key) result

let env_without ~(caps : < Ox_caps.Cap.env ; .. >) keys =
  Ox_caps.Cap_unix.environment ~caps ()
  |> Array.to_list
  |> List.filter (fun entry ->
      not
        (List.exists
           (fun key ->
             String.length entry >= String.length key + 1
             && String.sub entry 0 (String.length key) = key
             && entry.[String.length key] = '=')
           keys))
  |> Array.of_list

let spawn_case
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdin
       ; Ox_caps.Cap.stdout
       ; .. >) name ~with_key =
  let base =
    env_without ~caps [ "OPENROUTER_API_KEY"; "OPENROUTER_BASE_URL"; case_env ]
  in
  let extra =
    if with_key then
      [| "OPENROUTER_API_KEY=synthetic-correction-key"; case_env ^ "=" ^ name |]
    else [| case_env ^ "=" ^ name |]
  in
  let env =
    Array.append base (Array.append extra [| "OTEL_SDK_DISABLED=true" |])
  in
  let exe = Sys.executable_name in
  let pid =
    Ox_caps.Cap_unix.create_process_env ~caps exe [| exe |] env
      (Unix.descr_of_in_channel (Ox_caps.Cap_stdlib.stdin ~caps))
      (Unix.descr_of_out_channel (Ox_caps.Cap_stdlib.stdout ~caps))
      (Unix.descr_of_out_channel (Ox_caps.Cap_stdlib.stderr ~caps))
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Alcotest.failf "correction child %s exited %d" name n
  | Unix.WSIGNALED s -> Alcotest.failf "correction child %s signaled %d" name s
  | Unix.WSTOPPED s -> Alcotest.failf "correction child %s stopped %d" name s

module C = Pie.Stt.Correction

(* A four-minute dictation (7.7 MB of PCM) was refused locally as "input too
   large" by Ox's 4 MiB default request limit. 6 MiB of mp3 must reach the
   provider with its format. *)
let child_large_audio ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let mp3 = Bytes.make (6 * 1024 * 1024) 'm' in
  let result, hits, captured =
    with_fixture ~caps
      ~reply:(Json (completed "heard"))
      (fun ~sw ~net ~clock ~logger ->
        call ~caps ~sw ~net ~clock ~logger ~audio:mp3 ~audio_format:"mp3"
          ~transcript:Pie.Stt.Correction.audio_cue ())
  in
  Alcotest.(check int) "one request" 1 hits;
  (match result with
  | Ok "heard" -> ()
  | Ok text -> Alcotest.failf "unexpected text %s" text
  | Error failure ->
      Alcotest.failf "request failed: %s"
        (Pie.Stt.Correction.diagnostic failure));
  match captured with
  | None -> Alcotest.fail "fixture saw no request"
  | Some (_head, body) ->
      let json = Yojson.Safe.from_string body in
      let audio_part =
        match field "input" json with
        | `List [ message ] -> (
            match field "content" message with
            | `List [ _; audio_part ] -> audio_part
            | _ -> Alcotest.fail "user content was not text plus audio")
        | _ -> Alcotest.fail "input was not one message"
      in
      Alcotest.(check string)
        "format" "mp3"
        (string_field "format" (field "input_audio" audio_part))

let child_audio ~(caps : < Ox_caps.Cap.env ; .. >) () =
  let wav = Bytes.of_string "RIFF-wav" in
  let order = [ "Google AI Studio"; "Google" ] in
  let result, hits, captured =
    with_fixture ~caps
      ~reply:(Json (completed "heard"))
      (fun ~sw ~net ~clock ~logger ->
        call ~caps ~sw ~net ~clock ~logger ~audio:wav ~provider_order:order
          ~transcript:Pie.Stt.Correction.audio_cue ~reasoning_effort:"low" ())
  in
  Alcotest.(check int) "one request" 1 hits;
  (match result with
  | Ok "heard" -> ()
  | Ok text -> Alcotest.failf "unexpected text %s" text
  | Error failure ->
      Alcotest.failf "request failed: %s"
        (Pie.Stt.Correction.diagnostic failure));
  match captured with
  | None -> Alcotest.fail "fixture saw no request"
  | Some (head, body) ->
      if not (String.starts_with ~prefix:"POST /api/v1/responses " head) then
        Alcotest.fail "request line was not POST /api/v1/responses";
      let json = Yojson.Safe.from_string body in
      let input =
        match field "input" json with
        | `List [ message ] -> message
        | _ -> Alcotest.fail "input was not one message"
      in
      let text_part, audio_part =
        match field "content" input with
        | `List [ text_part; audio_part ] -> (text_part, audio_part)
        | _ -> Alcotest.fail "user content was not text plus audio"
      in
      Alcotest.(check string)
        "text type" "input_text"
        (string_field "type" text_part);
      Alcotest.(check string)
        "cue" Pie.Stt.Correction.audio_cue
        (string_field "text" text_part);
      Alcotest.(check string)
        "audio type" "input_audio"
        (string_field "type" audio_part);
      let audio = field "input_audio" audio_part in
      Alcotest.(check string) "format" "wav" (string_field "format" audio);
      Alcotest.(check string)
        "wav base64"
        (Pie.Stt.Openrouter.base64_encode wav)
        (string_field "data" audio);
      let provider = field "provider" json in
      let names =
        match field "order" provider with
        | `List items ->
            List.map
              (function
                | `String name -> name
                | other ->
                    Alcotest.failf "order item %s" (Yojson.Safe.to_string other))
              items
        | _ -> Alcotest.fail "provider.order was not a list"
      in
      Alcotest.(check (list string)) "provider.order" order names;
      Alcotest.(check bool)
        "fallbacks allowed" true
        (List.assoc_opt "allow_fallbacks" (fields provider) = None)

let child_main ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) name =
  match name with
  | "translate-framing" -> child_request ~caps ~output_language:"English" ()
  | "request" -> child_request ~caps () (* dict-ldo9 dict-l4zp dict-g52m *)
  | "retry-provider-cancel" -> child_retry_success ~caps ~cancelled:true ()
  | "retry-success" -> child_retry_success ~caps ()
  | "retry-cancel" -> child_retry_cancel ~caps ()
  | "retry-medium" ->
      child_retry ~caps ~effort:"medium" ~first:(Json choice_error_body)
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:true ()
  | "retry-high" ->
      child_retry ~caps ~effort:"high" ~first:(Json choice_error_body)
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:true ()
  | "retry-wav" ->
      child_retry ~caps ~wav:true ~first:(Json choice_error_body)
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:true ()
  | "retry-malformed" ->
      child_retry ~caps ~first_diagnostic:"invalid response"
        ~first:(Json "{bad")
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:true ()
  | "retry-timeout" ->
      child_retry ~caps ~first_diagnostic:"timeout" ~first:Hang
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:false ()
  | "retry-429" ->
      child_retry ~caps
        ~first:(Http (429, choice_error_body))
        ~second:(Http (429, choice_error_body))
        ~success:false ~audio:false ()
  | "retry-close" ->
      child_retry ~caps ~first:Close
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:false ()
  | "retry-output" ->
      child_retry ~caps ~first_diagnostic:"invalid response"
        ~first:(Json (completed_raw "not JSON"))
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:false ()
  | "retry-tool" ->
      child_retry ~caps ~first_diagnostic:"tool continuation"
        ~first:(Json tool_body)
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:false ()
  | "choice-error" -> child_choice_error ~caps ()
  | "wav-retry" ->
      child_retry ~caps ~first:(Json choice_error_body)
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:true ()
  | "retry-fails" ->
      child_retry ~caps ~first:(Json choice_error_body)
        ~second:(Json choice_error_body) ~success:false ~audio:true ()
  | "http-retry" ->
      child_retry ~caps
        ~first:(Http (500, choice_error_body))
        ~second:(Json (completed "WAV recovery"))
        ~success:true ~audio:false ()
  | "success" -> child_success ~caps () (* dict-ldo9 dict-g52m *)
  | "answer-envelope" -> child_answer_envelope ~caps ()
  | "unframed-analysis" -> child_unframed_analysis ~caps ()
  | "invalid-envelopes" -> child_invalid_envelopes ~caps ()
  | "reasoning-item" -> child_reasoning_item ~caps () (* dict-dq3o dict-ldo9 *)
  | "blank" -> child_blank ~caps () (* dict-bpuh *)
  | "at-budget" -> child_at_budget ~caps () (* dict-fk8i *)
  | "oversize" -> child_oversize ~caps () (* dict-fk8i *)
  | "refusal" ->
      child_reject ~caps (Json refusal_body) (C.Rejected C.Refusal) ()
      (* dict-bpuh dict-swyn *)
  | "tool" ->
      child_reject ~caps (Json tool_body) (C.Rejected C.Tool_continuation) ()
      (* dict-bpuh dict-swyn *)
  | "incomplete" ->
      child_reject ~caps
        (Json (status_body "incomplete" "should-not-deliver"))
        (C.Rejected C.Incomplete) ()
      (* dict-bpuh dict-swyn *)
  | "cancelled" ->
      child_reject ~caps
        (Json (status_body "cancelled" "should-not-deliver"))
        (C.Rejected C.Cancelled) ()
      (* dict-bpuh dict-swyn *)
  | "unknown-status" ->
      child_reject ~caps
        (Json (status_body provider_sentinel "should-not-deliver"))
        (C.Rejected C.Not_completed) ()
      (* dict-bpuh dict-swyn *)
  | "failed" ->
      child_reject ~caps (Json failed_body) (C.Provider C.Unavailable) ()
      (* dict-bpuh dict-swyn *)
  | "transport" ->
      child_reject ~caps
        (Http (500, provider_sentinel ^ transcript_sentinel))
        (C.Provider C.Unavailable) ()
      (* dict-bpuh dict-swyn *)
  | "timeout" -> child_timeout ~caps () (* dict-l4zp dict-bpuh *)
  | "missing-key" -> child_missing_key ~caps () (* dict-bpuh *)
  | "audio" -> child_audio ~caps () (* dict-dq3o dict-4y4r *)
  | "large-audio" -> child_large_audio ~caps () (* dict-dq3o *)
  | other -> Alcotest.failf "unknown child case %s" other

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.time
       ; .. >) =
  match Ox_caps.Cap_sys.getenv_opt ~caps case_env with
  | Some name -> child_main ~caps name
  | None ->
      let case name stories ~with_key =
        Alcotest.test_case
          (name ^ " " ^ stories)
          `Quick
          (fun () -> spawn_case ~caps name ~with_key)
      in
      Alcotest.run "correction"
        [
          ( "adapter",
            [
              case "translate-framing" "dict-7k2q" ~with_key:true;
              case "request" "dict-ldo9 dict-l4zp dict-g52m" ~with_key:true;
              case "retry-close" "dict-6wq3" ~with_key:true;
              case "retry-output" "dict-6wq3" ~with_key:true;
              case "retry-tool" "dict-6wq3" ~with_key:true;
              case "retry-provider-cancel" "dict-6wq3 dict-x6e9" ~with_key:true;
              case "retry-success" "dict-6wq3 dict-x6e9" ~with_key:true;
              case "retry-cancel" "dict-6wq3 dict-x6e9" ~with_key:true;
              case "retry-medium" "dict-6wq3" ~with_key:true;
              case "retry-high" "dict-6wq3" ~with_key:true;
              case "retry-wav" "dict-6wq3" ~with_key:true;
              case "retry-malformed" "dict-6wq3" ~with_key:true;
              case "retry-timeout" "dict-6wq3" ~with_key:true;
              case "retry-429" "dict-6wq3" ~with_key:true;
              case "choice-error" "dict-6wq3" ~with_key:true;
              case "wav-retry" "dict-6wq3 dict-x6e9" ~with_key:true;
              case "retry-fails" "dict-6wq3" ~with_key:true;
              case "http-retry" "dict-6wq3 dict-x6e9" ~with_key:true;
              case "success" "dict-ldo9 dict-g52m" ~with_key:true;
              case "answer-envelope" "dict-dq3o dict-ldo9" ~with_key:true;
              case "unframed-analysis" "dict-dq3o dict-ldo9" ~with_key:true;
              case "invalid-envelopes" "dict-dq3o dict-ldo9" ~with_key:true;
              case "reasoning-item" "dict-dq3o dict-ldo9" ~with_key:true;
              case "blank" "dict-bpuh" ~with_key:true;
              case "at-budget" "dict-fk8i" ~with_key:true;
              case "oversize" "dict-fk8i" ~with_key:true;
              case "refusal" "dict-bpuh dict-swyn" ~with_key:true;
              case "tool" "dict-bpuh dict-swyn" ~with_key:true;
              case "incomplete" "dict-bpuh dict-swyn" ~with_key:true;
              case "cancelled" "dict-bpuh dict-swyn" ~with_key:true;
              case "unknown-status" "dict-bpuh dict-swyn" ~with_key:true;
              case "failed" "dict-bpuh dict-swyn" ~with_key:true;
              case "transport" "dict-bpuh dict-swyn" ~with_key:true;
              case "timeout" "dict-l4zp dict-bpuh" ~with_key:true;
              case "missing-key" "dict-bpuh" ~with_key:false;
              case "audio" "dict-dq3o dict-4y4r" ~with_key:true;
              case "large-audio" "dict-dq3o" ~with_key:true;
            ] );
        ]

let () = main ~caps:(Ox_test.powerbox ())
