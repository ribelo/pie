(* Pass-2 correction adapter against a local Responses fixture.
   dict-ldo9 dict-l4zp dict-g52m dict-fk8i dict-bpuh dict-swyn *)

let caps = Ox_test.powerbox ()
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

let completed text =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("status", `String "completed"); ("output", `List [ output_text text ]);
       ])

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

type reply = Json of string | Http of int * string | Hang

let logger env =
  Ox_observability.Logger.create ~service:"pie-correction-test"
    ~clock:(Eio.Stdenv.clock env)
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~reporter:
      (Ox_observability.Logger.Memory.reporter
         (Ox_observability.Logger.Memory.create ()))
    ()

let with_fixture ~reply f =
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
  Unix.putenv "OPENROUTER_BASE_URL" (Printf.sprintf "http://127.0.0.1:%d" port);
  (* The server fiber finishes itself. A nested switch that only ends when the
     outer switch is cancelled can stick on accept, which hung the missing-key
     case. *)
  Eio.Fiber.fork ~sw (fun () ->
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
              match reply with
              | Json payload -> write_http flow 200 payload
              | Http (status, payload) -> write_http flow status payload
              | Hang -> (
                  let scratch = Cstruct.create 1 in
                  (* The client closes on timeout; that read is end-of-file,
                     not a test failure. *)
                  try
                    ignore
                      (Eio.Time.with_timeout clock 2.0 (fun () ->
                           ignore (Eio.Flow.single_read flow scratch);
                           Ok ()))
                  with End_of_file | Eio.Io _ -> ()))));
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

let call ~sw ~net ~clock ~logger ?temperature ?reasoning_effort
    ?(prompt_template = prompt_sentinel ^ " {{glossary}}")
    ?(glossary = glossary_sentinel) ?(language = "pl") ?(source_language = "pl")
    ?(target_language = "en") ?(transcript = transcript_sentinel)
    ?(timeout = Ox.Duration.seconds 5) () =
  Pie.Stt.Correction.run ~caps ~sw ~net ~clock ~logger ~model:"test-model"
    ~prompt_template ~transcript ~glossary ~language ~source_language
    ~target_language ?temperature ?reasoning_effort ~timeout ()

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

let assert_request ~temperature ~reasoning head body =
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
  Alcotest.(check string) "model" "test-model" (string_field "model" json);
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
  match reasoning with
  | None ->
      Alcotest.(check bool)
        "reasoning omitted" true
        (List.assoc_opt "reasoning" (fields json) = None)
  | Some effort ->
      Alcotest.(check string)
        "reasoning effort" effort
        (string_field "effort" (field "reasoning" json))

let child_request () =
  let result, hits, captured =
    with_fixture
      ~reply:(Json (completed "done"))
      (fun ~sw ~net ~clock ~logger ->
        call ~sw ~net ~clock ~logger ~temperature:0.4 ~reasoning_effort:"low"
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
      assert_request ~temperature:0.4 ~reasoning:(Some "low") head body

let child_success () =
  let result, hits, captured =
    with_fixture
      ~reply:(Json (completed "  hello\nworld  "))
      (fun ~sw ~net ~clock ~logger ->
        call ~sw ~net ~clock ~logger
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

let child_blank () =
  let result, _, _ =
    with_fixture
      ~reply:(Json (completed " \n\t "))
      (fun ~sw ~net ~clock ~logger -> call ~sw ~net ~clock ~logger ())
  in
  expect_failure Pie.Stt.Correction.Blank result

let child_at_budget () =
  let text = String.make Pie.Dictation_limits.result_utf8_bytes 'y' in
  let result, _, _ =
    with_fixture
      ~reply:(Json (completed text))
      (fun ~sw ~net ~clock ~logger -> call ~sw ~net ~clock ~logger ())
  in
  match result with
  | Ok got ->
      Alcotest.(check int)
        "budget length" Pie.Dictation_limits.result_utf8_bytes
        (String.length got)
  | Error failure ->
      Alcotest.failf "text at the budget was rejected: %s"
        (Pie.Stt.Correction.diagnostic failure)

let child_oversize () =
  let text = String.make (Pie.Dictation_limits.result_utf8_bytes + 1) 'x' in
  let result, _, _ =
    with_fixture
      ~reply:(Json (completed text))
      (fun ~sw ~net ~clock ~logger -> call ~sw ~net ~clock ~logger ())
  in
  expect_failure Pie.Stt.Correction.Over_budget result

let child_reject reply expected () =
  let result, _, _ =
    with_fixture ~reply (fun ~sw ~net ~clock ~logger ->
        call ~sw ~net ~clock ~logger ())
  in
  expect_failure expected result

let child_timeout () =
  let elapsed = ref 0. in
  let result, hits, captured =
    with_fixture ~reply:Hang (fun ~sw ~net ~clock ~logger ->
        let started = Unix.gettimeofday () in
        let result =
          call ~sw ~net ~clock ~logger ~timeout:(Ox.Duration.ms 300) ()
        in
        elapsed := Unix.gettimeofday () -. started;
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

let child_missing_key () =
  (* A closed port, not a listening socket: a mistaken call fails as
     unavailable instead of hanging on accept. *)
  Unix.putenv "OPENROUTER_API_KEY" "";
  Unix.putenv "OPENROUTER_BASE_URL" "http://127.0.0.1:1";
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    call ~sw ~net:(Eio.Stdenv.net env) ~clock:(Eio.Stdenv.clock env)
      ~logger:(logger env) ()
  in
  expect_failure (Pie.Stt.Correction.Provider Missing_api_key) result

let env_without keys =
  Unix.environment () |> Array.to_list
  |> List.filter (fun entry ->
      not
        (List.exists
           (fun key ->
             String.length entry >= String.length key + 1
             && String.sub entry 0 (String.length key) = key
             && entry.[String.length key] = '=')
           keys))
  |> Array.of_list

let spawn_case name ~with_key =
  let base =
    env_without [ "OPENROUTER_API_KEY"; "OPENROUTER_BASE_URL"; case_env ]
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
    Unix.create_process_env exe [| exe |] env Unix.stdin Unix.stdout Unix.stderr
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Alcotest.failf "correction child %s exited %d" name n
  | Unix.WSIGNALED s -> Alcotest.failf "correction child %s signaled %d" name s
  | Unix.WSTOPPED s -> Alcotest.failf "correction child %s stopped %d" name s

module C = Pie.Stt.Correction

let child_main name =
  match name with
  | "request" -> child_request () (* dict-ldo9 dict-l4zp dict-g52m *)
  | "success" -> child_success () (* dict-ldo9 dict-g52m *)
  | "blank" -> child_blank () (* dict-bpuh *)
  | "at-budget" -> child_at_budget () (* dict-fk8i *)
  | "oversize" -> child_oversize () (* dict-fk8i *)
  | "refusal" ->
      child_reject (Json refusal_body) (C.Rejected C.Refusal) ()
      (* dict-bpuh dict-swyn *)
  | "tool" ->
      child_reject (Json tool_body) (C.Rejected C.Tool_continuation) ()
      (* dict-bpuh dict-swyn *)
  | "incomplete" ->
      child_reject
        (Json (status_body "incomplete" "should-not-deliver"))
        (C.Rejected C.Incomplete) ()
      (* dict-bpuh dict-swyn *)
  | "cancelled" ->
      child_reject
        (Json (status_body "cancelled" "should-not-deliver"))
        (C.Rejected C.Cancelled) ()
      (* dict-bpuh dict-swyn *)
  | "unknown-status" ->
      child_reject
        (Json (status_body provider_sentinel "should-not-deliver"))
        (C.Rejected C.Not_completed) ()
      (* dict-bpuh dict-swyn *)
  | "failed" ->
      child_reject (Json failed_body) (C.Provider C.Invalid_response) ()
      (* dict-bpuh dict-swyn *)
  | "transport" ->
      child_reject
        (Http (500, provider_sentinel ^ transcript_sentinel))
        (C.Provider C.Unavailable) ()
      (* dict-bpuh dict-swyn *)
  | "timeout" -> child_timeout () (* dict-l4zp dict-bpuh *)
  | "missing-key" -> child_missing_key () (* dict-bpuh *)
  | other -> Alcotest.failf "unknown child case %s" other

let () =
  match Sys.getenv_opt case_env with
  | Some name -> child_main name
  | None ->
      let case name stories ~with_key =
        Alcotest.test_case
          (name ^ " " ^ stories)
          `Quick
          (fun () -> spawn_case name ~with_key)
      in
      Alcotest.run "correction"
        [
          ( "adapter",
            [
              case "request" "dict-ldo9 dict-l4zp dict-g52m" ~with_key:true;
              case "success" "dict-ldo9 dict-g52m" ~with_key:true;
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
            ] );
        ]
