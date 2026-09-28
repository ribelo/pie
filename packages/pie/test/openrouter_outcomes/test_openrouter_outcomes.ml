let pcm = Bytes.make 320 '\001'
let case_env = "PIE_OR_CASE"

let has_suffix suffix s =
  let sl = String.length suffix in
  let n = String.length s in
  n >= sl && String.sub s (n - sl) sl = suffix

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
    match String.split_on_char '\n' lower with
    | lines ->
        List.find_map
          (fun line ->
            if String.starts_with ~prefix:"content-length:" line then
              int_of_string_opt
                (String.trim (String.sub line 15 (String.length line - 15)))
            else None)
          lines
  in
  (match content_length with
  | None | Some 0 -> ()
  | Some n ->
      let body = Cstruct.create n in
      let rec loop off =
        if off < n then
          match Eio.Flow.single_read flow (Cstruct.sub body off (n - off)) with
          | 0 -> raise End_of_file
          | k -> loop (off + k)
          | exception End_of_file -> raise End_of_file
      in
      loop 0);
  head

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

let structured_text value =
  Yojson.Safe.to_string (`Assoc [ ("text", `String value) ])

let output_message inner =
  `Assoc
    [
      ("type", `String "message");
      ( "content",
        `List
          [
            `Assoc [ ("type", `String "output_text"); ("text", `String inner) ];
          ] );
    ]

let responses_body ~status ?extra_output inner =
  let output =
    match extra_output with
    | None -> [ output_message inner ]
    | Some extra -> extra :: [ output_message inner ]
  in
  let fields =
    match status with
    | None -> [ ("output", `List output) ]
    | Some status -> [ ("status", `String status); ("output", `List output) ]
  in
  Yojson.Safe.to_string (`Assoc fields)

let completed_body text =
  responses_body ~status:(Some "completed") (structured_text text)

let incomplete_body text =
  responses_body ~status:(Some "incomplete") (structured_text text)

let cancelled_body text =
  responses_body ~status:(Some "cancelled") (structured_text text)

let missing_body text = responses_body ~status:None (structured_text text)
let malformed_body = responses_body ~status:(Some "completed") "not-json"

let failed_body =
  Yojson.Safe.to_string
    (`Assoc
       [
         ("status", `String "failed");
         ("error", `Assoc [ ("message", `String "boom") ]);
       ])

let tool_body text =
  let fn =
    `Assoc
      [
        ("type", `String "function_call");
        ("name", `String "lookup");
        ("call_id", `String "call_1");
        ("arguments", `String "{}");
      ]
  in
  responses_body ~status:(Some "completed") ~extra_output:fn
    (structured_text text)

type op = Transcribe | Translate

let run_public ~body ~op () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
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
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run (fun conn_sw ->
          let flow, _ = Eio.Net.accept ~sw:conn_sw socket in
          ignore (read_http_request flow);
          write_http_json flow body));
  let pcm_bytes = pcm in
  let clock = Eio.Stdenv.clock env in
  let logger =
    Eta_observability.Logger.create ~service:"pie-test" ~clock
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~reporter:
        (Eta_observability.Logger.Memory.reporter
           (Eta_observability.Logger.Memory.create ()))
      ()
  in
  let operation =
    match op with
    | Transcribe ->
        Pie.Stt.Openrouter.transcribe_with_prompt ~sw ~net ~clock ~logger
          ~model:"test-model" ~language:"en" ~prompt_template:"Transcribe."
          pcm_bytes
    | Translate ->
        Pie.Stt.Openrouter.translate_with_prompt ~sw ~net ~clock ~logger
          ~model:"test-model" ~source_language:"en" ~target_language:"pl"
          ~prompt_template:"Translate." pcm_bytes
  in
  operation

let expect_ok expected result =
  match result with
  | Ok text -> Alcotest.(check string) "delivered text" expected text
  | Error (`Stt_error message) -> Alcotest.failf "expected success: %s" message

let expect_reject needle result =
  match result with
  | Ok text -> Alcotest.failf "delivery continuation ran: %s" text
  | Error (`Stt_error rendered) ->
      if
        not
          (String.length rendered >= String.length needle
          &&
          let rec loop i =
            i <= String.length rendered - String.length needle
            && (String.sub rendered i (String.length needle) = needle
               || loop (i + 1))
          in
          loop 0)
      then Alcotest.failf "expected %S in %s" needle rendered

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

let spawn_case name =
  let env =
    Array.append
      (env_without [ "OPENROUTER_API_KEY"; "OPENROUTER_BASE_URL"; case_env ])
      [|
        "OPENROUTER_API_KEY=synthetic-or-outcome";
        case_env ^ "=" ^ name;
        "OTEL_SDK_DISABLED=true";
      |]
  in
  let exe = Sys.executable_name in
  let pid =
    Unix.create_process_env exe [| exe |] env Unix.stdin Unix.stdout Unix.stderr
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Alcotest.failf "outcome child %s exited %d" name n
  | Unix.WSIGNALED s -> Alcotest.failf "outcome child %s signaled %d" name s
  | Unix.WSTOPPED s -> Alcotest.failf "outcome child %s stopped %d" name s

let child_main name =
  match name with
  | "completed-transcribe" ->
      expect_ok "hello-t"
        (run_public ~body:(completed_body "hello-t") ~op:Transcribe ())
  | "completed-translate" ->
      expect_ok "hello-x"
        (run_public ~body:(completed_body "hello-x") ~op:Translate ())
  | "incomplete" ->
      expect_reject "OpenRouter outcome rejected: incomplete"
        (run_public
           ~body:(incomplete_body "should-not-deliver")
           ~op:Transcribe ())
  | "cancelled" ->
      expect_reject "OpenRouter outcome rejected: cancelled"
        (run_public
           ~body:(cancelled_body "should-not-deliver")
           ~op:Transcribe ())
  | "missing" ->
      expect_reject "OpenRouter request failed: invalid response"
        (run_public ~body:(missing_body "should-not-deliver") ~op:Transcribe ())
  | "tool" ->
      expect_reject "OpenRouter outcome rejected: tool continuation"
        (run_public ~body:(tool_body "should-not-deliver") ~op:Transcribe ())
  | "malformed" ->
      expect_reject "OpenRouter structured response was not valid JSON"
        (run_public ~body:malformed_body ~op:Transcribe ())
  | "failed" ->
      expect_reject "OpenRouter request failed:"
        (run_public ~body:failed_body ~op:Transcribe ())
  | other -> Alcotest.failf "unknown child case %s" other

let () =
  match Sys.getenv_opt case_env with
  | Some name -> child_main name
  | None ->
      Alcotest.run "openrouter_outcomes"
        [
          ( "public",
            [
              Alcotest.test_case "completed transcribe" `Quick (fun () ->
                  spawn_case "completed-transcribe");
              Alcotest.test_case "completed translate" `Quick (fun () ->
                  spawn_case "completed-translate");
              Alcotest.test_case "incomplete rejected" `Quick (fun () ->
                  spawn_case "incomplete");
              Alcotest.test_case "cancelled rejected" `Quick (fun () ->
                  spawn_case "cancelled");
              Alcotest.test_case "missing markers rejected" `Quick (fun () ->
                  spawn_case "missing");
              Alcotest.test_case "tool continuation rejected" `Quick (fun () ->
                  spawn_case "tool");
              Alcotest.test_case "malformed content rejected" `Quick (fun () ->
                  spawn_case "malformed");
              Alcotest.test_case "failed provider error retained" `Quick
                (fun () -> spawn_case "failed");
            ] );
        ]
