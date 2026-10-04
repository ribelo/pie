(* ^pcaps-n1rg *)
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
  let body =
    match content_length with
    | None | Some 0 -> ""
    | Some n ->
        let raw = Cstruct.create n in
        let rec loop off =
          if off < n then
            match Eio.Flow.single_read flow (Cstruct.sub raw off (n - off)) with
            | 0 -> raise End_of_file
            | k -> loop (off + k)
            | exception End_of_file -> raise End_of_file
        in
        loop 0;
        Cstruct.to_string raw
  in
  (head, body)

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

let expect_ok expected result =
  match result with
  | Ok text -> Alcotest.(check string) "delivered text" expected text
  | Error (`Stt_error message) -> Alcotest.failf "expected success: %s" message

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
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >)
    name =
  let env =
    Array.append
      (env_without ~caps
         [ "OPENROUTER_API_KEY"; "OPENROUTER_BASE_URL"; case_env ])
      [|
        "OPENROUTER_API_KEY=synthetic-or-outcome";
        case_env ^ "=" ^ name;
        "OTEL_SDK_DISABLED=true";
      |]
  in
  let exe = Sys.executable_name in
  let pid =
    Ox_caps.Cap_unix.create_process_env ~caps exe [| exe |] env Unix.stdin
      Unix.stdout Unix.stderr
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Alcotest.failf "outcome child %s exited %d" name n
  | Unix.WSIGNALED s -> Alcotest.failf "outcome child %s signaled %d" name s
  | Unix.WSTOPPED s -> Alcotest.failf "outcome child %s stopped %d" name s

let contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let request_target head =
  match String.split_on_char '\n' head with
  | line :: _ -> (
      let line =
        let len = String.length line in
        if len > 0 && line.[len - 1] = '\r' then String.sub line 0 (len - 1)
        else line
      in
      match String.split_on_char ' ' line with
      | _method :: target :: _ -> target
      | _ -> "")
  | [] -> ""

let part_present name value body =
  contains body ("name=\"" ^ name ^ "\"\r\n\r\n" ^ value ^ "\r\n")

let assert_part name value body =
  if not (part_present name value body) then
    Alcotest.failf "missing %s part" name

let assert_part_absent name body =
  if contains body ("name=\"" ^ name ^ "\"") then
    Alcotest.failf "unexpected %s part" name

(* dict-m2xr dict-wha3: buffered OpenRouter pass 1 sends language and the
   glossary prompt, and omits either field when it is absent. *)
let run_multipart ~(caps : < Ox_caps.Cap.env ; .. >) ?language ?prompt () =
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
  Ox_caps.Cap_unix.putenv ~caps "OPENROUTER_BASE_URL"
    (Printf.sprintf "http://127.0.0.1:%d" port);
  let captured = ref ("", "") in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run (fun conn_sw ->
          let flow, _ = Eio.Net.accept ~sw:conn_sw socket in
          let head, body = read_http_request flow in
          captured := (request_target head, body);
          write_http_json flow
            (Yojson.Safe.to_string (`Assoc [ ("text", `String "hello-pass1") ]))));
  let result =
    Pie.Stt.Openrouter.transcribe ~caps ~sw ~net ~clock:(Eio.Stdenv.clock env)
      ~api_key:(Ox_observability.Redact.make "synthetic-or-outcome")
      ~model:"microsoft/mai-transcribe-2" ?language ?prompt pcm
  in
  let target, body = !captured in
  (result, target, body)

let child_main ~(caps : < Ox_caps.Cap.env ; .. >) name =
  match name with
  | "multipart-both" ->
      let result, target, body =
        run_multipart ~caps ~language:"pl" ~prompt:"Nema\nPie" ()
      in
      expect_ok "hello-pass1" result;
      if not (contains target "/api/v1/audio/transcriptions") then
        Alcotest.failf "unexpected target %s" target;
      assert_part "language" "pl" body;
      assert_part "prompt" "Nema\nPie" body
  | "multipart-neither" ->
      let result, target, body = run_multipart ~caps () in
      expect_ok "hello-pass1" result;
      if not (contains target "/api/v1/audio/transcriptions") then
        Alcotest.failf "unexpected target %s" target;
      assert_part_absent "language" body;
      assert_part_absent "prompt" body
  | other -> Alcotest.failf "unknown child case %s" other

let main
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.exec ; Ox_caps.Cap.fork ; .. >) =
  match Ox_caps.Cap_sys.getenv_opt ~caps case_env with
  | Some name -> child_main ~caps name
  | None ->
      Alcotest.run "openrouter_outcomes"
        [
          ( "public",
            [
              Alcotest.test_case
                "multipart carries language and prompt (dict-m2xr dict-wha3)"
                `Quick (fun () -> spawn_case ~caps "multipart-both");
              Alcotest.test_case
                "multipart omits language and prompt when absent (dict-wha3)"
                `Quick (fun () -> spawn_case ~caps "multipart-neither");
            ] );
        ]

let () = main ~caps:(Ox_test.powerbox ())
