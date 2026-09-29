let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

(* PIE013 provider byte budgets. Synthetic PCM and local WS only. *)

let ( let* ) = Result.bind

module Codex = Pie.Stt.Codex_realtime
module Limits = Pie.Dictation_limits

let pp_stt ppf = function `Stt_error msg -> Format.pp_print_string ppf msg

let contains hay needle =
  let hlen = String.length hay in
  let nlen = String.length needle in
  let rec loop i =
    i <= hlen - nlen && (String.sub hay i nlen = needle || loop (i + 1))
  in
  nlen = 0 || loop 0

let cause_text (`Stt_error message) = message

let expect_budget result =
  match result with
  | Ok _ -> Alcotest.fail "expected budget error"
  | Error cause ->
      let text = cause_text cause in
      Alcotest.(check bool) "mentions budget" true (contains text "budget")

let pcm n = Bytes.make n '\001'

let tcp_port = function
  | `Tcp (_, port) -> port
  | _ -> Alcotest.fail "expected tcp port"

let with_listening f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = tcp_port (Eio.Net.listening_addr socket) in
  let url = Printf.sprintf "ws://127.0.0.1:%d" port in
  let memory = Ox_observability.Logger.Memory.create () in
  let logger =
    Ox_observability.Logger.create ~service:"pie-budget-test" ~clock
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~reporter:(Ox_observability.Logger.Memory.reporter memory)
      ()
  in
  f ~sw ~net ~clock ~logger ~url ~socket

let run_offer ~logger ~url ~sw ~net ~clock offer =
  let operation =
    Codex.with_stream_session ~caps ~sw ~net ~clock ~logger ~url
      ~access_token:"synthetic" ~mode:Codex.Transcription ~operation:"budget"
      ~model:"gpt-4o-mini-transcribe"
      ~cancel_message:"Codex realtime transcription cancelled" (fun handle ->
        offer handle)
  in
  operation

let test_queue_minus_one_succeeds () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket:_ ->
  match
    run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
        handle.offer_chunk (pcm (Limits.queue_pcm_bytes - 1)))
  with
  | Ok () -> ()
  | Error cause -> Alcotest.failf "queue-1 failed: %s" (cause_text cause)

let test_queue_exact_succeeds () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket:_ ->
  match
    run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
        handle.offer_chunk (pcm Limits.queue_pcm_bytes))
  with
  | Ok () -> ()
  | Error cause -> Alcotest.failf "queue exact failed: %s" (cause_text cause)

let test_queue_plus_one_rejected () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket:_ ->
  expect_budget
    (run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
         handle.offer_chunk (pcm (Limits.queue_pcm_bytes + 1))))

let test_stalled_then_overflow_rejected () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket:_ ->
  expect_budget
    (run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
         let* () = handle.offer_chunk (pcm Limits.queue_pcm_bytes) in
         handle.offer_chunk (pcm 1)))

let test_cancel_then_offer_fails () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket:_ ->
  match
    run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
        let* () = handle.cancel_stream () in
        handle.offer_chunk (pcm 2))
  with
  | Ok () -> Alcotest.fail "offer after cancel succeeded"
  | Error cause ->
      let text = cause_text cause in
      Alcotest.(check bool)
        "cancelled or closed" true
        (contains text "cancelled" || contains text "closed"
       || contains text "budget")

let test_two_blueprints_independent () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket:_ ->
  let first =
    run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
        handle.offer_chunk (pcm Limits.queue_pcm_bytes))
  in
  let second =
    run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
        handle.offer_chunk (pcm Limits.queue_pcm_bytes))
  in
  (match first with
  | Ok () -> ()
  | Error cause -> Alcotest.failf "first blueprint: %s" (cause_text cause));
  match second with
  | Ok () -> ()
  | Error cause -> Alcotest.failf "second blueprint: %s" (cause_text cause)

let test_buffered_capture_plus_one () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pcm_bytes = pcm (Limits.capture_pcm_bytes + 1) in
  let operation =
    Codex.run_realtime_session ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~url:"ws://127.0.0.1:1"
      ~access_token:"synthetic" ~mode:Codex.Transcription
      ~input_sample_rate:16000 pcm_bytes
  in
  expect_budget operation

let test_openrouter_capture_plus_one () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pcm_bytes = pcm (Limits.capture_pcm_bytes + 1) in
  let operation =
    Pie.Stt.Openrouter.transcribe ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env)
      ~api_key:(Ox_observability.Redact.make "synthetic")
      ~model:"m" ~language:"en" pcm_bytes
  in
  expect_budget operation

let test_empty_offers_are_noop_until_item_cap () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket:_ ->
  match
    run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
        let rec empties n =
          if n = 0 then Ok ()
          else
            let* () = handle.offer_chunk (pcm 0) in
            empties (n - 1)
        in
        let rec tinies n =
          if n = 0 then Ok ()
          else
            let* () = handle.offer_chunk (pcm 1) in
            tinies (n - 1)
        in
        let* () = empties (Limits.queue_items * 2) in
        let* () = tinies Limits.queue_items in
        handle.offer_chunk (pcm 1))
  with
  | Ok () -> Alcotest.fail "tiny item cap not enforced"
  | Error cause ->
      Alcotest.(check bool)
        "item cap" true
        (contains (cause_text cause) "budget")

let test_empty_offer_after_cancel_fails () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket:_ ->
  match
    run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
        let* () = handle.cancel_stream () in
        handle.offer_chunk (pcm 0))
  with
  | Ok () -> Alcotest.fail "empty offer after cancel succeeded"
  | Error _ -> ()

let has_suffix suffix value =
  let suffix_len = String.length suffix in
  let value_len = String.length value in
  value_len >= suffix_len
  && String.equal suffix (String.sub value (value_len - suffix_len) suffix_len)

let header_value name raw =
  let expected = String.lowercase_ascii name in
  raw |> String.split_on_char '\n'
  |> List.find_map (fun line ->
      let line =
        let len = String.length line in
        if len > 0 && Char.equal line.[len - 1] '\r' then
          String.sub line 0 (len - 1)
        else line
      in
      match String.index_opt line ':' with
      | None -> None
      | Some index ->
          let actual =
            String.sub line 0 index |> String.trim |> String.lowercase_ascii
          in
          if String.equal actual expected then
            Some
              (String.sub line (index + 1) (String.length line - index - 1)
              |> String.trim)
          else None)

let read_http_head flow =
  let scratch = Cstruct.create 1 in
  let buffer = Buffer.create 512 in
  let rec loop () =
    let read = Eio.Flow.single_read flow scratch in
    if read = 0 then raise End_of_file;
    Buffer.add_char buffer (Cstruct.get_char scratch 0);
    let contents = Buffer.contents buffer in
    if has_suffix "\r\n\r\n" contents then contents else loop ()
  in
  loop ()

let write_ws_text flow payload =
  Ox_http.Ws_codec.encode
    { fin = true; opcode = Text; payload = Bytes.of_string payload }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let write_ws_close flow =
  let payload = Bytes.create 2 in
  Bytes.set payload 0 (Char.chr 3);
  Bytes.set payload 1 (Char.chr 232);
  Ox_http.Ws_codec.encode { fin = true; opcode = Close; payload }
  |> Bytes.to_string
  |> fun encoded -> Eio.Flow.copy_string encoded flow

let write_ws_switching_response flow key =
  let response =
    "HTTP/1.1 101 Switching Protocols\r\n" ^ "Upgrade: websocket\r\n"
    ^ "Connection: keep-alive, Upgrade\r\n" ^ "Sec-WebSocket-Accept: "
    ^ Ox_http.Ws_codec.accept_key ~sha1:Ox_http.Ox_http_tls_openssl.sha1 key
    ^ "\r\n\r\n"
  in
  Eio.Flow.copy_string response flow

type ws_frame = Text of string | Close | Other

let read_ws_frame flow =
  let read_exact len =
    let cs = Cstruct.create len in
    let rec loop off =
      if off < len then
        match Eio.Flow.single_read flow (Cstruct.sub cs off (len - off)) with
        | 0 -> raise End_of_file
        | n -> loop (off + n)
    in
    loop 0;
    Cstruct.to_bytes cs
  in
  let header = read_exact 2 in
  let opcode = Char.code (Bytes.get header 0) land 0x0f in
  let masked = Char.code (Bytes.get header 1) land 0x80 <> 0 in
  let len0 = Char.code (Bytes.get header 1) land 0x7f in
  let payload_len =
    if len0 = 126 then
      let ext = read_exact 2 in
      (Char.code (Bytes.get ext 0) lsl 8) lor Char.code (Bytes.get ext 1)
    else if len0 = 127 then (
      let ext = read_exact 8 in
      let v = ref 0 in
      for i = 0 to 7 do
        v := (!v lsl 8) lor Char.code (Bytes.get ext i)
      done;
      !v)
    else len0
  in
  let mask_key = if masked then Some (read_exact 4) else None in
  let payload = read_exact payload_len in
  (match mask_key with
  | Some key ->
      for i = 0 to payload_len - 1 do
        let b =
          Char.code (Bytes.get payload i)
          lxor Char.code (Bytes.get key (i mod 4))
        in
        Bytes.set payload i (Char.chr b)
      done
  | None -> ());
  match opcode with
  | 1 -> Text (Bytes.to_string payload)
  | 8 -> Close
  | _ -> Other

let handshake flow =
  let head = read_http_head flow in
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> Alcotest.fail "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key

let is_audio_append payload = contains payload "input_audio_buffer.append"
let updated = {|{"type":"session.updated"}|}
let created = {|{"type":"session.created"}|}

let serve_script ~after_handshake ~after_update ~after_append flow =
  handshake flow;
  List.iter (write_ws_text flow) after_handshake;
  let rec loop sent =
    match read_ws_frame flow with
    | exception End_of_file -> ()
    | Close -> ()
    | Other -> loop sent
    | Text payload ->
        if contains payload "session.update" then (
          List.iter (write_ws_text flow) after_update;
          loop sent)
        else if (not sent) && is_audio_append payload then (
          List.iter (write_ws_text flow) after_append;
          write_ws_close flow;
          loop true)
        else loop sent
  in
  loop false

let run_buffered ~mode ~session_update ~script_handshake ~script_append
    pcm_bytes =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket ->
  Eio.Fiber.fork ~sw (fun () ->
      try
        let flow, _ = Eio.Net.accept ~sw socket in
        serve_script ~after_handshake:script_handshake ~after_update:[ updated ]
          ~after_append:script_append flow
      with _ -> ());
  let operation =
    Codex.run_realtime_session ~caps ~sw ~net ~clock ~url
      ~access_token:"synthetic" ~mode ~session_update ~input_sample_rate:16000
      pcm_bytes
  in
  operation

let json_delta text =
  Printf.sprintf {|{"type":"response.output_text.delta","delta":%S}|} text

let json_final text =
  Printf.sprintf
    {|{"type":"conversation.item.input_audio_transcription.completed","transcript":%S}|}
    text

let conversation_update =
  Codex.build_conversation_session_update ~model:"gpt-realtime-2.1" ~prompt:"p"

let json_done_completed =
  {|{"type":"response.done","response":{"status":"completed"}}|}

let delta_chunks ~n ~size fill =
  List.init n (fun _ -> json_delta (String.make size fill))

let test_result_minus_one () =
  let size = 32_768 in
  let n = (Limits.result_utf8_bytes - 1) / size in
  let rem = Limits.result_utf8_bytes - 1 - (n * size) in
  let append =
    delta_chunks ~n ~size 'a'
    @ (if rem = 0 then [] else [ json_delta (String.make rem 'a') ])
    @ [ json_done_completed ]
  in
  match
    run_buffered ~mode:Codex.Conversation ~session_update:conversation_update
      ~script_handshake:[ created ] ~script_append:append (pcm 320)
  with
  | Ok got ->
      Alcotest.(check int)
        "len-1"
        (Limits.result_utf8_bytes - 1)
        (String.length got)
  | Error cause -> Alcotest.failf "result-1 failed: %s" (cause_text cause)

let test_result_exact () =
  let size = 32_768 in
  let n = Limits.result_utf8_bytes / size in
  let append = delta_chunks ~n ~size 'b' @ [ json_done_completed ] in
  match
    run_buffered ~mode:Codex.Conversation ~session_update:conversation_update
      ~script_handshake:[ created ] ~script_append:append (pcm 320)
  with
  | Ok got ->
      Alcotest.(check int) "exact" Limits.result_utf8_bytes (String.length got)
  | Error cause -> Alcotest.failf "result exact failed: %s" (cause_text cause)

let test_result_plus_one () =
  let size = 32_768 in
  let n = Limits.result_utf8_bytes / size in
  let append =
    delta_chunks ~n ~size 'c' @ [ json_delta "x"; json_done_completed ]
  in
  expect_budget
    (run_buffered ~mode:Codex.Conversation ~session_update:conversation_update
       ~script_handshake:[ created ] ~script_append:append (pcm 320))

let test_final_replaces_partials () =
  let chunk = String.make (Limits.result_utf8_bytes * 2 / 3) 'd' in
  match
    run_buffered ~mode:Codex.Transcription
      ~session_update:
        (Codex.build_transcription_session_update
           ~model:"gpt-4o-mini-transcribe" ())
      ~script_handshake:[ created ]
      ~script_append:[ json_delta chunk; json_final chunk ]
      (pcm 320)
  with
  | Ok got -> Alcotest.(check string) "replaced" chunk got
  | Error cause ->
      Alcotest.failf "replacement double-counted or failed: %s"
        (cause_text cause)

let test_empty_deltas_do_not_grow () =
  let empties = List.init 512 (fun _ -> json_delta "") in
  match
    run_buffered ~mode:Codex.Transcription
      ~session_update:
        (Codex.build_transcription_session_update
           ~model:"gpt-4o-mini-transcribe" ())
      ~script_handshake:[ created ]
      ~script_append:(empties @ [ json_final "ok" ])
      (pcm 320)
  with
  | Ok got -> Alcotest.(check string) "final after empties" "ok" got
  | Error cause -> Alcotest.failf "empty deltas failed: %s" (cause_text cause)

let test_offer_after_provider_failure () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket ->
  Eio.Fiber.fork ~sw (fun () ->
      try
        let flow, _ = Eio.Net.accept ~sw socket in
        handshake flow;
        let rec wait () =
          match read_ws_frame flow with
          | exception End_of_file -> ()
          | Close -> ()
          | Other -> wait ()
          | Text _ ->
              write_ws_text flow
                {|{"type":"error","error":{"message":"SECRET","code":"bad"}}|};
              write_ws_close flow
        in
        wait ()
      with _ -> ());
  match
    run_offer ~logger ~url ~sw ~net ~clock (fun handle ->
        let rec spin n =
          if n <= 0 then Error (`Stt_error "timeout")
          else
            let offered = handle.offer_chunk (pcm 320) in
            match offered with
            | Error err -> Error err
            | Ok () ->
                Eio.Time.sleep clock 0.01;
                spin (n - 1)
        in
        spin 200)
  with
  | Ok () -> Alcotest.fail "offer never observed provider failure"
  | Error cause ->
      let text = cause_text cause in
      Alcotest.(check bool) "no secret" false (contains text "SECRET");
      if
        not
          (contains text "server error"
          || contains text "closed" || contains text "failed"
          || contains text "cancelled" || contains text "error")
      then Alcotest.failf "unexpected provider-failure cause: %s" text

let test_closed_after_success () =
  with_listening @@ fun ~sw ~net ~clock ~logger ~url ~socket ->
  Eio.Fiber.fork ~sw (fun () ->
      try
        let flow, _ = Eio.Net.accept ~sw socket in
        serve_script ~after_handshake:[ created ] ~after_update:[ updated ]
          ~after_append:[ json_final "hello" ]
          flow
      with _ -> ());
  let operation =
    Codex.with_stream_session ~caps ~sw ~net ~clock ~logger ~url
      ~access_token:"synthetic" ~mode:Codex.Transcription ~operation:"budget"
      ~model:"gpt-4o-mini-transcribe"
      ~session_update:
        (Codex.build_transcription_session_update
           ~model:"gpt-4o-mini-transcribe" ())
      ~cancel_message:"Codex realtime transcription cancelled" (fun handle ->
        let* () = handle.offer_chunk (pcm 320) in
        let* text = handle.finish_stream () in
        let offer_after = handle.offer_chunk (pcm 2) in
        Ok (text, offer_after))
  in
  match operation with
  | Ok (text, Error _) -> Alcotest.(check string) "completed" "hello" text
  | Ok (_, Ok ()) -> Alcotest.fail "offer after success was admitted"
  | Error cause -> Alcotest.failf "success path failed: %s" (cause_text cause)

let test_queue_accounting_underflow_is_visible () =
  let queued = ref 0 in
  let items = ref 0 in
  let take n =
    if !items < 1 || !queued < n then
      failwith "Codex realtime queue accounting underflow"
    else (
      queued := !queued - n;
      items := !items - 1)
  in
  (match take 1 with
  | exception Failure msg ->
      Alcotest.(check bool) "underflow" true (contains msg "underflow")
  | () -> Alcotest.fail "underflow was clamped");
  queued := 4;
  items := 1;
  take 4;
  Alcotest.(check int) "queued" 0 !queued;
  Alcotest.(check int) "items" 0 !items

let () =
  Alcotest.run "provider-budgets"
    [
      ( "admission",
        [
          Alcotest.test_case "queue minus one" `Quick
            test_queue_minus_one_succeeds;
          Alcotest.test_case "queue exact" `Quick test_queue_exact_succeeds;
          Alcotest.test_case "queue plus one rejected" `Quick
            test_queue_plus_one_rejected;
          Alcotest.test_case "stalled queue then overflow" `Quick
            test_stalled_then_overflow_rejected;
          Alcotest.test_case "cancel then offer fails" `Quick
            test_cancel_then_offer_fails;
          Alcotest.test_case "two blueprint runs independent" `Quick
            test_two_blueprints_independent;
          Alcotest.test_case "empty offers noop until item cap" `Quick
            test_empty_offers_are_noop_until_item_cap;
          Alcotest.test_case "empty offer after cancel fails" `Quick
            test_empty_offer_after_cancel_fails;
          Alcotest.test_case "offer after provider failure" `Quick
            test_offer_after_provider_failure;
          Alcotest.test_case "closed after success" `Quick
            test_closed_after_success;
          Alcotest.test_case "queue accounting underflow is visible" `Quick
            test_queue_accounting_underflow_is_visible;
        ] );
      ( "capture",
        [
          Alcotest.test_case "buffered capture plus one" `Quick
            test_buffered_capture_plus_one;
          Alcotest.test_case "openrouter capture plus one" `Quick
            test_openrouter_capture_plus_one;
        ] );
      ( "output",
        [
          Alcotest.test_case "result minus one" `Quick test_result_minus_one;
          Alcotest.test_case "result exact" `Quick test_result_exact;
          Alcotest.test_case "result plus one" `Quick test_result_plus_one;
          Alcotest.test_case "final replaces partials" `Quick
            test_final_replaces_partials;
          Alcotest.test_case "empty deltas do not grow" `Quick
            test_empty_deltas_do_not_grow;
        ] );
    ]
