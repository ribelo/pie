(* ^pcaps-n1rg *)

(* Isolated PIE012 chunk-invariant resampling.
   Synthetic PCM and a local fake WebSocket only. No provider, mic or secrets. *)

module Resampler = Pie.Audio.Resampler
module Codex = Pie.Stt.Codex_realtime

let samples_of_pcm pcm =
  let n = Bytes.length pcm / 2 in
  Array.init n (fun i -> Bytes.get_int16_le pcm (i * 2))

let pcm_of_samples samples =
  let pcm = Bytes.create (Array.length samples * 2) in
  Array.iteri (fun i value -> Bytes.set_int16_le pcm (i * 2) value) samples;
  pcm

(* Independent transcription of the established whole-buffer formula. *)
let oracle ~from_rate ~to_rate pcm =
  if from_rate = to_rate then Bytes.sub pcm 0 (Bytes.length pcm)
  else
    let n = Bytes.length pcm / 2 in
    if n = 0 then Bytes.empty
    else
      let output_samples = max 1 (n * to_rate / from_rate) in
      let out = Bytes.create (output_samples * 2) in
      for i = 0 to output_samples - 1 do
        let source_num = i * from_rate in
        let source_idx = min (n - 1) (source_num / to_rate) in
        let next_idx = min (n - 1) (source_idx + 1) in
        let rem = source_num mod to_rate in
        let a = Bytes.get_int16_le pcm (source_idx * 2) in
        let b = Bytes.get_int16_le pcm (next_idx * 2) in
        let sample = ((a * (to_rate - rem)) + (b * rem)) / to_rate in
        Bytes.set_int16_le out (i * 2) (max (-32768) (min 32767 sample))
      done;
      out

let require_ok = function
  | Ok value -> value
  | Error (`Resample_error msg) -> Alcotest.failf "resampler error: %s" msg

let run_partition ~from_rate ~to_rate chunks =
  let resampler = require_ok (Resampler.create ~from_rate ~to_rate) in
  let buf = Buffer.create 32 in
  List.iter
    (fun chunk ->
      Buffer.add_bytes buf (require_ok (Resampler.push resampler chunk)))
    chunks;
  Buffer.add_bytes buf (require_ok (Resampler.finish resampler));
  Buffer.to_bytes buf

let concat_bytes chunks =
  let buf = Buffer.create 32 in
  List.iter (fun chunk -> Buffer.add_bytes buf chunk) chunks;
  Buffer.to_bytes buf

let even_prefix pcm = Bytes.sub pcm 0 (Bytes.length pcm land lnot 1)

let check_match label ~from_rate ~to_rate source actual =
  let expected = oracle ~from_rate ~to_rate (even_prefix source) in
  Alcotest.(check int)
    (label ^ " byte count") (Bytes.length expected) (Bytes.length actual);
  Alcotest.(check (array int))
    (label ^ " samples") (samples_of_pcm expected) (samples_of_pcm actual)

let sample_pcm = pcm_of_samples [| 0; 12000; -32768; 32767; -1; 3; -4; 8000 |]

let rates =
  [
    (16000, 24000); (24000, 16000); (16000, 16000); (8000, 48000); (48000, 8000);
  ]

let sample_splits pcm =
  let n = Bytes.length pcm / 2 in
  let rec at offset acc =
    if offset >= n then [ List.rev acc ]
    else
      let rest_len = n - offset in
      let rec sizes k acc_k =
        if k > rest_len then acc_k
        else
          let chunk = Bytes.sub pcm (offset * 2) (k * 2) in
          sizes (k + 1) (at (offset + k) (chunk :: acc) @ acc_k)
      in
      sizes 1 []
  in
  at 0 []

let byte_splits pcm =
  let len = Bytes.length pcm in
  let rec loop i acc =
    if i >= len then List.rev acc
    else loop (i + 1) ([ Bytes.sub pcm 0 i; Bytes.sub pcm i (len - i) ] :: acc)
  in
  loop 0 []

let test_empty_is_empty () =
  List.iter
    (fun (from_rate, to_rate) ->
      let actual = run_partition ~from_rate ~to_rate [ Bytes.empty ] in
      Alcotest.(check int) "empty" 0 (Bytes.length actual))
    rates

let test_equal_rate_passthrough () =
  let actual = run_partition ~from_rate:16000 ~to_rate:16000 [ sample_pcm ] in
  Alcotest.(check (array int))
    "equal-rate samples"
    (samples_of_pcm sample_pcm)
    (samples_of_pcm actual)

let test_partitions_match_oracle () =
  let short = Bytes.sub sample_pcm 0 8 in
  List.iter
    (fun (from_rate, to_rate) ->
      let source = short in
      List.iter
        (fun chunks ->
          let actual = run_partition ~from_rate ~to_rate chunks in
          check_match "partition" ~from_rate ~to_rate source actual)
        (sample_splits source);
      List.iter
        (fun chunks ->
          let actual = run_partition ~from_rate ~to_rate chunks in
          check_match "byte split" ~from_rate ~to_rate source actual)
        (byte_splits source);
      let with_empty = [ Bytes.empty; source; Bytes.empty; Bytes.empty ] in
      check_match "empty chunks" ~from_rate ~to_rate source
        (run_partition ~from_rate ~to_rate with_empty);
      let singles =
        let n = Bytes.length source / 2 in
        List.init n (fun i -> Bytes.sub source (i * 2) 2)
      in
      check_match "single samples" ~from_rate ~to_rate source
        (run_partition ~from_rate ~to_rate singles))
    rates

let test_negative_and_extreme () =
  let pcm = pcm_of_samples [| -32768; 32767; -32768; 32767 |] in
  List.iter
    (fun (from_rate, to_rate) ->
      check_match "extremes" ~from_rate ~to_rate pcm
        (run_partition ~from_rate ~to_rate [ pcm ]))
    rates

let test_nonpositive_rate () =
  (match Resampler.create ~from_rate:0 ~to_rate:16000 with
  | Error (`Resample_error msg) ->
      Alcotest.(check bool) "mentions rate" true (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected rate error");
  match Resampler.create ~from_rate:16000 ~to_rate:(-24000) with
  | Error (`Resample_error _) -> ()
  | Ok _ -> Alcotest.fail "expected negative rate error"

let test_trailing_byte_rejected () =
  let resampler =
    require_ok (Resampler.create ~from_rate:16000 ~to_rate:24000)
  in
  let pcm = pcm_of_samples [| 10; 20 |] in
  ignore (require_ok (Resampler.push resampler pcm));
  ignore (require_ok (Resampler.push resampler (Bytes.make 1 '\x00')));
  match Resampler.finish resampler with
  | Error (`Resample_error msg) ->
      Alcotest.(check bool)
        "odd byte is format error" true
        (String.length msg > 0)
  | Ok _ -> Alcotest.fail "expected trailing byte rejection"

let test_repeated_finish () =
  let resampler =
    require_ok (Resampler.create ~from_rate:16000 ~to_rate:24000)
  in
  let pcm = pcm_of_samples [| 0; 12000; -100; 50 |] in
  let first = require_ok (Resampler.push resampler pcm) in
  let tail = require_ok (Resampler.finish resampler) in
  let again = require_ok (Resampler.finish resampler) in
  check_match "first finish" ~from_rate:16000 ~to_rate:24000 pcm
    (concat_bytes [ first; tail ]);
  Alcotest.(check int) "second finish empty" 0 (Bytes.length again)

let test_repeated_finish_after_error () =
  let resampler =
    require_ok (Resampler.create ~from_rate:16000 ~to_rate:24000)
  in
  ignore (require_ok (Resampler.push resampler (pcm_of_samples [| 10; 20 |])));
  ignore (require_ok (Resampler.push resampler (Bytes.make 1 '\x00')));
  (match Resampler.finish resampler with
  | Error (`Resample_error _) -> ()
  | Ok _ -> Alcotest.fail "expected odd-byte finish error");
  (match Resampler.finish resampler with
  | Error (`Resample_error _) -> ()
  | Ok _ -> Alcotest.fail "repeat finish after error must not succeed");
  match Resampler.push resampler Bytes.empty with
  | Error (`Resample_error _) -> ()
  | Ok _ -> Alcotest.fail "push after failed finish must not succeed"

let test_helper_rejects_odd () =
  let odd = Bytes.create 3 in
  Bytes.set_int16_le odd 0 12;
  Bytes.set odd 2 '\x00';
  let expect_invalid label pcm ~from_rate ~to_rate =
    match Codex.resample_s16le_mono ~from_rate ~to_rate pcm with
    | exception Invalid_argument msg ->
        Alcotest.(check bool) label true (String.length msg > 0)
    | _ -> Alcotest.fail (label ^ ": expected Invalid_argument")
  in
  expect_invalid "unequal odd" odd ~from_rate:16000 ~to_rate:24000;
  expect_invalid "equal-rate odd" odd ~from_rate:16000 ~to_rate:16000

let contains hay needle =
  let n = String.length needle in
  let h = String.length hay in
  let rec loop i =
    i <= h - n && (String.sub hay i n = needle || loop (i + 1))
  in
  n = 0 || loop 0

let test_unrepresentable_rates () =
  (match Resampler.create ~from_rate:max_int ~to_rate:max_int with
  | Error (`Resample_error msg) ->
      Alcotest.(check bool)
        "engine mentions bounds or rate" true
        (contains msg "representable"
        || contains msg "rate" || contains msg "bound")
  | Ok _ -> Alcotest.fail "expected unrepresentable rate error");
  match
    Codex.resample_s16le_mono ~from_rate:max_int ~to_rate:24000
      (pcm_of_samples [| 1; 2 |])
  with
  | exception Invalid_argument msg ->
      Alcotest.(check bool) "helper invalid_arg" true (String.length msg > 0)
  | _ -> Alcotest.fail "expected helper Invalid_argument for huge rate"

let test_huge_accepted_downsample_is_bounded () =
  let from_rate = max_int / 32768 in
  let to_rate = 1 in
  let resampler = require_ok (Resampler.create ~from_rate ~to_rate) in
  let pcm = pcm_of_samples [| 1234 |] in
  let first = require_ok (Resampler.push resampler pcm) in
  let tail = require_ok (Resampler.finish resampler) in
  let out = concat_bytes [ first; tail ] in
  Alcotest.(check int) "max 1 output sample" 2 (Bytes.length out);
  Alcotest.(check int) "passthrough sample" 1234 (Bytes.get_int16_le out 0);
  Alcotest.(check bool)
    "shifts O(retained) not O(ratio)" true
    (Resampler.shift_count resampler <= 4);
  Alcotest.(check int)
    "second finish empty" 0
    (Bytes.length (require_ok (Resampler.finish resampler)))

let pp_stt ppf = function `Stt_error msg -> Format.pp_print_string ppf msg

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

let base64_decode s =
  let len = String.length s in
  let rec skip_pad n =
    if n > 0 && s.[n - 1] = '=' then skip_pad (n - 1) else n
  in
  let data_len = skip_pad len in
  let value c =
    if c >= 'A' && c <= 'Z' then Char.code c - Char.code 'A'
    else if c >= 'a' && c <= 'z' then Char.code c - Char.code 'a' + 26
    else if c >= '0' && c <= '9' then Char.code c - Char.code '0' + 52
    else if c = '+' then 62
    else if c = '/' then 63
    else Alcotest.failf "invalid base64 %C" c
  in
  let out = Buffer.create (data_len * 3 / 4) in
  let rec loop i =
    if i >= data_len then ()
    else
      let v n = if i + n < data_len then value s.[i + n] else 0 in
      let b0 = v 0 in
      let b1 = v 1 in
      let b2 = v 2 in
      let b3 = v 3 in
      Buffer.add_char out (Char.chr ((b0 lsl 2) lor (b1 lsr 4)));
      if i + 2 < data_len then
        Buffer.add_char out (Char.chr (((b1 land 0xf) lsl 4) lor (b2 lsr 2)));
      if i + 3 < data_len then
        Buffer.add_char out (Char.chr (((b2 land 0x3) lsl 6) lor b3));
      loop (i + 4)
  in
  loop 0;
  Buffer.to_bytes out

let drain_until_eof ~clock flow =
  let buf = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read flow buf with
    | 0 -> ()
    | _ -> loop ()
    | exception End_of_file -> ()
    | exception Eio.Io _ -> ()
  in
  try Eio.Time.with_timeout_exn clock 2.0 loop with _ -> ()

let write_ws_switching_response flow key =
  let response =
    "HTTP/1.1 101 Switching Protocols\r\n" ^ "Upgrade: websocket\r\n"
    ^ "Connection: keep-alive, Upgrade\r\n" ^ "Sec-WebSocket-Accept: "
    ^ Ox_http.Ws_codec.accept_key ~sha1:Ox_http.Ox_http_tls_openssl.sha1 key
    ^ "\r\n\r\n"
  in
  Eio.Flow.copy_string response flow

let tcp_port = function
  | `Tcp (_, port) -> port
  | _ -> Alcotest.fail "expected tcp port"

let serve_audio_capture ~clock flow captured commit_before_response =
  let head = read_http_head flow in
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> Alcotest.fail "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key;
  let rec loop committed =
    match read_ws_frame flow with
    | exception End_of_file -> ()
    | Close -> ()
    | Other -> loop committed
    | Text payload -> (
        match Yojson.Safe.from_string payload with
        | exception _ -> loop committed
        | `Assoc fields -> (
            match List.assoc_opt "type" fields with
            | Some (`String "input_audio_buffer.append") ->
                if committed then Alcotest.fail "audio append after commit";
                (match List.assoc_opt "audio" fields with
                | Some (`String b64) ->
                    Buffer.add_bytes captured (base64_decode b64)
                | _ -> Alcotest.fail "append missing audio");
                loop committed
            | Some (`String "input_audio_buffer.commit") ->
                if not committed then commit_before_response := true;
                write_ws_text flow
                  {|{"type":"conversation.item.input_audio_transcription.completed","transcript":"ok"}|};
                write_ws_close flow;
                drain_until_eof ~clock flow
            | _ -> loop committed)
        | _ -> loop committed)
  in
  loop false

let audio_of_chunks chunks =
  let remaining = ref chunks in
  fun () ->
    match !remaining with
    | [] -> Ok None
    | chunk :: rest ->
        remaining := rest;
        Ok (Some chunk)

let run_streaming_sender ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >)
    chunks ~from_rate =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port = tcp_port (Eio.Net.listening_addr socket) in
  let captured = Buffer.create 64 in
  let commit_before_response = ref false in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run @@ fun conn_sw ->
      let flow, _ = Eio.Net.accept ~sw:conn_sw socket in
      serve_audio_capture ~clock flow captured commit_before_response);
  let audio = audio_of_chunks chunks in
  let url = Printf.sprintf "ws://127.0.0.1:%d" port in
  let operation =
    try
      Eio.Time.with_timeout_exn clock 5.0 (fun () ->
          Codex.run_realtime_stream_session ~caps ~sw ~net ~clock ~url
            ~access_token:"synthetic-resampling-token" ~mode:Codex.Transcription
            ~input_sample_rate:from_rate audio)
    with Eio.Time.Timeout -> Error (`Stt_error "timeout")
  in
  match operation with
  | Error error -> Alcotest.failf "streaming sender failed: %a" pp_stt error
  | Ok _ ->
      Alcotest.(check bool)
        "audio commit before response" true !commit_before_response;
      Buffer.to_bytes captured

let test_streaming_sender_matches_oracle
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  let source = pcm_of_samples [| 0; 12000; -200; 400 |] in
  let from_rate = 16000 in
  let to_rate = 24000 in
  let partitions =
    [
      [ source ];
      [ Bytes.sub source 0 2; Bytes.sub source 2 6 ];
      [ Bytes.sub source 0 4; Bytes.sub source 4 4 ];
      [ Bytes.sub source 0 6; Bytes.sub source 6 2 ];
      List.init 4 (fun i -> Bytes.sub source (i * 2) 2);
      [ Bytes.empty; source; Bytes.empty ];
      [ Bytes.sub source 0 1; Bytes.sub source 1 7 ];
    ]
  in
  List.iter
    (fun chunks ->
      let actual = run_streaming_sender ~caps chunks ~from_rate in
      check_match "streaming sender" ~from_rate ~to_rate source actual)
    partitions

let test_buffered_odd_is_stt_error
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let odd = Bytes.make 1 '\x00' in
  let run rate =
    Codex.run_realtime_session ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~url:"ws://127.0.0.1:1"
      ~access_token:"synthetic-resampling-token" ~mode:Codex.Transcription
      ~input_sample_rate:rate odd
  in
  (match run 16000 with
  | exception exn ->
      Alcotest.failf "buffered odd must not Die: %s" (Printexc.to_string exn)
  | Error error ->
      let rendered = Format.asprintf "%a" pp_stt error in
      Alcotest.(check bool)
        "buffered odd is Stt_error" true
        (contains rendered "odd" || contains rendered "byte")
  | Ok _ -> Alcotest.fail "expected Stt_error for buffered odd PCM");
  match run 24000 with
  | exception exn ->
      Alcotest.failf "buffered equal-rate odd must not Die: %s"
        (Printexc.to_string exn)
  | Error error ->
      let rendered = Format.asprintf "%a" pp_stt error in
      Alcotest.(check bool)
        "buffered equal-rate odd is Stt_error" true
        (contains rendered "odd" || contains rendered "byte")
  | Ok _ -> Alcotest.fail "expected Stt_error for buffered equal-rate odd PCM"

let test_buffered_huge_rate_is_stt_error
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let pcm = pcm_of_samples [| 1; 2 |] in
  let operation =
    Codex.run_realtime_session ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~url:"ws://127.0.0.1:1"
      ~access_token:"synthetic-resampling-token" ~mode:Codex.Transcription
      ~input_sample_rate:max_int pcm
  in
  match operation with
  | exception exn ->
      Alcotest.failf "huge rate must not Die: %s" (Printexc.to_string exn)
  | Error error ->
      let rendered = Format.asprintf "%a" pp_stt error in
      Alcotest.(check bool)
        "typed huge-rate error" true
        (String.length rendered > 0)
  | Ok _ -> Alcotest.fail "expected Stt_error for huge sample rate"

let is_audio_append type_name =
  String.equal type_name "input_audio_buffer.append"

let serve_translation_capture ~clock ~expected_bytes flow captured
    tail_before_retire =
  let head = read_http_head flow in
  let key =
    match header_value "Sec-WebSocket-Key" head with
    | Some key -> key
    | None -> Alcotest.fail "missing Sec-WebSocket-Key"
  in
  write_ws_switching_response flow key;
  let sent_done = ref false in
  let acknowledge_tail () =
    if (not !sent_done) && Buffer.length captured >= expected_bytes then (
      sent_done := true;
      tail_before_retire := true;
      write_ws_text flow
        {|{"type":"conversation.item.input_audio_transcription.completed","transcript":"ok"}|})
  in
  let rec loop () =
    match read_ws_frame flow with
    | exception End_of_file -> ()
    | Close -> ()
    | Other -> loop ()
    | Text payload -> (
        match Yojson.Safe.from_string payload with
        | exception _ -> loop ()
        | `Assoc fields -> (
            match List.assoc_opt "type" fields with
            | Some (`String type_name) when is_audio_append type_name ->
                (match List.assoc_opt "audio" fields with
                | Some (`String b64) ->
                    Buffer.add_bytes captured (base64_decode b64);
                    acknowledge_tail ()
                | _ -> Alcotest.fail "translation append missing audio");
                loop ()
            | _ -> loop ())
        | _ -> loop ())
  in
  loop ();
  drain_until_eof ~clock flow

let test_streaming_nonpositive_rate
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let operation =
    Codex.run_realtime_stream_session ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~url:"ws://127.0.0.1:1"
      ~access_token:"synthetic-resampling-token" ~mode:Codex.Transcription
      ~input_sample_rate:0
      (audio_of_chunks [ sample_pcm ])
  in
  match operation with
  | Error error ->
      let rendered = Format.asprintf "%a" pp_stt error in
      Alcotest.(check bool)
        "mentions sample rate" true
        (contains rendered "sample rate")
  | Ok _ -> Alcotest.fail "expected Stt_error for nonpositive rate"

let main ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. >) =
  Alcotest.run "streaming_resampling"
    [
      ( "resampler",
        [
          Alcotest.test_case "empty input is empty" `Quick test_empty_is_empty;
          Alcotest.test_case "equal-rate passthrough" `Quick
            test_equal_rate_passthrough;
          Alcotest.test_case "all partitions match oracle" `Quick
            test_partitions_match_oracle;
          Alcotest.test_case "negative and extreme int16" `Quick
            test_negative_and_extreme;
          Alcotest.test_case "nonpositive rate is typed error" `Quick
            test_nonpositive_rate;
          Alcotest.test_case "trailing odd byte rejected" `Quick
            test_trailing_byte_rejected;
          Alcotest.test_case "repeated finish does not replay" `Quick
            test_repeated_finish;
          Alcotest.test_case "repeat finish after error stays failed" `Quick
            test_repeated_finish_after_error;
          Alcotest.test_case "helper rejects odd PCM including equal-rate"
            `Quick test_helper_rejects_odd;
          Alcotest.test_case "unrepresentable rates fail explicitly" `Quick
            test_unrepresentable_rates;
          Alcotest.test_case
            "accepted huge downsample is O(retained) with max-1 output" `Quick
            test_huge_accepted_downsample_is_bounded;
        ] );
      ( "sender",
        [
          Alcotest.test_case "streaming sender matches oracle for partitions"
            `Quick
            (test_streaming_sender_matches_oracle ~caps);
          Alcotest.test_case "nonpositive rate fails at operation boundary"
            `Quick
            (test_streaming_nonpositive_rate ~caps);
          Alcotest.test_case "buffered odd PCM is Stt_error not Die" `Quick
            (test_buffered_odd_is_stt_error ~caps);
          Alcotest.test_case "buffered huge rate is Stt_error not Die" `Quick
            (test_buffered_huge_rate_is_stt_error ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
