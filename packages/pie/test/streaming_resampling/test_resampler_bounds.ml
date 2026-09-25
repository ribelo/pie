(* Pie-free bounded-advance gate. Canonical engine is copied from audio/resampler. *)

let pcm_of_samples samples =
  let pcm = Bytes.create (Array.length samples * 2) in
  Array.iteri (fun i value -> Bytes.set_int16_le pcm (i * 2) value) samples;
  pcm

let require_ok = function
  | Ok value -> value
  | Error (`Resample_error msg) -> Alcotest.failf "resampler error: %s" msg

let test_huge_accepted_downsample_is_bounded () =
  let from_rate = max_int / 32768 in
  let to_rate = 1 in
  let resampler = require_ok (Resampler.create ~from_rate ~to_rate) in
  let pcm = pcm_of_samples [| 1234 |] in
  let first = require_ok (Resampler.push resampler pcm) in
  let tail = require_ok (Resampler.finish resampler) in
  let out =
    let buf = Buffer.create 4 in
    Buffer.add_bytes buf first;
    Buffer.add_bytes buf tail;
    Buffer.to_bytes buf
  in
  Alcotest.(check int) "max 1 output sample" 2 (Bytes.length out);
  Alcotest.(check int) "passthrough sample" 1234 (Bytes.get_int16_le out 0);
  Alcotest.(check bool)
    "shifts O(retained) not O(ratio)" true
    (Resampler.shift_count resampler <= 4);
  (match Resampler.finish resampler with
  | Ok leftover ->
      Alcotest.(check int) "second finish empty" 0 (Bytes.length leftover)
  | Error (`Resample_error msg) ->
      Alcotest.failf "finish sticky after success: %s" msg);
  match Resampler.push resampler pcm with
  | Error (`Resample_error _) -> ()
  | Ok _ -> Alcotest.fail "push after successful finish must fail"

let test_format_error_stays_sticky () =
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
  match Resampler.push resampler (pcm_of_samples [| 1 |]) with
  | Error (`Resample_error _) -> ()
  | Ok _ -> Alcotest.fail "push after failed finish must not succeed"

let () =
  Alcotest.run "resampler_bounds"
    [
      ( "bounds",
        [
          Alcotest.test_case
            "accepted huge downsample is O(retained) with max-1 output" `Quick
            test_huge_accepted_downsample_is_bounded;
          Alcotest.test_case "format error stays sticky" `Quick
            test_format_error_stays_sticky;
        ] );
    ]
