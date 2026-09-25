module Low = Pie.Ptt.Low_volume

let create () = Low.create ~min_rms:0.0015 ~min_peak:0.01 ()

let pcm n value =
  let bytes = Bytes.create (2 * n) in
  for i = 0 to n - 1 do
    Bytes.set_int16_le bytes (2 * i) value
  done;
  bytes

let quiet n = pcm n 16
let window = 1600
let duration = 30 * window
let check = Alcotest.(check bool)

let step t bytes expected =
  let t, warning = Low.process_chunk t bytes in
  check "warning" expected warning;
  t

let test_boundary () =
  let t = step (create ()) (quiet (duration - 1)) false in
  let last = quiet 1 in
  let t = step t (Bytes.sub last 0 1) false in
  ignore (step t (Bytes.sub last 1 1) true)

let test_short () = ignore (step (create ()) (quiet (29 * window)) false)

let test_reset value () =
  let t = step (create ()) (quiet (29 * window)) false in
  let t = step t (pcm window value) false in
  let t = step t (quiet (29 * window)) false in
  ignore (step t (quiet window) true)

let test_silence () =
  ignore (step (create ()) (pcm (duration * 2) 0) false);
  test_reset 0 ()

let test_one_shot () =
  let t = step (create ()) (quiet (duration * 4)) true in
  let t = step t (pcm duration 0) false in
  let t = step t (pcm duration 32767) false in
  ignore (step t (quiet duration) false)

let test_fresh () =
  ignore (step (create ()) (quiet duration) true);
  ignore (step (create ()) (quiet duration) true)

let test_empty () =
  let t = step (create ()) Bytes.empty false in
  let t = step t (Bytes.make 1 '\016') false in
  let t = step t Bytes.empty false in
  let t = step t (Bytes.make 1 '\000') false in
  ignore (step t (quiet (duration - 1)) true)

let feed size t bytes =
  let rec loop pos t warnings =
    if pos = Bytes.length bytes then (t, warnings)
    else
      let count = min size (Bytes.length bytes - pos) in
      let t, warning = Low.process_chunk t (Bytes.sub bytes pos count) in
      loop (pos + count) t (warnings + if warning then 1 else 0)
  in
  loop 0 t 0

let test_partition () =
  let prefix =
    Bytes.concat Bytes.empty
      [
        quiet (29 * window);
        pcm window 0;
        quiet (29 * window);
        pcm window (-32768);
        quiet (duration - 1);
      ]
  in
  List.iter
    (fun size ->
      let t, warnings = feed size (create ()) prefix in
      Alcotest.(check int)
        "no early warning across partitioned resets" 0 warnings;
      let t, warnings = feed size t (quiet 1) in
      Alcotest.(check int) "same three-second boundary" 1 warnings;
      let _, warnings = feed size t (quiet duration) in
      Alcotest.(check int) "no repeat across partitions" 0 warnings)
    [ 1; 2; 3; 97; 3199; 3200; 3201; 8192; Bytes.length prefix ]

let test_rms_boundary () =
  let threshold = 32. /. 32768. in
  let t = Low.create ~min_rms:threshold ~min_peak:0.1 () in
  ignore (step t (pcm duration 32) false);
  ignore (step t (pcm duration 31) true)

let test_peak_boundary () =
  let t = Low.create ~min_rms:0.1 ~min_peak:(32. /. 32768.) () in
  let impulse = pcm window 0 in
  Bytes.set_int16_le impulse 2 (-32);
  let t = ref t in
  for _ = 1 to 30 do
    t := step !t impulse false
  done;
  ignore (step !t (pcm duration (-31)) true)

let test_negative_quiet () = ignore (step (create ()) (pcm duration (-16)) true)

let test_immutable () =
  let t = step (create ()) (quiet (duration - 1)) false in
  ignore (step t (quiet 1) true);
  ignore (step t (quiet 1) true)

let test_no_buffer_alias () =
  let low = Bytes.make 1 '\016' in
  let t = step (create ()) low false in
  Bytes.set_uint8 low 0 0;
  let t = step t (Bytes.make 1 '\000') false in
  let t = step t (pcm (window - 1) 0) false in
  ignore (step t (quiet (29 * window)) true)

let test_long_stream () =
  let t = ref (create ()) in
  let loud = pcm window 16384 in
  for _ = 1 to 10000 do
    t := step !t loud false
  done;
  ignore (step !t (quiet duration) true)

let () =
  Alcotest.run "low_volume"
    [
      ( "advisory",
        List.map
          (fun (name, test) -> Alcotest.test_case name `Quick test)
          [
            ("three seconds including split final sample", test_boundary);
            ("short quiet stretch", test_short);
            ("normal window resets", test_reset 1024);
            ("negative full-scale resets", test_reset (-32768));
            ("exact silence resets and never warns", test_silence);
            ("one shot including oversized chunk", test_one_shot);
            ("new capture warns again", test_fresh);
            ("empty and incomplete samples", test_empty);
            ("partition invariance with mixed signal", test_partition);
            ("strict RMS boundary", test_rms_boundary);
            ("strict peak boundary independently of RMS", test_peak_boundary);
            ("negative quiet samples", test_negative_quiet);
            ("state can be reused independently", test_immutable);
            ("input buffer not retained", test_no_buffer_alias);
            ("long stream with bounded window state", test_long_stream);
          ] );
    ]
