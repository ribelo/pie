(* PIE007: incremental PCM16LE stats equal whole-concat reference; PTT
   eligibility does not prefix-concat. Synthetic audio only. *)

open Eta.Syntax

let sample_bytes values =
  let b = Bytes.create (List.length values * 2) in
  List.iteri (fun i v -> Bytes.set_int16_le b (i * 2) v) values;
  b

let odd_prefix values leftover =
  let even = sample_bytes values in
  let out = Bytes.create (Bytes.length even + 1) in
  Bytes.blit even 0 out 0 (Bytes.length even);
  Bytes.set out (Bytes.length even) leftover;
  out

let acc_of_chunks chunks =
  List.fold_left Pie.Audio.Pcm.s16le_update Pie.Audio.Pcm.s16le_empty chunks

let concat chunks =
  let len = List.fold_left (fun n c -> n + Bytes.length c) 0 chunks in
  let out = Bytes.create len in
  ignore
    (List.fold_left
       (fun off c ->
         Bytes.blit c 0 out off (Bytes.length c);
         off + Bytes.length c)
       0 chunks);
  out

let test_odd_byte_carry_matches_concat () =
  let a = odd_prefix [ 1000; -2000 ] '\x10' in
  let b = Bytes.create 1 in
  Bytes.set_uint8 b 0 0x20;
  let acc = acc_of_chunks [ a; b ] in
  let whole = concat [ a; b ] in
  Alcotest.(check int)
    "complete samples"
    (Bytes.length whole / 2)
    acc.sample_count;
  Alcotest.(check (option char)) "no leftover" None acc.pending;
  Alcotest.(check (float 1e-12))
    "rms"
    (Pie.Audio.Pcm.pcm_rms whole)
    (Pie.Audio.Pcm.s16le_rms acc);
  Alcotest.(check (float 1e-12))
    "peak"
    (Pie.Audio.Pcm.pcm_peak whole)
    (Pie.Audio.Pcm.s16le_peak acc)

let test_trailing_odd_byte_is_not_a_sample () =
  let chunk = odd_prefix [ 1234 ] '\x7f' in
  let acc = Pie.Audio.Pcm.s16le_update Pie.Audio.Pcm.s16le_empty chunk in
  Alcotest.(check int) "one complete sample" 1 acc.sample_count;
  Alcotest.(check (option char)) "pending leftover" (Some '\x7f') acc.pending;
  Alcotest.(check (float 1e-12))
    "rms ignores leftover"
    (Pie.Audio.Pcm.pcm_rms chunk)
    (Pie.Audio.Pcm.s16le_rms acc)

let test_s16le_visits_are_linear () =
  let chunk = sample_bytes (List.init 160 (fun _ -> 80)) in
  Pie.Audio.Pcm.Accounting.reset ();
  ignore (acc_of_chunks (List.init 32 (fun _ -> chunk)));
  let v32, c32 = Pie.Audio.Pcm.Accounting.snapshot () in
  Pie.Audio.Pcm.Accounting.reset ();
  ignore (acc_of_chunks (List.init 64 (fun _ -> chunk)));
  let v64, c64 = Pie.Audio.Pcm.Accounting.snapshot () in
  Alcotest.(check int) "32 chunks visit 32*320" (32 * 320) v32;
  Alcotest.(check int) "64 chunks visit 64*320" (64 * 320) v64;
  Alcotest.(check int) "stats path copies nothing" 0 c32;
  Alcotest.(check int) "doubling visits doubles" (v32 * 2) v64

let pcm_chunk sample samples =
  let chunk = Bytes.create (samples * 2) in
  for i = 0 to samples - 1 do
    Bytes.set_int16_le chunk (i * 2) sample
  done;
  chunk

let recognize_test event =
  if event.Pie.Keyboard.Monitor.keysym = 1 then
    Some
      (if event.released then Pie.Ptt_loop.Release "test"
       else Pie.Ptt_loop.Press "test")
  else None

let emit keyboard released =
  Pie.Keyboard.Monitor.emit keyboard
    { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }

let run_loop ~env ~sw ~clock ~handle ~min_duration_ms ~rms ~peak ~audio_fn
    ~timeout_s =
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 64 in
  Eio.Fiber.fork ~sw (fun () -> audio_fn ~clock ~keyboard ~audio);
  let config : Pie.Ptt_loop.config =
    {
      recognize = recognize_test;
      min_duration_ms;
      capture_thresholds = (fun () -> (rms, peak));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press = (fun _ -> Eta.Effect.pure (Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort = Eta.Effect.pure false;
      on_dead_input = Eta.Effect.unit;
      admission_active = (fun () -> false);
    }
  in
  Eta_eio.run_once ~sw ~clock
    (Pie.Ptt_loop.run
       ~mono_clock:(Eio.Stdenv.mono_clock env)
       ~keyboard
       ~audio:(Eta_stream_eio.Stream.from_eio_stream audio)
       config
    |> Eta.Effect.timeout_as
         (Eta.Duration.seconds timeout_s)
         ~on_timeout:(`Ptt_error "timeout"))

let test_ptt_quiet_nonzero_visits_linear () =
  Eio_main.run @@ fun env ->
  let run n =
    Eio.Switch.run @@ fun sw ->
    let clock = Eio.Stdenv.clock env in
    let finishes = ref 0 in
    let cancels = ref 0 in
    let handle : Pie.Ptt_loop.handle =
      {
        offer = (fun _ -> Eta.Effect.unit);
        finish =
          Pie.Ptt_loop.Buffered
            (fun _ -> Eta.Effect.sync (fun () -> incr finishes));
        cancel = Eta.Effect.sync (fun () -> incr cancels);
      }
    in
    Pie.Audio.Pcm.Accounting.reset ();
    let result =
      run_loop ~env ~sw ~clock ~handle ~min_duration_ms:0 ~rms:0.5 ~peak:0.5
        ~timeout_s:8 ~audio_fn:(fun ~clock ~keyboard ~audio ->
          Eio.Time.sleep clock 0.05;
          emit keyboard false;
          for _ = 1 to n do
            Eio.Stream.add audio (pcm_chunk 80 160);
            Eio.Time.sleep clock 0.005
          done;
          emit keyboard true)
    in
    let visits, copies = Pie.Audio.Pcm.Accounting.snapshot () in
    ignore result;
    (!finishes, !cancels, visits, copies)
  in
  let f32, c32, v32, copies32 = run 32 in
  let f64, c64, v64, copies64 = run 64 in
  Alcotest.(check int) "quiet 32 never finishes" 0 f32;
  Alcotest.(check int) "quiet 64 never finishes" 0 f64;
  Alcotest.(check bool) "quiet 32 cancelled" true (c32 >= 1);
  Alcotest.(check bool) "quiet 64 cancelled" true (c64 >= 1);
  Alcotest.(check int) "32 visits 32*320" (32 * 320) v32;
  Alcotest.(check int) "64 visits 64*320" (64 * 320) v64;
  Alcotest.(check int) "no prefix concat at 32" 0 copies32;
  Alcotest.(check int) "no prefix concat at 64" 0 copies64

let test_ptt_silence_and_crossing () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let last_len = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Buffered
          (fun clip ->
            Eta.Effect.sync (fun () ->
                incr finishes;
                last_len := Bytes.length clip.pcm_bytes));
      cancel = Eta.Effect.unit;
    }
  in
  ignore
    (run_loop ~env ~sw ~clock ~handle ~min_duration_ms:0 ~rms:0.01 ~peak:0.01
       ~timeout_s:8 ~audio_fn:(fun ~clock ~keyboard ~audio ->
         Eio.Time.sleep clock 0.05;
         emit keyboard false;
         Eio.Stream.add audio (pcm_chunk 0 160);
         Eio.Time.sleep clock 0.02;
         Eio.Stream.add audio (pcm_chunk 20000 160);
         Eio.Time.sleep clock 0.02;
         emit keyboard true));
  Alcotest.(check int) "crossing finishes once" 1 !finishes;
  Alcotest.(check int) "buffered concat is whole clip" (320 + 320) !last_len

let test_ptt_streaming_finish_does_not_concat () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Streaming
          (fun ~duration_ms:_ -> Eta.Effect.sync (fun () -> incr finishes));
      cancel = Eta.Effect.unit;
    }
  in
  Pie.Audio.Pcm.Accounting.reset ();
  ignore
    (run_loop ~env ~sw ~clock ~handle ~min_duration_ms:0 ~rms:0.0 ~peak:0.0
       ~timeout_s:8 ~audio_fn:(fun ~clock ~keyboard ~audio ->
         Eio.Time.sleep clock 0.05;
         emit keyboard false;
         Eio.Stream.add audio (pcm_chunk 20000 160);
         Eio.Time.sleep clock 0.02;
         emit keyboard true));
  let _visits, copies = Pie.Audio.Pcm.Accounting.snapshot () in
  Alcotest.(check int) "streaming finished" 1 !finishes;
  Alcotest.(check int) "streaming finish copies no clip" 0 copies

let one_byte b =
  let c = Bytes.create 1 in
  Bytes.set_uint8 c 0 b;
  c

let test_ptt_odd_split_pair_succeeds kind =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish =
        (match kind with
        | `Buffered ->
            Pie.Ptt_loop.Buffered
              (fun _ -> Eta.Effect.sync (fun () -> incr finishes))
        | `Streaming ->
            Pie.Ptt_loop.Streaming
              (fun ~duration_ms:_ -> Eta.Effect.sync (fun () -> incr finishes)));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  ignore
    (run_loop ~env ~sw ~clock ~handle ~min_duration_ms:0 ~rms:0.0 ~peak:0.0
       ~timeout_s:8 ~audio_fn:(fun ~clock ~keyboard ~audio ->
         Eio.Time.sleep clock 0.05;
         emit keyboard false;
         let first = Bytes.create 321 in
         Bytes.blit (pcm_chunk 20000 160) 0 first 0 320;
         Bytes.set_uint8 first 320 0x10;
         Eio.Stream.add audio first;
         Eio.Time.sleep clock 0.02;
         Eio.Stream.add audio (one_byte 0x20);
         Eio.Time.sleep clock 0.02;
         emit keyboard true));
  Alcotest.(check int) "odd-split pair finished" 1 !finishes;
  Alcotest.(check int) "odd-split pair did not cancel" 0 !cancels

let test_ptt_odd_final_framing_fails kind =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish =
        (match kind with
        | `Buffered ->
            Pie.Ptt_loop.Buffered
              (fun _ -> Eta.Effect.sync (fun () -> incr finishes))
        | `Streaming ->
            Pie.Ptt_loop.Streaming
              (fun ~duration_ms:_ -> Eta.Effect.sync (fun () -> incr finishes)));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  ignore
    (run_loop ~env ~sw ~clock ~handle ~min_duration_ms:0 ~rms:0.0 ~peak:0.0
       ~timeout_s:8 ~audio_fn:(fun ~clock ~keyboard ~audio ->
         Eio.Time.sleep clock 0.05;
         emit keyboard false;
         Eio.Stream.add audio (pcm_chunk 20000 160);
         Eio.Time.sleep clock 0.02;
         Eio.Stream.add audio (one_byte 0x7f);
         Eio.Time.sleep clock 0.02;
         emit keyboard true));
  Alcotest.(check int) "odd-final did not finish" 0 !finishes;
  Alcotest.(check bool) "odd-final cancelled owned handle" true (!cancels >= 1)

let () =
  Alcotest.run "incremental-capture"
    [
      ( "stats",
        [
          Alcotest.test_case "odd-byte carry matches concat" `Quick
            test_odd_byte_carry_matches_concat;
          Alcotest.test_case "trailing odd byte is not a sample" `Quick
            test_trailing_odd_byte_is_not_a_sample;
          Alcotest.test_case "s16le visits are linear" `Quick
            test_s16le_visits_are_linear;
        ] );
      ( "ptt",
        [
          Alcotest.test_case "quiet nonzero visits linear no prefix copy" `Quick
            test_ptt_quiet_nonzero_visits_linear;
          Alcotest.test_case "silence then crossing concatenates once" `Quick
            test_ptt_silence_and_crossing;
          Alcotest.test_case "streaming finish does not concat" `Quick
            test_ptt_streaming_finish_does_not_concat;
          Alcotest.test_case "odd-split pair succeeds Buffered" `Quick
            (fun () -> test_ptt_odd_split_pair_succeeds `Buffered);
          Alcotest.test_case "odd-split pair succeeds Streaming" `Quick
            (fun () -> test_ptt_odd_split_pair_succeeds `Streaming);
          Alcotest.test_case "odd-final framing fails Buffered" `Quick
            (fun () -> test_ptt_odd_final_framing_fails `Buffered);
          Alcotest.test_case "odd-final framing fails Streaming" `Quick
            (fun () -> test_ptt_odd_final_framing_fails `Streaming);
        ] );
    ]
