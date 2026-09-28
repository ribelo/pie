(* Isolated PIE026 monotonic capture-clock controls.
   Never changes the host clock. *)

let mtime_of_ms ms = Mtime.of_uint64_ns (Int64.mul (Int64.of_int ms) 1_000_000L)
let chunk = Bytes.make 320 '\x7f'

let test_mock_mono_sleep_wakes () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let real = Eio.Stdenv.clock env in
  let mono = Eio_mock.Clock.Mono.make () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep real 0.05;
      Eio_mock.Clock.Mono.set_time mono (mtime_of_ms 2000));
  Eio.Time.Mono.sleep mono 1.0

let start_pump ~sw ~real mono ~start_ms =
  let stop = Atomic.make false in
  let done_p, done_u = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let rec loop ms =
        if Atomic.get stop then ()
        else (
          Eio.Time.sleep real 0.02;
          Eio_mock.Clock.Mono.set_time mono (mtime_of_ms ms);
          loop (ms + 500))
      in
      loop start_ms;
      Eio.Promise.resolve done_u ());
  fun () ->
    Atomic.set stop true;
    Eio.Promise.await done_p

let stop_msg = "capture_clock.stop"

let audio_until_stop ~sw ~terminal audio () =
  match
    Eta.Concurrent.race ~sw
      [
        (fun () ->
          Eio.Promise.await terminal;
          Ok `Stop);
        (fun () -> Ok (`Chunk (Eio.Stream.take audio)));
      ]
  with
  | Ok `Stop -> Error (`Pulse_error (Pie.Pulse.Client.Disconnected stop_msg))
  | Ok (`Chunk chunk) -> Ok (Some chunk)
  | Error _ -> Error (`Pulse_error (Pie.Pulse.Client.Disconnected "audio-race"))

let run_with_test_stop ~real ~clock ~mono_clock ~terminal operation =
  try
    Eio.Time.with_timeout_exn real 7.0 @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let logger =
      Eta_observability.Logger.create ~service:"pie-test" ~clock ~mono_clock
        ~reporter:(Eta_observability.Logger.Reporter.writer ignore)
        ()
    in
    match operation ~sw ~logger ~terminal with
    | Error (`Pulse_error (Pie.Pulse.Client.Disconnected msg))
      when String.equal msg stop_msg ->
        ()
    | Error _ -> Alcotest.fail "unexpected PTT error"
    | Ok () -> Alcotest.fail "PTT loop returned before stop"
  with Eio.Time.Timeout -> Alcotest.fail "7s deadline"

let trace ~starts ~releases ~finishes ~cancels =
  Printf.sprintf "starts=%d releases=%d finishes=%d cancels=%d" !starts
    !releases !finishes !cancels

let resolve_once p u =
  match Eio.Promise.peek p with
  | None -> Eio.Promise.resolve u ()
  | Some _ -> ()

let test_direct_mono_sleep () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let real = Eio.Stdenv.clock env in
  let mono = Eio_mock.Clock.Mono.make () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep real 0.05;
      Eio_mock.Clock.Mono.set_time mono (mtime_of_ms 2000));
  Eio.Time.Mono.sleep mono 1.0

let control_config ~starts ~finishes ~min_duration_ms handle :
    Pie.Ptt_loop.config =
  {
    recognize = (fun _ -> None);
    min_duration_ms;
    capture_thresholds = (fun () -> (0.0, 0.0));
    log_prefix = (fun mode -> mode);
    source = None;
    on_ready = (fun () -> Ok ());
    on_press =
      (fun _ ->
        incr starts;
        Ok (Pie.Ptt_loop.Handle handle));
    on_release = (fun _ -> Ok ());
    on_abort = (fun () -> Ok false);
    on_dead_input = (fun () -> Ok ());
    admission_active = (fun () -> false);
  }

let traced_config ~starts ~releases ~pressed ~released ~min_duration_ms handle :
    Pie.Ptt_loop.config =
  let pressed_p, pressed_u = pressed in
  let released_p, released_u = released in
  {
    recognize = (fun _ -> None);
    min_duration_ms;
    capture_thresholds = (fun () -> (0.0, 0.0));
    log_prefix = (fun mode -> mode);
    source = None;
    on_ready = (fun () -> Ok ());
    on_press =
      (fun _ ->
        incr starts;
        resolve_once pressed_p pressed_u;
        Ok (Pie.Ptt_loop.Handle handle));
    on_release =
      (fun _ ->
        incr releases;
        resolve_once released_p released_u;
        Ok ());
    on_abort = (fun () -> Ok false);
    on_dead_input = (fun () -> Ok ());
    admission_active = (fun () -> false);
  }

let test_real_clock_smoke () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun outer ->
  let real = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let starts = ref 0 in
  let finishes = ref 0 in
  let terminal, resolve_terminal = Eio.Promise.create () in
  Eio.Fiber.fork ~sw:outer (fun () ->
      Eio.Time.sleep real 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Press "test");
      Eio.Stream.add audio chunk;
      Eio.Time.sleep real 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Release "test"));
  let handle =
    {
      Pie.Ptt_loop.offer = (fun _ -> Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun clip ->
            incr finishes;
            Alcotest.(check bool) "nonnegative" true (clip.duration_ms >= 0);
            resolve_once terminal resolve_terminal;
            Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  let config = control_config ~starts ~finishes ~min_duration_ms:0 handle in
  run_with_test_stop ~real ~clock:real ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~terminal (fun ~sw ~logger ~terminal ->
      Pie.Ptt_loop.run ~sw ~logger
        ~mono_clock:(Eio.Stdenv.mono_clock env)
        ~control ~keyboard
        ~audio:(audio_until_stop ~sw ~terminal audio)
        config);
  Alcotest.(check int) "starts" 1 !starts;
  Alcotest.(check int) "finishes" 1 !finishes

let test_wall_jump_does_not_grant_eligibility () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun outer ->
  let real = Eio.Stdenv.clock env in
  let wall = Eio_mock.Clock.make () in
  let mono = Eio_mock.Clock.Mono.make () in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let starts = ref 0 in
  let releases = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let pressed = Eio.Promise.create () in
  let released = Eio.Promise.create () in
  let stop_pump = ref (fun () -> ()) in
  let stop_tick =
    let stop = Atomic.make false in
    let done_p, done_u = Eio.Promise.create () in
    Eio.Fiber.fork ~sw:outer (fun () ->
        let rec tick t =
          if Atomic.get stop then ()
          else (
            Eio.Time.sleep real 0.01;
            let now = Eio.Time.now wall in
            if t > now then Eio_mock.Clock.set_time wall t;
            tick (t +. 0.05))
        in
        tick 0.05;
        Eio.Promise.resolve done_u ());
    fun () ->
      Atomic.set stop true;
      Eio.Promise.await done_p
  in
  let terminal, resolve_terminal = Eio.Promise.create () in
  let handle =
    {
      Pie.Ptt_loop.offer = (fun _ -> Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ ->
            incr finishes;
            Ok ());
      cancel =
        (fun () ->
          incr cancels;
          resolve_once terminal resolve_terminal;
          Ok ());
    }
  in
  let config =
    traced_config ~starts ~releases ~pressed ~released ~min_duration_ms:2000
      handle
  in
  Eio.Fiber.fork ~sw:outer (fun () ->
      Eio.Time.sleep real 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Press "test");
      Eio.Stream.add audio chunk;
      Eio.Promise.await (fst pressed);
      Eio_mock.Clock.set_time wall 60.0;
      Eio.Stream.add control (Pie.Ptt_loop.Release "test");
      Eio.Promise.await (fst released);
      stop_pump := start_pump ~sw:outer ~real mono ~start_ms:500);
  let () =
    Fun.protect
      ~finally:(fun () ->
        !stop_pump ();
        stop_tick ())
      (fun () ->
        run_with_test_stop ~real ~clock:wall ~mono_clock:mono ~terminal
          (fun ~sw ~logger ~terminal ->
            Pie.Ptt_loop.run ~sw ~logger ~mono_clock:mono ~control ~keyboard
              ~audio:(audio_until_stop ~sw ~terminal audio)
              config))
  in
  Alcotest.(check int) "press consumed" 1 !starts;
  Alcotest.(check int) "release consumed" 1 !releases;
  Alcotest.(check int) "wall jump did not finish" 0 !finishes;
  Alcotest.(check bool) "ineligible cancel before test stop" true (!cancels >= 1)

let test_mono_hold_and_postroll_eligible () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun outer ->
  let real = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let starts = ref 0 in
  let releases = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let durations = ref [] in
  let mono = Eio_mock.Clock.Mono.make () in
  let pressed = Eio.Promise.create () in
  let released = Eio.Promise.create () in
  let terminal, resolve_terminal = Eio.Promise.create () in
  let stop_pump = start_pump ~sw:outer ~real mono ~start_ms:500 in
  let handle =
    {
      Pie.Ptt_loop.offer = (fun _ -> Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun clip ->
            incr finishes;
            durations := clip.duration_ms :: !durations;
            resolve_once terminal resolve_terminal;
            Ok ());
      cancel =
        (fun () ->
          incr cancels;
          Ok ());
    }
  in
  let config =
    traced_config ~starts ~releases ~pressed ~released ~min_duration_ms:2000
      handle
  in
  Eio.Fiber.fork ~sw:outer (fun () ->
      Eio.Time.sleep real 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Press "test");
      Eio.Stream.add audio chunk;
      Eio.Promise.await (fst pressed);
      Eio.Time.sleep real 0.15;
      Eio.Stream.add control (Pie.Ptt_loop.Release "test");
      Eio.Promise.await (fst released));
  let () =
    Fun.protect ~finally:stop_pump (fun () ->
        run_with_test_stop ~real ~clock:(Eio.Stdenv.clock env) ~mono_clock:mono
          ~terminal (fun ~sw ~logger ~terminal ->
            Pie.Ptt_loop.run ~sw ~logger ~mono_clock:mono ~control ~keyboard
              ~audio:(audio_until_stop ~sw ~terminal audio)
              config))
  in
  Alcotest.(check int) "press consumed" 1 !starts;
  Alcotest.(check int) "release consumed" 1 !releases;
  Alcotest.(check int) "finishes" 1 !finishes;
  match !durations with
  | [ duration_ms ] ->
      Alcotest.(check bool) "nonnegative duration" true (duration_ms >= 0);
      Alcotest.(check bool) "held at least min-hold" true (duration_ms >= 2000)
  | _ -> Alcotest.fail "expected one finished clip"

let test_repress_during_postroll () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun outer ->
  let real = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 16 in
  let audio = Eio.Stream.create 16 in
  let starts = ref 0 in
  let releases = ref 0 in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let mono = Eio_mock.Clock.Mono.make () in
  let pressed = Eio.Promise.create () in
  let released = Eio.Promise.create () in
  let terminal, resolve_terminal = Eio.Promise.create () in
  let stop_pump = start_pump ~sw:outer ~real mono ~start_ms:500 in
  let handle =
    {
      Pie.Ptt_loop.offer = (fun _ -> Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun clip ->
            incr finishes;
            Alcotest.(check bool)
              "repress duration nonnegative" true (clip.duration_ms >= 0);
            resolve_once terminal resolve_terminal;
            Ok ());
      cancel =
        (fun () ->
          incr cancels;
          Ok ());
    }
  in
  let config =
    traced_config ~starts ~releases ~pressed ~released ~min_duration_ms:2000
      handle
  in
  Eio.Fiber.fork ~sw:outer (fun () ->
      Eio.Time.sleep real 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Press "test");
      Eio.Stream.add audio chunk;
      Eio.Promise.await (fst pressed);
      Eio.Time.sleep real 0.15;
      Eio.Stream.add control (Pie.Ptt_loop.Release "test");
      Eio.Promise.await (fst released);
      Eio.Stream.add control (Pie.Ptt_loop.Press "test");
      Eio.Time.sleep real 0.15;
      Eio.Stream.add control (Pie.Ptt_loop.Release "test"));
  let () =
    Fun.protect ~finally:stop_pump (fun () ->
        run_with_test_stop ~real ~clock:(Eio.Stdenv.clock env) ~mono_clock:mono
          ~terminal (fun ~sw ~logger ~terminal ->
            Pie.Ptt_loop.run ~sw ~logger ~mono_clock:mono ~control ~keyboard
              ~audio:(audio_until_stop ~sw ~terminal audio)
              config))
  in
  Alcotest.(check int) "press consumed" 1 !starts;
  Alcotest.(check bool) "release consumed" true (!releases >= 1);
  Alcotest.(check int) "one finish after repress" 1 !finishes

let test_postroll_take_survives_cancellation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun outer ->
  let real = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let control = Eio.Stream.create 8 in
  let audio = Eio.Stream.create 8 in
  let first, first_done = Eio.Promise.create () in
  let terminal, stop = Eio.Promise.create () in
  let starts = ref 0 and finishes = ref 0 in
  let handle =
    {
      Pie.Ptt_loop.offer = (fun _ -> Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ ->
            incr finishes;
            if !finishes = 1 then Eio.Promise.resolve first_done ();
            if !finishes = 2 then Eio.Promise.resolve stop ();
            Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  let config = control_config ~starts ~finishes ~min_duration_ms:0 handle in
  Eio.Fiber.fork ~sw:outer (fun () ->
      Eio.Time.sleep real 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Press "test");
      Eio.Stream.add audio chunk;
      Eio.Time.sleep real 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Release "test");
      Eio.Promise.await first;
      Eio.Stream.add control (Pie.Ptt_loop.Press "test");
      Eio.Stream.add audio chunk;
      Eio.Time.sleep real 0.05;
      Eio.Stream.add control (Pie.Ptt_loop.Release "test"));
  run_with_test_stop ~real ~clock:real ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~terminal (fun ~sw ~logger ~terminal ->
      Pie.Ptt_loop.run ~sw ~logger
        ~mono_clock:(Eio.Stdenv.mono_clock env)
        ~control ~keyboard
        ~audio:(audio_until_stop ~sw ~terminal audio)
        config);
  Alcotest.(check int) "second capture after cancelled take" 2 !finishes

let test_external_cancellation () =
  Eio_main.run @@ fun env ->
  let real = Eio.Stdenv.clock env in
  try
    Eio.Time.with_timeout_exn real 2.0 @@ fun () ->
    Eio.Switch.run @@ fun sw ->
    let terminal, stop = Eio.Promise.create () in
    let keyboard = Pie.Keyboard.Monitor.create () in
    let audio = Eio.Stream.create 1 in
    Eio.Fiber.fork ~sw (fun () ->
        Eio.Time.sleep real 0.05;
        Eio.Promise.resolve stop ());
    let logger =
      Eta_observability.Logger.create ~service:"pie-test" ~clock:real
        ~mono_clock:(Eio.Stdenv.mono_clock env)
        ~reporter:(Eta_observability.Logger.Reporter.writer ignore)
        ()
    in
    let handle =
      {
        Pie.Ptt_loop.offer = (fun _ -> Ok ());
        finish = Pie.Ptt_loop.Buffered (fun _ -> Ok ());
        cancel = (fun () -> Ok ());
      }
    in
    let starts = ref 0 and finishes = ref 0 in
    let config = control_config ~starts ~finishes ~min_duration_ms:0 handle in
    match
      Eta.Concurrent.with_background ~sw
        (fun () ->
          Eio.Promise.await terminal;
          Error (`Ptt_error stop_msg))
        (fun () ->
          Pie.Ptt_loop.run ~sw ~logger
            ~mono_clock:(Eio.Stdenv.mono_clock env)
            ~keyboard
            ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
            config)
    with
    | Error (`Ptt_error msg) when String.equal msg stop_msg -> ()
    | Error _ -> Alcotest.fail "unexpected external cancellation error"
    | Ok () -> Alcotest.fail "PTT loop returned before external cancel"
  with Eio.Time.Timeout -> Alcotest.fail "external cancellation stalled"

let test_idle_wait_cancellable () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun outer ->
  let real = Eio.Stdenv.clock env in
  let terminal, stop = Eio.Promise.create () in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 1 in
  Eio.Fiber.fork ~sw:outer (fun () ->
      Eio.Time.sleep real 0.05;
      Eio.Promise.resolve stop ());
  let handle =
    {
      Pie.Ptt_loop.offer = (fun _ -> Ok ());
      finish = Pie.Ptt_loop.Buffered (fun _ -> Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  let starts = ref 0 and finishes = ref 0 in
  let config = control_config ~starts ~finishes ~min_duration_ms:0 handle in
  run_with_test_stop ~real ~clock:real ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~terminal (fun ~sw ~logger ~terminal ->
      Pie.Ptt_loop.run ~sw ~logger
        ~mono_clock:(Eio.Stdenv.mono_clock env)
        ~keyboard
        ~audio:(audio_until_stop ~sw ~terminal audio)
        config)

let () =
  Alcotest.run "capture_clock"
    [
      ( "monotonic",
        [
          Alcotest.test_case "mock mono sleep wakes" `Quick
            test_mock_mono_sleep_wakes;
          Alcotest.test_case "direct Mono.sleep wakes" `Quick
            test_direct_mono_sleep;
          Alcotest.test_case "post-roll second take survives cancelled wait"
            `Quick test_postroll_take_survives_cancellation;
          Alcotest.test_case "external PTT cancellation stops queue wait" `Quick
            test_external_cancellation;
          Alcotest.test_case "idle queue wait cancels promptly" `Quick
            test_idle_wait_cancellable;
          Alcotest.test_case "post-roll timer wins and completes" `Quick
            test_real_clock_smoke;
          Alcotest.test_case "mono hold+postroll stays eligible" `Quick
            test_mono_hold_and_postroll_eligible;
          Alcotest.test_case "wall +60s does not grant eligibility" `Quick
            test_wall_jump_does_not_grant_eligibility;
          Alcotest.test_case "repress during post-roll" `Quick
            test_repress_during_postroll;
        ] );
    ]
