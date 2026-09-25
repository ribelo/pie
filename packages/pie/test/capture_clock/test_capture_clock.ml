(* Isolated PIE026 monotonic capture-clock controls.
   Never changes the host clock. *)

open Eta.Syntax

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

let run_with_test_stop ~real ~clock ~terminal operation =
  let watcher =
    Eta.Effect.sync (fun () -> Eio.Promise.await terminal)
    |> Eta.Effect.bind (fun () -> Eta.Effect.fail (`Ptt_error stop_msg))
  in
  let wrapped =
    Eta.Effect.with_background ~name:"capture_clock.stop" watcher (fun () ->
        operation)
    |> Eta.Effect.catch_some (function
      | `Ptt_error msg when String.equal msg stop_msg -> Some Eta.Effect.unit
      | _ -> None)
  in
  try
    Eio.Time.with_timeout_exn real 7.0 (fun () ->
        Eio.Switch.run @@ fun inner ->
        match Eta_eio.run_once ~sw:inner ~clock wrapped with
        | Ok () -> ()
        | Error _ -> Alcotest.fail "unexpected PTT error")
  with Eio.Time.Timeout -> Alcotest.fail "7s deadline"

let trace ~starts ~releases ~finishes ~cancels =
  Printf.sprintf "starts=%d releases=%d finishes=%d cancels=%d" !starts
    !releases !finishes !cancels

let resolve_once p u =
  match Eio.Promise.peek p with
  | None -> Eio.Promise.resolve u ()
  | Some _ -> ()

let test_eta_sync_mono_sleep () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let real = Eio.Stdenv.clock env in
  let mono = Eio_mock.Clock.Mono.make () in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep real 0.05;
      Eio_mock.Clock.Mono.set_time mono (mtime_of_ms 2000));
  match
    Eta.Effect.sync (fun () -> Eio.Time.Mono.sleep mono 1.0)
    |> Eta_eio.run_once ~sw ~clock:real
  with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "Eta.Effect.sync Mono.sleep failed"

let control_config ~starts ~finishes ~min_duration_ms handle :
    Pie.Ptt_loop.config =
  {
    recognize = (fun _ -> None);
    min_duration_ms;
    capture_thresholds = (fun () -> (0.0, 0.0));
    log_prefix = (fun mode -> mode);
    source = None;
    on_ready = Eta.Effect.unit;
    on_press =
      (fun _ ->
        Eta.Effect.sync (fun () -> incr starts)
        |> Eta.Effect.map (fun () -> Pie.Ptt_loop.Handle handle));
    on_release = (fun _ -> Eta.Effect.unit);
    on_abort = Eta.Effect.pure false;
    on_dead_input = Eta.Effect.unit;
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
    on_ready = Eta.Effect.unit;
    on_press =
      (fun _ ->
        Eta.Effect.sync (fun () ->
            incr starts;
            resolve_once pressed_p pressed_u)
        |> Eta.Effect.map (fun () -> Pie.Ptt_loop.Handle handle));
    on_release =
      (fun _ ->
        Eta.Effect.sync (fun () ->
            incr releases;
            resolve_once released_p released_u));
    on_abort = Eta.Effect.pure false;
    on_dead_input = Eta.Effect.unit;
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
      Pie.Ptt_loop.offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Buffered
          (fun clip ->
            Eta.Effect.sync (fun () ->
                incr finishes;
                Alcotest.(check bool) "nonnegative" true (clip.duration_ms >= 0);
                resolve_once terminal resolve_terminal));
      cancel = Eta.Effect.unit;
    }
  in
  let config = control_config ~starts ~finishes ~min_duration_ms:0 handle in
  run_with_test_stop ~real ~clock:real ~terminal
    (Pie.Ptt_loop.run
       ~mono_clock:(Eio.Stdenv.mono_clock env)
       ~control ~keyboard
       ~audio:(Eta_stream_eio.Stream.from_eio_stream audio)
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
      Pie.Ptt_loop.offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ -> Eta.Effect.sync (fun () -> incr finishes));
      cancel =
        Eta.Effect.sync (fun () ->
            incr cancels;
            resolve_once terminal resolve_terminal);
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
        run_with_test_stop ~real ~clock:wall ~terminal
          (Pie.Ptt_loop.run ~mono_clock:mono ~control ~keyboard
             ~audio:(Eta_stream_eio.Stream.from_eio_stream audio)
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
      Pie.Ptt_loop.offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Buffered
          (fun clip ->
            Eta.Effect.sync (fun () ->
                incr finishes;
                durations := clip.duration_ms :: !durations;
                resolve_once terminal resolve_terminal));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
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
        run_with_test_stop ~real ~clock:(Eio.Stdenv.clock env) ~terminal
          (Pie.Ptt_loop.run ~mono_clock:mono ~control ~keyboard
             ~audio:(Eta_stream_eio.Stream.from_eio_stream audio)
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
      Pie.Ptt_loop.offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Buffered
          (fun clip ->
            Eta.Effect.sync (fun () ->
                incr finishes;
                Alcotest.(check bool)
                  "repress duration nonnegative" true (clip.duration_ms >= 0);
                resolve_once terminal resolve_terminal));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
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
        run_with_test_stop ~real ~clock:(Eio.Stdenv.clock env) ~terminal
          (Pie.Ptt_loop.run ~mono_clock:mono ~control ~keyboard
             ~audio:(Eta_stream_eio.Stream.from_eio_stream audio)
             config))
  in
  Alcotest.(check int) "press consumed" 1 !starts;
  Alcotest.(check bool) "release consumed" true (!releases >= 1);
  Alcotest.(check int) "one finish after repress" 1 !finishes

let () =
  Alcotest.run "capture_clock"
    [
      ( "monotonic",
        [
          Alcotest.test_case "mock mono sleep wakes" `Quick
            test_mock_mono_sleep_wakes;
          Alcotest.test_case "Eta.sync Mono.sleep wakes" `Quick
            test_eta_sync_mono_sleep;
          Alcotest.test_case "real clocks min-hold 0 smoke" `Quick
            test_real_clock_smoke;
          Alcotest.test_case "mono hold+postroll stays eligible" `Quick
            test_mono_hold_and_postroll_eligible;
          Alcotest.test_case "wall +60s does not grant eligibility" `Quick
            test_wall_jump_does_not_grant_eligibility;
          Alcotest.test_case "repress during post-roll" `Quick
            test_repress_during_postroll;
        ] );
    ]
