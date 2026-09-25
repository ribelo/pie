(* PIE013 PTT slice: queue and capture PCM byte admission. Synthetic audio. *)

open Eta.Syntax

let pcm_chunk sample samples =
  let chunk = Bytes.create (samples * 2) in
  for i = 0 to samples - 1 do
    Bytes.set_int16_le chunk (i * 2) sample
  done;
  chunk

let emit keyboard released =
  Pie.Keyboard.Monitor.emit keyboard
    { released; state = 0; keysym = 1; unichar = 0; keycode = 1 }

let recognize_test event =
  if event.Pie.Keyboard.Monitor.keysym = 1 then
    Some
      (if event.released then Pie.Ptt_loop.Release "test"
       else Pie.Ptt_loop.Press "test")
  else None

let run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes ~capture_pcm_bytes
    ?(press = fun handle -> Pie.Ptt_loop.Handle handle) ~on_abort ~audio_fn
    ~timeout_s ?(queue_capacity = 1024 + 256) () =
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 64 in
  Eio.Fiber.fork ~sw (fun () -> audio_fn ~clock ~keyboard ~audio);
  let config : Pie.Ptt_loop.config =
    {
      recognize = recognize_test;
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0.0, 0.0));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = Eta.Effect.unit;
      on_press = (fun _ -> Eta.Effect.pure (press handle));
      on_release = (fun _ -> Eta.Effect.unit);
      on_abort;
      on_dead_input = Eta.Effect.unit;
      admission_active = (fun () -> false);
    }
  in
  Eta_eio.run_once ~sw ~clock
    (Pie.Ptt_loop.run
       ~mono_clock:(Eio.Stdenv.mono_clock env)
       ~keyboard
       ~audio:(Eta_stream_eio.Stream.from_eio_stream audio)
       ~queue_pcm_bytes ~capture_pcm_bytes ~queue_capacity config
    |> Eta.Effect.timeout_as
         (Eta.Duration.seconds timeout_s)
         ~on_timeout:(`Ptt_error "timeout"))

let cause_has cause needle =
  let rendered =
    Format.asprintf "%a"
      (Eta.Cause.pp (fun fmt -> function
        | `Ptt_error m -> Format.pp_print_string fmt m
        | `Pulse_error e ->
            Format.pp_print_string fmt (Pie.Pulse.Client.error_message e)))
      cause
  in
  let rec loop i =
    i <= String.length rendered - String.length needle
    && (String.sub rendered i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let test_capture_budget_plus_one_rejects () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let offered = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.sync (fun () -> incr offered));
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ -> Eta.Effect.sync (fun () -> incr finishes));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  let result =
    run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes:4096 ~capture_pcm_bytes:8
      ~on_abort:(Eta.Effect.pure false) ~timeout_s:8
      ~audio_fn:(fun ~clock ~keyboard ~audio ->
        Eio.Time.sleep clock 0.05;
        emit keyboard false;
        Eio.Stream.add audio (pcm_chunk 20000 5);
        Eio.Time.sleep clock 0.02;
        emit keyboard true)
      ()
  in
  Alcotest.(check int) "no successful finish" 0 !finishes;
  Alcotest.(check int) "no offer of rejected bytes" 0 !offered;
  Alcotest.(check bool) "cancelled" true (!cancels >= 1);
  match result with
  | Error cause ->
      Alcotest.(check bool)
        "capture budget visible" true
        (cause_has cause "pcm:capture_budget")
  | Ok () -> Alcotest.fail "expected capture budget error or timeout after fail"

let test_capture_budget_exact_accepted () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Buffered
          (fun clip ->
            Eta.Effect.sync (fun () ->
                incr finishes;
                Alcotest.(check int)
                  "exact cap bytes" 8
                  (Bytes.length clip.pcm_bytes)));
      cancel = Eta.Effect.unit;
    }
  in
  ignore
    (run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes:4096 ~capture_pcm_bytes:8
       ~on_abort:(Eta.Effect.pure false) ~timeout_s:8
       ~audio_fn:(fun ~clock ~keyboard ~audio ->
         Eio.Time.sleep clock 0.05;
         emit keyboard false;
         Eio.Stream.add audio (pcm_chunk 20000 4);
         Eio.Time.sleep clock 0.02;
         emit keyboard true)
       ());
  Alcotest.(check int) "exact cap finished" 1 !finishes

let test_queue_budget_plus_one_visible () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish = Pie.Ptt_loop.Buffered (fun _ -> Eta.Effect.unit);
      cancel = Eta.Effect.unit;
    }
  in
  match
    run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes:5 ~capture_pcm_bytes:4096
      ~on_abort:(Eta.Effect.pure false) ~timeout_s:8
      ~audio_fn:(fun ~clock ~keyboard ~audio ->
        Eio.Time.sleep clock 0.05;
        Eio.Stream.add audio (pcm_chunk 1 2);
        Eio.Stream.add audio (pcm_chunk 1 2);
        Eio.Time.sleep clock 0.05;
        emit keyboard false;
        emit keyboard true)
      ()
  with
  | Error cause ->
      Alcotest.(check bool)
        "queue budget visible" true
        (cause_has cause "pcm:queue_budget")
  | Ok () -> Alcotest.fail "expected queue budget error"

let test_stalled_finish_keyboard_and_pause_live () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let extra_seen = Atomic.make 0 in
  let started, resolve_started = Eio.Promise.create () in
  let proceed, resolve_proceed = Eio.Promise.create () in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ ->
            Eta.Effect.sync (fun () ->
                Eio.Promise.resolve resolve_started ();
                Eio.Promise.await proceed;
                incr finishes));
      cancel = Eta.Effect.unit;
    }
  in
  ignore
    (run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes:4096
       ~capture_pcm_bytes:4096 ~on_abort:(Eta.Effect.pure false) ~timeout_s:8
       ~audio_fn:(fun ~clock ~keyboard ~audio ->
         Eio.Time.sleep clock 0.05;
         emit keyboard false;
         Eio.Stream.add audio (pcm_chunk 20000 4);
         Eio.Time.sleep clock 0.02;
         emit keyboard true;
         Eio.Promise.await started;
         emit keyboard false;
         Atomic.set extra_seen 1;
         Eio.Promise.resolve resolve_proceed ())
       ());
  Alcotest.(check int) "finish completed after extra key" 1 !finishes;
  Alcotest.(check int)
    "keyboard live during stalled finish" 1 (Atomic.get extra_seen)

let test_cancel_releases_full_queue () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let cancels = ref 0 in
  let finishes = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ -> Eta.Effect.sync (fun () -> incr finishes));
      cancel = Eta.Effect.sync (fun () -> incr cancels);
    }
  in
  let abort = Atomic.make false in
  ignore
    (run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes:64
       ~capture_pcm_bytes:4096
       ~on_abort:(Eta.Effect.sync (fun () -> Atomic.get abort))
       ~timeout_s:8
       ~audio_fn:(fun ~clock ~keyboard ~audio ->
         Eio.Time.sleep clock 0.05;
         emit keyboard false;
         for _ = 1 to 8 do
           Eio.Stream.add audio (pcm_chunk 20000 2)
         done;
         Atomic.set abort true;
         Eio.Time.sleep clock 0.2;
         emit keyboard true)
       ());
  Alcotest.(check int) "aborted capture did not finish" 0 !finishes;
  Alcotest.(check bool) "cancel ran" true (!cancels >= 1)

let test_item_full_cancel_settles () =
  Eio_main.run @@ fun env ->
  let finished = Atomic.make false in
  (try
     Eio.Switch.run @@ fun sw ->
     let clock = Eio.Stdenv.clock env in
     let stall, resolve_stall = Eio.Promise.create () in
     let queued, resolve_queued = Eio.Promise.create () in
     let handle : Pie.Ptt_loop.handle =
       {
         offer =
           (fun _ ->
             Eta.Effect.sync (fun () ->
                 (try Eio.Promise.resolve resolve_queued () with _ -> ());
                 Eio.Promise.await stall));
         finish = Pie.Ptt_loop.Buffered (fun _ -> Eta.Effect.unit);
         cancel = Eta.Effect.unit;
       }
     in
     Eio.Fiber.fork ~sw (fun () ->
         Eio.Promise.await queued;
         Eio.Time.sleep clock 0.1;
         Eio.Cancel.protect (fun () -> Eio.Switch.fail sw Exit));
     ignore
       (run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes:1_048_576
          ~capture_pcm_bytes:1_048_576 ~queue_capacity:2
          ~on_abort:(Eta.Effect.pure false) ~timeout_s:8
          ~audio_fn:(fun ~clock ~keyboard ~audio ->
            Eio.Time.sleep clock 0.05;
            emit keyboard false;
            Eio.Stream.add audio (pcm_chunk 20000 2);
            Eio.Stream.add audio (pcm_chunk 20000 2);
            Eio.Stream.add audio (pcm_chunk 20000 2);
            Eio.Time.sleep clock 0.2;
            emit keyboard true)
          ())
   with Exit -> ());
  Atomic.set finished true;
  Alcotest.(check bool)
    "item-full root cancel settled" true (Atomic.get finished)

let test_blueprint_rerun_does_not_share_queue () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let finishes = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ -> Eta.Effect.sync (fun () -> incr finishes));
      cancel = Eta.Effect.unit;
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize = recognize_test;
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0.0, 0.0));
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
  let blueprint =
    Pie.Ptt_loop.run
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~keyboard
      ~audio:(Eta_stream_eio.Stream.from_eio_stream audio)
      ~queue_pcm_bytes:4096 ~capture_pcm_bytes:4096 config
    |> Eta.Effect.timeout_as (Eta.Duration.seconds 3)
         ~on_timeout:(`Ptt_error "timeout")
  in
  let fire () =
    emit keyboard false;
    Eio.Stream.add audio (pcm_chunk 20000 4);
    emit keyboard true
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep clock 0.05;
      fire ());
  ignore (Eta_eio.run_once ~sw ~clock blueprint);
  let first = !finishes in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep clock 0.05;
      fire ());
  ignore (Eta_eio.run_once ~sw ~clock blueprint);
  Alcotest.(check bool)
    "second execution still finishes" true (!finishes > first)

let test_capture_budget_and_cleanup_defect_both_visible () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish = Pie.Ptt_loop.Buffered (fun _ -> Eta.Effect.unit);
      cancel = Eta.Effect.sync (fun () -> failwith "cleanup-defect");
    }
  in
  match
    run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes:4096 ~capture_pcm_bytes:8
      ~on_abort:(Eta.Effect.pure false) ~timeout_s:8
      ~audio_fn:(fun ~clock ~keyboard ~audio ->
        Eio.Time.sleep clock 0.05;
        emit keyboard false;
        Eio.Stream.add audio (pcm_chunk 20000 5);
        Eio.Time.sleep clock 0.02;
        emit keyboard true)
      ()
  with
  | Ok () -> Alcotest.fail "expected budget failure"
  | Error cause ->
      Alcotest.(check bool)
        "capture budget retained" true
        (cause_has cause "pcm:capture_budget");
      Alcotest.(check bool)
        "cleanup defect retained" true
        (Eta.Cause.defects cause <> [])

let scoped_press handle = Pie.Ptt_loop.Scoped (fun use -> use handle)

let test_scoped_capture_budget_propagates () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish = Pie.Ptt_loop.Buffered (fun _ -> Eta.Effect.unit);
      cancel = Eta.Effect.unit;
    }
  in
  match
    run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes:4096 ~capture_pcm_bytes:8
      ~press:scoped_press ~on_abort:(Eta.Effect.pure false) ~timeout_s:8
      ~audio_fn:(fun ~clock ~keyboard ~audio ->
        Eio.Time.sleep clock 0.05;
        emit keyboard false;
        Eio.Stream.add audio (pcm_chunk 20000 5);
        Eio.Time.sleep clock 0.02;
        emit keyboard true)
      ()
  with
  | Ok () -> Alcotest.fail "scoped capture budget must propagate"
  | Error cause ->
      Alcotest.(check bool)
        "scoped capture budget visible" true
        (cause_has cause "pcm:capture_budget")

let test_scoped_budget_and_cleanup_defect_both_visible () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Eta.Effect.unit);
      finish = Pie.Ptt_loop.Buffered (fun _ -> Eta.Effect.unit);
      cancel = Eta.Effect.sync (fun () -> failwith "cleanup-defect");
    }
  in
  match
    run_loop ~env ~sw ~clock ~handle ~queue_pcm_bytes:4096 ~capture_pcm_bytes:8
      ~press:scoped_press ~on_abort:(Eta.Effect.pure false) ~timeout_s:8
      ~audio_fn:(fun ~clock ~keyboard ~audio ->
        Eio.Time.sleep clock 0.05;
        emit keyboard false;
        Eio.Stream.add audio (pcm_chunk 20000 5);
        Eio.Time.sleep clock 0.02;
        emit keyboard true)
      ()
  with
  | Ok () -> Alcotest.fail "expected scoped budget failure"
  | Error cause ->
      Alcotest.(check bool)
        "scoped capture budget retained" true
        (cause_has cause "pcm:capture_budget");
      Alcotest.(check bool)
        "scoped cleanup defect retained" true
        (Eta.Cause.defects cause <> [])

let () =
  Alcotest.run "ptt-budgets"
    [
      ( "capture",
        [
          Alcotest.test_case "capture cap plus one rejects" `Quick
            test_capture_budget_plus_one_rejects;
          Alcotest.test_case "capture exact cap accepted" `Quick
            test_capture_budget_exact_accepted;
        ] );
      ( "queue",
        [
          Alcotest.test_case "queue cap plus one visible" `Quick
            test_queue_budget_plus_one_visible;
          Alcotest.test_case "stalled finish keeps pause live" `Quick
            test_stalled_finish_keyboard_and_pause_live;
          Alcotest.test_case "cancel full queue does not finish" `Quick
            test_cancel_releases_full_queue;
          Alcotest.test_case "item-full root cancel settles" `Quick
            test_item_full_cancel_settles;
          Alcotest.test_case "blueprint rerun does not share queue" `Quick
            test_blueprint_rerun_does_not_share_queue;
          Alcotest.test_case "budget and cleanup defect both visible" `Quick
            test_capture_budget_and_cleanup_defect_both_visible;
          Alcotest.test_case "scoped capture budget propagates" `Quick
            test_scoped_capture_budget_propagates;
          Alcotest.test_case "scoped budget and cleanup defect both visible"
            `Quick test_scoped_budget_and_cleanup_defect_both_visible;
        ] );
    ]
