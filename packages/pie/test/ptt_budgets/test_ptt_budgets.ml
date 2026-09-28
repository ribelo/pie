(* PIE013 PTT slice: queue and capture PCM byte admission. Synthetic audio only,
   no microphone or provider. Direct style: [Eta.Scope.run] owns the boundary, the
   fixture is a private [Eio.Stream] behind the loop's own pull, and every wait is
   bounded so a regression fails instead of hanging.
   ^pienext-rgpp ^pienext-6nnq ^pienext-bee9 ^pienext-b3x8 *)

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

let test_logger env =
  Eta_observability.Logger.create ~service:"pie-ptt-budgets-test"
    ~clock:(Eio.Stdenv.clock env)
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~reporter:(Eta_observability.Logger.Reporter.records (fun _ -> ()))
    ()

(* One runtime clock; the loop's own timing stays on the monotonic clock. *)
let wait_until ?(steps = 80) ~clock pred =
  let rec loop n =
    if pred () then ()
    else if n <= 0 then Alcotest.fail "ptt-deadline"
    else (
      Eio.Time.sleep clock 0.05;
      loop (n - 1))
  in
  loop steps

let pp_run_error fmt : Pie.Ptt_loop.run_error -> unit = function
  | `Pulse_error error ->
      Format.pp_print_string fmt (Pie.Pulse.Client.error_message error)
  | `Ptt_error msg -> Format.pp_print_string fmt msg
  | `Ptt_cause cause ->
      Eta.Cause.pp
        (fun fmt (`Ptt_error msg) -> Format.pp_print_string fmt msg)
        fmt cause

(* Every defect anywhere in a failure tree, the new spelling of the removed
   [Eta.Cause.defects]. *)
let rec defects : 'e Eta.Cause.t -> exn list = function
  | Eta.Cause.Die (exn, _) -> [ exn ]
  | Eta.Cause.Suppressed { primary; finalizer } ->
      defects primary @ defects finalizer
  | Eta.Cause.Concurrent causes -> List.concat_map defects causes
  | Eta.Cause.Finalizer cause -> defects cause
  | Eta.Cause.Fail _ | Eta.Cause.Interrupt _ -> []

(* The scope wraps the loop's own compound failure in [Fail] of a [`Ptt_cause], so
   a retained defect lives inside that typed payload and the search must descend. *)
let rec defects_of_exit : Pie.Ptt_loop.run_error Eta.Cause.t -> exn list =
  function
  | Eta.Cause.Fail (`Ptt_cause cause) -> defects cause
  | Eta.Cause.Die (exn, _) -> [ exn ]
  | Eta.Cause.Suppressed { primary; finalizer } ->
      defects_of_exit primary @ defects_of_exit finalizer
  | Eta.Cause.Concurrent causes -> List.concat_map defects_of_exit causes
  | Eta.Cause.Finalizer cause -> defects_of_exit cause
  | Eta.Cause.Fail _ | Eta.Cause.Interrupt _ -> []

let contains haystack needle =
  let length = String.length needle in
  let rec loop i =
    i <= String.length haystack - length
    && (String.sub haystack i length = needle || loop (i + 1))
  in
  loop 0

(* Same meaning as the old [cause_has]: the message is visible in the failure the
   boundary reports. *)
let cause_has cause needle =
  contains (Format.asprintf "%a" (Eta.Cause.pp pp_run_error) cause) needle

(* A missing message fails with the whole observed failure, so a regression names
   what it saw instead of only "false". *)
let expect_cause_has ~label cause needle =
  if not (cause_has cause needle) then
    Alcotest.failf "%s: %S missing from %a" label needle
      (Eta.Cause.pp pp_run_error)
      cause

let scoped_press handle = Pie.Ptt_loop.Scoped (fun use -> use handle)

(* One [Ptt_loop.run] lifetime over a private keyboard and audio stream; [audio_fn]
   drives the fixture on the same switch. *)
let loop_body ~env ~sw ~clock ~handle ~queue_pcm_bytes ~capture_pcm_bytes
    ?(press = fun (handle : Pie.Ptt_loop.handle) -> Pie.Ptt_loop.Handle handle)
    ~on_abort ~audio_fn ?(queue_capacity = 1024 + 256) () =
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
      on_ready = (fun () -> Ok ());
      on_press = (fun _ -> Ok (press handle));
      on_release = (fun _ -> Ok ());
      on_abort;
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  Pie.Ptt_loop.run ~sw
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~logger:(test_logger env) ~keyboard
    ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
    ~queue_pcm_bytes ~capture_pcm_bytes ~queue_capacity config

(* [Eta.Timeout.timeout] is the deadline: it cancels the loop and its own result, so a
   regression that never settles fails as [`Ptt_error "timeout"] instead of hanging. *)
let run_lifetime ~env ~clock ~timeout_s ~handle ~queue_pcm_bytes
    ~capture_pcm_bytes ?press ~on_abort ~audio_fn ?queue_capacity () :
    (unit, Pie.Ptt_loop.run_error) Eta.Exit.t =
  match
    Eta.Timeout.timeout ~clock (Eta.Duration.seconds timeout_s) (fun () ->
        Ok
          (Eta.Scope.run (fun sw ->
               loop_body ~env ~sw ~clock ~handle ~queue_pcm_bytes
                 ~capture_pcm_bytes ?press ~on_abort ~audio_fn ?queue_capacity
                 ())))
  with
  | Ok exit -> exit
  | Error `Timeout -> Eta.Exit.Failure (Eta.Cause.Fail (`Ptt_error "timeout"))

(* The old [with_background]: the loop stays live while the assertions run, the first
   side to finish stops the other, and a typed failure of either fails the test. *)
let with_live_loop ~clock ~timeout_s ~sw body assertions =
  match
    Eta.Timeout.timeout ~clock (Eta.Duration.seconds timeout_s) (fun () ->
        Ok
          (Eta.Concurrent.with_background ~sw body (fun () ->
               assertions ();
               Ok ())))
  with
  | Ok (Ok ()) -> ()
  | Ok (Error error) -> Alcotest.failf "ptt loop failed: %a" pp_run_error error
  | Error `Timeout -> Alcotest.fail "ptt loop deadline"

(* The scope is the test boundary: a typed failure or a defect in it must fail the
   test, never be reported as success. *)
let run_scoped (body : Eio.Switch.t -> (unit, Pie.Ptt_loop.run_error) result) =
  match Eta.Scope.run body with
  | Eta.Exit.Success () -> ()
  | Eta.Exit.Failure cause ->
      Alcotest.failf "ptt scope failed: %a" (Eta.Cause.pp pp_run_error) cause

let test_capture_budget_plus_one_rejects () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let cancels = ref 0 in
  let offered = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer =
        (fun _ ->
          incr offered;
          Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ ->
            incr finishes;
            Ok ());
      cancel =
        (fun () ->
          incr cancels;
          Ok ());
    }
  in
  let exit =
    run_lifetime ~env ~clock ~timeout_s:8 ~handle ~queue_pcm_bytes:4096
      ~capture_pcm_bytes:8
      ~on_abort:(fun () -> Ok false)
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
  match exit with
  | Eta.Exit.Failure cause ->
      expect_cause_has ~label:"capture budget visible" cause
        "pcm:capture_budget"
  | Eta.Exit.Success () ->
      Alcotest.fail "expected capture budget error or timeout after fail"

let test_capture_budget_exact_accepted () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun clip ->
            incr finishes;
            Alcotest.(check int)
              "exact cap bytes" 8
              (Bytes.length clip.pcm_bytes);
            Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  run_scoped (fun sw ->
      with_live_loop ~clock ~timeout_s:8 ~sw
        (fun () ->
          loop_body ~env ~sw ~clock ~handle ~queue_pcm_bytes:4096
            ~capture_pcm_bytes:8
            ~on_abort:(fun () -> Ok false)
            ~audio_fn:(fun ~clock ~keyboard ~audio ->
              Eio.Time.sleep clock 0.05;
              emit keyboard false;
              Eio.Stream.add audio (pcm_chunk 20000 4);
              Eio.Time.sleep clock 0.02;
              emit keyboard true)
            ())
        (fun () -> wait_until ~clock (fun () -> !finishes >= 1));
      Alcotest.(check int) "exact cap finished" 1 !finishes;
      Ok ())

let test_queue_budget_plus_one_visible () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Ok ());
      finish = Pie.Ptt_loop.Buffered (fun _ -> Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  match
    run_lifetime ~env ~clock ~timeout_s:8 ~handle ~queue_pcm_bytes:5
      ~capture_pcm_bytes:4096
      ~on_abort:(fun () -> Ok false)
      ~audio_fn:(fun ~clock ~keyboard ~audio ->
        Eio.Time.sleep clock 0.05;
        Eio.Stream.add audio (pcm_chunk 1 2);
        Eio.Stream.add audio (pcm_chunk 1 2);
        Eio.Time.sleep clock 0.05;
        emit keyboard false;
        emit keyboard true)
      ()
  with
  | Eta.Exit.Failure cause ->
      expect_cause_has ~label:"queue budget visible" cause "pcm:queue_budget"
  | Eta.Exit.Success () -> Alcotest.fail "expected queue budget error"

let test_stalled_finish_keyboard_and_pause_live () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let finishes = ref 0 in
  let extra_seen = Atomic.make 0 in
  let started, resolve_started = Eio.Promise.create () in
  let proceed, resolve_proceed = Eio.Promise.create () in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ ->
            Eio.Promise.resolve resolve_started ();
            Eio.Promise.await proceed;
            incr finishes;
            Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  run_scoped (fun sw ->
      with_live_loop ~clock ~timeout_s:8 ~sw
        (fun () ->
          loop_body ~env ~sw ~clock ~handle ~queue_pcm_bytes:4096
            ~capture_pcm_bytes:4096
            ~on_abort:(fun () -> Ok false)
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
            ())
        (fun () ->
          wait_until ~clock (fun () ->
              !finishes >= 1 && Atomic.get extra_seen = 1));
      Alcotest.(check int) "finish completed after extra key" 1 !finishes;
      Alcotest.(check int)
        "keyboard live during stalled finish" 1 (Atomic.get extra_seen);
      Ok ())

let test_cancel_releases_full_queue () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let cancels = ref 0 in
  let finishes = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ ->
            incr finishes;
            Ok ());
      cancel =
        (fun () ->
          incr cancels;
          Ok ());
    }
  in
  let abort = Atomic.make false in
  run_scoped (fun sw ->
      with_live_loop ~clock ~timeout_s:8 ~sw
        (fun () ->
          loop_body ~env ~sw ~clock ~handle ~queue_pcm_bytes:64
            ~capture_pcm_bytes:4096
            ~on_abort:(fun () -> Ok (Atomic.get abort))
            ~audio_fn:(fun ~clock ~keyboard ~audio ->
              Eio.Time.sleep clock 0.05;
              emit keyboard false;
              for _ = 1 to 8 do
                Eio.Stream.add audio (pcm_chunk 20000 2)
              done;
              Atomic.set abort true;
              Eio.Time.sleep clock 0.2;
              emit keyboard true)
            ())
        (fun () -> wait_until ~clock (fun () -> !cancels >= 1));
      Alcotest.(check int) "aborted capture did not finish" 0 !finishes;
      Alcotest.(check bool) "cancel ran" true (!cancels >= 1);
      Ok ())

let test_item_full_cancel_settles () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let settled = Atomic.make false in
  let stall, _resolve_stall = Eio.Promise.create () in
  let queued, resolve_queued = Eio.Promise.create () in
  let handle : Pie.Ptt_loop.handle =
    {
      offer =
        (fun _ ->
          (try Eio.Promise.resolve resolve_queued () with _ -> ());
          Eio.Promise.await stall;
          Ok ());
      finish = Pie.Ptt_loop.Buffered (fun _ -> Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  let done_ =
    match
      Eta.Timeout.timeout ~clock (Eta.Duration.seconds 8) (fun () ->
          Ok
            (Eta.Scope.run (fun sw ->
                 Eio.Fiber.fork ~sw (fun () ->
                     Eio.Promise.await queued;
                     Eio.Time.sleep clock 0.1;
                     Eio.Cancel.protect (fun () -> Eio.Switch.fail sw Exit));
                 loop_body ~env ~sw ~clock ~handle ~queue_pcm_bytes:1_048_576
                   ~capture_pcm_bytes:1_048_576 ~queue_capacity:2
                   ~on_abort:(fun () -> Ok false)
                   ~audio_fn:(fun ~clock ~keyboard ~audio ->
                     Eio.Time.sleep clock 0.05;
                     emit keyboard false;
                     Eio.Stream.add audio (pcm_chunk 20000 2);
                     Eio.Stream.add audio (pcm_chunk 20000 2);
                     Eio.Stream.add audio (pcm_chunk 20000 2);
                     Eio.Time.sleep clock 0.2;
                     emit keyboard true)
                   ())))
    with
    | Ok _ -> true
    | Error `Timeout -> false
  in
  Atomic.set settled done_;
  Alcotest.(check bool)
    "item-full root cancel settled" true (Atomic.get settled)

let test_blueprint_rerun_does_not_share_queue () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let keyboard = Pie.Keyboard.Monitor.create () in
  let audio = Eio.Stream.create 16 in
  let finishes = ref 0 in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Ok ());
      finish =
        Pie.Ptt_loop.Buffered
          (fun _ ->
            incr finishes;
            Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  let config : Pie.Ptt_loop.config =
    {
      recognize = recognize_test;
      min_duration_ms = 0;
      capture_thresholds = (fun () -> (0.0, 0.0));
      log_prefix = (fun mode -> mode);
      source = None;
      on_ready = (fun () -> Ok ());
      on_press = (fun _ -> Ok (Pie.Ptt_loop.Handle handle));
      on_release = (fun _ -> Ok ());
      on_abort = (fun () -> Ok false);
      on_dead_input = (fun () -> Ok ());
      admission_active = (fun () -> false);
    }
  in
  (* The blueprint is one loop call over shared inputs: a second run must start from a
     fresh queue, not the first run's state. *)
  let blueprint ~sw () =
    Pie.Ptt_loop.run ~sw
      ~mono_clock:(Eio.Stdenv.mono_clock env)
      ~logger:(test_logger env) ~keyboard
      ~audio:(fun () -> Ok (Some (Eio.Stream.take audio)))
      ~queue_pcm_bytes:4096 ~capture_pcm_bytes:4096 config
  in
  let fire () =
    emit keyboard false;
    Eio.Stream.add audio (pcm_chunk 20000 4);
    emit keyboard true
  in
  run_scoped (fun sw ->
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Time.sleep clock 0.05;
          fire ());
      let first = ref 0 in
      with_live_loop ~clock ~timeout_s:8 ~sw
        (fun () -> blueprint ~sw ())
        (fun () ->
          wait_until ~clock (fun () -> !finishes >= 1);
          first := !finishes);
      Eio.Fiber.fork ~sw (fun () ->
          Eio.Time.sleep clock 0.05;
          fire ());
      with_live_loop ~clock ~timeout_s:8 ~sw
        (fun () -> blueprint ~sw ())
        (fun () -> wait_until ~clock (fun () -> !finishes > !first));
      Alcotest.(check bool)
        "second execution still finishes" true (!finishes > !first);
      Ok ())

let test_capture_budget_and_cleanup_defect_both_visible () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Ok ());
      finish = Pie.Ptt_loop.Buffered (fun _ -> Ok ());
      cancel = (fun () -> failwith "cleanup-defect");
    }
  in
  match
    run_lifetime ~env ~clock ~timeout_s:8 ~handle ~queue_pcm_bytes:4096
      ~capture_pcm_bytes:8
      ~on_abort:(fun () -> Ok false)
      ~audio_fn:(fun ~clock ~keyboard ~audio ->
        Eio.Time.sleep clock 0.05;
        emit keyboard false;
        Eio.Stream.add audio (pcm_chunk 20000 5);
        Eio.Time.sleep clock 0.02;
        emit keyboard true)
      ()
  with
  | Eta.Exit.Success () -> Alcotest.fail "expected budget failure"
  | Eta.Exit.Failure cause ->
      Alcotest.(check bool)
        "cleanup defect retained" true
        (defects_of_exit cause <> []);
      expect_cause_has ~label:"capture budget retained" cause
        "pcm:capture_budget"

let test_scoped_capture_budget_propagates () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Ok ());
      finish = Pie.Ptt_loop.Buffered (fun _ -> Ok ());
      cancel = (fun () -> Ok ());
    }
  in
  match
    run_lifetime ~env ~clock ~timeout_s:8 ~handle ~queue_pcm_bytes:4096
      ~capture_pcm_bytes:8 ~press:scoped_press
      ~on_abort:(fun () -> Ok false)
      ~audio_fn:(fun ~clock ~keyboard ~audio ->
        Eio.Time.sleep clock 0.05;
        emit keyboard false;
        Eio.Stream.add audio (pcm_chunk 20000 5);
        Eio.Time.sleep clock 0.02;
        emit keyboard true)
      ()
  with
  | Eta.Exit.Success () -> Alcotest.fail "scoped capture budget must propagate"
  | Eta.Exit.Failure cause ->
      expect_cause_has ~label:"scoped capture budget visible" cause
        "pcm:capture_budget"

let test_scoped_budget_and_cleanup_defect_both_visible () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let handle : Pie.Ptt_loop.handle =
    {
      offer = (fun _ -> Ok ());
      finish = Pie.Ptt_loop.Buffered (fun _ -> Ok ());
      cancel = (fun () -> failwith "cleanup-defect");
    }
  in
  match
    run_lifetime ~env ~clock ~timeout_s:8 ~handle ~queue_pcm_bytes:4096
      ~capture_pcm_bytes:8 ~press:scoped_press
      ~on_abort:(fun () -> Ok false)
      ~audio_fn:(fun ~clock ~keyboard ~audio ->
        Eio.Time.sleep clock 0.05;
        emit keyboard false;
        Eio.Stream.add audio (pcm_chunk 20000 5);
        Eio.Time.sleep clock 0.02;
        emit keyboard true)
      ()
  with
  | Eta.Exit.Success () -> Alcotest.fail "expected scoped budget failure"
  | Eta.Exit.Failure cause ->
      Alcotest.(check bool)
        "scoped cleanup defect retained" true
        (defects_of_exit cause <> []);
      expect_cause_has ~label:"scoped capture budget retained" cause
        "pcm:capture_budget"

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
