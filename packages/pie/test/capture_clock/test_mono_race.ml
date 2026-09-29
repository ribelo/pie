(* PIE026 diagnosis: Concurrent.race mock Mono.sleep vs queue take, 7s real timeout.
   No pie library. Never changes the host clock. *)

let mtime_of_ms ms = Mtime.of_uint64_ns (Int64.mul (Int64.of_int ms) 1_000_000L)
let stages = ref []
let note s = stages := s :: !stages
let dump () = String.concat " | " (List.rev !stages)

let test_race_sleep_vs_empty_take_wakes () =
  stages := [];
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let real = Eio.Stdenv.clock env in
  let mono = Eio_mock.Clock.Mono.make () in
  let q = Eio.Stream.create 4 in
  let stop_pump = Atomic.make false in
  note "origin";
  Eio.Fiber.fork ~sw (fun () ->
      let rec pump ms =
        if Atomic.get stop_pump then ()
        else (
          Eio.Time.sleep real 0.02;
          Eio_mock.Clock.Mono.set_time mono (mtime_of_ms ms);
          pump (ms + 500))
      in
      pump 500);
  let timeout =
   fun () ->
    note "sleep-registered";
    Eio.Time.Mono.sleep mono 2.0;
    note "sleep-finished";
    Ok `Timeout
  in
  let take =
   fun () ->
    note "take-blocked";
    ignore (Eio.Stream.take q);
    Ok `Item
  in
  let operation () = Ox.Concurrent.race ~sw [ timeout; take ] in
  let result =
    try Eio.Time.with_timeout_exn real 7.0 (fun () -> operation ())
    with Eio.Time.Timeout -> Alcotest.fail ("7s real timeout: " ^ dump ())
  in
  Atomic.set stop_pump true;
  match result with
  | Ok `Timeout ->
      note "winner-timeout";
      Alcotest.(check bool) "sleep won race" true true
  | Ok `Item -> Alcotest.fail ("take won unexpectedly: " ^ dump ())
  | Error _ -> Alcotest.fail ("race error: " ^ dump ())

let test_canceled_sleeper_unregisters () =
  stages := [];
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let real = Eio.Stdenv.clock env in
  let mono = Eio_mock.Clock.Mono.make () in
  let q = Eio.Stream.create 4 in
  note "origin";
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Time.sleep real 0.05;
      note "enqueue";
      Eio.Stream.add q "item");
  let timeout =
   fun () ->
    note "sleep-registered";
    Eio.Time.Mono.sleep mono 30.0;
    note "sleep-finished-unexpected";
    Ok `Timeout
  in
  let take =
   fun () ->
    note "take-blocked";
    ignore (Eio.Stream.take q);
    note "take-won";
    Ok `Item
  in
  let operation () = Ox.Concurrent.race ~sw [ timeout; take ] in
  let result =
    try Eio.Time.with_timeout_exn real 7.0 (fun () -> operation ())
    with Eio.Time.Timeout -> Alcotest.fail ("7s real timeout: " ^ dump ())
  in
  match result with
  | Ok `Item ->
      Eio.Time.sleep real 0.1;
      Alcotest.(check bool)
        "canceled sleeper did not finish" true
        (not (List.mem "sleep-finished-unexpected" !stages))
  | Ok `Timeout -> Alcotest.fail ("sleep won: " ^ dump ())
  | Error _ -> Alcotest.fail ("race error: " ^ dump ())

let test_par_finite_body_with_blocked_take () =
  Eio_main.run @@ fun env ->
  let real = Eio.Stdenv.clock env in
  let q = Eio.Stream.create 1 in
  match
    try
      Eio.Switch.run @@ fun inner ->
      Eio.Time.with_timeout_exn real 7.0 (fun () ->
          Ox.Concurrent.par ~sw:inner
            (fun () ->
              ignore (Eio.Stream.take q);
              Ok ())
            (fun () -> Error (`Done "finite")))
    with Eio.Time.Timeout -> Alcotest.fail "par finite body blocked 7s"
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected finite body error"

let () =
  Alcotest.run "mono_race"
    [
      ( "race",
        [
          Alcotest.test_case "race mock sleep vs empty take wakes under 7s"
            `Quick test_race_sleep_vs_empty_take_wakes;
          Alcotest.test_case "canceled mock sleeper unregisters" `Quick
            test_canceled_sleeper_unregisters;
          Alcotest.test_case "par finite body unblocks take under 7s" `Quick
            test_par_finite_body_with_blocked_take;
        ] );
    ]
