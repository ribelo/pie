let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

module C = Pie.Recording.Coordinator
module P = Pie.Recording.Phase
module Contract = Pie.Assistant_service.Contract

let with_coordinator_state f =
  Eio_main.run @@ fun _env ->
  let dir = Filename.temp_dir "pie-phase" "" in
  Fun.protect
    ~finally:(fun () ->
      Sys.readdir dir
      |> Array.iter (fun name -> Sys.remove (Filename.concat dir name));
      Unix.rmdir dir)
    (fun () ->
      let path = Filename.concat dir "state.json" in
      f (C.create ~caps ~persist_path:path ()) path)

let with_coordinator f = with_coordinator_state (fun t _ -> f t)
let phase t = C.snapshot t |> fun s -> P.to_string s.phase
let check_phase expected t = Alcotest.(check string) "phase" expected (phase t)

let start t =
  match C.try_start t C.Ptt_translate () with
  | C.Started attempt -> attempt
  | C.Busy _ -> Alcotest.fail "unexpected busy"
  | C.Disabled -> Alcotest.fail "unexpected disabled"

let test_full_sequence () =
  with_coordinator @@ fun t ->
  check_phase "ready" t;
  let attempt = start t in
  check_phase "starting" t;
  List.iter
    (fun (next, expected) ->
      Alcotest.(check bool)
        "transition accepted" true
        (C.set_phase t attempt next);
      check_phase expected t;
      Alcotest.(check bool)
        "capture ownership matches phase" (expected <> "ready") (C.is_active t);
      Alcotest.(check int)
        "detached processing slot remains"
        (if expected = "ready" then 1 else 0)
        (C.snapshot t).processing)
    [
      (P.Recording, "recording");
      (P.Stopping, "stopping");
      (P.Recording, "recording");
      (P.Processing, "ready");
      (P.Delivering, "ready");
      (P.Finishing, "ready");
    ];
  Alcotest.(check bool) "release owner" true (C.release t attempt);
  check_phase "ready" t;
  Alcotest.(check bool) "idle after settlement" false (C.is_active t)

let test_processing_settlement () =
  with_coordinator_state @@ fun t path ->
  let process a =
    ignore (C.set_phase t a P.Recording);
    ignore (C.set_phase t a P.Processing)
  in
  let first = start t in
  process first;
  let second = start t in
  process second;
  Alcotest.(check int) "two processing" 2 (C.snapshot t).processing;
  ignore (C.release t first);
  let persisted () =
    Yojson.Safe.Util.member "processing" (Yojson.Safe.from_file path)
  in
  Alcotest.(check bool)
    "persist detached settlement" true
    (persisted () = `Int 1);
  let third = start t in
  Alcotest.(check bool)
    "old delivery accepted" true
    (C.set_phase t second P.Delivering);
  check_phase "starting" t;
  Alcotest.(check bool) "old settlement accepted" true (C.release t second);
  Alcotest.(check bool) "new capture preserved" true (C.is_active t);
  Alcotest.(check bool)
    "persist while newer capture owns input" true
    (persisted () = `Int 0);
  ignore (C.release t third)

let test_stale_and_backwards_rejected () =
  with_coordinator @@ fun t ->
  let old = start t in
  ignore (C.set_phase t old P.Recording);
  ignore (C.release t old);
  let current = start t in
  Alcotest.(check bool)
    "stale owner cannot alter current" false
    (C.set_phase t old P.Processing);
  Alcotest.(check bool)
    "stale owner cannot release current" false (C.release t old);
  ignore (C.set_phase t current P.Processing);
  Alcotest.(check bool)
    "processing cannot become recording" false
    (C.set_phase t current P.Recording);
  check_phase "ready" t

let test_cancel_phase_is_sticky () =
  with_coordinator @@ fun t ->
  let attempt = start t in
  ignore (C.set_phase t attempt P.Recording);
  C.set_enabled t false;
  check_phase "cancelling" t;
  C.set_enabled t true;
  check_phase "cancelling" t;
  Alcotest.(check bool)
    "cancelled owner remains cancellable" true
    (C.set_phase t attempt P.Processing);
  check_phase "cancelling" t;
  ignore (C.release t attempt);
  check_phase "ready" t

let test_idle_projection () =
  with_coordinator (fun t ->
      C.set_enabled t false;
      check_phase "disabled" t);
  with_coordinator (fun t ->
      C.set_error t "failure";
      check_phase "failed" t)

let json fields = `Assoc fields

let common ~active ~mode ~enabled =
  [
    ("active", `Bool active);
    ("mode", `String mode);
    ("enabled", `Bool enabled);
    ("startedAt", `Null);
    ("updatedAt", `Null);
    ("transcriptPath", `Null);
    ("lastError", `Null);
  ]

let decode value =
  match Contract.snapshot_of_json value with
  | Ok snapshot -> snapshot
  | Error error -> Alcotest.failf "decode failed: %s" error

let test_protocol () =
  let snapshot =
    decode
      (json
         (("phase", `String "processing")
         :: common ~active:true ~mode:"ptt-translate" ~enabled:true))
  in
  Alcotest.(check string)
    "explicit phase" "processing"
    (P.to_string snapshot.phase);
  let encoded = Contract.json_of_snapshot snapshot in
  let roundtrip = decode encoded in
  Alcotest.(check string) "roundtrip" "processing" (P.to_string roundtrip.phase);
  let legacy =
    decode (json (common ~active:true ~mode:"ptt-translate" ~enabled:true))
  in
  Alcotest.(check string)
    "legacy active never claims recording" "busy" (P.to_string legacy.phase);
  (match
     Contract.snapshot_of_json
       (json
          (("phase", `String "future")
          :: common ~active:true ~mode:"ptt-translate" ~enabled:true))
   with
  | Error label ->
      Alcotest.(check string) "unknown phase" Contract.unknown_tag label
  | Ok _ -> Alcotest.fail "unknown phase accepted");
  match
    Contract.snapshot_of_json
      (json
         (("phase", `Null)
         :: common ~active:true ~mode:"ptt-translate" ~enabled:true))
  with
  | Error label ->
      Alcotest.(check string) "null phase" Contract.wrong_type label
  | Ok _ -> Alcotest.fail "null phase accepted"

let () =
  Alcotest.run "lifecycle_phase"
    [
      ( "coordinator",
        [
          Alcotest.test_case "capture handoff retains processing admission"
            `Quick test_full_sequence;
          Alcotest.test_case
            "dict-b1yt processing settlement preserves capture and persists \
             counts"
            `Quick test_processing_settlement;
          Alcotest.test_case "stale and backwards rejected" `Quick
            test_stale_and_backwards_rejected;
          Alcotest.test_case "cancellation phase sticky" `Quick
            test_cancel_phase_is_sticky;
          Alcotest.test_case "idle projection" `Quick test_idle_projection;
        ] );
      ( "protocol",
        [ Alcotest.test_case "explicit and legacy phases" `Quick test_protocol ]
      );
    ]
