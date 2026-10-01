module C = Pie.Assistant_service.Contract

let armed_snapshot : C.recording_snapshot =
  {
    phase = Pie.Recording.Phase.Ready;
    active = false;
    mode = "idle";
    enabled = true;
    started_at = None;
    updated_at = None;
    transcript_path = None;
    last_error = None;
    build_commit = None;
    build_time = None;
  }

let expect_ok label = function
  | Ok value -> value
  | Error err -> Alcotest.failf "%s: protocol error %s" label err

let expect_error label expected = function
  | Error actual -> Alcotest.(check string) label expected actual
  | Ok _ -> Alcotest.failf "%s: expected protocol error %s" label expected

let test_status_valid () =
  let json = C.json_of_snapshot armed_snapshot in
  match C.response_of_json C.Status json with
  | Ok (C.Status_resp snap) ->
      Alcotest.(check bool) "enabled" true snap.enabled;
      Alcotest.(check string) "mode" "idle" snap.mode
  | _ -> Alcotest.fail "expected Status_resp"

let test_build_identity () =
  let snapshot =
    {
      armed_snapshot with
      build_commit = Some "abc123-dirty";
      build_time = Some "2026-10-01T10:00:00Z";
    }
  in
  let encoded = C.json_of_snapshot snapshot in
  let decode json =
    match expect_ok "status" (C.response_of_json C.Status json) with
    | C.Status_resp snapshot -> snapshot
    | _ -> Alcotest.fail "expected status"
  in
  let decoded = decode encoded in
  Alcotest.(check (option string))
    "commit" snapshot.build_commit decoded.build_commit;
  Alcotest.(check (option string)) "time" snapshot.build_time decoded.build_time;
  let fields =
    match encoded with `Assoc fields -> fields | _ -> assert false
  in
  let old =
    decode
      (`Assoc
         (List.filter
            (fun (key, _) -> key <> "buildCommit" && key <> "buildTime")
            fields))
  in
  Alcotest.(check (option string))
    "old daemon commit absent" None old.build_commit;
  Alcotest.(check (option string)) "old daemon time absent" None old.build_time;
  expect_error "identity type is checked" C.wrong_type
    (C.response_of_json C.Status
       (`Assoc
          (("buildCommit", `Int 3) :: List.remove_assoc "buildCommit" fields)))

let test_status_missing_mode () =
  expect_error "missing mode" C.missing_field
    (C.response_of_json C.Status
       (`Assoc [ ("active", `Bool false); ("enabled", `Bool true) ]))

let test_status_wrong_type () =
  expect_error "wrong type" C.wrong_type
    (C.response_of_json C.Status
       (`Assoc
          [
            ("active", `String "no");
            ("mode", `String "idle");
            ("enabled", `Bool true);
          ]))

let test_status_duplicate () =
  expect_error "duplicate" C.duplicate_field
    (C.response_of_json C.Status
       (`Assoc
          [
            ("active", `Bool false);
            ("mode", `String "idle");
            ("enabled", `Bool true);
            ("active", `Bool true);
          ]))

let test_status_null_optional () =
  let json =
    `Assoc
      [
        ("active", `Bool false);
        ("mode", `String "idle");
        ("enabled", `Bool true);
        ("startedAt", `Null);
        ("lastError", `Null);
      ]
  in
  ignore (expect_ok "optional null" (C.response_of_json C.Status json))

let test_pause_empty_object () =
  match C.response_of_json C.Pause (`Assoc []) with
  | Ok C.Pause_resp -> ()
  | _ -> Alcotest.fail "expected Pause_resp"

let test_pause_rejects_scalar () =
  expect_error "pause string" C.not_object
    (C.response_of_json C.Pause (`String "ok"));
  expect_error "pause list" C.not_object (C.response_of_json C.Pause (`List []));
  expect_error "pause null" C.not_object (C.response_of_json C.Pause `Null)

let test_resume_rejects_extra () =
  expect_error "resume extra" C.unexpected_field
    (C.response_of_json C.Resume (`Assoc [ ("ok", `Bool true) ]))

let test_toggle_requires_bool () =
  expect_error "toggle missing" C.missing_field
    (C.response_of_json C.Toggle (`Assoc []));
  match C.response_of_json C.Toggle (`Assoc [ ("value", `Bool true) ]) with
  | Ok (C.Toggle_resp true) -> ()
  | _ -> Alcotest.fail "expected Toggle_resp true"

let test_explicit_error_resp () =
  match
    C.response_of_json C.Status
      (`Assoc [ ("error", `String "synthetic protocol error") ])
  with
  | Ok (C.Error_resp "synthetic protocol error") -> ()
  | _ -> Alcotest.fail "expected Error_resp"

let test_malformed_error_field () =
  expect_error "malformed error" C.malformed_error
    (C.response_of_json C.Pause (`Assoc [ ("error", `Bool true) ]))

let test_snapshot_of_status_matching () =
  (match C.snapshot_of_status_response (C.Status_resp armed_snapshot) with
  | Ok snap -> Alcotest.(check string) "mode" "idle" snap.mode
  | Error label -> Alcotest.failf "unexpected %s" label);
  expect_error "not status" C.wrong_type
    (C.snapshot_of_status_response C.Pause_resp)

let action_json ~action =
  `Assoc
    [
      ("action", `String action);
      ("accepted", `Bool true);
      ("message", `Null);
      ("snapshot", C.json_of_snapshot armed_snapshot);
    ]

let test_action_mismatch () =
  expect_error "echoed different action" C.action_mismatch
    (C.response_of_json (C.Action "retry") (action_json ~action:"ptt-start"))

let test_action_matching () =
  match C.response_of_json (C.Action "retry") (action_json ~action:"retry") with
  | Ok (C.Action_resp result) ->
      Alcotest.(check string) "action" "retry" result.action
  | _ -> Alcotest.fail "expected Action_resp retry"

let test_unknown_method () =
  match C.request_of_json (`Assoc [ ("method", `String "MeetingStart") ]) with
  | Error "unknown method: MeetingStart" -> ()
  | _ -> Alcotest.fail "expected unknown method"

let test_line_codec () =
  expect_error "invalid JSON line" "malformed-request"
    (C.decode_line "{not-json}");
  (match C.decode_line {|{"method":"Status"}|} with
  | Ok C.Status -> ()
  | _ -> Alcotest.fail "Status line did not decode");
  Alcotest.(check string)
    "error response" {|{"error":"malformed-request"}|}
    (C.encode_response (C.Error_resp "malformed-request"))

let serve_one ~socket_path reply =
  let server = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock server;
  (try Unix.unlink socket_path with Unix.Unix_error _ -> ());
  Unix.bind server (Unix.ADDR_UNIX socket_path);
  Unix.listen server 1;
  let worker =
    Thread.create
      (fun () ->
        let rec accept_loop () =
          try Unix.accept server
          with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
            Thread.delay 0.01;
            accept_loop ()
        in
        let client, _ = accept_loop () in
        Fun.protect
          ~finally:(fun () -> try Unix.close client with _ -> ())
          (fun () ->
            Unix.clear_nonblock client;
            let buf = Bytes.create 1024 in
            ignore (Unix.read client buf 0 (Bytes.length buf));
            let payload = Bytes.of_string (reply ^ "\n") in
            ignore (Unix.write client payload 0 (Bytes.length payload))))
      ()
  in
  (server, worker)

let with_temp_dir f =
  let runtime_dir = Filename.temp_file "pie-control-protocol" "" in
  Sys.remove runtime_dir;
  Unix.mkdir runtime_dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name ->
          try Sys.remove (Filename.concat runtime_dir name)
          with Sys_error _ -> ())
        (try Sys.readdir runtime_dir with Sys_error _ -> [||]);
      try Unix.rmdir runtime_dir with Unix.Unix_error _ -> ())
    (fun () -> f runtime_dir)

let request_op ~socket_path req =
  let client = Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> try Unix.close client with _ -> ())
    (fun () ->
      Unix.connect client (Unix.ADDR_UNIX socket_path);
      let payload =
        Bytes.of_string (Yojson.Safe.to_string (C.request_to_json req) ^ "\n")
      in
      ignore (Unix.write client payload 0 (Bytes.length payload));
      let buf = Bytes.create 4096 in
      let n = Unix.read client buf 0 (Bytes.length buf) in
      Yojson.Safe.from_string (String.trim (Bytes.sub_string buf 0 n)))

let run_op_roundtrip req reply =
  with_temp_dir @@ fun runtime_dir ->
  let socket_path = Filename.concat runtime_dir "control.sock" in
  let server, worker = serve_one ~socket_path reply in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.close server with _ -> ());
      (try Thread.join worker with _ -> ());
      try Unix.unlink socket_path with Unix.Unix_error _ -> ())
    (fun () ->
      let json = request_op ~socket_path req in
      C.response_of_json req json)

let test_roundtrip_status () =
  let json = Yojson.Safe.to_string (C.json_of_snapshot armed_snapshot) in
  match run_op_roundtrip C.Status json with
  | Ok (C.Status_resp snap) -> Alcotest.(check bool) "enabled" true snap.enabled
  | Ok _ -> Alcotest.fail "expected Status_resp"
  | Error label -> Alcotest.failf "status roundtrip protocol error %s" label

let test_roundtrip_missing_status () =
  match run_op_roundtrip C.Status {|{"active":false,"enabled":true}|} with
  | Error label -> Alcotest.(check string) "missing-field" C.missing_field label
  | Ok (C.Status_resp _) -> Alcotest.fail "missing field fabricated a snapshot"
  | Ok _ -> Alcotest.fail "expected protocol error"

let test_roundtrip_wrong_type () =
  match
    run_op_roundtrip C.Status {|{"active":"no","mode":"idle","enabled":true}|}
  with
  | Error label -> Alcotest.(check string) "wrong-type" C.wrong_type label
  | Ok (C.Status_resp _) -> Alcotest.fail "wrong type fabricated a snapshot"
  | Ok _ -> Alcotest.fail "expected protocol error"

let test_roundtrip_pause () =
  match run_op_roundtrip C.Pause "{}" with
  | Ok C.Pause_resp -> ()
  | _ -> Alcotest.fail "expected Pause_resp"

let test_roundtrip_toggle () =
  match run_op_roundtrip C.Toggle {|{"value":true}|} with
  | Ok (C.Toggle_resp true) -> ()
  | _ -> Alcotest.fail "expected Toggle_resp"

let test_roundtrip_error () =
  match run_op_roundtrip C.Status {|{"error":"synthetic protocol error"}|} with
  | Ok (C.Error_resp "synthetic protocol error") -> ()
  | _ -> Alcotest.fail "expected Error_resp"

let test_roundtrip_action_mismatch () =
  let reply = Yojson.Safe.to_string (action_json ~action:"ptt-start") in
  match run_op_roundtrip (C.Action "retry") reply with
  | Error label ->
      Alcotest.(check string) "action-mismatch" C.action_mismatch label
  | Ok (C.Action_resp result) ->
      Alcotest.failf "fabricated acknowledgement of %s" result.action
  | Ok _ -> Alcotest.fail "expected action-mismatch"

let () =
  Alcotest.run "control_protocol"
    [
      ( "decode",
        [
          Alcotest.test_case "status valid" `Quick test_status_valid;
          Alcotest.test_case "dict-w1pe build identity and old daemon" `Quick
            test_build_identity;
          Alcotest.test_case "status missing field" `Quick
            test_status_missing_mode;
          Alcotest.test_case "status wrong type" `Quick test_status_wrong_type;
          Alcotest.test_case "status duplicate field" `Quick
            test_status_duplicate;
          Alcotest.test_case "status optional null" `Quick
            test_status_null_optional;
          Alcotest.test_case "pause empty object" `Quick test_pause_empty_object;
          Alcotest.test_case "pause rejects scalar list null" `Quick
            test_pause_rejects_scalar;
          Alcotest.test_case "resume rejects extra" `Quick
            test_resume_rejects_extra;
          Alcotest.test_case "toggle requires bool" `Quick
            test_toggle_requires_bool;
          Alcotest.test_case "explicit error object" `Quick
            test_explicit_error_resp;
          Alcotest.test_case "malformed error field" `Quick
            test_malformed_error_field;
          Alcotest.test_case "snapshot_of_status matching" `Quick
            test_snapshot_of_status_matching;
          Alcotest.test_case "unknown meeting method" `Quick test_unknown_method;
          Alcotest.test_case "newline request codec" `Quick test_line_codec;
          Alcotest.test_case "action matching" `Quick test_action_matching;
          Alcotest.test_case "action mismatch rejected" `Quick
            test_action_mismatch;
        ] );
      ( "roundtrip",
        [
          Alcotest.test_case "status valid unix" `Quick test_roundtrip_status;
          Alcotest.test_case "status missing unix" `Quick
            test_roundtrip_missing_status;
          Alcotest.test_case "status wrong type unix" `Quick
            test_roundtrip_wrong_type;
          Alcotest.test_case "pause empty unix" `Quick test_roundtrip_pause;
          Alcotest.test_case "toggle unix" `Quick test_roundtrip_toggle;
          Alcotest.test_case "explicit error unix" `Quick test_roundtrip_error;
          Alcotest.test_case "action mismatch unix" `Quick
            test_roundtrip_action_mismatch;
        ] );
    ]
