let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

module S = Pie.Assistant_service.Server

let with_temp_dir f =
  let dir = Filename.temp_file "pie-control-socket" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name ->
          try Sys.remove (Filename.concat dir name) with Sys_error _ -> ())
        (try Sys.readdir dir with Sys_error _ -> [||]);
      try Unix.rmdir dir with Unix.Unix_error _ -> ())
    (fun () -> f dir)

let socket_in dir = Filename.concat dir "control.sock"
let persist_in dir = Filename.concat dir "recording.json"

let coordinator dir =
  Pie.Recording.Coordinator.create ~caps ~persist_path:(persist_in dir) ()

exception Start_error of S.error

let category = function
  | `Invalid_policy _ -> "invalid-policy"
  | `Unsafe_runtime_dir -> S.err_unsafe_runtime_dir
  | `Inaccessible_runtime_dir -> S.err_inaccessible_runtime_dir
  | `Ownership_conflict _ -> S.err_endpoint_conflict
  | `Active_listener -> S.err_active_listener
  | `Lock_conflict -> S.err_lock_conflict
  | `Lock_identity -> S.err_lock_identity
  | `Probe_failed -> S.err_stale_probe_failed
  | `Bind_failed _ -> "bind-failed"

let start_in ~sw env ~socket_path ?budget coordinator =
  match
    S.start ~caps ~sw ~fs:(Eio.Stdenv.fs env) ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~socket_path ?budget coordinator
  with
  | Ok () -> ()
  | Error reason -> raise (Start_error reason)

let expect_error expected f =
  match f () with
  | exception Start_error actual ->
      Alcotest.(check string) "error category" expected (category actual)
  | () -> Alcotest.failf "expected %s" expected

let send_status ~sw env socket_path =
  let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> try Eio.Flow.close flow with _ -> ())
    (fun () ->
      let req =
        Yojson.Safe.to_string
          (Pie.Assistant_service.Contract.request_to_json
             Pie.Assistant_service.Contract.Status)
        ^ "\n"
      in
      Eio.Flow.write flow [ Cstruct.of_string req ];
      let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
      Eio.Buf_read.line reader)

let test_regular_file_preserved () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  let oc = open_out socket_path in
  output_string oc "sentinel";
  close_out oc;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_endpoint_conflict (fun () ->
      start_in ~sw env ~socket_path (coordinator dir));
  let ic = open_in socket_path in
  let body = input_line ic in
  close_in ic;
  Alcotest.(check string) "sentinel preserved" "sentinel" body

let test_live_listener_conflict () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  Eio.Switch.run @@ fun sw2 ->
  expect_error S.err_lock_conflict (fun () ->
      start_in ~sw:sw2 env ~socket_path (coordinator dir));
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool) "first listener reachable" true (String.length line > 0)

let test_stale_socket_recovered () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind fd (Unix.ADDR_UNIX socket_path);
  Unix.close fd;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool) "recovered listener" true (String.length line > 0)

let test_racing_processes () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  match Unix.fork () with
  | 0 -> (
      try
        Eio_main.run @@ fun env ->
        Eio.Switch.run @@ fun sw ->
        start_in ~sw env ~socket_path (coordinator dir);
        Unix.sleep 2
      with _ -> exit 1)
  | child ->
      Fun.protect
        ~finally:(fun () ->
          (try Unix.kill child Sys.sigterm with _ -> ());
          ignore (Unix.waitpid [] child))
        (fun () ->
          Unix.sleepf 0.2;
          Eio_main.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          match start_in ~sw env ~socket_path (coordinator dir) with
          | () -> Alcotest.fail "second process bound the live endpoint"
          | exception Start_error reason ->
              let category = category reason in
              Alcotest.(check bool)
                "race rejected" true
                (category = S.err_lock_conflict
                || category = S.err_active_listener))

let test_old_owner_retirement () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  ( Eio.Switch.run @@ fun sw ->
    start_in ~sw env ~socket_path (coordinator dir);
    Eio.Switch.run @@ fun sw2 ->
    expect_error S.err_lock_conflict (fun () ->
        start_in ~sw:sw2 env ~socket_path (coordinator dir)) );
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool)
    "replacement listener reachable" true
    (String.length line > 0)

let test_unsafe_dir () =
  with_temp_dir @@ fun dir ->
  Unix.chmod dir 0o777;
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_unsafe_runtime_dir (fun () ->
      start_in ~sw env ~socket_path (coordinator dir))

let test_inaccessible_dir () =
  with_temp_dir @@ fun parent ->
  let not_dir = Filename.concat parent "notdir" in
  let oc = open_out not_dir in
  close_out oc;
  let socket_path = Filename.concat not_dir "control.sock" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_inaccessible_runtime_dir (fun () ->
      start_in ~sw env ~socket_path (coordinator parent))

let test_never_newline_expires () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  let budget = { S.peer_timeout_s = 0.05 } in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path ~budget (coordinator dir);
  let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> try Eio.Flow.close flow with _ -> ())
    (fun () ->
      Eio.Flow.write flow [ Cstruct.of_string "no-newline" ];
      let started = Unix.gettimeofday () in
      let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
      (match Eio.Buf_read.line reader with
      | _ -> ()
      | exception End_of_file -> ()
      | exception _ -> ());
      let elapsed = Unix.gettimeofday () -. started in
      Alcotest.(check bool)
        "expired within whole-peer budget" true (elapsed < 0.8))

let test_shutdown_unlinks_owned_socket () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  (Eio.Switch.run @@ fun sw -> start_in ~sw env ~socket_path (coordinator dir));
  Alcotest.(check bool)
    "owned socket removed" false
    (Sys.file_exists socket_path);
  Alcotest.(check bool)
    "lockfile remains" true
    (Sys.file_exists (socket_path ^ ".lock"))

let fd_count () =
  let dir = Unix.opendir "/proc/self/fd" in
  let rec loop n =
    match Unix.readdir dir with
    | exception End_of_file ->
        Unix.closedir dir;
        n
    | _ -> loop (n + 1)
  in
  loop 0

let connect_partial ~sw env socket_path =
  let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
  Eio.Flow.write flow [ Cstruct.of_string "partial-without-newline" ];
  flow

let test_failed_start_releases_registry () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  let oc = open_out socket_path in
  output_string oc "keep-me";
  close_out oc;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_endpoint_conflict (fun () ->
      start_in ~sw env ~socket_path (coordinator dir));
  let ic = open_in socket_path in
  let body = input_line ic in
  close_in ic;
  Alcotest.(check string) "no destructive replacement" "keep-me" body;
  Sys.remove socket_path;
  start_in ~sw env ~socket_path (coordinator dir);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool)
    "second start after failed claim" true
    (String.length line > 0)

let test_permit_released_on_parse_fail () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> try Eio.Flow.close flow with _ -> ())
    (fun () ->
      Eio.Flow.write flow [ Cstruct.of_string "{not-json}\n" ];
      let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
      Alcotest.(check string)
        "malformed response" {|{"error":"malformed-request"}|}
        (Eio.Buf_read.line reader));
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool) "status after parse fail" true (String.length line > 0)

let test_client_cancellation_retires_handler () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  (try
     Eio.Switch.run @@ fun client_sw ->
     let flow =
       Eio.Net.connect ~sw:client_sw (Eio.Stdenv.net env) (`Unix socket_path)
     in
     Eio.Flow.write flow [ Cstruct.of_string "cancelled-without-newline" ];
     raise Exit
   with Exit -> ());
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool)
    "status after client cancel" true
    (String.length line > 0)

let test_shutdown_with_stalled_handlers () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  ( Eio.Switch.run @@ fun sw ->
    start_in ~sw env ~socket_path (coordinator dir);
    let _stalled = List.init 4 (fun _ -> connect_partial ~sw env socket_path) in
    () );
  Alcotest.(check bool)
    "socket removed with stalled handlers" false
    (Sys.file_exists socket_path)

let test_capacity_saturation () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  let before = fd_count () in
  let stalled = List.init 16 (fun _ -> connect_partial ~sw env socket_path) in
  let during = fd_count () in
  Alcotest.(check bool) "accepted fds bounded" true (during - before <= 40);
  let finished = ref false in
  let waiter, resolve = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      (try ignore (send_status ~sw env socket_path) with _ -> ());
      finished := true;
      Eio.Promise.resolve resolve ());
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
  Alcotest.(check bool) "17th handler waits for a permit" false !finished;
  Eio.Promise.await waiter;
  Alcotest.(check bool) "17th handler runs after a slot frees" true !finished;
  List.iter (fun flow -> try Eio.Flow.close flow with _ -> ()) stalled

let test_saturated_foreign_listener_not_stale () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  let foreign = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.bind foreign (Unix.ADDR_UNIX socket_path);
  Unix.listen foreign 1;
  let fillers =
    List.init 8 (fun _ ->
        let c = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        Unix.set_nonblock c;
        (try Unix.connect c (Unix.ADDR_UNIX socket_path) with
        | Unix.Unix_error ((Unix.EINPROGRESS | Unix.EAGAIN), _, _) -> ()
        | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> ()
        | Unix.Unix_error _ -> ());
        c)
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun c -> try Unix.close c with _ -> ()) fillers;
      try Unix.close foreign with _ -> ())
    (fun () ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      (match start_in ~sw env ~socket_path (coordinator dir) with
      | () -> Alcotest.fail "saturated foreign listener treated as stale"
      | exception Start_error reason ->
          let category = category reason in
          Alcotest.(check bool)
            "fail closed" true
            (category = S.err_stale_probe_failed
            || category = S.err_active_listener));
      Alcotest.(check bool)
        "foreign socket not unlinked" true
        (Sys.file_exists socket_path);
      match Unix.lstat socket_path with
      | { st_kind = Unix.S_SOCK; _ } -> ()
      | _ -> Alcotest.fail "foreign listener replaced")

let test_equivalent_path_spellings () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  let alias = Filename.concat dir "./control.sock" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  Eio.Switch.run @@ fun sw2 ->
  expect_error S.err_lock_conflict (fun () ->
      start_in ~sw:sw2 env ~socket_path:alias (coordinator dir))

let test_oversize_line_malformed_then_status () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () -> try Eio.Flow.close flow with _ -> ())
    (fun () ->
      let oversized = String.make 65537 'x' ^ "\n" in
      Eio.Flow.write flow [ Cstruct.of_string oversized ];
      let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
      match Eio.Buf_read.line reader with
      | line ->
          Alcotest.(check string)
            "oversize malformed category" {|{"error":"malformed-request"}|} line
      | exception End_of_file -> ());
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool) "status after oversize" true (String.length line > 0)

let test_client_cancel_releases_fd () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  let before = fd_count () in
  (try
     Eio.Switch.run @@ fun client_sw ->
     let flow =
       Eio.Net.connect ~sw:client_sw (Eio.Stdenv.net env) (`Unix socket_path)
     in
     Eio.Flow.write flow [ Cstruct.of_string "cancelled-without-newline" ];
     raise Exit
   with Exit -> ());
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
  let after = fd_count () in
  Alcotest.(check bool)
    "physical fds retired after client cancel" true
    (after <= before + 2);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool) "status after fd retire" true (String.length line > 0)

let test_cleanup_continues_after_unlink_failure () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  let raised = ref None in
  (try
     Eio.Switch.run @@ fun sw ->
     start_in ~sw env ~socket_path (coordinator dir);
     Unix.chmod dir 0o500
   with exn -> raised := Some exn);
  Unix.chmod dir 0o700;
  Alcotest.(check bool) "unlink failure surfaced" true (!raised <> None);
  Eio.Switch.run @@ fun sw ->
  start_in ~sw env ~socket_path (coordinator dir);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool)
    "registry and lock released after cleanup defect" true
    (String.length line > 0)

let test_cancel_before_peer_deadline () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  let budget = { S.peer_timeout_s = 5.0 } in
  let started = Unix.gettimeofday () in
  Eio_main.run @@ fun env ->
  (try
     Eio.Switch.run @@ fun sw ->
     start_in ~sw env ~socket_path ~budget (coordinator dir);
     ignore (connect_partial ~sw env socket_path);
     raise Exit
   with Exit -> ());
  let elapsed = Unix.gettimeofday () -. started in
  Alcotest.(check bool) "settled well before peer budget" true (elapsed < 1.0);
  Alcotest.(check bool)
    "owned socket gone after cancel" false
    (Sys.file_exists socket_path)

let test_hardlinked_lock_rejected () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  let lock = socket_path ^ ".lock" in
  let other = Filename.concat dir "other.lock" in
  let oc = open_out lock in
  close_out oc;
  Unix.chmod lock 0o600;
  Unix.link lock other;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_lock_identity (fun () ->
      start_in ~sw env ~socket_path (coordinator dir));
  Alcotest.(check bool) "hardlinked lock preserved" true (Sys.file_exists lock)

let test_symlink_lock_rejected () =
  with_temp_dir @@ fun dir ->
  let socket_path = socket_in dir in
  let target = Filename.concat dir "other" in
  let oc = open_out target in
  close_out oc;
  Unix.symlink target (socket_path ^ ".lock");
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_lock_identity (fun () ->
      start_in ~sw env ~socket_path (coordinator dir));
  Alcotest.(check bool)
    "symlink lock preserved" true
    (match Unix.lstat (socket_path ^ ".lock") with
    | { st_kind = Unix.S_LNK; _ } -> true
    | _ -> false)

let () =
  Alcotest.run "control_socket_ownership"
    [
      ( "ownership",
        [
          Alcotest.test_case "regular file preserved" `Quick
            test_regular_file_preserved;
          Alcotest.test_case "live listener conflict" `Quick
            test_live_listener_conflict;
          Alcotest.test_case "stale socket recovered" `Quick
            test_stale_socket_recovered;
          Alcotest.test_case "racing processes" `Quick test_racing_processes;
          Alcotest.test_case "old owner retirement" `Quick
            test_old_owner_retirement;
          Alcotest.test_case "unsafe dir" `Quick test_unsafe_dir;
          Alcotest.test_case "inaccessible dir" `Quick test_inaccessible_dir;
          Alcotest.test_case "never newline expires" `Quick
            test_never_newline_expires;
          Alcotest.test_case "shutdown unlinks owned socket" `Quick
            test_shutdown_unlinks_owned_socket;
          Alcotest.test_case "symlink lock rejected" `Quick
            test_symlink_lock_rejected;
          Alcotest.test_case "failed start releases registry" `Quick
            test_failed_start_releases_registry;
          Alcotest.test_case "permit released on parse fail" `Quick
            test_permit_released_on_parse_fail;
          Alcotest.test_case "client cancellation retires handler" `Quick
            test_client_cancellation_retires_handler;
          Alcotest.test_case "shutdown with stalled handlers" `Quick
            test_shutdown_with_stalled_handlers;
          Alcotest.test_case "capacity saturation bounded" `Quick
            test_capacity_saturation;
          Alcotest.test_case "saturated foreign listener not stale" `Quick
            test_saturated_foreign_listener_not_stale;
          Alcotest.test_case "equivalent path spellings denied" `Quick
            test_equivalent_path_spellings;
          Alcotest.test_case "hardlinked lock rejected" `Quick
            test_hardlinked_lock_rejected;
          Alcotest.test_case "oversize line then healthy status" `Quick
            test_oversize_line_malformed_then_status;
          Alcotest.test_case "client cancel releases fd" `Quick
            test_client_cancel_releases_fd;
          Alcotest.test_case "cleanup continues after unlink failure" `Quick
            test_cleanup_continues_after_unlink_failure;
          Alcotest.test_case "cancel before peer deadline" `Quick
            test_cancel_before_peer_deadline;
        ] );
    ]
