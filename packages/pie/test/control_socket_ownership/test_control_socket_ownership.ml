(* ^pcaps-n1rg *)

module S = Pie.Assistant_service.Server

let with_temp_dir
    ~(caps :
       < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >) f
    =
  let dir =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-control-socket" ""
  in
  Ox_caps.Cap_sys.remove ~caps dir;
  Ox_caps.Cap_unix.mkdir ~caps dir 0o700;
  Fun.protect
    ~finally:(fun () ->
      Array.iter
        (fun name ->
          try Ox_caps.Cap_sys.remove ~caps (Filename.concat dir name)
          with Sys_error _ -> ())
        (try Ox_caps.Cap_sys.readdir ~caps dir with Sys_error _ -> [||]);
      try Ox_caps.Cap_unix.rmdir ~caps dir with Unix.Unix_error _ -> ())
    (fun () -> f dir)

let socket_in dir = Filename.concat dir "control.sock"
let persist_in dir = Filename.concat dir "recording.json"

let coordinator
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; .. >) dir =
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

let start_in
    ~(caps :
       < Ox_caps.Cap.env ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.network ; .. >)
    ~sw env ~socket_path ?budget coordinator =
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
    ~finally:(fun () ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      match
        try Result.Ok (Eio.Flow.close flow) with exn -> Result.Error exn
      with
      | Result.Ok value -> value
      | Result.Error _ -> ())
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

let test_regular_file_preserved
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let oc = Ox_caps.Cap_stdlib.open_out ~caps socket_path in
  output_string oc "sentinel";
  close_out oc;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_endpoint_conflict (fun () ->
      start_in ~caps ~sw env ~socket_path (coordinator ~caps dir));
  let ic = Ox_caps.Cap_stdlib.open_in ~caps socket_path in
  let body = input_line ic in
  close_in ic;
  Alcotest.(check string) "sentinel preserved" "sentinel" body

let test_live_listener_conflict
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  Eio.Switch.run @@ fun sw2 ->
  expect_error S.err_lock_conflict (fun () ->
      start_in ~caps ~sw:sw2 env ~socket_path (coordinator ~caps dir));
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool) "first listener reachable" true (String.length line > 0)

let test_stale_socket_recovered
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let fd =
    Ox_caps.Cap_unix.socket ~caps ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  Unix.bind fd (Unix.ADDR_UNIX socket_path);
  Unix.close fd;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool) "recovered listener" true (String.length line > 0)

let test_racing_processes
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exit
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let ready_r, ready_w = Unix.pipe () in
  let lifetime_r, lifetime_w = Unix.pipe () in
  match Ox_caps.Cap_unix.fork ~caps () with
  | 0 ->
      Unix.close ready_r;
      Unix.close lifetime_w;
      let code =
        (* Preserve the exception as data before this test classifies its outcome. *)
        match
          try
            Result.Ok
              (Eio_main.run (fun env ->
                   Eio.Switch.run (fun sw ->
                       start_in ~caps ~sw env ~socket_path
                         (coordinator ~caps dir);
                       ignore (Unix.write ready_w (Bytes.of_string "r") 0 1);
                       Unix.close ready_w;
                       (* Stay owner until the parent finishes, without blocking
                     this child's listener scheduler. *)
                       ignore
                         (Pie.Fs.blocking (fun () ->
                              Unix.read lifetime_r (Bytes.create 1) 0 1))));
               0)
          with exn -> Result.Error exn
        with
        | Result.Ok value -> value
        | Result.Error _ -> 1
      in
      Unix.close lifetime_r;
      Ox_caps.Cap_stdlib.exit ~caps code
  | child ->
      Unix.close ready_w;
      Unix.close lifetime_r;
      Fun.protect
        ~finally:(fun () ->
          Unix.close ready_r;
          Unix.close lifetime_w;
          ignore (Unix.waitpid [] child))
        (fun () ->
          (* A fixed delay proved neither successful bind nor continued
             ownership on a slow filesystem. *)
          if Unix.read ready_r (Bytes.create 1) 0 1 <> 1 then
            Alcotest.fail "child failed before listener readiness";
          Eio_main.run @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          match start_in ~caps ~sw env ~socket_path (coordinator ~caps dir) with
          | () -> Alcotest.fail "second process bound the live endpoint"
          | exception Start_error reason ->
              let category = category reason in
              Alcotest.(check bool)
                "race rejected" true
                (category = S.err_lock_conflict
                || category = S.err_active_listener))

let test_old_owner_retirement
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  ( Eio.Switch.run @@ fun sw ->
    start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
    Eio.Switch.run @@ fun sw2 ->
    expect_error S.err_lock_conflict (fun () ->
        start_in ~caps ~sw:sw2 env ~socket_path (coordinator ~caps dir)) );
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool)
    "replacement listener reachable" true
    (String.length line > 0)

let test_unsafe_dir
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  Ox_caps.Cap_unix.chmod ~caps dir 0o777;
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_unsafe_runtime_dir (fun () ->
      start_in ~caps ~sw env ~socket_path (coordinator ~caps dir))

let test_inaccessible_dir
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun parent ->
  let not_dir = Filename.concat parent "notdir" in
  let oc = Ox_caps.Cap_stdlib.open_out ~caps not_dir in
  close_out oc;
  let socket_path = Filename.concat not_dir "control.sock" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_inaccessible_runtime_dir (fun () ->
      start_in ~caps ~sw env ~socket_path (coordinator ~caps parent))

let test_never_newline_expires
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let budget = { S.peer_timeout_s = 0.05 } in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path ~budget (coordinator ~caps dir);
  let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      match
        try Result.Ok (Eio.Flow.close flow) with exn -> Result.Error exn
      with
      | Result.Ok value -> value
      | Result.Error _ -> ())
    (fun () ->
      Eio.Flow.write flow [ Cstruct.of_string "no-newline" ];
      let started = Ox_caps.Cap_unix.gettimeofday ~caps () in
      let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
      (* Preserve the exception as data before this test classifies its outcome. *)
      (match
         try Result.Ok (Eio.Buf_read.line reader) with exn -> Result.Error exn
       with
      | Result.Ok _ -> ()
      | Result.Error End_of_file -> ()
      | Result.Error _ -> ());
      let elapsed = Ox_caps.Cap_unix.gettimeofday ~caps () -. started in
      Alcotest.(check bool)
        "expired within whole-peer budget" true (elapsed < 0.8))

let test_shutdown_unlinks_owned_socket
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  ( Eio.Switch.run @@ fun sw ->
    start_in ~caps ~sw env ~socket_path (coordinator ~caps dir) );
  Alcotest.(check bool)
    "owned socket removed" false
    (Sys.file_exists socket_path);
  Alcotest.(check bool)
    "lockfile remains" true
    (Sys.file_exists (socket_path ^ ".lock"))

let fd_count ~(caps : < Ox_caps.Cap.fs_read ; .. >) () =
  let dir = Ox_caps.Cap_unix.opendir ~caps "/proc/self/fd" in
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

let test_failed_start_releases_registry
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let oc = Ox_caps.Cap_stdlib.open_out ~caps socket_path in
  output_string oc "keep-me";
  close_out oc;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_endpoint_conflict (fun () ->
      start_in ~caps ~sw env ~socket_path (coordinator ~caps dir));
  let ic = Ox_caps.Cap_stdlib.open_in ~caps socket_path in
  let body = input_line ic in
  close_in ic;
  Alcotest.(check string) "no destructive replacement" "keep-me" body;
  Ox_caps.Cap_sys.remove ~caps socket_path;
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool)
    "second start after failed claim" true
    (String.length line > 0)

let test_permit_released_on_parse_fail
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      match
        try Result.Ok (Eio.Flow.close flow) with exn -> Result.Error exn
      with
      | Result.Ok value -> value
      | Result.Error _ -> ())
    (fun () ->
      Eio.Flow.write flow [ Cstruct.of_string "{not-json}\n" ];
      let reader = Eio.Buf_read.of_flow ~max_size:65536 flow in
      Alcotest.(check string)
        "malformed response" {|{"error":"malformed-request"}|}
        (Eio.Buf_read.line reader));
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool) "status after parse fail" true (String.length line > 0)

let test_client_cancellation_retires_handler
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
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

let test_shutdown_with_stalled_handlers
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  ( Eio.Switch.run @@ fun sw ->
    start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
    let _stalled = List.init 4 (fun _ -> connect_partial ~sw env socket_path) in
    () );
  Alcotest.(check bool)
    "socket removed with stalled handlers" false
    (Sys.file_exists socket_path)

let test_capacity_saturation
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  let before = fd_count ~caps () in
  let stalled = List.init 16 (fun _ -> connect_partial ~sw env socket_path) in
  let during = fd_count ~caps () in
  Alcotest.(check bool) "accepted fds bounded" true (during - before <= 40);
  let finished = ref false in
  let waiter, resolve = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      (match
         try Result.Ok (ignore (send_status ~sw env socket_path))
         with exn -> Result.Error exn
       with
      | Result.Ok value -> value
      | Result.Error _ -> ());
      finished := true;
      Eio.Promise.resolve resolve ());
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
  Alcotest.(check bool) "17th handler waits for a permit" false !finished;
  Eio.Promise.await waiter;
  Alcotest.(check bool) "17th handler runs after a slot frees" true !finished;
  List.iter
    (fun flow ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      match
        try Result.Ok (Eio.Flow.close flow) with exn -> Result.Error exn
      with
      | Result.Ok value -> value
      | Result.Error _ -> ())
    stalled

let test_saturated_foreign_listener_not_stale
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let foreign =
    Ox_caps.Cap_unix.socket ~caps ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  Unix.bind foreign (Unix.ADDR_UNIX socket_path);
  Unix.listen foreign 1;
  let fillers =
    List.init 8 (fun _ ->
        let c =
          Ox_caps.Cap_unix.socket ~caps ~cloexec:true Unix.PF_UNIX
            Unix.SOCK_STREAM 0
        in
        Unix.set_nonblock c;
        (try Unix.connect c (Unix.ADDR_UNIX socket_path) with
        | Unix.Unix_error ((Unix.EINPROGRESS | Unix.EAGAIN), _, _) -> ()
        | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> ()
        | Unix.Unix_error _ -> ());
        c)
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun c ->
          (* Preserve the exception as data before this test classifies its outcome. *)
          match try Result.Ok (Unix.close c) with exn -> Result.Error exn with
          | Result.Ok value -> value
          | Result.Error _ -> ())
        fillers;
      (* Preserve the exception as data before this test classifies its outcome. *)
      match
        try Result.Ok (Unix.close foreign) with exn -> Result.Error exn
      with
      | Result.Ok value -> value
      | Result.Error _ -> ())
    (fun () ->
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
      (match start_in ~caps ~sw env ~socket_path (coordinator ~caps dir) with
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

let test_equivalent_path_spellings
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let alias = Filename.concat dir "./control.sock" in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  Eio.Switch.run @@ fun sw2 ->
  expect_error S.err_lock_conflict (fun () ->
      start_in ~caps ~sw:sw2 env ~socket_path:alias (coordinator ~caps dir))

let test_oversize_line_malformed_then_status
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  let flow = Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path) in
  Fun.protect
    ~finally:(fun () ->
      (* Preserve the exception as data before this test classifies its outcome. *)
      match
        try Result.Ok (Eio.Flow.close flow) with exn -> Result.Error exn
      with
      | Result.Ok value -> value
      | Result.Error _ -> ())
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

let test_client_cancel_releases_fd
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  let before = fd_count ~caps () in
  (try
     Eio.Switch.run @@ fun client_sw ->
     let flow =
       Eio.Net.connect ~sw:client_sw (Eio.Stdenv.net env) (`Unix socket_path)
     in
     Eio.Flow.write flow [ Cstruct.of_string "cancelled-without-newline" ];
     raise Exit
   with Exit -> ());
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
  let after = fd_count ~caps () in
  Alcotest.(check bool)
    "physical fds retired after client cancel" true
    (after <= before + 2);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool) "status after fd retire" true (String.length line > 0)

let test_cleanup_continues_after_unlink_failure
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  let raised = ref None in
  (try
     Eio.Switch.run @@ fun sw ->
     start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
     Ox_caps.Cap_unix.chmod ~caps dir 0o500
   with exn -> raised := Some exn);
  Ox_caps.Cap_unix.chmod ~caps dir 0o700;
  Alcotest.(check bool) "unlink failure surfaced" true (!raised <> None);
  Eio.Switch.run @@ fun sw ->
  start_in ~caps ~sw env ~socket_path (coordinator ~caps dir);
  let line = send_status ~sw env socket_path in
  Alcotest.(check bool)
    "registry and lock released after cleanup defect" true
    (String.length line > 0)

let test_cancel_before_peer_deadline
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let budget = { S.peer_timeout_s = 5.0 } in
  let started = Ox_caps.Cap_unix.gettimeofday ~caps () in
  Eio_main.run @@ fun env ->
  (try
     Eio.Switch.run @@ fun sw ->
     start_in ~caps ~sw env ~socket_path ~budget (coordinator ~caps dir);
     ignore (connect_partial ~sw env socket_path);
     raise Exit
   with Exit -> ());
  let elapsed = Ox_caps.Cap_unix.gettimeofday ~caps () -. started in
  Alcotest.(check bool) "settled well before peer budget" true (elapsed < 1.0);
  Alcotest.(check bool)
    "owned socket gone after cancel" false
    (Sys.file_exists socket_path)

let test_hardlinked_lock_rejected
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let lock = socket_path ^ ".lock" in
  let other = Filename.concat dir "other.lock" in
  let oc = Ox_caps.Cap_stdlib.open_out ~caps lock in
  close_out oc;
  Ox_caps.Cap_unix.chmod ~caps lock 0o600;
  Ox_caps.Cap_unix.link ~caps lock other;
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_lock_identity (fun () ->
      start_in ~caps ~sw env ~socket_path (coordinator ~caps dir));
  Alcotest.(check bool) "hardlinked lock preserved" true (Sys.file_exists lock)

let test_symlink_lock_rejected
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  let target = Filename.concat dir "other" in
  let oc = Ox_caps.Cap_stdlib.open_out ~caps target in
  close_out oc;
  Ox_caps.Cap_unix.symlink ~caps target (socket_path ^ ".lock");
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  expect_error S.err_lock_identity (fun () ->
      start_in ~caps ~sw env ~socket_path (coordinator ~caps dir));
  Alcotest.(check bool)
    "symlink lock preserved" true
    (match Unix.lstat (socket_path ^ ".lock") with
    | { st_kind = Unix.S_LNK; _ } -> true
    | _ -> false)

let test_status_while_persistence_waits
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let socket_path = socket_in dir in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let entered = Semaphore.Binary.make false in
  let released = Semaphore.Binary.make false in
  let settled, settle = Eio.Promise.create () in
  let hold = ref false in
  let persist_io write =
    let wait = !hold in
    Pie.Fs.blocking (fun () ->
        if wait then (
          Semaphore.Binary.release entered;
          Semaphore.Binary.acquire released);
        write ())
  in
  let c =
    Pie.Recording.Coordinator.create ~caps ~persist_path:(persist_in dir)
      ~persist_io ()
  in
  let attempt =
    match Pie.Recording.Coordinator.try_start c Ptt_transcribe () with
    | Started attempt -> attempt
    | _ -> Alcotest.fail "capture admission rejected"
  in
  start_in ~caps ~sw env ~socket_path c;
  hold := true;
  Eio.Fiber.fork ~sw (fun () ->
      ignore (Pie.Recording.Coordinator.set_phase c attempt Processing);
      Eio.Promise.resolve settle ());
  Pie.Fs.blocking (fun () -> Semaphore.Binary.acquire entered);
  Fun.protect
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
          Semaphore.Binary.release released;
          Eio.Promise.await settled))
    (fun () ->
      let body = send_status ~sw env socket_path |> Yojson.Safe.from_string in
      let open Yojson.Safe.Util in
      Alcotest.(check int)
        "authoritative processing while persistence waits" 1
        (body |> member "processing" |> to_int);
      Alcotest.(check bool)
        "microphone already released" false
        (body |> member "active" |> to_bool))

let test_actions_while_state_writer_waits
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_temp_dir ~caps @@ fun dir ->
  let executed = Atomic.make [] in
  Eio_main.run @@ fun env ->
  Eio.Switch.run (fun sw ->
      let entered = Semaphore.Binary.make false in
      let released = Semaphore.Binary.make false in
      let hold = ref false in
      let submitted = ref 0 in
      let buffered =
        Pie.Recording.State_writer.create ~sw
          ~on_error:(fun exn -> raise exn)
          ()
      in
      let persist_io write =
        incr submitted;
        let sequence = !submitted in
        let wait = !hold in
        buffered (fun () ->
            if wait then (
              Semaphore.Binary.release entered;
              Semaphore.Binary.acquire released);
            write ();
            Atomic.set executed (Atomic.get executed @ [ sequence ]))
      in
      let c =
        Pie.Recording.Coordinator.create ~caps ~persist_path:(persist_in dir)
          ~persist_io ()
      in
      let socket_path = socket_in dir in
      start_in ~caps ~sw env ~socket_path c;
      hold := true;
      (match
         Pie.Recording.Coordinator.try_start_processing c Ptt_transcribe ()
       with
      | Started _ -> ()
      | _ -> Alcotest.fail "processing admission rejected");
      Pie.Fs.blocking (fun () -> Semaphore.Binary.acquire entered);
      hold := false;
      Fun.protect
        ~finally:(fun () -> Semaphore.Binary.release released)
        (fun () ->
          let action method_name =
            let flow =
              Eio.Net.connect ~sw (Eio.Stdenv.net env) (`Unix socket_path)
            in
            Fun.protect
              ~finally:(fun () -> Eio.Flow.close flow)
              (fun () ->
                Eio.Flow.copy_string
                  (Printf.sprintf "{\"method\":\"%s\"}\n" method_name)
                  flow;
                ignore
                  (Eio.Buf_read.of_flow ~max_size:65536 flow
                  |> Eio.Buf_read.line))
          in
          action "Pause";
          Alcotest.(check bool)
            "pause already authoritative" false
            (Pie.Recording.Coordinator.snapshot c).enabled;
          action "Resume";
          let body =
            send_status ~sw env socket_path |> Yojson.Safe.from_string
          in
          let open Yojson.Safe.Util in
          Alcotest.(check bool)
            "resume already authoritative" true
            (body |> member "enabled" |> to_bool);
          Alcotest.(check int)
            "processing retained" 1
            (body |> member "processing" |> to_int)));
  Alcotest.(check (list int))
    "bounded coalescing and shutdown drain" [ 1; 2; 4 ] (Atomic.get executed);
  let body =
    Ox_caps.Cap_stdlib.In_channel.with_open_text ~caps (persist_in dir)
      In_channel.input_all
  in
  let open Yojson.Safe.Util in
  Alcotest.(check bool)
    "latest projection persisted" true
    (Yojson.Safe.from_string body |> member "enabled" |> to_bool)

let test_state_writer_failure_reporting () =
  Eio_main.run @@ fun _env ->
  let errors = ref [] in
  let retried = Atomic.make false in
  Eio.Switch.run (fun sw ->
      let failed, resolve_failed = Eio.Promise.create () in
      let persist =
        Pie.Recording.State_writer.create ~sw
          ~on_error:(fun exn ->
            errors := Printexc.to_string exn :: !errors;
            Eio.Promise.resolve resolve_failed ())
          ()
      in
      persist (fun () -> ());
      persist (fun () -> failwith "projection write failed");
      Eio.Promise.await failed;
      persist (fun () -> Atomic.set retried true));
  Alcotest.(check (list string))
    "write failure surfaced once"
    [ "Failure(\"projection write failed\")" ]
    !errors;
  Alcotest.(check bool)
    "later projection still drains" true (Atomic.get retried);
  match
    Eio.Switch.run (fun sw ->
        let persist =
          Pie.Recording.State_writer.create ~sw
            ~on_error:(fun _ -> Alcotest.fail "startup failure swallowed")
            ()
        in
        persist (fun () -> raise Exit))
  with
  | () -> Alcotest.fail "startup failure swallowed"
  | exception Exit -> ()

(* pcapslint-t4bn: cancel actual owning switches, not fabricated exceptions. *)
let test_coordinator_persist_cancellation ~caps () =
  with_temp_dir ~caps @@ fun dir ->
  Eio_main.run @@ fun _env ->
  let completed = ref false in
  let saw_cancellation = ref false in
  let published = ref None in
  let outcome =
    try
      Eio.Switch.run (fun sw ->
          let entered, signal_entered = Eio.Promise.create () in
          let released, release = Eio.Promise.create () in
          let settled, settle = Eio.Promise.create () in
          let hold = ref false in
          let persist_io write =
            if !hold then (
              Eio.Promise.resolve signal_entered ();
              Eio.Promise.await released;
              try Eio.Switch.check sw
              with Eio.Cancel.Cancelled _ as exn ->
                saw_cancellation := true;
                raise exn);
            write ()
          in
          let coordinator =
            Pie.Recording.Coordinator.create ~caps
              ~persist_path:(persist_in dir) ~persist_io ()
          in
          published := Some coordinator;
          let attempt =
            match
              Pie.Recording.Coordinator.try_start coordinator Ptt_transcribe ()
            with
            | Started attempt -> attempt
            | _ -> Alcotest.fail "capture admission rejected"
          in
          hold := true;
          Eio.Fiber.fork ~sw (fun () ->
              Fun.protect
                ~finally:(fun () -> Eio.Promise.resolve settle ())
                (fun () ->
                  ignore
                    (Pie.Recording.Coordinator.set_phase coordinator attempt
                       Processing);
                  completed := true));
          Eio.Promise.await entered;
          Eio.Switch.fail sw Exit;
          Eio.Cancel.protect (fun () ->
              Eio.Promise.resolve release ();
              Eio.Promise.await settled));
      Ok ()
    with exn -> Result.Error exn
  in
  (match outcome with
  | Error Exit -> ()
  | Error exn -> Alcotest.fail (Printexc.to_string exn)
  | Ok () -> Alcotest.fail "owning switch cancellation was lost");
  Alcotest.(check bool)
    "actual cancellation reached persist_io" true !saw_cancellation;
  Alcotest.(check bool)
    "cancelled persistence did not return success" false !completed;
  let snapshot = Pie.Recording.Coordinator.snapshot (Option.get !published) in
  Alcotest.(check int)
    "published phase is not rolled back" 1 snapshot.processing;
  Alcotest.(check bool)
    "published microphone release remains committed" false snapshot.active

let test_state_writer_cancellation () =
  Eio_main.run @@ fun _env ->
  let entered = Semaphore.Binary.make false in
  let released = Semaphore.Binary.make false in
  let saw_cancellation = Atomic.make false in
  let completed = Atomic.make false in
  let reported = ref 0 in
  let outcome =
    try
      Eio.Switch.run (fun sw ->
          let persist =
            Pie.Recording.State_writer.create ~sw
              ~on_error:(fun _ -> incr reported)
              ()
          in
          persist (fun () -> ());
          persist (fun () ->
              Semaphore.Binary.release entered;
              Semaphore.Binary.acquire released;
              (try Eio.Switch.check sw
               with Eio.Cancel.Cancelled _ as exn ->
                 Atomic.set saw_cancellation true;
                 raise exn);
              Atomic.set completed true);
          Fun.protect
            ~finally:(fun () -> Semaphore.Binary.release released)
            (fun () ->
              Pie.Fs.blocking (fun () -> Semaphore.Binary.acquire entered);
              Eio.Switch.fail sw Exit));
      Ok ()
    with exn -> Result.Error exn
  in
  (match outcome with
  | Error Exit -> ()
  | Error exn -> Alcotest.fail (Printexc.to_string exn)
  | Ok () -> Alcotest.fail "owning switch cancellation was lost");
  Alcotest.(check bool)
    "native callback observed actual cancellation" true
    (Atomic.get saw_cancellation);
  Alcotest.(check bool)
    "cancelled native write did not complete" false (Atomic.get completed);
  Alcotest.(check int)
    "cancellation is not an ordinary write failure" 0 !reported

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exit
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.network
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.time
       ; Ox_caps.Cap.tmp
       ; .. >) =
  Alcotest.run "control_socket_ownership"
    [
      ( "ownership",
        [
          Alcotest.test_case "regular file preserved" `Quick
            (test_regular_file_preserved ~caps);
          Alcotest.test_case "live listener conflict" `Quick
            (test_live_listener_conflict ~caps);
          Alcotest.test_case "stale socket recovered" `Quick
            (test_stale_socket_recovered ~caps);
          Alcotest.test_case "racing processes" `Quick
            (test_racing_processes ~caps);
          Alcotest.test_case "old owner retirement" `Quick
            (test_old_owner_retirement ~caps);
          Alcotest.test_case "unsafe dir" `Quick (test_unsafe_dir ~caps);
          Alcotest.test_case "inaccessible dir" `Quick
            (test_inaccessible_dir ~caps);
          Alcotest.test_case "never newline expires" `Quick
            (test_never_newline_expires ~caps);
          Alcotest.test_case "shutdown unlinks owned socket" `Quick
            (test_shutdown_unlinks_owned_socket ~caps);
          Alcotest.test_case "symlink lock rejected" `Quick
            (test_symlink_lock_rejected ~caps);
          Alcotest.test_case "failed start releases registry" `Quick
            (test_failed_start_releases_registry ~caps);
          Alcotest.test_case "permit released on parse fail" `Quick
            (test_permit_released_on_parse_fail ~caps);
          Alcotest.test_case "client cancellation retires handler" `Quick
            (test_client_cancellation_retires_handler ~caps);
          Alcotest.test_case "shutdown with stalled handlers" `Quick
            (test_shutdown_with_stalled_handlers ~caps);
          Alcotest.test_case "capacity saturation bounded" `Quick
            (test_capacity_saturation ~caps);
          Alcotest.test_case "saturated foreign listener not stale" `Quick
            (test_saturated_foreign_listener_not_stale ~caps);
          Alcotest.test_case "equivalent path spellings denied" `Quick
            (test_equivalent_path_spellings ~caps);
          Alcotest.test_case "hardlinked lock rejected" `Quick
            (test_hardlinked_lock_rejected ~caps);
          Alcotest.test_case "oversize line then healthy status" `Quick
            (test_oversize_line_malformed_then_status ~caps);
          Alcotest.test_case "client cancel releases fd" `Quick
            (test_client_cancel_releases_fd ~caps);
          Alcotest.test_case "cleanup continues after unlink failure" `Quick
            (test_cleanup_continues_after_unlink_failure ~caps);
          Alcotest.test_case "cancel before peer deadline" `Quick
            (test_cancel_before_peer_deadline ~caps);
          Alcotest.test_case "status while persistence waits" `Quick
            (test_status_while_persistence_waits ~caps);
          Alcotest.test_case "actions while state writer waits" `Quick
            (test_actions_while_state_writer_waits ~caps);
          Alcotest.test_case "state writer failure reporting" `Quick
            test_state_writer_failure_reporting;
          Alcotest.test_case
            "pcapslint-t4bn persist propagates real cancellation" `Quick
            (test_coordinator_persist_cancellation ~caps);
          Alcotest.test_case
            "pcapslint-t4bn state writer does not classify cancellation" `Quick
            test_state_writer_cancellation;
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
