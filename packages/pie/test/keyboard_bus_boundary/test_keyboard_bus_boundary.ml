(* ^pcaps-n1rg *)

(* PIE023 untrusted local bus boundary. Never the desktop session bus. *)

module Monitor = Pie.Keyboard.Monitor

let rec rm_rf ~(caps : < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >) path
    =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Ox_caps.Cap_sys.readdir ~caps path with
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                rm_rf ~caps (Filename.concat path name))
            names
      | exception Sys_error _ -> ());
      try Ox_caps.Cap_unix.rmdir ~caps path with Unix.Unix_error _ -> ())
  | _ -> (
      try Ox_caps.Cap_unix.unlink ~caps path with Unix.Unix_error _ -> ())

let fd_count ~(caps : < Ox_caps.Cap.fs_read ; .. >) () =
  let dir = Ox_caps.Cap_unix.opendir ~caps "/proc/self/fd" in
  Fun.protect
    ~finally:(fun () -> Unix.closedir dir)
    (fun () ->
      let rec loop n =
        match Unix.readdir dir with
        | exception End_of_file -> n
        | "." | ".." -> loop n
        | _ -> loop (n + 1)
      in
      loop 0 - 1)

let isolate_process_env
    ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >)
    () =
  let root =
    Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-keyboard-bus-home" ""
  in
  Ox_caps.Cap_sys.remove ~caps root;
  Ox_caps.Cap_unix.mkdir ~caps root 0o700;
  let mkdir_env key rel =
    let path = Filename.concat root rel in
    Ox_caps.Cap_unix.mkdir ~caps path 0o700;
    Ox_caps.Cap_unix.putenv ~caps key path;
    path
  in
  Ox_caps.Cap_unix.putenv ~caps "HOME" root;
  ignore (mkdir_env "XDG_CONFIG_HOME" "config");
  ignore (mkdir_env "XDG_DATA_HOME" "data");
  let runtime = mkdir_env "XDG_RUNTIME_DIR" "runtime" in
  Ox_caps.Cap_unix.putenv ~caps "TMPDIR" root;
  Ox_caps.Cap_unix.putenv ~caps "TMP" root;
  Ox_caps.Cap_unix.putenv ~caps "DBUS_SESSION_BUS_ADDRESS"
    ("unix:path=" ^ Filename.concat runtime "no-session-bus")

let with_sandbox
    ~(caps :
       < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; Ox_caps.Cap.tmp ; .. >) f
    =
  let dir = Ox_caps.Cap_stdlib.Filename.temp_file ~caps "pie-keyboard-bus" "" in
  Ox_caps.Cap_sys.remove ~caps dir;
  Ox_caps.Cap_unix.mkdir ~caps dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf ~caps dir) (fun () -> f dir)

let contains ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec scan i =
    i <= haystack_len - needle_len
    && (String.sub haystack i needle_len = needle || scan (i + 1))
  in
  needle_len = 0 || scan 0

let expect_unsupported f needle =
  match f () with
  | exception Monitor.Unsupported_input msg ->
      if not (contains ~needle msg) then
        Alcotest.failf "unexpected unsupported %S" msg
  | _ -> Alcotest.fail "expected Unsupported_input"

let test_address_path_and_guid () =
  let addr = Monitor.parse_bus_address "unix:path=/synthetic/bus,guid=abc" in
  match addr with
  | { Monitor.endpoint = Monitor.Filesystem path; guid = Some "abc" } ->
      Alcotest.(check string) "path" "/synthetic/bus" path
  | _ -> Alcotest.fail "expected filesystem path plus guid"

let test_address_abstract_not_filesystem () =
  let addr = Monitor.parse_bus_address "unix:abstract=pie-bus" in
  match addr.Monitor.endpoint with
  | Monitor.Abstract name -> Alcotest.(check string) "abstract" "pie-bus" name
  | Monitor.Filesystem _ -> Alcotest.fail "abstract must not become a path"

let test_address_escaped_comma () =
  let addr = Monitor.parse_bus_address "unix:path=/tmp/a%2cb" in
  match addr.Monitor.endpoint with
  | Monitor.Filesystem path ->
      Alcotest.(check string) "decoded comma" "/tmp/a,b" path
  | _ -> Alcotest.fail "expected filesystem path"

let test_address_duplicate_path () =
  expect_unsupported
    (fun () -> Monitor.parse_bus_address "unix:path=/a,path=/b")
    "duplicate"

let test_address_conflict () =
  expect_unsupported
    (fun () -> Monitor.parse_bus_address "unix:path=/a,abstract=b")
    "conflict"

let test_address_unsupported_transport () =
  expect_unsupported
    (fun () -> Monitor.parse_bus_address "tcp:host=127.0.0.1,port=1")
    "transport"

let test_address_unsupported_option () =
  expect_unsupported
    (fun () -> Monitor.parse_bus_address "unix:path=/a,tmpdir=/tmp")
    "option"

let test_address_invalid_percent () =
  expect_unsupported
    (fun () -> Monitor.parse_bus_address "unix:path=/a%zz")
    "percent"

let test_abstract_sockaddr_leading_nul ~(caps : < Ox_caps.Cap.env ; .. >) () =
  Ox_caps.Cap_unix.putenv ~caps "DBUS_SESSION_BUS_ADDRESS"
    "unix:abstract=pie-boundary";
  match Monitor.session_bus_sockaddr ~caps () with
  | `Unix path ->
      Alcotest.(check bool)
        "leading NUL" true
        (String.length path > 0 && path.[0] = '\000');
      Alcotest.(check string)
        "name" "pie-boundary"
        (String.sub path 1 (String.length path - 1))
  | `Tcp _ -> Alcotest.fail "expected unix sockaddr"

let test_parse_unsupported_endian () =
  expect_unsupported
    (fun () -> Monitor.parse_message_bytes (Bytes.make 16 'B'))
    "endian"

let test_parse_oversize_before_alloc () =
  let fixed = Bytes.make 16 '\000' in
  Bytes.set fixed 0 'l';
  (* body_len = 0x00100001 > 64KiB little-endian at offset 4 *)
  Bytes.set fixed 4 '\001';
  Bytes.set fixed 5 '\000';
  Bytes.set fixed 6 '\016';
  Bytes.set fixed 7 '\000';
  expect_unsupported (fun () -> Monitor.parse_message_bytes fixed) "64KiB"

let test_truncated_string () =
  let body = Bytes.create 4 in
  Bytes.set body 0 '\005';
  expect_unsupported
    (fun () -> ignore (Monitor.parse_string_body body))
    "truncated"

let short_budgets =
  { Monitor.acquire = Ox.Duration.ms 50; unwatch = Ox.Duration.ms 20 }

let drain_until_eof flow =
  let buf = Cstruct.create 256 in
  let rec loop () =
    match Eio.Flow.single_read flow buf with
    | 0 -> ()
    | _ -> loop ()
    | exception End_of_file -> ()
  in
  loop ()

let read_auth_line flow =
  let cs = Cstruct.create 1 in
  ignore (Eio.Flow.single_read flow cs);
  Monitor.read_crlf_line flow

let make_method_return ~serial ~reply_serial ?(sender = Monitor.dbus_service)
    ?destination ?signature ?(body = "") () =
  let fields = Buffer.create 64 in
  Monitor.add_header_field fields 5 "u" (fun buf ->
      Monitor.put_u32 buf reply_serial);
  Option.iter (Monitor.add_string_field fields 6) destination;
  Monitor.add_string_field fields 7 sender;
  Option.iter (Monitor.add_signature_field fields 8) signature;
  let fields_s = Buffer.contents fields in
  let header = Buffer.create 64 in
  Buffer.add_char header 'l';
  Buffer.add_char header (Char.chr 2);
  Buffer.add_char header '\000';
  Buffer.add_char header (Char.chr 1);
  Monitor.put_u32 header (String.length body);
  Monitor.put_u32 header serial;
  Monitor.put_u32 header (String.length fields_s);
  Buffer.add_string header fields_s;
  Monitor.pad_buffer header 8;
  Buffer.contents header ^ body

let bus_unique = ":1.0"
let a11y_owner = ":1.42"

let reply_ok flow ~serial ~reply_serial ~sender =
  Monitor.write flow (make_method_return ~serial ~reply_serial ~sender ())

let reply_string flow ~serial ~reply_serial ?(sender = Monitor.dbus_service)
    value =
  Monitor.write flow
    (make_method_return ~serial ~reply_serial ~sender ~signature:"s"
       ~body:(Monitor.dbus_string_body value)
       ())

let reply_member flow ~server_serial msg =
  match msg.Monitor.member with
  | Some "Hello" ->
      reply_string flow ~serial:server_serial ~reply_serial:msg.serial ":1.1"
  | Some "GetNameOwner" ->
      let requested =
        try Monitor.parse_string_body msg.body with _ -> Monitor.a11y_service
      in
      let value =
        if requested = Monitor.dbus_service then bus_unique else a11y_owner
      in
      reply_string flow ~serial:server_serial ~reply_serial:msg.serial value
  | Some "WatchKeyboard" ->
      reply_ok flow ~serial:server_serial ~reply_serial:msg.serial
        ~sender:a11y_owner
  | Some "AddMatch" | Some "UnwatchKeyboard" ->
      reply_ok flow ~serial:server_serial ~reply_serial:msg.serial
        ~sender:Monitor.dbus_service
  | _ -> ()

let with_fake ~(caps : < Ox_caps.Cap.env ; Ox_caps.Cap.fs_write ; .. >) ~sw ~net
    ~clock ~dir script f =
  let path = Filename.concat dir "bus" in
  (try Ox_caps.Cap_unix.unlink ~caps path
   with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  let listening =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:8 net (`Unix path)
  in
  Ox_caps.Cap_unix.putenv ~caps "DBUS_SESSION_BUS_ADDRESS" ("unix:path=" ^ path);
  let closed, resolve_closed = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Fun.protect
        ~finally:(fun () -> Eio.Promise.resolve resolve_closed ())
        (fun () ->
          Eio.Switch.run @@ fun conn_sw ->
          let flow, _ = Eio.Net.accept ~sw:conn_sw listening in
          Fun.protect
            ~finally:(fun () -> try Eio.Flow.close flow with _ -> ())
            (fun () ->
              try script flow with
              | End_of_file -> ()
              | Eio.Cancel.Cancelled _ -> ())));
  Eio.Fiber.yield ();
  f ~await_conn:(fun () ->
      try
        Eio.Time.with_timeout_exn clock 3.0 (fun () -> Eio.Promise.await closed)
      with Eio.Time.Timeout -> Alcotest.fail "server conn did not close")

let park_auth flow =
  ignore (read_auth_line flow);
  drain_until_eof flow

let complete_watch flow =
  ignore (read_auth_line flow);
  Monitor.write flow "OK 0123456789abcdef0123456789abcdef\r\n";
  ignore (Monitor.read_crlf_line flow);
  let rec loop serial =
    let msg = Monitor.read_message flow in
    reply_member flow ~server_serial:serial msg;
    if msg.Monitor.member = Some "WatchKeyboard" then () else loop (serial + 1)
  in
  loop 1

let run_direct operation = Ox.Scope.run (fun _ -> operation ())

let show_cause cause =
  Format.asprintf "%a"
    (Ox.Cause.pp (fun ppf _ -> Format.pp_print_string ppf "<err>"))
    cause

let rec defects = function
  | Ox.Cause.Die (exn, _) -> [ exn ]
  | Ox.Cause.Finalizer cause -> defects cause
  | Ox.Cause.Suppressed { primary; finalizer } ->
      defects primary @ defects finalizer
  | Ox.Cause.Concurrent causes -> List.concat_map defects causes
  | Ox.Cause.Fail _ | Ox.Cause.Interrupt _ -> []

let take_event ~clock ~within keyboard =
  match
    Ox.Timeout.timeout ~clock within (fun () ->
        Ok (Eio.Stream.take (Monitor.subscribe keyboard)))
  with
  | Ok event -> Ok (Some event)
  | Error `Timeout -> Ok None

let expect_input_lost ~label result =
  match result with
  | Ox.Exit.Success _ ->
      Alcotest.failf "%s: consumer succeeded after input loss" label
  | Ox.Exit.Failure cause ->
      let hit =
        List.exists
          (fun exn -> match exn with Monitor.Input_lost -> true | _ -> false)
          (defects cause)
      in
      if not hit then
        Alcotest.failf "%s: expected Input_lost, got %s" label
          (show_cause cause)

let test_stalled_setup_times_out
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake ~caps ~sw ~net ~clock ~dir park_auth (fun ~await_conn ->
      let baseline = fd_count ~caps () in
      let result =
        run_direct (fun () ->
            Monitor.with_monitor_budgets ~caps ~budgets:short_budgets ~sw ~net
              ~clock ~process_mgr (fun _ -> Ok ()))
      in
      (match result with
      | Ox.Exit.Failure cause ->
          let hit =
            List.exists
              (fun exn ->
                match exn with Monitor.Setup_timeout -> true | _ -> false)
              (defects cause)
          in
          if not hit then
            Alcotest.failf "expected Setup_timeout, got %s" (show_cause cause)
      | Ox.Exit.Success _ -> Alcotest.fail "stalled setup succeeded");
      await_conn ();
      Alcotest.(check int)
        "timeout closed the flow" baseline (fd_count ~caps ()))

let test_unwatch_budget_still_closes
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake ~caps ~sw ~net ~clock ~dir
    (fun flow ->
      complete_watch flow;
      drain_until_eof flow)
    (fun ~await_conn ->
      let baseline = fd_count ~caps () in
      let result =
        run_direct (fun () ->
            Monitor.with_monitor_budgets ~caps
              ~budgets:
                {
                  Monitor.acquire = Ox.Duration.seconds 2;
                  unwatch = Ox.Duration.ms 0;
                }
              ~sw ~net ~clock ~process_mgr
              (fun _ -> Ok ()))
      in
      (match result with
      | Ox.Exit.Success () -> ()
      | Ox.Exit.Failure cause ->
          Alcotest.failf "unwatch budget failed consumer %s" (show_cause cause));
      await_conn ();
      Alcotest.(check int) "unwatch budget closed" baseline (fd_count ~caps ()))

(* A stop (cancellation of the monitor's user) must end as a pure interruption: the
   unwatch in release used to observe the cancellation and raise out of the finalizer,
   which the Door rendered as a defect and exit 2 on every service stop (found at the
   eta-next cut-over, 2026-09-28). *)
let test_stop_is_clean_interruption
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake ~caps ~sw ~net ~clock ~dir
    (fun flow ->
      complete_watch flow;
      drain_until_eof flow)
    (fun ~await_conn ->
      let result =
        Ox.Scope.run (fun scope ->
            Eio.Fiber.fork ~sw:scope (fun () ->
                Eio.Time.sleep clock 0.2;
                Eio.Switch.fail scope
                  (Eio.Cancel.Cancelled (Failure "service stop")));
            Monitor.with_monitor_budgets ~caps ~budgets:short_budgets ~sw:scope
              ~net ~clock ~process_mgr (fun _ -> Eio.Fiber.await_cancel ()))
      in
      (match result with
      | Ox.Exit.Success () -> Alcotest.fail "stopped monitor reported success"
      | Ox.Exit.Failure cause ->
          if defects cause <> [] then
            Alcotest.failf "stop raised a defect: %s" (show_cause cause));
      await_conn ())

let add_string_field = Monitor.add_string_field
let add_object_path_field = Monitor.add_object_path_field

let make_signal ~sender ~path ~interface ~member ~signature ~body =
  let fields = Buffer.create 128 in
  add_object_path_field fields 1 path;
  add_string_field fields 2 interface;
  add_string_field fields 3 member;
  add_string_field fields 7 sender;
  Monitor.add_signature_field fields 8 signature;
  let fields_s = Buffer.contents fields in
  let header = Buffer.create 128 in
  Buffer.add_char header 'l';
  Buffer.add_char header (Char.chr 4);
  Buffer.add_char header '\000';
  Buffer.add_char header (Char.chr 1);
  Monitor.put_u32 header (String.length body);
  Monitor.put_u32 header 99;
  Monitor.put_u32 header (String.length fields_s);
  Buffer.add_string header fields_s;
  Monitor.pad_buffer header 8;
  Buffer.contents header ^ body

let key_event_body () =
  let buf = Buffer.create 18 in
  Monitor.put_u32 buf 0;
  Monitor.put_u32 buf 0;
  Monitor.put_u32 buf 65470;
  Monitor.put_u32 buf 0;
  Monitor.put_u16 buf 1;
  Buffer.contents buf

let noc_body ~name ~old_owner ~new_owner =
  let buf = Buffer.create 64 in
  Monitor.add_dbus_string buf name;
  Monitor.add_dbus_string buf old_owner;
  Monitor.add_dbus_string buf new_owner;
  Buffer.contents buf

let test_wrong_sender_is_ignored
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake ~caps ~sw ~net ~clock ~dir
    (fun flow ->
      complete_watch flow;
      Monitor.write flow
        (make_signal ~sender:":1.99" ~path:Monitor.a11y_path
           ~interface:Monitor.a11y_interface ~member:"KeyEvent"
           ~signature:"buuuq" ~body:(key_event_body ()));
      drain_until_eof flow)
    (fun ~await_conn ->
      let result =
        run_direct (fun () ->
            Monitor.with_monitor_budgets ~caps
              ~budgets:
                {
                  Monitor.acquire = Ox.Duration.seconds 2;
                  unwatch = Ox.Duration.ms 50;
                }
              ~sw ~net ~clock ~process_mgr
              (fun keyboard ->
                take_event ~clock ~within:(Ox.Duration.ms 100) keyboard))
      in
      await_conn ();
      match result with
      | Ox.Exit.Success None -> ()
      | Ox.Exit.Success (Some _) -> Alcotest.fail "wrong sender was accepted"
      | Ox.Exit.Failure cause ->
          Alcotest.failf "wrong sender test failed %s" (show_cause cause))

let test_expected_sender_is_accepted
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake ~caps ~sw ~net ~clock ~dir
    (fun flow ->
      complete_watch flow;
      Monitor.write flow
        (make_signal ~sender:a11y_owner ~path:Monitor.a11y_path
           ~interface:Monitor.a11y_interface ~member:"KeyEvent"
           ~signature:"buuuq" ~body:(key_event_body ()));
      drain_until_eof flow)
    (fun ~await_conn ->
      let result =
        run_direct (fun () ->
            Monitor.with_monitor_budgets ~caps
              ~budgets:
                {
                  Monitor.acquire = Ox.Duration.seconds 2;
                  unwatch = Ox.Duration.ms 50;
                }
              ~sw ~net ~clock ~process_mgr
              (fun keyboard ->
                take_event ~clock ~within:(Ox.Duration.seconds 2) keyboard))
      in
      await_conn ();
      match result with
      | Ox.Exit.Success (Some event) ->
          Alcotest.(check int) "keysym" 65470 event.Monitor.keysym
      | Ox.Exit.Success None ->
          Alcotest.fail "expected sender: no key event before the deadline"
      | Ox.Exit.Failure cause ->
          Alcotest.failf "expected sender failed %s" (show_cause cause))

let test_owner_change_is_input_lost
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake ~caps ~sw ~net ~clock ~dir
    (fun flow ->
      complete_watch flow;
      Monitor.write flow
        (make_signal ~sender:Monitor.dbus_service ~path:Monitor.dbus_path
           ~interface:Monitor.dbus_interface ~member:"NameOwnerChanged"
           ~signature:"sss"
           ~body:
             (noc_body ~name:Monitor.a11y_service ~old_owner:a11y_owner
                ~new_owner:":1.99"));
      drain_until_eof flow)
    (fun ~await_conn ->
      let result =
        run_direct (fun () ->
            Monitor.with_monitor_budgets ~caps
              ~budgets:
                {
                  Monitor.acquire = Ox.Duration.seconds 2;
                  unwatch = Ox.Duration.ms 50;
                }
              ~sw ~net ~clock ~process_mgr
              (fun keyboard ->
                take_event ~clock ~within:(Ox.Duration.seconds 2) keyboard))
      in
      await_conn ();
      expect_input_lost ~label:"owner change" result)

let test_owner_loss_during_setup
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  with_fake ~caps ~sw ~net ~clock ~dir
    (fun flow ->
      ignore (read_auth_line flow);
      Monitor.write flow "OK 0123456789abcdef0123456789abcdef\r\n";
      ignore (Monitor.read_crlf_line flow);
      let rec loop serial =
        let msg = Monitor.read_message flow in
        if msg.Monitor.member = Some "GetNameOwner" then (
          Monitor.write flow
            (make_signal ~sender:Monitor.dbus_service ~path:Monitor.dbus_path
               ~interface:Monitor.dbus_interface ~member:"NameOwnerChanged"
               ~signature:"sss"
               ~body:
                 (noc_body ~name:Monitor.a11y_service ~old_owner:""
                    ~new_owner:""));
          reply_member flow ~server_serial:serial msg)
        else (
          reply_member flow ~server_serial:serial msg;
          loop (serial + 1))
      in
      loop 1)
    (fun ~await_conn ->
      let result =
        run_direct (fun () ->
            ignore (Monitor.connect_and_watch ~caps ~sw ~net);
            Ok ())
      in
      await_conn ();
      expect_input_lost ~label:"owner loss during setup" result)

let test_noc_wrong_sender_ignored
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake ~caps ~sw ~net ~clock ~dir
    (fun flow ->
      complete_watch flow;
      Monitor.write flow
        (make_signal ~sender:":1.99" ~path:Monitor.dbus_path
           ~interface:Monitor.dbus_interface ~member:"NameOwnerChanged"
           ~signature:"sss"
           ~body:
             (noc_body ~name:Monitor.a11y_service ~old_owner:a11y_owner
                ~new_owner:""));
      Monitor.write flow
        (make_signal ~sender:a11y_owner ~path:Monitor.a11y_path
           ~interface:Monitor.a11y_interface ~member:"KeyEvent"
           ~signature:"buuuq" ~body:(key_event_body ()));
      drain_until_eof flow)
    (fun ~await_conn ->
      let result =
        run_direct (fun () ->
            Monitor.with_monitor_budgets ~caps
              ~budgets:
                {
                  Monitor.acquire = Ox.Duration.seconds 2;
                  unwatch = Ox.Duration.ms 50;
                }
              ~sw ~net ~clock ~process_mgr
              (fun keyboard ->
                take_event ~clock ~within:(Ox.Duration.seconds 2) keyboard))
      in
      await_conn ();
      match result with
      | Ox.Exit.Success (Some _) -> ()
      | Ox.Exit.Success None ->
          Alcotest.fail
            "spoofed NameOwnerChanged: no key event before the deadline"
      | Ox.Exit.Failure cause ->
          Alcotest.failf "spoofed NameOwnerChanged took down watch %s"
            (show_cause cause))

let test_production_budget_is_ten_seconds () =
  Alcotest.(check int)
    "acquire ms" 10_000
    (Ox.Duration.to_ms Monitor.production_budgets.acquire);
  Alcotest.(check int)
    "unwatch ms" 250
    (Ox.Duration.to_ms Monitor.production_budgets.unwatch)

let test_auth_line_limits ~(caps : < Ox_caps.Cap.env ; .. >) () =
  Pie_test_runtime.run ~caps @@ fun _env ->
  let try_line n =
    let src = Eio.Flow.string_source (String.make n 'A' ^ "\r\n") in
    String.length (Monitor.read_crlf_line src)
  in
  Alcotest.(check int) "4095 accepted" 4095 (try_line 4095);
  Alcotest.(check int) "4096 accepted" 4096 (try_line 4096);
  expect_unsupported (fun () -> ignore (try_line 4097)) "4KiB"

let test_noncanonical_boolean_is_malformed () =
  let body = key_event_body () in
  let b = Bytes.of_string body in
  Bytes.set_int32_le b 0 2l;
  let msg =
    {
      Monitor.msg_type = 4;
      serial = 1;
      reply_serial = None;
      path = Some Monitor.a11y_path;
      interface = Some Monitor.a11y_interface;
      member = Some "KeyEvent";
      error_name = None;
      sender = Some a11y_owner;
      destination = None;
      body_signature = Some "buuuq";
      body = b;
    }
  in
  expect_unsupported
    (fun () -> ignore (Monitor.parse_key_event ~owner:a11y_owner msg))
    "boolean"

let test_short_key_event_body_is_malformed () =
  let msg =
    {
      Monitor.msg_type = 4;
      serial = 1;
      reply_serial = None;
      path = Some Monitor.a11y_path;
      interface = Some Monitor.a11y_interface;
      member = Some "KeyEvent";
      error_name = None;
      sender = Some a11y_owner;
      destination = None;
      body_signature = Some "buuuq";
      body = Bytes.create 17;
    }
  in
  expect_unsupported
    (fun () -> ignore (Monitor.parse_key_event ~owner:a11y_owner msg))
    "length"

let key_event_msg signature =
  {
    Monitor.msg_type = 4;
    serial = 1;
    reply_serial = None;
    path = Some Monitor.a11y_path;
    interface = Some Monitor.a11y_interface;
    member = Some "KeyEvent";
    error_name = None;
    sender = Some a11y_owner;
    destination = None;
    body_signature = signature;
    body = Bytes.of_string (key_event_body ());
  }

let test_keyevent_missing_signature () =
  expect_unsupported
    (fun () ->
      ignore (Monitor.parse_key_event ~owner:a11y_owner (key_event_msg None)))
    "signature"

let test_keyevent_wrong_signature () =
  expect_unsupported
    (fun () ->
      ignore
        (Monitor.parse_key_event ~owner:a11y_owner (key_event_msg (Some "s"))))
    "signature"

let noc_msg signature =
  {
    Monitor.msg_type = 4;
    serial = 1;
    reply_serial = None;
    path = Some Monitor.dbus_path;
    interface = Some Monitor.dbus_interface;
    member = Some "NameOwnerChanged";
    error_name = None;
    sender = Some Monitor.dbus_service;
    destination = None;
    body_signature = signature;
    body =
      Bytes.of_string
        (noc_body ~name:Monitor.a11y_service ~old_owner:a11y_owner
           ~new_owner:":1.99");
  }

let test_noc_missing_signature () =
  let pin =
    { Monitor.a11y_owner = Some a11y_owner; bus_unique = None; lost = false }
  in
  expect_unsupported
    (fun () -> Monitor.handle_name_owner_changed pin (noc_msg None))
    "signature"

let test_noc_wrong_signature () =
  let pin =
    { Monitor.a11y_owner = Some a11y_owner; bus_unique = None; lost = false }
  in
  expect_unsupported
    (fun () -> Monitor.handle_name_owner_changed pin (noc_msg (Some "s")))
    "signature"

let test_auth_ok_grammar () =
  Monitor.accept_auth_ok_line "OK 0123456789abcdef0123456789abcdef";
  expect_unsupported (fun () -> Monitor.accept_auth_ok_line "OKgarbage") "auth";
  expect_unsupported
    (fun () ->
      Monitor.accept_auth_ok_line "OK 0123456789abcdef0123456789abcdeX")
    "auth";
  expect_unsupported (fun () -> Monitor.accept_auth_ok_line "REJECTED") "auth"

let test_duplicate_header_field () =
  let fields = Buffer.create 32 in
  Monitor.add_header_field fields 5 "u" (fun buf -> Monitor.put_u32 buf 1);
  Monitor.add_header_field fields 5 "u" (fun buf -> Monitor.put_u32 buf 2);
  let fields_s = Buffer.contents fields in
  let header = Buffer.create 32 in
  Buffer.add_char header 'l';
  Buffer.add_char header (Char.chr 2);
  Buffer.add_char header '\000';
  Buffer.add_char header (Char.chr 1);
  Monitor.put_u32 header 0;
  Monitor.put_u32 header 1;
  Monitor.put_u32 header (String.length fields_s);
  Buffer.add_string header fields_s;
  Monitor.pad_buffer header 8;
  expect_unsupported
    (fun () ->
      ignore
        (Monitor.parse_message_bytes (Bytes.of_string (Buffer.contents header))))
    "duplicate"

let test_path_requires_object_signature () =
  let fields = Buffer.create 32 in
  Monitor.add_header_field fields 1 "s" (fun buf ->
      Monitor.add_dbus_string buf "/bad");
  let fields_s = Buffer.contents fields in
  let header = Buffer.create 32 in
  Buffer.add_char header 'l';
  Buffer.add_char header (Char.chr 1);
  Buffer.add_char header '\000';
  Buffer.add_char header (Char.chr 1);
  Monitor.put_u32 header 0;
  Monitor.put_u32 header 1;
  Monitor.put_u32 header (String.length fields_s);
  Buffer.add_string header fields_s;
  Monitor.pad_buffer header 8;
  expect_unsupported
    (fun () ->
      ignore
        (Monitor.parse_message_bytes (Bytes.of_string (Buffer.contents header))))
    "signature"

let test_forged_get_name_owner_sender
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  with_sandbox ~caps @@ fun dir ->
  Pie_test_runtime.run ~caps @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  with_fake ~caps ~sw ~net ~clock ~dir
    (fun flow ->
      ignore (read_auth_line flow);
      Monitor.write flow "OK 0123456789abcdef0123456789abcdef\r\n";
      ignore (Monitor.read_crlf_line flow);
      let rec loop serial =
        let msg = Monitor.read_message flow in
        match msg.Monitor.member with
        | Some "GetNameOwner" ->
            let requested =
              try Monitor.parse_string_body msg.body
              with _ -> Monitor.a11y_service
            in
            if requested = Monitor.a11y_service then
              reply_string flow ~serial ~reply_serial:msg.serial ~sender:":1.99"
                a11y_owner
            else (
              reply_member flow ~server_serial:serial msg;
              loop (serial + 1))
        | _ ->
            reply_member flow ~server_serial:serial msg;
            loop (serial + 1)
      in
      try loop 1 with End_of_file -> ())
    (fun ~await_conn ->
      let result =
        run_direct (fun () ->
            ignore (Monitor.connect_and_watch ~caps ~sw ~net);
            Ok ())
      in
      await_conn ();
      match result with
      | Ox.Exit.Failure cause ->
          let text =
            match
              List.find_map
                (function
                  | Monitor.Unsupported_input msg -> Some msg | _ -> None)
                (defects cause)
            with
            | Some msg -> msg
            | None -> show_cause cause
          in
          if String.length text < 6 then Alcotest.fail text;
          let needles = [ "sender"; "mismatch" ] in
          List.iter
            (fun needle ->
              if not (contains ~needle text) then
                Alcotest.failf "expected %S in %s" needle text)
            needles
      | Ox.Exit.Success _ ->
          Alcotest.fail "forged GetNameOwner sender was accepted")

let wait_exists path n =
  let rec loop i =
    if Sys.file_exists path then ()
    else if i <= 0 then failwith "dbus-daemon socket missing"
    else (
      Unix.sleepf 0.05;
      loop (i - 1))
  in
  loop n

let wait_reply flow serial =
  let rec loop () =
    let msg = Monitor.read_message flow in
    match (msg.Monitor.msg_type, msg.reply_serial) with
    | 2, Some reply when reply = serial -> msg
    | 3, Some reply when reply = serial ->
        failwith
          ("bus method error "
          ^ Option.value ~default:"<unknown>" msg.error_name)
    | _ -> loop ()
  in
  loop ()

let test_isolated_dbus_daemon_optional
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.tmp
       ; .. >) () =
  match
    Ox_caps.Cap_unix.system ~caps "command -v dbus-daemon >/dev/null 2>&1"
  with
  | Unix.WEXITED 0 ->
      with_sandbox ~caps @@ fun dir ->
      let path = Filename.concat dir "bus" in
      let addr = "unix:path=" ^ path in
      let pid =
        Ox_caps.Cap_unix.create_process ~caps "dbus-daemon"
          [| "dbus-daemon"; "--session"; "--nofork"; "--address=" ^ addr |]
          Unix.stdin Unix.stdout Unix.stderr
      in
      Fun.protect
        ~finally:(fun () ->
          (try Ox_caps.Cap_unix.kill ~caps pid Sys.sigterm with _ -> ());
          ignore (Unix.waitpid [] pid))
        (fun () ->
          wait_exists path 40;
          Ox_caps.Cap_unix.putenv ~caps "DBUS_SESSION_BUS_ADDRESS" addr;
          Pie_test_runtime.run ~caps @@ fun env ->
          Eio.Switch.run @@ fun sw ->
          let net = Eio.Stdenv.net env in
          let clock = Eio.Stdenv.clock env in
          let process_mgr = Eio.Stdenv.process_mgr env in
          let named, resolve_named = Eio.Promise.create () in
          let release, resolve_release = Eio.Promise.create () in
          let peer_done, resolve_peer_done = Eio.Promise.create () in
          Eio.Fiber.fork ~sw (fun () ->
              Fun.protect
                ~finally:(fun () ->
                  try Eio.Promise.resolve resolve_peer_done () with _ -> ())
                (fun () ->
                  let flow = Eio.Net.connect ~sw net (`Unix path) in
                  Fun.protect
                    ~finally:(fun () -> try Eio.Flow.close flow with _ -> ())
                    (fun () ->
                      let uid_s = string_of_int (Unix.getuid ()) in
                      let uid =
                        String.concat ""
                          (List.init (String.length uid_s) (fun i ->
                               Printf.sprintf "%02x" (Char.code uid_s.[i])))
                      in
                      Monitor.write flow ("\000AUTH EXTERNAL " ^ uid ^ "\r\n");
                      ignore (Monitor.read_crlf_line flow);
                      Monitor.write flow "BEGIN\r\n";
                      Monitor.write flow
                        (Monitor.make_method_call ~serial:1
                           ~destination:Monitor.dbus_service
                           ~path:Monitor.dbus_path
                           ~interface:Monitor.dbus_interface ~member:"Hello" ());
                      let hello = wait_reply flow 1 in
                      let unique = Monitor.parse_string_body hello.body in
                      let flags = Buffer.create 8 in
                      Monitor.add_dbus_string flags Monitor.a11y_service;
                      Monitor.pad_buffer flags 4;
                      Monitor.put_u32 flags 0;
                      Monitor.write flow
                        (Monitor.make_method_call ~serial:2
                           ~destination:Monitor.dbus_service
                           ~path:Monitor.dbus_path
                           ~interface:Monitor.dbus_interface
                           ~member:"RequestName" ~signature:"su"
                           ~body:(Buffer.contents flags) ());
                      let owned = wait_reply flow 2 in
                      let reply_code = Monitor.get_u32 owned.body 0 in
                      if reply_code <> 1 && reply_code <> 4 then
                        failwith "RequestName did not take primary ownership";
                      Eio.Promise.resolve resolve_named unique;
                      let rec serve next_serial =
                        let msg = Monitor.read_message flow in
                        if msg.Monitor.member = Some "WatchKeyboard" then (
                          let destination =
                            match msg.sender with
                            | Some s -> s
                            | None -> failwith "WatchKeyboard missing sender"
                          in
                          Monitor.write flow
                            (make_method_return ~serial:next_serial
                               ~reply_serial:msg.serial ~sender:unique
                               ~destination ());
                          Monitor.write flow
                            (make_signal ~sender:unique ~path:Monitor.a11y_path
                               ~interface:Monitor.a11y_interface
                               ~member:"KeyEvent" ~signature:"buuuq"
                               ~body:(key_event_body ()));
                          Eio.Promise.await release)
                        else serve next_serial
                      in
                      serve 3)));
          let unique = Eio.Promise.await named in
          let result =
            Fun.protect
              ~finally:(fun () ->
                (try Eio.Promise.resolve resolve_release () with _ -> ());
                Eio.Promise.await peer_done)
              (fun () ->
                run_direct (fun () ->
                    Monitor.with_monitor_budgets ~caps
                      ~budgets:
                        {
                          Monitor.acquire = Ox.Duration.seconds 2;
                          unwatch = Ox.Duration.ms 50;
                        }
                      ~sw ~net ~clock ~process_mgr
                      (fun keyboard ->
                        take_event ~clock ~within:(Ox.Duration.seconds 2)
                          keyboard)))
          in
          match result with
          | Ox.Exit.Success (Some event) ->
              Alcotest.(check bool) "unique name" true (unique.[0] = ':');
              Alcotest.(check int) "keysym" 65470 event.Monitor.keysym
          | Ox.Exit.Success None ->
              Alcotest.fail
                "private dbus-daemon: no key event before the deadline"
          | Ox.Exit.Failure cause ->
              Alcotest.failf "private dbus-daemon handshake failed %s"
                (show_cause cause))
  | _ -> ()

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.signal
       ; Ox_caps.Cap.tmp
       ; .. >) =
  isolate_process_env ~caps ();
  Alcotest.run "keyboard-bus-boundary"
    [
      ( "address",
        [
          Alcotest.test_case "path plus guid metadata" `Quick
            test_address_path_and_guid;
          Alcotest.test_case "abstract is not a filesystem path" `Quick
            test_address_abstract_not_filesystem;
          Alcotest.test_case "percent-decode after comma split" `Quick
            test_address_escaped_comma;
          Alcotest.test_case "duplicate path is an error" `Quick
            test_address_duplicate_path;
          Alcotest.test_case "path and abstract conflict" `Quick
            test_address_conflict;
          Alcotest.test_case "unsupported transport" `Quick
            test_address_unsupported_transport;
          Alcotest.test_case "unsupported option" `Quick
            test_address_unsupported_option;
          Alcotest.test_case "invalid percent-encoding" `Quick
            test_address_invalid_percent;
          Alcotest.test_case "abstract sockaddr has leading NUL" `Quick
            (test_abstract_sockaddr_leading_nul ~caps);
        ] );
      ( "framing",
        [
          Alcotest.test_case "unsupported endianness" `Quick
            test_parse_unsupported_endian;
          Alcotest.test_case "oversize length rejected before extra alloc"
            `Quick test_parse_oversize_before_alloc;
          Alcotest.test_case "truncated string" `Quick test_truncated_string;
          Alcotest.test_case "AUTH line 4095/4096/4097" `Quick
            (test_auth_line_limits ~caps);
          Alcotest.test_case "noncanonical boolean" `Quick
            test_noncanonical_boolean_is_malformed;
          Alcotest.test_case "short KeyEvent body" `Quick
            test_short_key_event_body_is_malformed;
          Alcotest.test_case "KeyEvent missing signature" `Quick
            test_keyevent_missing_signature;
          Alcotest.test_case "KeyEvent wrong signature" `Quick
            test_keyevent_wrong_signature;
          Alcotest.test_case "NameOwnerChanged missing signature" `Quick
            test_noc_missing_signature;
          Alcotest.test_case "NameOwnerChanged wrong signature" `Quick
            test_noc_wrong_signature;
          Alcotest.test_case "AUTH OK requires space and hex GUID" `Quick
            test_auth_ok_grammar;
          Alcotest.test_case "duplicate header field" `Quick
            test_duplicate_header_field;
          Alcotest.test_case "PATH requires object signature" `Quick
            test_path_requires_object_signature;
          Alcotest.test_case "production budgets" `Quick
            test_production_budget_is_ten_seconds;
        ] );
      ( "timeouts",
        [
          Alcotest.test_case "stalled setup times out and closes" `Quick
            (test_stalled_setup_times_out ~caps);
          Alcotest.test_case "stop is a clean interruption" `Quick
            (test_stop_is_clean_interruption ~caps);
          Alcotest.test_case "unwatch budget still closes" `Quick
            (test_unwatch_budget_still_closes ~caps);
        ] );
      ( "identity",
        [
          Alcotest.test_case "wrong sender is not a key event" `Quick
            (test_wrong_sender_is_ignored ~caps);
          Alcotest.test_case "pinned owner key event is accepted" `Quick
            (test_expected_sender_is_accepted ~caps);
          Alcotest.test_case "owner change is input loss" `Quick
            (test_owner_change_is_input_lost ~caps);
          Alcotest.test_case "owner loss during setup is input loss" `Quick
            (test_owner_loss_during_setup ~caps);
          Alcotest.test_case "NameOwnerChanged from non-bus is ignored" `Quick
            (test_noc_wrong_sender_ignored ~caps);
          Alcotest.test_case "forged GetNameOwner sender is rejected" `Quick
            (test_forged_get_name_owner_sender ~caps);
        ] );
      ( "isolated-bus",
        [
          Alcotest.test_case "private dbus-daemon Hello/GetNameOwner/Watch"
            `Quick
            (test_isolated_dbus_daemon_optional ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
