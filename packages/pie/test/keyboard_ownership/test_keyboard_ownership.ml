(* Isolated PIE021 keyboard setup/input-loss ownership.
   Private Unix D-Bus fake only. No real session bus, keystrokes, or credentials. *)

module Monitor = Pie.Keyboard.Monitor

let rec rm_rf path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Sys.readdir path with
      | names ->
          Array.iter
            (fun name ->
              if name <> "." && name <> ".." then
                rm_rf (Filename.concat path name))
            names
      | exception Sys_error _ -> ());
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let fd_count () =
  let dir = Unix.opendir "/proc/self/fd" in
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

let contains ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec scan i =
    i <= haystack_len - needle_len
    && (String.sub haystack i needle_len = needle || scan (i + 1))
  in
  needle_len = 0 || scan 0

let isolate_process_env () =
  let root = Filename.temp_file "pie-keyboard-owner-home" "" in
  Sys.remove root;
  Unix.mkdir root 0o700;
  let mkdir_env key rel =
    let path = Filename.concat root rel in
    Unix.mkdir path 0o700;
    Unix.putenv key path;
    path
  in
  Unix.putenv "HOME" root;
  ignore (mkdir_env "XDG_CONFIG_HOME" "config");
  ignore (mkdir_env "XDG_DATA_HOME" "data");
  let runtime = mkdir_env "XDG_RUNTIME_DIR" "runtime" in
  Unix.putenv "TMPDIR" root;
  Unix.putenv "TMP" root;
  Unix.putenv "DBUS_SESSION_BUS_ADDRESS"
    ("unix:path=" ^ Filename.concat runtime "no-session-bus")

let with_sandbox f =
  let dir = Filename.temp_file "pie-keyboard-owner" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let show_cause cause =
  Format.asprintf "%a"
    (Eta.Cause.pp (fun ppf _ -> Format.pp_print_string ppf "<err>"))
    cause

let rec defects = function
  | Eta.Cause.Die (exn, _) -> [ exn ]
  | Eta.Cause.Finalizer cause -> defects cause
  | Eta.Cause.Suppressed { primary; finalizer } ->
      defects primary @ defects finalizer
  | Eta.Cause.Concurrent causes -> List.concat_map defects causes
  | Eta.Cause.Fail _ | Eta.Cause.Interrupt _ -> []

let rec failures = function
  | Eta.Cause.Fail error -> [ error ]
  | Eta.Cause.Finalizer cause -> failures cause
  | Eta.Cause.Suppressed { primary; finalizer } ->
      failures primary @ failures finalizer
  | Eta.Cause.Concurrent causes -> List.concat_map failures causes
  | Eta.Cause.Die _ | Eta.Cause.Interrupt _ -> []

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
  let first =
    match Eio.Flow.single_read flow cs with
    | 0 -> raise End_of_file
    | _ -> Cstruct.get_char cs 0
    | exception End_of_file -> raise End_of_file
  in
  let rest = Monitor.read_crlf_line flow in
  if first = '\000' then rest else String.make 1 first ^ rest

let make_method_return ~serial ~reply_serial ?(sender = Monitor.dbus_service)
    ?signature ?(body = "") () =
  let fields = Buffer.create 64 in
  Monitor.add_header_field fields 5 "u" (fun buf ->
      Monitor.put_u32 buf reply_serial);
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

let make_error ~serial ~reply_serial ~name =
  let fields = Buffer.create 64 in
  Monitor.add_string_field fields 4 name;
  Monitor.add_header_field fields 5 "u" (fun buf ->
      Monitor.put_u32 buf reply_serial);
  Monitor.add_string_field fields 7 Monitor.dbus_service;
  let fields_s = Buffer.contents fields in
  let header = Buffer.create 64 in
  Buffer.add_char header 'l';
  Buffer.add_char header (Char.chr 3);
  Buffer.add_char header '\000';
  Buffer.add_char header (Char.chr 1);
  Monitor.put_u32 header 0;
  Monitor.put_u32 header serial;
  Monitor.put_u32 header (String.length fields_s);
  Buffer.add_string header fields_s;
  Monitor.pad_buffer header 8;
  Buffer.contents header

let dbus_error_name = "org.freedesktop.DBus.Error.Failed"
let auth_ok = "OK 0123456789abcdef0123456789abcdef\r\n"

type park_stage = [ `Auth | `Hello | `Add_match | `Watch ]
type after_watch = [ `Eof | `Malformed | `Hold ]

type script =
  | Reject_auth
  | Fail_hello
  | Fail_watch
  | Park of park_stage
  | Watch_then of after_watch

let auth_and_begin flow =
  ignore (read_auth_line flow);
  Monitor.write flow auth_ok;
  ignore (Monitor.read_crlf_line flow)

let reply_error flow ~serial ~reply_serial =
  Monitor.write flow (make_error ~serial ~reply_serial ~name:dbus_error_name)

let hello_unique = ":1.1"
let bus_unique = ":1.0"
let a11y_owner = ":1.42"

let reply_string flow ~serial ~reply_serial ?(sender = Monitor.dbus_service)
    value =
  Monitor.write flow
    (make_method_return ~serial ~reply_serial ~sender ~signature:"s"
       ~body:(Monitor.dbus_string_body value)
       ())

let reply_ok flow ~serial ~reply_serial ~sender =
  Monitor.write flow (make_method_return ~serial ~reply_serial ~sender ())

let reply_member flow ~server_serial msg =
  match msg.Monitor.member with
  | Some "Hello" ->
      reply_string flow ~serial:server_serial ~reply_serial:msg.serial
        hello_unique
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

let handle_script ~on_stage script flow =
  let signal stage =
    match on_stage with None -> () | Some resolve -> resolve stage
  in
  let rec pump ~on_member server_serial =
    let msg = Monitor.read_message flow in
    match on_member msg with
    | `Park stage -> signal stage
    | `Error -> reply_error flow ~serial:server_serial ~reply_serial:msg.serial
    | `Continue ->
        reply_member flow ~server_serial msg;
        pump ~on_member (server_serial + 1)
    | `Watched -> reply_member flow ~server_serial msg
  in
  match script with
  | Reject_auth ->
      ignore (read_auth_line flow);
      Monitor.write flow "REJECTED\r\n"
  | Fail_hello ->
      auth_and_begin flow;
      ignore
        (pump
           ~on_member:(fun msg ->
             if msg.Monitor.member = Some "Hello" then `Error else `Continue)
           1)
  | Fail_watch ->
      auth_and_begin flow;
      ignore
        (pump
           ~on_member:(fun msg ->
             if msg.Monitor.member = Some "WatchKeyboard" then `Error
             else `Continue)
           1)
  | Park `Auth ->
      ignore (read_auth_line flow);
      signal `Auth;
      drain_until_eof flow
  | Park `Hello ->
      auth_and_begin flow;
      ignore
        (pump
           ~on_member:(fun msg ->
             if msg.Monitor.member = Some "Hello" then `Park `Hello
             else `Continue)
           1);
      drain_until_eof flow
  | Park `Add_match ->
      auth_and_begin flow;
      ignore
        (pump
           ~on_member:(fun msg ->
             if msg.Monitor.member = Some "AddMatch" then `Park `Add_match
             else `Continue)
           1);
      drain_until_eof flow
  | Park `Watch ->
      auth_and_begin flow;
      ignore
        (pump
           ~on_member:(fun msg ->
             if msg.Monitor.member = Some "WatchKeyboard" then `Park `Watch
             else `Continue)
           1);
      drain_until_eof flow
  | Watch_then after -> (
      auth_and_begin flow;
      ignore
        (pump
           ~on_member:(fun msg ->
             if msg.Monitor.member = Some "WatchKeyboard" then `Watched
             else `Continue)
           1);
      match after with
      | `Eof -> ()
      | `Malformed ->
          Monitor.write flow (String.make 16 'X');
          drain_until_eof flow
      | `Hold -> drain_until_eof flow)

let with_fake_bus ~sw ~net ~clock ~dir ~count script ?on_stage f =
  let path = Filename.concat dir "bus" in
  (try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  let listening =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:128 net (`Unix path)
  in
  Unix.putenv "DBUS_SESSION_BUS_ADDRESS" ("unix:path=" ^ path);
  let closed = Eio.Stream.create count in
  let remaining = ref count in
  Eio.Fiber.fork ~sw (fun () ->
      let rec loop () =
        if !remaining <= 0 then ()
        else (
          decr remaining;
          (try
             Eio.Switch.run @@ fun conn_sw ->
             let flow, _ = Eio.Net.accept ~sw:conn_sw listening in
             Fun.protect
               ~finally:(fun () -> try Eio.Flow.close flow with _ -> ())
               (fun () ->
                 try handle_script ~on_stage script flow with
                 | End_of_file -> ()
                 | Eio.Cancel.Cancelled _ -> ())
           with
          | End_of_file -> ()
          | Eio.Cancel.Cancelled _ -> ());
          Eio.Stream.add closed ();
          loop ())
      in
      loop ());
  Eio.Fiber.yield ();
  f ~await_conn:(fun () ->
      try Eio.Time.with_timeout_exn clock 3.0 (fun () -> Eio.Stream.take closed)
      with Eio.Time.Timeout ->
        Alcotest.fail
          "server connection scope did not close (client likely leaked the \
           flow)")

let run_direct operation = Eta.Scope.run (fun _ -> operation ())

let expect_die ~label ~needle result =
  match result with
  | Eta.Exit.Success _ ->
      Alcotest.fail (label ^ ": expected defect, got success")
  | Eta.Exit.Failure cause ->
      let dies = defects cause in
      if dies = [] then
        Alcotest.failf "%s: expected defect, got %s" label (show_cause cause);
      let matched =
        List.exists (fun die -> contains ~needle (Printexc.to_string die)) dies
      in
      if not matched then
        Alcotest.failf "%s: expected %S in %s" label needle (show_cause cause)

let expect_input_lost ~label result =
  match result with
  | Eta.Exit.Success _ ->
      Alcotest.fail (label ^ ": consumer succeeded after input loss")
  | Eta.Exit.Failure cause -> (
      match failures cause with
      | [ `Harness_timeout ] ->
          Alcotest.fail
            (label ^ ": consumer hung; background treated EOF as success")
      | _ ->
          if Eta.Cause.is_interrupt_only cause then
            Alcotest.fail (label ^ ": input loss became interruption");
          let dies = defects cause in
          let hit =
            List.exists
              (fun die ->
                match die with
                | Monitor.Input_lost -> true
                | Failure msg ->
                    contains ~needle:"endian" msg
                    || contains ~needle:"D-Bus" msg
                | _ -> false)
              dies
          in
          if not hit then
            Alcotest.failf "%s: expected input-loss/malformed defect, got %s"
              label (show_cause cause))

let test_rejected_auth_fd_flat () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  with_fake_bus ~sw ~net ~clock ~dir ~count:100 Reject_auth (fun ~await_conn ->
      let baseline = fd_count () in
      for i = 1 to 100 do
        let result =
          run_direct (fun () ->
              Ok (ignore (Monitor.connect_and_watch ~sw ~net)))
        in
        expect_die ~label:(Printf.sprintf "auth %d" i) ~needle:"auth" result;
        await_conn ();
        let after = fd_count () in
        if after > baseline then
          Alcotest.failf
            "fd grew after rejected AUTH %d: baseline=%d after=%d (no GC)" i
            baseline after
      done;
      Alcotest.(check int)
        "rejected AUTH fd count flat on long-lived switch" baseline
        (fd_count ()))

let test_failed_hello_closes () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 Fail_hello (fun ~await_conn ->
      let baseline = fd_count () in
      let result =
        run_direct (fun () -> Ok (ignore (Monitor.connect_and_watch ~sw ~net)))
      in
      expect_die ~label:"hello" ~needle:"Hello" result;
      await_conn ();
      Alcotest.(check int)
        "hello failure closed the flow" baseline (fd_count ()))

let test_failed_watch_closes () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 Fail_watch (fun ~await_conn ->
      let baseline = fd_count () in
      let result =
        run_direct (fun () -> Ok (ignore (Monitor.connect_and_watch ~sw ~net)))
      in
      expect_die ~label:"watch" ~needle:"WatchKeyboard" result;
      await_conn ();
      Alcotest.(check int)
        "watch failure closed the flow" baseline (fd_count ()))

let rec exn_texts exn =
  match exn with
  | Eio.Exn.Multiple pairs ->
      List.concat_map (fun (e, _) -> Printexc.to_string e :: exn_texts e) pairs
  | Eio.Cancel.Cancelled inner -> Printexc.to_string exn :: exn_texts inner
  | e -> [ Printexc.to_string e ]

let cause_texts cause =
  List.concat_map
    (fun die -> Printexc.to_string die :: exn_texts die)
    (defects cause)
  |> String.concat "\n"

let expect_combined ~label ~needles result =
  match result with
  | Eta.Exit.Success _ -> Alcotest.fail (label ^ ": expected combined failure")
  | Eta.Exit.Failure cause ->
      let text = cause_texts cause ^ "\n" ^ show_cause cause in
      List.iter
        (fun needle ->
          if not (contains ~needle text) then
            Alcotest.failf "%s: missing %S in %s" label needle text)
        needles

let close_then_raise flow =
  Eio.Cancel.protect (fun () -> Eio.Flow.close flow);
  failwith "synthetic-close"

let test_auth_reject_combines_close_failure () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 Reject_auth (fun ~await_conn ->
      let baseline = fd_count () in
      let result =
        run_direct (fun () ->
            Ok
              (ignore
                 (Monitor.connect_and_watch_with ~close:close_then_raise ~sw
                    ~net)))
      in
      expect_combined ~label:"auth+close"
        ~needles:[ "auth"; "synthetic-close" ]
        result;
      await_conn ();
      Alcotest.(check int)
        "synthetic close still retired the fd" baseline (fd_count ()))

let test_auth_cancel_combines_close_failure () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let seen, resolve_seen = Eio.Promise.create () in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 (Park `Auth)
    ~on_stage:(fun _ -> Eio.Promise.resolve resolve_seen ())
    (fun ~await_conn ->
      let baseline = fd_count () in
      let result =
        run_direct (fun () ->
            Ok
              (Eio.Cancel.sub (fun cc ->
                   Eio.Fiber.fork ~sw (fun () ->
                       Eio.Promise.await seen;
                       Eio.Cancel.cancel cc (Failure "stage-cancel"));
                   ignore
                     (Monitor.connect_and_watch_with ~close:close_then_raise ~sw
                        ~net))))
      in
      expect_combined ~label:"cancel+close"
        ~needles:[ "synthetic-close"; "stage-cancel" ]
        result;
      await_conn ();
      Alcotest.(check int)
        "cancelled synthetic close retired the fd" baseline (fd_count ()))

let test_unwatch_cancel_combines_close_failure () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 (Watch_then `Hold)
    (fun ~await_conn ->
      let baseline = fd_count () in
      let result =
        run_direct (fun () ->
            Ok
              (let flow = Monitor.connect_and_watch ~sw ~net in
               Eio.Cancel.sub (fun cc ->
                   Eio.Cancel.cancel cc (Failure "unwatch-cancel");
                   Monitor.unwatch_and_close_with ~close:close_then_raise flow)))
      in
      expect_combined ~label:"unwatch-cancel+close"
        ~needles:[ "synthetic-close"; "unwatch-cancel" ]
        result;
      await_conn ();
      Alcotest.(check int)
        "unwatch synthetic close retired the fd" baseline (fd_count ()))

let cancel_at_stage stage () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let seen, resolve_seen = Eio.Promise.create () in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 (Park stage)
    ~on_stage:(fun reached ->
      if reached = stage then Eio.Promise.resolve resolve_seen ())
    (fun ~await_conn ->
      let baseline = fd_count () in
      let operation () =
        Eta.Concurrent.race ~sw
          [
            (fun () ->
              ignore (Monitor.connect_and_watch ~sw ~net);
              Ok `Setup_finished);
            (fun () ->
              Eio.Promise.await seen;
              Ok `Cancelled_at_stage);
          ]
      in
      (match run_direct operation with
      | Eta.Exit.Success `Cancelled_at_stage -> ()
      | Eta.Exit.Success `Setup_finished ->
          Alcotest.fail "setup completed before tagged cancellation"
      | Eta.Exit.Failure cause when Eta.Cause.is_interrupt_only cause ->
          Alcotest.fail
            "Eta.race used a failed/interrupt branch as cancellation; \
             the              tagged success branch must win"
      | Eta.Exit.Failure cause ->
          Alcotest.failf "unexpected race failure: %s" (show_cause cause));
      await_conn ();
      Alcotest.(check int)
        "cancelled setup closed the flow" baseline (fd_count ()))

let test_failed_unwatch_write_still_closes () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 (Watch_then `Eof)
    (fun ~await_conn ->
      let result =
        run_direct (fun () ->
            let flow = Monitor.connect_and_watch ~sw ~net in
            await_conn ();
            let before = fd_count () in
            Monitor.unwatch_and_close flow;
            let after = fd_count () in
            if after >= before then
              Alcotest.failf
                "failed Unwatch write skipped close: fd before=%d after=%d"
                before after;
            Ok ())
      in
      match result with
      | Eta.Exit.Success () -> ()
      | Eta.Exit.Failure cause ->
          Alcotest.failf "unwatch/close raised %s" (show_cause cause))

let test_unexpected_eof_fails_consumer () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 (Watch_then `Eof)
    (fun ~await_conn ->
      let operation () =
        Eta.Timeout.timeout ~clock (Eta.Duration.seconds 2) (fun () ->
            Monitor.with_monitor ~sw ~net ~clock ~process_mgr (fun _keyboard ->
                Eio.Fiber.await_cancel ()))
        |> Result.map_error (function
          | `Timeout -> `Harness_timeout
          | error -> error)
      in
      let result = run_direct operation in
      await_conn ();
      expect_input_lost ~label:"unexpected EOF" result;
      match result with
      | Eta.Exit.Failure cause ->
          let hit =
            List.exists
              (fun die ->
                match die with Monitor.Input_lost -> true | _ -> false)
              (defects cause)
          in
          Alcotest.(check bool) "EOF is named Input_lost" true hit
      | Eta.Exit.Success _ -> ())

let test_malformed_packet_fails_consumer () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 (Watch_then `Malformed)
    (fun ~await_conn ->
      let operation () =
        Eta.Timeout.timeout ~clock (Eta.Duration.seconds 2) (fun () ->
            Monitor.with_monitor ~sw ~net ~clock ~process_mgr (fun _keyboard ->
                Eio.Fiber.await_cancel ()))
        |> Result.map_error (function
          | `Timeout -> `Harness_timeout
          | error -> error)
      in
      let result = run_direct operation in
      await_conn ();
      match result with
      | Eta.Exit.Success _ ->
          Alcotest.fail "malformed packet: consumer succeeded"
      | Eta.Exit.Failure cause -> (
          match failures cause with
          | [ `Harness_timeout ] ->
              Alcotest.fail "malformed packet swallowed; consumer hung"
          | _ ->
              let dies = defects cause in
              if dies = [] then
                Alcotest.failf "expected parse defect, got %s"
                  (show_cause cause)))

let test_normal_disconnect_closes () =
  with_sandbox @@ fun dir ->
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let net = Eio.Stdenv.net env in
  let process_mgr = Eio.Stdenv.process_mgr env in
  with_fake_bus ~sw ~net ~clock ~dir ~count:1 (Watch_then `Hold)
    (fun ~await_conn ->
      let baseline = fd_count () in
      let result =
        run_direct (fun () ->
            Monitor.with_monitor ~sw ~net ~clock ~process_mgr (fun _keyboard ->
                Ok ()))
      in
      (match result with
      | Eta.Exit.Success () -> ()
      | Eta.Exit.Failure cause ->
          Alcotest.failf "normal disconnect failed: %s" (show_cause cause));
      await_conn ();
      Alcotest.(check int)
        "normal disconnect retired the flow" baseline (fd_count ()))

let () =
  isolate_process_env ();
  Alcotest.run "keyboard-ownership"
    [
      ( "setup",
        [
          Alcotest.test_case
            "repeated rejected AUTH keeps fds flat on one runtime" `Quick
            test_rejected_auth_fd_flat;
          Alcotest.test_case "failed Hello closes the acquired flow" `Quick
            test_failed_hello_closes;
          Alcotest.test_case "failed WatchKeyboard closes the acquired flow"
            `Quick test_failed_watch_closes;
          Alcotest.test_case
            "rejected AUTH plus close failure keeps both causes" `Quick
            test_auth_reject_combines_close_failure;
        ] );
      ( "cancel",
        [
          Alcotest.test_case "cancel during AUTH closes the flow" `Quick
            (cancel_at_stage `Auth);
          Alcotest.test_case "cancel during Hello closes the flow" `Quick
            (cancel_at_stage `Hello);
          Alcotest.test_case "cancel during AddMatch closes the flow" `Quick
            (cancel_at_stage `Add_match);
          Alcotest.test_case "cancel during WatchKeyboard closes the flow"
            `Quick (cancel_at_stage `Watch);
          Alcotest.test_case "AUTH cancel plus close failure keeps both causes"
            `Quick test_auth_cancel_combines_close_failure;
        ] );
      ( "release",
        [
          Alcotest.test_case "failed Unwatch write still closes" `Quick
            test_failed_unwatch_write_still_closes;
          Alcotest.test_case
            "Unwatch cancel plus close failure keeps both causes" `Quick
            test_unwatch_cancel_combines_close_failure;
          Alcotest.test_case "normal disconnect retires the flow" `Quick
            test_normal_disconnect_closes;
        ] );
      ( "input-loss",
        [
          Alcotest.test_case "unexpected EOF fails the long-lived consumer"
            `Quick test_unexpected_eof_fails_consumer;
          Alcotest.test_case "malformed packet fails the consumer" `Quick
            test_malformed_packet_fails_consumer;
        ] );
    ]
