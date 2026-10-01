let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

let contains hay needle =
  let rec loop i =
    i + String.length needle <= String.length hay
    && (String.sub hay i (String.length needle) = needle || loop (i + 1))
  in
  needle = "" || loop 0

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

let child_count () =
  let self = Unix.getpid () in
  let task_dir = Printf.sprintf "/proc/%d/task" self in
  let tasks =
    try Array.to_list (Sys.readdir task_dir) with Sys_error _ -> []
  in
  List.fold_left
    (fun acc tid ->
      let path = Printf.sprintf "/proc/%d/task/%s/children" self tid in
      match open_in path with
      | exception Sys_error _ -> acc
      | ic ->
          Fun.protect
            ~finally:(fun () -> close_in_noerr ic)
            (fun () ->
              match input_line ic with
              | line ->
                  acc
                  + List.length
                      (List.filter
                         (fun p -> String.trim p <> "")
                         (String.split_on_char ' ' line))
              | exception End_of_file -> acc))
    0 tasks

let with_temp_dir f =
  let dir = Filename.temp_file "pie-clipboard-owner" "" in
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

let write_exec dir name body =
  let path = Filename.concat dir name in
  let oc = open_out path in
  output_string oc ("#!/bin/sh\nset -eu\n" ^ body);
  close_out oc;
  Unix.chmod path 0o700;
  path

let test_no_inspection_exact_stdin previous =
  with_temp_dir @@ fun dir ->
  let inspected = Filename.concat dir "inspected" in
  let stdin_path = Filename.concat dir "stdin" in
  ignore
    (write_exec dir "wl-paste"
       ("touch \"" ^ inspected ^ "\"\nprintf '%s' \"" ^ previous ^ "\"\n"));
  let copy =
    write_exec dir "wl-copy"
      ("cat > \"" ^ stdin_path ^ "\"\nwhile :; do sleep 0.05; done\n")
  in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  match Pie.Wayland.Clipboard.offer owner "offered-text" with
  | Error (`Clipboard_error msg) -> Alcotest.failf "offer failed: %s" msg
  | Ok () ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
      Alcotest.(check bool)
        "did not inspect previous" false
        (Sys.file_exists inspected);
      Alcotest.(check bool)
        "helper received stdin" true
        (Sys.file_exists stdin_path);
      let got = In_channel.with_open_text stdin_path In_channel.input_all in
      Alcotest.(check string) "exact stdin" "offered-text" got

let test_known_spawn_failure_hides_secret () =
  with_temp_dir @@ fun dir ->
  let secret = Filename.concat dir "secret-argv-token-wl-copy" in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:secret ()
  in
  match Pie.Wayland.Clipboard.offer owner "payload" with
  | Ok () -> Alcotest.fail "missing helper succeeded"
  | Error (`Clipboard_error message) ->
      Alcotest.(check bool)
        "no secret path" false
        (contains message "secret-argv-token");
      Alcotest.(check bool)
        "no execve context" false
        (contains message "execve")

let test_copy_early_failure () =
  with_temp_dir @@ fun dir ->
  let copy = Filename.concat dir "missing-wl-copy" in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  match Pie.Wayland.Clipboard.offer owner "payload" with
  | Error (`Clipboard_error _) -> ()
  | Ok () -> Alcotest.fail "early copy failure reported success"

let test_cancellation_at_acquisition () =
  with_temp_dir @@ fun dir ->
  let copy = write_exec dir "wl-copy" "sleep 30\n" in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun owner_sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw:owner_sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  let fds0 = fd_count () in
  let kids0 = child_count () in
  let payload = String.make (1024 * 1024) 'x' in
  (try
     Eio.Switch.run @@ fun offer_sw ->
     Eio.Fiber.fork ~sw:offer_sw (fun () ->
         Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
         raise Exit);
     ignore (Pie.Wayland.Clipboard.offer owner payload);
     Eio.Time.sleep (Eio.Stdenv.clock env) 10.0
   with Exit | Eio.Cancel.Cancelled _ -> ());
  Alcotest.(check int)
    "children settled before cancel returns" kids0 (child_count ());
  Alcotest.(check int) "fds settled before cancel returns" fds0 (fd_count ());
  match Pie.Wayland.Clipboard.offer owner "next" with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "next offer after cancelled call"

let test_slow_consumer_still_alive () =
  with_temp_dir @@ fun dir ->
  let alive = Filename.concat dir "alive" in
  let copy =
    write_exec dir "wl-copy"
      ("cat > /dev/null\ntouch \"" ^ alive ^ "\"\nsleep 2\n")
  in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  match Pie.Wayland.Clipboard.offer owner "payload" with
  | Error _ -> Alcotest.fail "offer failed"
  | Ok () ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.25;
      Alcotest.(check bool)
        "offer still available after 250ms" true (Sys.file_exists alive)

let test_repeated_offers_bounded () =
  with_temp_dir @@ fun dir ->
  let copy =
    write_exec dir "wl-copy" "cat > /dev/null\nwhile :; do sleep 0.05; done\n"
  in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  match Pie.Wayland.Clipboard.offer owner "0" with
  | Error _ -> Alcotest.fail "first offer failed"
  | Ok () ->
      let fds_one = fd_count () in
      let kids_one = child_count () in
      for i = 1 to 99 do
        match Pie.Wayland.Clipboard.offer owner (string_of_int i) with
        | Ok () -> ()
        | Error _ -> Alcotest.fail "offer failed during replacement"
      done;
      let fds_hundred = fd_count () in
      let kids_hundred = child_count () in
      Alcotest.(check int) "fd plateau after warm" fds_one fds_hundred;
      Alcotest.(check int) "process plateau after warm" kids_one kids_hundred

let test_shutdown_settles () =
  with_temp_dir @@ fun dir ->
  let copy =
    write_exec dir "wl-copy" "cat > /dev/null\nwhile :; do sleep 0.05; done\n"
  in
  Pie_test_runtime.run @@ fun env ->
  ( Eio.Switch.run @@ fun sw ->
    let owner =
      Pie.Wayland.Clipboard.create ~caps ~sw
        ~process_mgr:(Eio.Stdenv.process_mgr env)
        ~copy_program:copy ()
    in
    ignore (Pie.Wayland.Clipboard.offer owner "payload") );
  Alcotest.(check bool) "switch settled" true true

let test_settled_before_replacement () =
  with_temp_dir @@ fun dir ->
  let copy =
    write_exec dir "wl-copy" "cat > /dev/null\nwhile :; do sleep 0.05; done\n"
  in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  let offer text = Pie.Wayland.Clipboard.offer owner text in
  (match offer "a" with Ok () -> () | Error _ -> Alcotest.fail "first offer");
  let kids = child_count () in
  let fds = fd_count () in
  (match offer "b" with Ok () -> () | Error _ -> Alcotest.fail "second offer");
  Alcotest.(check int)
    "children settled when replacement returns" kids (child_count ());
  Alcotest.(check int) "fds settled when replacement returns" fds (fd_count ())

let test_natural_exit_releases_pipes () =
  with_temp_dir @@ fun dir ->
  let copy = write_exec dir "wl-copy" "cat > /dev/null\nexit 0\n" in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  let fds_idle = fd_count () in
  let kids_idle = child_count () in
  ignore (Pie.Wayland.Clipboard.offer owner "payload");
  Eio.Time.sleep (Eio.Stdenv.clock env) 0.2;
  Alcotest.(check int) "natural exit fds" fds_idle (fd_count ());
  Alcotest.(check int) "natural exit children" kids_idle (child_count ())

let test_large_stdout_discarded () =
  with_temp_dir @@ fun dir ->
  let copy =
    write_exec dir "wl-copy"
      "dd if=/dev/zero bs=65536 count=128 status=none\n\
       cat > /dev/null\n\
       while :; do sleep 0.05; done\n"
  in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  match Pie.Wayland.Clipboard.offer owner "payload" with
  | Error _ -> Alcotest.fail "large stdout should be discarded"
  | Ok () -> ()

let with_env bindings f =
  let previous =
    List.map (fun (key, _) -> (key, Sys.getenv_opt key)) bindings
  in
  List.iter (fun (key, value) -> Unix.putenv key value) bindings;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (key, old) ->
          Unix.putenv key (match old with None -> "" | Some value -> value))
        previous)
    f

let paste_rules =
  [
    {
      Pie.Input.Text_injection.app_id = None;
      title_regex = None;
      method_ = Paste;
    };
  ]

let setup_paste_path dir ~wtype_body =
  let inspected = Filename.concat dir "inspected" in
  let stdin_path = Filename.concat dir "stdin" in
  let wtype_args = Filename.concat dir "wtype-args" in
  let alive = Filename.concat dir "alive" in
  ignore (write_exec dir "wl-paste" ("touch \"" ^ inspected ^ "\"\n"));
  ignore
    (write_exec dir "niri"
       "printf '%s\n' '{\"id\":1,\"title\":\"t\",\"app_id\":\"app\"}'\n");
  let copy =
    write_exec dir "wl-copy"
      ("cat > \"" ^ stdin_path ^ "\"\ntouch \"" ^ alive
     ^ "\"\nwhile :; do sleep 0.05; done\n")
  in
  ignore (write_exec dir "wtype" wtype_body);
  (inspected, stdin_path, wtype_args, alive, copy)

let run_configured_inject ~dir ~copy env sw text =
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  Pie.Input.Text_injection.inject ~caps ~clock:(Eio.Stdenv.clock env)
    ~clipboard_owner:owner ~text_entry_rules:paste_rules
    ~target_window:
      (Some
         {
           Pie.Niri.id = 1;
           title = Some "t";
           app_id = Some "app";
           pid = None;
           workspace_id = None;
         })
    ~process_mgr:(Eio.Stdenv.process_mgr env)
    text

let test_inject_paste_key_success previous =
  with_temp_dir @@ fun dir ->
  let wtype_args = Filename.concat dir "wtype-args" in
  let inspected, stdin_path, _, alive, copy =
    setup_paste_path dir
      ~wtype_body:("printf '%s\n' \"$*\" >> \"" ^ wtype_args ^ "\"\n")
  in
  let old_path = Option.value ~default:"" (Sys.getenv_opt "PATH") in
  with_env
    [
      ("PATH", dir ^ ":" ^ old_path);
      ("XDG_SESSION_TYPE", "wayland");
      ("WAYLAND_DISPLAY", "pie-fake");
      ("XDG_CURRENT_DESKTOP", "niri");
      ("NIRI_SOCKET", Filename.concat dir "niri.sock");
    ]
  @@ fun () ->
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  match run_configured_inject ~dir ~copy env sw "offered-text" with
  | Error _ -> Alcotest.fail "configured paste inject failed"
  | Ok result ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
      Alcotest.(check bool)
        "did not inspect previous" false
        (Sys.file_exists inspected);
      Alcotest.(check string)
        "exact stdin" "offered-text"
        (In_channel.with_open_text stdin_path In_channel.input_all);
      let keys = In_channel.with_open_text wtype_args In_channel.input_all in
      Alcotest.(check string) "exact one ctrl-v" "-M ctrl -k v -m ctrl\n" keys;
      Alcotest.(check bool)
        "outcome is key attempted" true
        (result.outcome = Pie.Input.Text_injection.Clipboard_key_attempted);
      Alcotest.(check bool) "helper retained" true (Sys.file_exists alive);
      ignore previous

let test_inject_paste_key_fail_retains_helper () =
  with_temp_dir @@ fun dir ->
  let inspected, _, _, alive, copy =
    setup_paste_path dir ~wtype_body:"exit 1\n"
  in
  let old_path = Option.value ~default:"" (Sys.getenv_opt "PATH") in
  with_env
    [
      ("PATH", dir ^ ":" ^ old_path);
      ("XDG_SESSION_TYPE", "wayland");
      ("WAYLAND_DISPLAY", "pie-fake");
      ("XDG_CURRENT_DESKTOP", "niri");
      ("NIRI_SOCKET", Filename.concat dir "niri.sock");
    ]
  @@ fun () ->
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let kids_before = child_count () in
  match run_configured_inject ~dir ~copy env sw "payload" with
  | Ok _ -> Alcotest.fail "key failure reported success"
  | Error (`Wtype_error _) ->
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.1;
      Alcotest.(check bool) "did not inspect" false (Sys.file_exists inspected);
      Alcotest.(check bool)
        "committed helper retained" true (Sys.file_exists alive);
      Alcotest.(check bool)
        "no extra helper" true
        (child_count () - kids_before <= 2)
  | Error _ -> Alcotest.fail "unexpected error shape"

let test_paste_handoff_delay ~cancel () =
  with_temp_dir @@ fun dir ->
  let released = Filename.concat dir "handoff-released" in
  let keys = Filename.concat dir "handoff-keys" in
  let inspected, _, _, alive, copy =
    setup_paste_path dir
      ~wtype_body:
        ("test -f " ^ Filename.quote released ^ " || exit 42\n"
       ^ "printf key > " ^ Filename.quote keys ^ "\n")
  in
  let old_path = Option.value ~default:"" (Sys.getenv_opt "PATH") in
  with_env
    [
      ("PATH", dir ^ ":" ^ old_path);
      ("XDG_SESSION_TYPE", "wayland");
      ("WAYLAND_DISPLAY", "pie-fake");
      ("XDG_CURRENT_DESKTOP", "niri");
      ("NIRI_SOCKET", Filename.concat dir "niri.sock");
    ]
  @@ fun () ->
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let clock = Eio.Stdenv.clock env in
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  let waited = ref [] in
  let virtual_now = ref 0. in
  let module Clock = struct
    type t = unit
    type time = float

    let now () = !virtual_now

    let sleep_until () target =
      let ms = int_of_float (Float.round ((target -. !virtual_now) *. 1000.)) in
      waited := ms :: !waited;
      Alcotest.(check bool)
        "no key before handoff wait" false (Sys.file_exists keys);
      Eio.Time.with_timeout_exn clock 1.0 (fun () ->
          while not (Sys.file_exists alive) do
            Eio.Time.sleep clock 0.001
          done);
      if cancel then Eio.Fiber.await_cancel ();
      virtual_now := target;
      Out_channel.with_open_bin released (fun out ->
          output_string out "released")
  end in
  let fake_clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)) in
  let run () =
    Pie.Input.Text_injection.inject ~caps ~clock:fake_clock
      ~clipboard_owner:owner ~text_entry_rules:paste_rules
      ~target_window:
        (Some
           {
             Pie.Niri.id = 1;
             title = Some "t";
             app_id = Some "app";
             pid = None;
             workspace_id = None;
           })
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      "offered-text"
  in
  if cancel then (
    (match Eio.Time.with_timeout_exn clock 0.1 run with
    | exception Eio.Time.Timeout -> ()
    | _ -> Alcotest.fail "handoff wait should be cancelled");
    Alcotest.(check bool)
      "cancel never sends paste key" false (Sys.file_exists keys))
  else (
    (match run () with
    | Ok result ->
        Alcotest.(check bool)
          "receipt still unconfirmed" true
          (result.outcome = Pie.Input.Text_injection.Clipboard_key_attempted)
    | Error _ -> Alcotest.fail "paste key ran without the handoff interval");
    Alcotest.(check bool) "key sent after interval" true (Sys.file_exists keys));
  Alcotest.(check (list int))
    "one 25ms handoff interval" [ 25 ] (List.rev !waited);
  Alcotest.(check bool)
    "no clipboard inspection" false
    (Sys.file_exists inspected)

let blocking_payload () =
  let max_pipe =
    match
      In_channel.with_open_text "/proc/sys/fs/pipe-max-size"
        In_channel.input_all
    with
    | contents -> int_of_string (String.trim contents)
    | (exception Sys_error _) | (exception Failure _) | (exception End_of_file)
      ->
        1024 * 1024
  in
  String.make (max_pipe + 1) 'x'

let pid_alive pid =
  match Unix.kill pid 0 with () -> true | exception Unix.Unix_error _ -> false

let read_pid path =
  int_of_string
    (String.trim (In_channel.with_open_text path In_channel.input_all))

let test_default_mime_and_ready_before_retire () =
  with_temp_dir @@ fun dir ->
  let args_path = Filename.concat dir "args" in
  let pid_path = Filename.concat dir "owner.pid" in
  let pred_path = Filename.concat dir "predecessor" in
  let q = Filename.quote in
  (* Record the previous pid before reading stdin. The payload is larger than
     the pipe, so [offer] cannot observe readiness — and therefore cannot
     retire the previous helper — until this read starts. *)
  let copy =
    write_exec dir "wl-copy"
      ("printf '%s\n' \"$*\" >> " ^ q args_path ^ "\n" ^ "if [ -f " ^ q pid_path
     ^ " ]; then\n" ^ "  pred=$(cat " ^ q pid_path ^ ")\n"
     ^ "  if kill -0 \"$pred\" 2>/dev/null; then\n"
     ^ "    printf '%s\n' alive > " ^ q pred_path ^ "\n" ^ "  else\n"
     ^ "    printf '%s\n' dead > " ^ q pred_path ^ "\n" ^ "  fi\n" ^ "fi\n"
     ^ "printf '%s\n' \"$$\" > " ^ q pid_path ^ "\n" ^ "cat > /dev/null\n"
     ^ "while :; do sleep 0.05; done\n")
  in
  let payload = blocking_payload () in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  (match Pie.Wayland.Clipboard.offer owner payload with
  | Ok () -> ()
  | Error (`Clipboard_error msg) -> Alcotest.failf "first offer failed: %s" msg);
  let first_pid = read_pid pid_path in
  Alcotest.(check bool) "first helper alive" true (pid_alive first_pid);
  (match Pie.Wayland.Clipboard.offer owner payload with
  | Ok () -> ()
  | Error (`Clipboard_error msg) ->
      Alcotest.failf "replacement offer failed: %s" msg);
  Alcotest.(check string)
    "wl-copy arguments are --foreground only" "--foreground\n--foreground\n"
    (In_channel.with_open_text args_path In_channel.input_all);
  Alcotest.(check string)
    "previous source still alive when the new helper is ready" "alive\n"
    (In_channel.with_open_text pred_path In_channel.input_all);
  Alcotest.(check bool)
    "previous source retired after the new helper was ready" false
    (pid_alive first_pid);
  let second_pid = read_pid pid_path in
  Alcotest.(check bool)
    "replacement helper is a new pid" true (second_pid <> first_pid);
  Alcotest.(check bool)
    "replacement helper still owns the offer" true (pid_alive second_pid)

let test_failed_replacement_keeps_previous_source () =
  with_temp_dir @@ fun dir ->
  let pid_path = Filename.concat dir "owner.pid" in
  let started = Filename.concat dir "started" in
  let q = Filename.quote in
  let copy =
    write_exec dir "wl-copy"
      ("if [ -f " ^ q started ^ " ]; then\n" ^ "  exit 1\n" ^ "fi\n" ^ "touch "
     ^ q started ^ "\n" ^ "printf '%s\n' \"$$\" > " ^ q pid_path ^ "\n"
     ^ "cat > /dev/null\n" ^ "while :; do sleep 0.05; done\n")
  in
  let payload = blocking_payload () in
  Pie_test_runtime.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let owner =
    Pie.Wayland.Clipboard.create ~caps ~sw
      ~process_mgr:(Eio.Stdenv.process_mgr env)
      ~copy_program:copy ()
  in
  (match Pie.Wayland.Clipboard.offer owner payload with
  | Ok () -> ()
  | Error (`Clipboard_error msg) -> Alcotest.failf "first offer failed: %s" msg);
  let first_pid = read_pid pid_path in
  match Pie.Wayland.Clipboard.offer owner payload with
  | Ok () -> Alcotest.fail "replacement that exits before reading succeeded"
  | Error (`Clipboard_error _) ->
      Alcotest.(check bool)
        "failed replacement leaves the previous source" true
        (pid_alive first_pid);
      Alcotest.(check int) "owner pid unchanged" first_pid (read_pid pid_path)

let () =
  Alcotest.run "clipboard_owner"
    [
      ( "ownership",
        [
          Alcotest.test_case "image previous not inspected" `Quick (fun () ->
              test_no_inspection_exact_stdin "image/png");
          Alcotest.test_case "text previous not inspected" `Quick (fun () ->
              test_no_inspection_exact_stdin "text/plain");
          Alcotest.test_case "empty previous not inspected" `Quick (fun () ->
              test_no_inspection_exact_stdin "");
          Alcotest.test_case "arbitrary MIME not inspected" `Quick (fun () ->
              test_no_inspection_exact_stdin "application/octet-stream");
          Alcotest.test_case "copy early failure" `Quick test_copy_early_failure;
          Alcotest.test_case "known spawn failure hides secret" `Quick
            test_known_spawn_failure_hides_secret;
          Alcotest.test_case "cancellation at acquisition" `Quick
            test_cancellation_at_acquisition;
          Alcotest.test_case "slow consumer still alive after 250ms" `Quick
            test_slow_consumer_still_alive;
          Alcotest.test_case "100 offers bounded fds" `Quick
            test_repeated_offers_bounded;
          Alcotest.test_case "app shutdown settles" `Quick test_shutdown_settles;
          Alcotest.test_case "replacement settles the previous helper" `Quick
            test_settled_before_replacement;
          Alcotest.test_case "default text types, ready before retire" `Quick
            test_default_mime_and_ready_before_retire;
          Alcotest.test_case "failed replacement keeps previous source" `Quick
            test_failed_replacement_keeps_previous_source;
          Alcotest.test_case "natural exit releases pipes" `Quick
            test_natural_exit_releases_pipes;
          Alcotest.test_case "large stdout discarded" `Quick
            test_large_stdout_discarded;
          Alcotest.test_case "inject paste image previous" `Quick (fun () ->
              test_inject_paste_key_success "image/png");
          Alcotest.test_case "inject paste text previous" `Quick (fun () ->
              test_inject_paste_key_success "text/plain");
          Alcotest.test_case "inject paste empty previous" `Quick (fun () ->
              test_inject_paste_key_success "");
          Alcotest.test_case "inject paste arbitrary MIME" `Quick (fun () ->
              test_inject_paste_key_success "application/octet-stream");
          Alcotest.test_case "inject key fail retains helper" `Quick
            test_inject_paste_key_fail_retains_helper;
          Alcotest.test_case "paste waits before key dispatch" `Quick
            (test_paste_handoff_delay ~cancel:false);
          Alcotest.test_case "cancelled handoff never sends paste" `Quick
            (test_paste_handoff_delay ~cancel:true);
        ] );
    ]
