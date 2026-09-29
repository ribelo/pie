let caps = Ox_test.powerbox () (* ^pcaps-n1rg *)

(* PIE027: unresolved formatting rules fail before emission.
   Fake niri/wtype/xdotool on a private PATH. No real desktop. *)

module Text = Pie.Input.Text_injection

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

let write_file path contents =
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let env_keys =
  [
    "PATH";
    "HOME";
    "XDG_CONFIG_HOME";
    "XDG_DATA_HOME";
    "XDG_RUNTIME_DIR";
    "XDG_SESSION_TYPE";
    "XDG_CURRENT_DESKTOP";
    "NIRI_SOCKET";
    "WAYLAND_DISPLAY";
    "DISPLAY";
    "TMPDIR";
    "PIE_FOCUS_CAPTURE";
    "PIE_FOCUS_NIRI_MODE";
  ]

let save_env () = List.map (fun key -> (key, Sys.getenv_opt key)) env_keys

let restore_env saved =
  List.iter
    (fun (key, value) ->
      match value with
      | Some value -> Unix.putenv key value
      | None -> Unix.putenv key "")
    saved

let capture_script helper =
  "#!/bin/sh\n" ^ "capture=\"${PIE_FOCUS_CAPTURE:?}\"\n" ^ "{\n"
  ^ "  printf '%s\\n' " ^ helper ^ "\n" ^ "  printf '%d\\n' \"$#\"\n"
  ^ "  for arg in \"$@\"; do\n" ^ "    printf '%s\\n' \"$arg\"\n" ^ "  done\n"
  ^ "} >> \"$capture\"\n" ^ "exit 0\n"

let niri_script =
  "#!/bin/sh\n" ^ "capture=\"${PIE_FOCUS_CAPTURE:?}\"\n"
  ^ "printf 'niri\\n' >> \"$capture\"\n"
  ^ "mode=\"${PIE_FOCUS_NIRI_MODE:-match}\"\n" ^ "case \"$mode\" in\n"
  ^ "  error)\n" ^ "    printf 'secret-title helper-stderr\\n' >&2\n"
  ^ "    exit 1\n" ^ "    ;;\n" ^ "  null)\n" ^ "    printf 'null\\n'\n"
  ^ "    exit 0\n" ^ "    ;;\n" ^ "  nonmatch)\n"
  ^ "    printf '%s\\n' '{\"id\":1,\"title\":\"other\",\"app_id\":\"Firefox\"}'\n"
  ^ "    exit 0\n" ^ "    ;;\n" ^ "  *)\n"
  ^ "    printf '%s\\n' \
     '{\"id\":1,\"title\":\"codex\",\"app_id\":\"Alacritty\"}'\n"
  ^ "    exit 0\n" ^ "    ;;\n" ^ "esac\n"

let with_sandbox f =
  let dir = Filename.temp_file "pie-focus-rules" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let isolate ~sandbox ~session ~desktop ~socket ~niri_mode =
  let home = Filename.concat sandbox "home" in
  let bin = Filename.concat sandbox "bin" in
  let runtime = Filename.concat sandbox "runtime" in
  List.iter (fun path -> Unix.mkdir path 0o700) [ home; bin; runtime ];
  write_file (Filename.concat bin "wtype") (capture_script "wtype");
  write_file (Filename.concat bin "xdotool") (capture_script "xdotool");
  write_file (Filename.concat bin "niri") niri_script;
  List.iter
    (fun name -> Unix.chmod (Filename.concat bin name) 0o700)
    [ "wtype"; "xdotool"; "niri" ];
  Unix.putenv "HOME" home;
  Unix.putenv "PATH" bin;
  Unix.putenv "TMPDIR" (Filename.concat sandbox "tmp");
  (try Unix.mkdir (Filename.concat sandbox "tmp") 0o700 with _ -> ());
  Unix.putenv "XDG_SESSION_TYPE" session;
  Unix.putenv "XDG_CURRENT_DESKTOP" desktop;
  Unix.putenv "NIRI_SOCKET" socket;
  Unix.putenv "WAYLAND_DISPLAY" "";
  Unix.putenv "DISPLAY" "";
  Unix.putenv "PIE_FOCUS_CAPTURE" (Filename.concat sandbox "capture");
  Unix.putenv "PIE_FOCUS_NIRI_MODE" niri_mode

let with_env ?(session = "wayland") ?(desktop = "niri")
    ?(socket = "/tmp/pie-fake-niri.sock") ?(niri_mode = "match") f =
  with_sandbox (fun sandbox ->
      let saved = save_env () in
      Fun.protect
        ~finally:(fun () -> restore_env saved)
        (fun () ->
          isolate ~sandbox ~session ~desktop ~socket ~niri_mode;
          f sandbox))

let capture_text sandbox =
  let path = Filename.concat sandbox "capture" in
  match open_in_bin path with
  | exception Sys_error _ -> ""
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))

let niri_ran sandbox =
  let rec loop s =
    String.length s >= 4
    && (String.sub s 0 4 = "niri" || loop (String.sub s 1 (String.length s - 1)))
  in
  loop (capture_text sandbox)

let emitted sandbox =
  let t = capture_text sandbox in
  let rec has needle s =
    let n = String.length needle in
    let h = String.length s in
    let rec loop i =
      i <= h - n && (String.sub s i n = needle || loop (i + 1))
    in
    loop 0
  in
  has "wtype" t || has "xdotool" t

let pp_err ppf = function
  | `Clipboard_error m
  | `Injection_error m
  | `Wtype_error m
  | `Xdotool_error m
  | `Niri_error m ->
      Format.pp_print_string ppf m

let run_inject ~newline_rules ~text_entry_rules text ~clock process_mgr =
  Text.inject ~caps ~newline_rules ~text_entry_rules ~clock ~process_mgr text

let run_once operation =
  Pie_test_runtime.run @@ fun env ->
  operation ~clock:(Eio.Stdenv.clock env) (Eio.Stdenv.process_mgr env)

let injection_msg = function
  | `Injection_error msg -> msg
  | error -> Alcotest.failf "expected Injection_error: %a" pp_err error

let assert_content_free msg =
  let lower = String.lowercase_ascii msg in
  List.iter
    (fun needle ->
      let n = String.length needle in
      let h = String.length lower in
      let rec loop i =
        if i <= h - n then
          if String.sub lower i n = needle then
            Alcotest.failf "error leaked %S: %s" needle msg
          else loop (i + 1)
      in
      loop 0)
    [ "secret-title"; "helper-stderr"; "alacritty"; "codex"; "firefox" ]

let newline_rules : Text.newline_rule list =
  [
    {
      app_id = Some "Alacritty";
      title_regex = Some "codex";
      action = Text.Shift_enter;
    };
  ]

let entry_rules : Text.text_entry_rule list =
  [
    {
      app_id = Some "Alacritty";
      title_regex = Some "codex";
      method_ = Text.Type;
    };
  ]

let source = "hello\nworld"

let expect_fail ~newline_rules ~text_entry_rules sandbox =
  match run_once (run_inject ~newline_rules ~text_entry_rules source) with
  | Ok _ -> Alcotest.fail "expected selection failure"
  | Error cause ->
      let msg = injection_msg cause in
      assert_content_free msg;
      Alcotest.(check bool) "no emission" false (emitted sandbox);
      msg

let expect_ok ~newline_rules ~text_entry_rules sandbox =
  match run_once (run_inject ~newline_rules ~text_entry_rules source) with
  | Ok result ->
      Alcotest.(check bool) "emitted" true (emitted sandbox);
      result
  | Error cause -> Alcotest.failf "unexpected failure: %a" pp_err cause

let combinations =
  [
    ("newline-only", newline_rules, []);
    ("entry-only", [], entry_rules);
    ("both", newline_rules, entry_rules);
  ]

let test_lookup_error () =
  List.iter
    (fun (label, newline_rules, text_entry_rules) ->
      with_env ~niri_mode:"error" (fun sandbox ->
          let msg = expect_fail ~newline_rules ~text_entry_rules sandbox in
          Alcotest.(check bool) (label ^ " niri ran") true (niri_ran sandbox);
          Alcotest.(check string)
            (label ^ " unresolved") "focus information is unavailable" msg))
    combinations

let test_lookup_null () =
  List.iter
    (fun (label, newline_rules, text_entry_rules) ->
      with_env ~niri_mode:"null" (fun sandbox ->
          let msg = expect_fail ~newline_rules ~text_entry_rules sandbox in
          Alcotest.(check bool) (label ^ " niri ran") true (niri_ran sandbox);
          Alcotest.(check string)
            (label ^ " unresolved") "focus information is unavailable" msg))
    combinations

let test_match () =
  List.iter
    (fun (label, newline_rules, text_entry_rules) ->
      with_env ~niri_mode:"match" (fun sandbox ->
          let result = expect_ok ~newline_rules ~text_entry_rules sandbox in
          Alcotest.(check bool) (label ^ " niri ran") true (niri_ran sandbox);
          if newline_rules <> [] then
            Alcotest.(check bool)
              (label ^ " shift-enter") true
              (String.contains result.text '\n')))
    combinations

let test_nonmatch_defaults () =
  List.iter
    (fun (label, newline_rules, text_entry_rules) ->
      with_env ~niri_mode:"nonmatch" (fun sandbox ->
          let result = expect_ok ~newline_rules ~text_entry_rules sandbox in
          Alcotest.(check bool) (label ^ " niri ran") true (niri_ran sandbox);
          Alcotest.(check string)
            (label ^ " collapse default")
            "hello world" result.text))
    combinations

let test_no_rules_skips_niri () =
  with_env ~niri_mode:"error" (fun sandbox ->
      let _ = expect_ok ~newline_rules:[] ~text_entry_rules:[] sandbox in
      Alcotest.(check bool) "niri not invoked" false (niri_ran sandbox))

let test_x11_rejected () =
  List.iter
    (fun (label, newline_rules, text_entry_rules) ->
      with_env ~session:"x11" ~desktop:"" ~socket:"" ~niri_mode:"match"
        (fun sandbox ->
          let msg = expect_fail ~newline_rules ~text_entry_rules sandbox in
          Alcotest.(check bool) (label ^ " no niri") false (niri_ran sandbox);
          Alcotest.(check string)
            (label ^ " incompatible")
            "focus rules require a Niri Wayland session" msg))
    combinations

let test_gnome_rejected () =
  List.iter
    (fun (label, newline_rules, text_entry_rules) ->
      with_env ~session:"wayland" ~desktop:"GNOME" ~socket:"/tmp/sock"
        ~niri_mode:"match" (fun sandbox ->
          let msg = expect_fail ~newline_rules ~text_entry_rules sandbox in
          Alcotest.(check bool) (label ^ " no niri") false (niri_ran sandbox);
          Alcotest.(check string)
            (label ^ " incompatible")
            "focus rules require a Niri Wayland session" msg))
    combinations

let test_empty_socket_rejected () =
  with_env ~desktop:"niri" ~socket:"" ~niri_mode:"match" (fun sandbox ->
      let msg = expect_fail ~newline_rules ~text_entry_rules:[] sandbox in
      Alcotest.(check bool) "no niri" false (niri_ran sandbox);
      Alcotest.(check string)
        "incompatible" "focus rules require a Niri Wayland session" msg)

let test_colon_token_niri () =
  with_env ~desktop:"ubuntu:GNOME:niri" ~niri_mode:"match" (fun sandbox ->
      ignore (expect_ok ~newline_rules ~text_entry_rules:[] sandbox);
      Alcotest.(check bool) "niri ran" true (niri_ran sandbox))

let test_first_match_newline () =
  let rules : Text.newline_rule list =
    [
      {
        app_id = Some "Alacritty";
        title_regex = Some "codex";
        action = Text.Ctrl_j;
      };
      {
        app_id = Some "Alacritty";
        title_regex = Some "codex";
        action = Text.Shift_enter;
      };
    ]
  in
  with_env ~niri_mode:"match" (fun sandbox ->
      let result =
        expect_ok ~newline_rules:rules ~text_entry_rules:[] sandbox
      in
      Alcotest.(check string) "explicit newline kept" "hello\nworld" result.text)

let () =
  Alcotest.run "focus_rules"
    [
      ( "selection",
        [
          Alcotest.test_case "lookup error fails before emission" `Quick
            test_lookup_error;
          Alcotest.test_case "null focus fails before emission" `Quick
            test_lookup_null;
          Alcotest.test_case "matching window applies rules" `Quick test_match;
          Alcotest.test_case "nonmatching window defaults type/collapse" `Quick
            test_nonmatch_defaults;
          Alcotest.test_case "no rules skips niri" `Quick
            test_no_rules_skips_niri;
          Alcotest.test_case "x11 with rules rejected" `Quick test_x11_rejected;
          Alcotest.test_case "gnome wayland with rules rejected" `Quick
            test_gnome_rejected;
          Alcotest.test_case "empty niri socket rejected" `Quick
            test_empty_socket_rejected;
          Alcotest.test_case "colon-token niri is compatible" `Quick
            test_colon_token_niri;
          Alcotest.test_case "first-match newline action" `Quick
            test_first_match_newline;
        ] );
    ]
