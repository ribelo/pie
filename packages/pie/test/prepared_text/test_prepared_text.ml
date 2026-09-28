(* Isolated PIE020 prepared-text integrity controls.
   Fake wtype/xdotool/niri on a private PATH. Never real desktop helpers. *)

module Text = Pie.Input.Text_injection

type invocation = { helper : string; args : string list }

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
    "TMP";
    "LC_ALL";
    "PIE_PREPARED_TEXT_CAPTURE";
    "PIE_PREPARED_TEXT_FAIL_ON_KEY";
  ]

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

let save_env () = List.map (fun key -> (key, Sys.getenv_opt key)) env_keys

let restore_env saved =
  List.iter
    (fun (key, value) ->
      match value with
      | Some value -> Unix.putenv key value
      | None -> Unix.putenv key "")
    saved

let capture_script helper =
  "#!/bin/sh\n" ^ "export LC_ALL=C\n"
  ^ "if [ -n \"${PIE_PREPARED_TEXT_FAIL_ON_KEY:-}\" ]; then\n"
  ^ "  case \"${1:-}\" in\n" ^ "    -M|-k|key) exit 1 ;;\n" ^ "  esac\n"
  ^ "fi\n" ^ "capture=\"${PIE_PREPARED_TEXT_CAPTURE:?}\"\n" ^ "{\n"
  ^ "  printf '%s\\n' " ^ helper ^ "\n" ^ "  printf '%d\\n' \"$#\"\n"
  ^ "  for arg in \"$@\"; do\n" ^ "    printf '%d\\n' \"${#arg}\"\n"
  ^ "    printf '%s\\n' \"$arg\"\n" ^ "  done\n" ^ "} >> \"$capture\"\n"
  ^ "exit 0\n"

let niri_script =
  "#!/bin/sh\n"
  ^ "printf '%s\\n' \
     '{\"id\":1,\"title\":\"codex\",\"app_id\":\"Alacritty\",\"pid\":42,\"workspace_id\":1}'\n"
  ^ "exit 0\n"

let with_sandbox f =
  let dir = Filename.temp_file "pie-prepared-text" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o700;
  Fun.protect ~finally:(fun () -> rm_rf dir) (fun () -> f dir)

let isolate ~sandbox ~session ~fail_on_key =
  let home = Filename.concat sandbox "home" in
  let config = Filename.concat sandbox "config" in
  let data = Filename.concat sandbox "data" in
  let runtime = Filename.concat sandbox "runtime" in
  let tmp = Filename.concat sandbox "tmp" in
  let bin = Filename.concat sandbox "bin" in
  List.iter
    (fun path -> Unix.mkdir path 0o700)
    [ home; config; data; runtime; tmp; bin ];
  write_file (Filename.concat bin "wtype") (capture_script "wtype");
  write_file (Filename.concat bin "xdotool") (capture_script "xdotool");
  write_file (Filename.concat bin "niri") niri_script;
  Unix.chmod (Filename.concat bin "wtype") 0o700;
  Unix.chmod (Filename.concat bin "xdotool") 0o700;
  Unix.chmod (Filename.concat bin "niri") 0o700;
  Unix.putenv "HOME" home;
  Unix.putenv "XDG_CONFIG_HOME" config;
  Unix.putenv "XDG_DATA_HOME" data;
  Unix.putenv "XDG_RUNTIME_DIR" runtime;
  Unix.putenv "TMPDIR" tmp;
  Unix.putenv "TMP" tmp;
  Unix.putenv "PATH" bin;
  Unix.putenv "LC_ALL" "C";
  Unix.putenv "XDG_SESSION_TYPE" session;
  Unix.putenv "WAYLAND_DISPLAY" "";
  Unix.putenv "DISPLAY" "";
  if String.equal session "wayland" then (
    Unix.putenv "XDG_CURRENT_DESKTOP" "niri";
    Unix.putenv "NIRI_SOCKET" (Filename.concat runtime "niri.sock"))
  else (
    Unix.putenv "XDG_CURRENT_DESKTOP" "";
    Unix.putenv "NIRI_SOCKET" "");
  Unix.putenv "PIE_PREPARED_TEXT_CAPTURE" (Filename.concat sandbox "capture");
  Unix.putenv "PIE_PREPARED_TEXT_FAIL_ON_KEY" (if fail_on_key then "1" else "")

let with_fixtures ?(session = "wayland") ?(fail_on_key = false) f =
  with_sandbox (fun sandbox ->
      let saved = save_env () in
      Fun.protect
        ~finally:(fun () -> restore_env saved)
        (fun () ->
          isolate ~sandbox ~session ~fail_on_key;
          f sandbox))

let read_invocations path =
  match open_in_bin path with
  | exception Sys_error _ -> []
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let rec read_args remaining acc =
            if remaining = 0 then List.rev acc
            else
              let len = int_of_string (input_line ic) in
              let buf = Bytes.create len in
              really_input ic buf 0 len;
              (match input_char ic with
              | '\n' -> ()
              | c ->
                  failwith
                    (Printf.sprintf "expected newline after arg, got %C" c));
              read_args (remaining - 1) (Bytes.to_string buf :: acc)
          in
          let rec loop acc =
            match input_line ic with
            | exception End_of_file -> List.rev acc
            | helper ->
                let argc = int_of_string (input_line ic) in
                let args = read_args argc [] in
                loop ({ helper; args } :: acc)
          in
          loop [])

let invocations sandbox = read_invocations (Filename.concat sandbox "capture")

let pp_args ppf args =
  Format.fprintf ppf "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
       (fun ppf s -> Format.fprintf ppf "%S" s))
    args

let pp_invocation ppf inv =
  Format.fprintf ppf "%s %a" inv.helper pp_args inv.args

let invocation = Alcotest.testable pp_invocation ( = )
let invocations_testable = Alcotest.list invocation

let pp_inject_error ppf = function
  | `Clipboard_error message
  | `Injection_error message
  | `Niri_error message
  | `Wtype_error message
  | `Xdotool_error message ->
      Format.pp_print_string ppf message

let pp_type_error ppf = function
  | `Injection_error message | `Wtype_error message | `Xdotool_error message ->
      Format.pp_print_string ppf message

let pp_wtype_error ppf = function
  | `Wtype_error message -> Format.pp_print_string ppf message

let pp_xdotool_error ppf = function
  | `Xdotool_error message -> Format.pp_print_string ppf message

let run_ok ~pp_error operation =
  Eio_main.run @@ fun env ->
  match
    operation ~clock:(Eio.Stdenv.clock env) (Eio.Stdenv.process_mgr env)
  with
  | Ok value -> value
  | Error error -> Alcotest.failf "operation failed: %a" pp_error error

let run_error ~pp_error operation =
  Eio_main.run @@ fun env ->
  match
    operation ~clock:(Eio.Stdenv.clock env) (Eio.Stdenv.process_mgr env)
  with
  | Ok _ -> Alcotest.fail "expected helper failure, got Ok"
  | Error error ->
      ignore pp_error;
      error

let wtype_type text = { helper = "wtype"; args = [ "--"; text ] }

let wtype_shift_enter =
  { helper = "wtype"; args = [ "-M"; "shift"; "-k"; "Return"; "-m"; "shift" ] }

let wtype_ctrl_j =
  { helper = "wtype"; args = [ "-M"; "ctrl"; "-k"; "j"; "-m"; "ctrl" ] }

let xdotool_type text =
  { helper = "xdotool"; args = [ "type"; "--clearmodifiers"; "--"; text ] }

let xdotool_shift_enter =
  { helper = "xdotool"; args = [ "key"; "--clearmodifiers"; "shift+Return" ] }

let xdotool_ctrl_j =
  { helper = "xdotool"; args = [ "key"; "--clearmodifiers"; "ctrl+j" ] }

let evidence_text = "top\n  indented"
let multiline_text = "top  \n  indented\n\n  naïve 東京"
let crlf_text = "top  \r\n  indented"
let collapse_text = " first\n  second \r\n third "

let shift_enter_rules : Text.newline_rule list =
  [
    {
      app_id = Some "Alacritty";
      title_regex = Some "codex";
      action = Text.Shift_enter;
    };
  ]

let ctrl_j_rules : Text.newline_rule list =
  [
    {
      app_id = Some "Alacritty";
      title_regex = Some "codex";
      action = Text.Ctrl_j;
    };
  ]

let typed_payloads invocations =
  List.filter_map
    (fun inv ->
      match (inv.helper, inv.args) with
      | "wtype", [ "--"; text ] -> Some text
      | "xdotool", [ "type"; "--clearmodifiers"; "--"; text ] -> Some text
      | _ -> None)
    invocations

let reconstruct invocations =
  let rec loop acc = function
    | [] -> acc
    | inv :: rest -> (
        match (inv.helper, inv.args) with
        | "wtype", [ "--"; text ]
        | "xdotool", [ "type"; "--clearmodifiers"; "--"; text ] ->
            loop (acc ^ text) rest
        | "wtype", "-M" :: _ | "xdotool", "key" :: _ -> loop (acc ^ "\n") rest
        | _ ->
            Alcotest.failf "unexpected argv while reconstructing: %a"
              pp_invocation inv)
  in
  loop "" invocations

let require_literal_separator invocations =
  List.iter
    (fun inv ->
      match (inv.helper, inv.args) with
      | "wtype", [ "--"; _ ]
      | "xdotool", [ "type"; "--clearmodifiers"; "--"; _ ]
      | "wtype", "-M" :: _
      | "xdotool", "key" :: "--clearmodifiers" :: _ ->
          ()
      | _ ->
          Alcotest.failf "argv is not literal -- separated: %a" pp_invocation
            inv)
    invocations

let test_prepare_preserves_internal_whitespace () =
  let prepared_shift =
    Text.prepare_text ~method_:Text.Type ~newline_action:Text.Shift_enter
      evidence_text
  in
  let prepared_ctrl =
    Text.prepare_text ~method_:Text.Type ~newline_action:Text.Ctrl_j
      evidence_text
  in
  let prepared_multi =
    Text.prepare_text ~method_:Text.Type ~newline_action:Text.Shift_enter
      multiline_text
  in
  Alcotest.(check string) "shift-enter evidence" evidence_text prepared_shift;
  Alcotest.(check string) "ctrl-j evidence" evidence_text prepared_ctrl;
  Alcotest.(check string)
    "internal trailing spaces and indent" multiline_text prepared_multi

let test_prepare_collapse_policy () =
  let prepared =
    Text.prepare_text ~method_:Text.Type ~newline_action:Text.Collapse_to_space
      collapse_text
  in
  Alcotest.(check string) "collapse to space" "first second third" prepared

let test_leaf_empty_noop helper type_text pp_error () =
  with_fixtures (fun sandbox ->
      run_ok ~pp_error (fun ~clock:_ process_mgr -> type_text ~process_mgr "");
      Alcotest.(check invocations_testable)
        (helper ^ " empty is no-op")
        [] (invocations sandbox))

let test_leaf_whitespace_emitted helper expected type_text pp_error () =
  with_fixtures (fun sandbox ->
      run_ok ~pp_error (fun ~clock:_ process_mgr -> type_text ~process_mgr "  ");
      Alcotest.(check invocations_testable)
        (helper ^ " whitespace-only emitted verbatim")
        [ expected "  " ]
        (invocations sandbox))

let backend_of_session = function "x11" -> Text.X11 | _ -> Text.Wayland

let test_type_text_preserves ~session ~newline_action ~expected ~source () =
  with_fixtures ~session (fun sandbox ->
      let prepared =
        Text.prepare_text ~method_:Text.Type ~newline_action source
      in
      let backend = Text.choose_backend (backend_of_session session) in
      run_ok ~pp_error:pp_type_error (fun ~clock:_ process_mgr ->
          Text.type_text ~newline_action ~process_mgr ~backend prepared);
      let actual = invocations sandbox in
      require_literal_separator actual;
      Alcotest.(check invocations_testable) "emitted argv" expected actual;
      Alcotest.(check string)
        "reconstructed matches prepared" prepared (reconstruct actual))

let test_inject_preserves ~session ~rules ~expected ~source () =
  with_fixtures ~session (fun sandbox ->
      let result =
        run_ok ~pp_error:pp_inject_error (fun ~clock process_mgr ->
            Text.inject ~clock ~newline_rules:rules ~process_mgr source)
      in
      let newline_action =
        match rules with
        | { action; _ } :: _ -> action
        | [] -> Text.Collapse_to_space
      in
      let prepared =
        Text.prepare_text ~method_:Text.Type ~newline_action source
      in
      let actual = invocations sandbox in
      require_literal_separator actual;
      Alcotest.(check string) "reported result text" prepared result.text;
      Alcotest.(check invocations_testable) "emitted argv" expected actual;
      Alcotest.(check string)
        "reconstructed matches prepared" prepared (reconstruct actual);
      Alcotest.(check bool)
        "niri was not used as a typing backend" true
        (not (List.exists (fun inv -> String.equal inv.helper "niri") actual)))

let test_x11_inject_rules_rejected ~rules () =
  with_fixtures ~session:"x11" (fun sandbox ->
      let cause =
        run_error ~pp_error:pp_inject_error (fun ~clock process_mgr ->
            Text.inject ~clock ~newline_rules:rules ~process_mgr evidence_text)
      in
      (match cause with
      | `Injection_error msg ->
          if msg = "" then Alcotest.fail "empty injection error";
          let lower = String.lowercase_ascii msg in
          if
            List.exists
              (fun needle ->
                let rec loop i =
                  i <= String.length lower - String.length needle
                  && (String.sub lower i (String.length needle) = needle
                     || loop (i + 1))
                in
                loop 0)
              [ "alacritty"; "codex"; "niri command"; "stderr" ]
          then Alcotest.failf "error leaked helper details: %s" msg
      | _ -> Alcotest.failf "expected Injection_error: %a" pp_inject_error cause);
      Alcotest.(check invocations_testable)
        "no emission on x11 rule rejection" [] (invocations sandbox))

let test_inject_collapse () =
  with_fixtures ~session:"wayland" (fun sandbox ->
      let source = collapse_text in
      let prepared =
        Text.prepare_text ~method_:Text.Type
          ~newline_action:Text.Collapse_to_space source
      in
      let result =
        run_ok ~pp_error:pp_inject_error (fun ~clock process_mgr ->
            Text.inject ~clock ~process_mgr source)
      in
      let actual = invocations sandbox in
      Alcotest.(check string) "reported collapsed text" prepared result.text;
      Alcotest.(check invocations_testable)
        "single collapsed segment"
        [ wtype_type "first second third" ]
        actual)

let test_crlf_segments ~session ~newline_action ~type_event ~key_event () =
  with_fixtures ~session (fun sandbox ->
      let prepared =
        Text.prepare_text ~method_:Text.Type ~newline_action crlf_text
      in
      let backend = Text.choose_backend (backend_of_session session) in
      run_ok ~pp_error:pp_type_error (fun ~clock:_ process_mgr ->
          Text.type_text ~newline_action ~process_mgr ~backend prepared);
      let actual = invocations sandbox in
      Alcotest.(check (list string))
        "CRLF segments keep internal spaces" [ "top  "; "  indented" ]
        (typed_payloads actual);
      Alcotest.(check invocations_testable)
        "CRLF becomes one key between segments"
        [ type_event "top  "; key_event; type_event "  indented" ]
        actual)

let error_message = function
  | `Wtype_error msg | `Xdotool_error msg | `Injection_error msg -> msg

let test_later_helper_failure ~session ~newline_action () =
  with_fixtures ~session ~fail_on_key:true (fun sandbox ->
      let prepared =
        Text.prepare_text ~method_:Text.Type ~newline_action evidence_text
      in
      let backend = Text.choose_backend (backend_of_session session) in
      let cause =
        run_error ~pp_error:pp_type_error (fun ~clock:_ process_mgr ->
            Text.type_text ~newline_action ~process_mgr ~backend prepared)
      in
      let msg = error_message cause in
      Alcotest.(check bool)
        "failure message is visible" true
        (String.length msg > 0);
      let actual = invocations sandbox in
      let expected_first =
        if String.equal session "wayland" then wtype_type "top"
        else xdotool_type "top"
      in
      Alcotest.(check invocations_testable)
        "earlier segment emitted, later helper not replayed" [ expected_first ]
        actual;
      Alcotest.(check bool)
        "indented segment was not emitted after failure" true
        (not (List.exists (fun inv -> List.mem "  indented" inv.args) actual)))

let evidence_wtype_shift =
  [ wtype_type "top"; wtype_shift_enter; wtype_type "  indented" ]

let evidence_wtype_ctrl =
  [ wtype_type "top"; wtype_ctrl_j; wtype_type "  indented" ]

let evidence_xdotool_shift =
  [ xdotool_type "top"; xdotool_shift_enter; xdotool_type "  indented" ]

let evidence_xdotool_ctrl =
  [ xdotool_type "top"; xdotool_ctrl_j; xdotool_type "  indented" ]

let multiline_wtype_shift =
  [
    wtype_type "top  ";
    wtype_shift_enter;
    wtype_type "  indented";
    wtype_shift_enter;
    wtype_shift_enter;
    wtype_type "  naïve 東京";
  ]

let multiline_xdotool_ctrl =
  [
    xdotool_type "top  ";
    xdotool_ctrl_j;
    xdotool_type "  indented";
    xdotool_ctrl_j;
    xdotool_ctrl_j;
    xdotool_type "  naïve 東京";
  ]

let () =
  Alcotest.run "prepared_text"
    [
      ( "prepare_text",
        [
          Alcotest.test_case
            "line-preserving actions keep indent and internal spaces" `Quick
            test_prepare_preserves_internal_whitespace;
          Alcotest.test_case "collapse-to-space policy unchanged" `Quick
            test_prepare_collapse_policy;
        ] );
      ( "leaf",
        [
          Alcotest.test_case "wtype empty string no-op" `Quick
            (test_leaf_empty_noop "wtype" Pie.Wayland.Wtype.type_text
               pp_wtype_error);
          Alcotest.test_case "xdotool empty string no-op" `Quick
            (test_leaf_empty_noop "xdotool" Pie.X11.Xdotool.type_text
               pp_xdotool_error);
          Alcotest.test_case "wtype whitespace-only emitted verbatim" `Quick
            (test_leaf_whitespace_emitted "wtype" wtype_type
               Pie.Wayland.Wtype.type_text pp_wtype_error);
          Alcotest.test_case "xdotool whitespace-only emitted verbatim" `Quick
            (test_leaf_whitespace_emitted "xdotool" xdotool_type
               Pie.X11.Xdotool.type_text pp_xdotool_error);
        ] );
      ( "type_text",
        [
          Alcotest.test_case "wtype Shift_enter preserves indent" `Quick
            (test_type_text_preserves ~session:"wayland"
               ~newline_action:Text.Shift_enter ~expected:evidence_wtype_shift
               ~source:evidence_text);
          Alcotest.test_case "wtype Ctrl_j preserves indent" `Quick
            (test_type_text_preserves ~session:"wayland"
               ~newline_action:Text.Ctrl_j ~expected:evidence_wtype_ctrl
               ~source:evidence_text);
          Alcotest.test_case "xdotool Shift_enter preserves indent" `Quick
            (test_type_text_preserves ~session:"x11"
               ~newline_action:Text.Shift_enter ~expected:evidence_xdotool_shift
               ~source:evidence_text);
          Alcotest.test_case "xdotool Ctrl_j preserves indent" `Quick
            (test_type_text_preserves ~session:"x11" ~newline_action:Text.Ctrl_j
               ~expected:evidence_xdotool_ctrl ~source:evidence_text);
          Alcotest.test_case
            "wtype Shift_enter keeps trailing spaces, empty lines, Unicode"
            `Quick
            (test_type_text_preserves ~session:"wayland"
               ~newline_action:Text.Shift_enter ~expected:multiline_wtype_shift
               ~source:multiline_text);
          Alcotest.test_case
            "xdotool Ctrl_j keeps trailing spaces, empty lines, Unicode" `Quick
            (test_type_text_preserves ~session:"x11" ~newline_action:Text.Ctrl_j
               ~expected:multiline_xdotool_ctrl ~source:multiline_text);
          Alcotest.test_case "wtype Shift_enter CRLF segments" `Quick
            (test_crlf_segments ~session:"wayland"
               ~newline_action:Text.Shift_enter ~type_event:wtype_type
               ~key_event:wtype_shift_enter);
          Alcotest.test_case "xdotool Ctrl_j CRLF segments" `Quick
            (test_crlf_segments ~session:"x11" ~newline_action:Text.Ctrl_j
               ~type_event:xdotool_type ~key_event:xdotool_ctrl_j);
        ] );
      ( "inject",
        [
          Alcotest.test_case "wayland inject Shift_enter preserves indent"
            `Quick
            (test_inject_preserves ~session:"wayland" ~rules:shift_enter_rules
               ~expected:evidence_wtype_shift ~source:evidence_text);
          Alcotest.test_case "wayland inject Ctrl_j preserves indent" `Quick
            (test_inject_preserves ~session:"wayland" ~rules:ctrl_j_rules
               ~expected:evidence_wtype_ctrl ~source:evidence_text);
          Alcotest.test_case "x11 inject Shift_enter rules rejected" `Quick
            (test_x11_inject_rules_rejected ~rules:shift_enter_rules);
          Alcotest.test_case "x11 inject Ctrl_j rules rejected" `Quick
            (test_x11_inject_rules_rejected ~rules:ctrl_j_rules);
          Alcotest.test_case "inject collapse-to-space unchanged" `Quick
            test_inject_collapse;
        ] );
      ( "failure",
        [
          Alcotest.test_case "later wtype key failure stays visible" `Quick
            (test_later_helper_failure ~session:"wayland"
               ~newline_action:Text.Shift_enter);
          Alcotest.test_case "later xdotool key failure stays visible" `Quick
            (test_later_helper_failure ~session:"x11"
               ~newline_action:Text.Ctrl_j);
        ] );
    ]
