module T = Pie.Input.Text_injection

let window =
  {
    Pie.Niri.id = 42;
    app_id = Some "editor";
    title = Some "press title";
    pid = None;
    workspace_id = None;
  }

let run ?(action = T.Collapse_to_space) ?(copy_failure = false)
    ?(paste_failure = false) text =
  let events = ref [] in
  let record event = events := !events @ [ event ] in
  let result =
    T.deliver
      ~copy:(fun text ->
        record ("copy:" ^ text);
        if copy_failure then Error (`Clipboard_error "fake") else Ok ())
      ~paste:(fun () ->
        record "paste";
        if paste_failure then Error (`Wtype_error "fake") else Ok ())
      ~newline:(fun action ->
        record
          (match action with
          | T.Shift_enter -> "shift-enter"
          | Ctrl_j -> "ctrl-j"
          | Collapse_to_space -> Alcotest.fail "unexpected newline");
        Ok ())
      ~focused_window:(fun () -> Ok (Some window))
      ~target_window:(Some window)
      ~newline_rules:[ { T.app_id = None; title_regex = None; action } ]
      text
  in
  (result, !events)

let test_collapse () =
  match run "  hello\n  world\n " with
  | Ok (text, T.Clipboard_key_attempted), events ->
      Alcotest.(check string) "prepared" "hello world" text;
      Alcotest.(check (list string))
        "clipboard before paste"
        [ "copy:hello world"; "paste" ]
        events
  | _ -> Alcotest.fail "delivery failed"

let test_line_action action key () =
  match run ~action " hello\r\n\rworld " with
  | Ok (text, T.Clipboard_key_attempted), events ->
      Alcotest.(check string) "whole text" "hello\r\n\rworld" text;
      Alcotest.(check (list string))
        "line paste and final clipboard"
        [
          "copy:hello\r\n\rworld";
          "copy:hello";
          "paste";
          key;
          key;
          "copy:world";
          "paste";
          "copy:hello\r\n\rworld";
        ]
        events
  | _ -> Alcotest.fail "delivery failed"

let test_payload () =
  let text = "--key -M ctrl $HOME 'quote' \"quoted\" zażółć 🦊" in
  match run text with
  | Ok (prepared, _), [ copied; "paste" ] ->
      Alcotest.(check string) "not argv" ("copy:" ^ text) copied;
      Alcotest.(check string) "unchanged" text prepared
  | _ -> Alcotest.fail "payload changed"

let test_blank () =
  let result, events = run " \n\t " in
  Alcotest.(check bool) "blank rejected" true (Result.is_error result);
  Alcotest.(check (list string)) "no events" [] events

let test_copy_failure () =
  let result, events = run ~copy_failure:true "text" in
  Alcotest.(check bool) "offer failure" true (Result.is_error result);
  Alcotest.(check (list string)) "no key after failure" [ "copy:text" ] events

let test_failed_segment_restores_clipboard () =
  let result, events =
    run ~action:T.Shift_enter ~paste_failure:true "first\nsecond"
  in
  Alcotest.(check bool) "paste failure reported" true (Result.is_error result);
  Alcotest.(check (list string))
    "full text restored after key failure"
    [ "copy:first\nsecond"; "copy:first"; "paste"; "copy:first\nsecond" ]
    events

let () =
  Alcotest.run "prepared_text"
    [
      ( "clipboard delivery",
        [
          Alcotest.test_case "collapse applies before clipboard paste" `Quick
            test_collapse;
          Alcotest.test_case "shift-enter segments and full clipboard" `Quick
            (test_line_action T.Shift_enter "shift-enter");
          Alcotest.test_case "ctrl-j segments and full clipboard" `Quick
            (test_line_action T.Ctrl_j "ctrl-j");
          Alcotest.test_case
            "unicode and option-like text remain clipboard data" `Quick
            test_payload;
          Alcotest.test_case "blank text has no events" `Quick test_blank;
          Alcotest.test_case "failed segment restores full clipboard" `Quick
            test_failed_segment_restores_clipboard;
          Alcotest.test_case "clipboard failure sends no key" `Quick
            test_copy_failure;
        ] );
    ]
