module T = Pie.Input.Text_injection

let window id title =
  {
    Pie.Niri.id;
    title = Some title;
    app_id = Some "editor";
    pid = None;
    workspace_id = None;
  }

let target = window 42 "press title"

let check_delivery current expected_key () =
  let copies = ref [] and keys = ref 0 and lookups = ref 0 in
  let result =
    T.deliver ~target_window:(Some target) ~newline_rules:[]
      ~focused_window:(fun () ->
        incr lookups;
        current)
      ~copy:(fun text ->
        copies := text :: !copies;
        Ok ())
      ~paste:(fun () ->
        incr keys;
        Ok ())
      ~newline:(fun _ -> Alcotest.fail "unexpected newline")
      "secret text"
  in
  Alcotest.(check int) "one focus query" 1 !lookups;
  Alcotest.(check (list string)) "clipboard offered" [ "secret text" ] !copies;
  Alcotest.(check int) "paste count" (if expected_key then 1 else 0) !keys;
  match result with
  | Ok (_, outcome) ->
      Alcotest.(check bool)
        "reported outcome" true
        (outcome
        = if expected_key then T.Clipboard_key_attempted else T.Clipboard_only)
  | Error _ -> Alcotest.fail "delivery failed"

let test_unknown_target () =
  let keys = ref 0 in
  let result =
    T.deliver ~target_window:None ~newline_rules:[]
      ~focused_window:(fun () -> Ok (Some target))
      ~copy:(fun _ -> Ok ())
      ~paste:(fun () ->
        incr keys;
        Ok ())
      ~newline:(fun _ -> Ok ())
      "text"
  in
  Alcotest.(check int) "no key" 0 !keys;
  Alcotest.(check bool) "clipboard" true (result = Ok ("text", T.Clipboard_only))

let test_rules () =
  let rules =
    [
      {
        T.app_id = Some "EDITOR";
        title_regex = Some "press";
        action = T.Ctrl_j;
      };
      { T.app_id = None; title_regex = None; action = T.Shift_enter };
    ]
  in
  Alcotest.(check bool)
    "first matching rule" true
    (T.newline_action_for_window rules (Some target) = T.Ctrl_j);
  Alcotest.(check bool)
    "paste default, even missing focus" true
    (T.entry_method_for_window [] None = T.Paste)

let () =
  Alcotest.run "focus_rules"
    [
      ( "target-window guard",
        [
          Alcotest.test_case "same id despite changed title pastes" `Quick
            (check_delivery (Ok (Some (window 42 "renamed title"))) true);
          Alcotest.test_case
            "different id despite same app and title copies only" `Quick
            (check_delivery (Ok (Some (window 43 "press title"))) false);
          Alcotest.test_case "no focused window copies only" `Quick
            (check_delivery (Ok None) false);
          Alcotest.test_case "niri unavailable copies only" `Quick
            (check_delivery (Error (`Niri_error "unavailable")) false);
          Alcotest.test_case "no key-press target copies only" `Quick
            test_unknown_target;
          Alcotest.test_case "first matcher and paste default" `Quick test_rules;
        ] );
    ]
