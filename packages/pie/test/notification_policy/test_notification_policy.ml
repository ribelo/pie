module P = Pie.Desktop.Notification_policy

let test_allowlist () =
  let cases =
    [
      (P.Capacity_reached, P.Notify);
      (P.Clipboard_only, P.Notify);
      (Config_rejected, Notify);
      (Attempt_failed { retry_exhausted = true; audio_kept = true }, Notify);
      (Attempt_failed { retry_exhausted = false; audio_kept = true }, Log);
      (Attempt_failed { retry_exhausted = true; audio_kept = false }, Log);
      (Provider_outage { retry_exhausted = true }, Notify);
      (Provider_outage { retry_exhausted = false }, Log);
      (Capture_ignored { held_ms = 0 }, Log);
      (Capture_ignored { held_ms = 2999 }, Log);
      (Capture_ignored { held_ms = 3000 }, Log);
      (Capture_ignored { held_ms = 3001 }, Notify);
      (Low_input, Log);
      (Dead_input, Log);
      (No_transcript, Log);
      (Delivery_failed, Log);
      (Correction_skipped, Log);
    ]
  in
  List.iteri
    (fun index (event, expected) ->
      Alcotest.(check bool)
        (string_of_int index) true
        (P.decide event = expected))
    cases

let test_effect_routing () =
  let notices = ref [] and logs = ref [] in
  let report =
    Pie.Desktop.Notification.report
      ~notify:(fun title message -> notices := (title, message) :: !notices)
      ~log:(fun message -> logs := message :: !logs)
  in
  report P.Low_input "quiet" "recording continues";
  report (Capture_ignored { held_ms = 3000 }) "ignored" "tap";
  report P.Clipboard_only "clipboard" "paste manually";
  Alcotest.(check (list (pair string string)))
    "only actionable notification"
    [ ("clipboard", "paste manually") ]
    !notices;
  Alcotest.(check (list string))
    "demotions remain in the log"
    [ "ignored: tap"; "quiet: recording continues" ]
    !logs

let () =
  Alcotest.run "notification-policy"
    [
      ( "dict-f2vx dict-j9sw",
        [
          Alcotest.test_case "allowlist and strict boundary" `Quick
            test_allowlist;
          Alcotest.test_case "notify or log hook" `Quick test_effect_routing;
        ] );
    ]
