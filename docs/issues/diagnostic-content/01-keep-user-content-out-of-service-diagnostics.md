# Keep user content out of service diagnostics

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Diagnostics omit sensitive content and child output uses discard sinks.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete on packages/pie. Integrated suite pending. No deployment. Does not claim the whole audit. PIE019.

Deleted CAMELPIE_*_DEBUG payload dumps. Provider/OAuth/WS errors are local category + HTTP/WS numeric status only; remote message/code/provider/body/JSON snippets are not diagnostic strings. Sh.run discards stdout and stderr on a non-retaining Flow.Pi sink (unit helpers: wtype, xdotool, notify-send; none needed helper stdout). Sh.run_capture keeps stdout as Niri's return value and never echoes it. Clipboard spawn discards stdio; the protocol is Wayland selection, not helper stdout. Subprocess failures are basename + exit class; ordinary spawn I/O (Eio.Io, Unix execve, Failure execve:) is basename + io-failure without command context. Cancelled/Multiple/unknown non-IO defects are rethrown. Foreground transcript CLI prints remain outside Sh.run. Focused gates: diagnostic-content 13, openrouter_outcomes 8, prepared_text 21.

## Requirements

- PIE shall restrict service diagnostics to operational metadata and exclude dictated text, audio, prompts, credentials, raw provider bodies and content-bearing subprocess arguments. ^piediagnostic-qqwu

## Evidence

Service467–470 prints successful transcript/translation text to stdout unconditionally. Its startup dump894–897 also serializes configured prompt contents from314–315; remove those fields from diagnostic projection. Read-only systemctl show returned StandardOutput=journal and StandardError=inherit for the live unit. This establishes unit routing plus current-source behavior, not independent confirmation of the deployed binary’s exact print behavior or retained journal contents; no transcript logs were inspected.

Actual synthetic helper control: both CAMELPIE_CODEX_REALTIME_DEBUG=0 and CAMELPIE_OPENROUTER_DEBUG=0 enable payload dumps. They test nonempty strings, not booleans. Dumps retain first2000/1000 characters; truncation is not redaction. Codex send_text includes encoded audio and configured instructions; response logging can include recognized text.

Sh.run failure from a harmless false executable includes the synthetic argv marker. Wtype.type_text and Xdotool.type_text place normalized text in argv, so serializing their exception can disclose it. Service459–465 forwards injection errors to desktop notifications and the PTT error path. Token_refresh_failure376–394 also embeds provider body prefixes without the debug gate.

## Implementation direction

Delete raw payload debug helpers and successful-text service printouts. Keep operation/provider/model, phase, status/error category, byte/character counts and timing where needed. Do not add another logging framework or a raw-content fallback. Projected errors must be safe before reaching logs, notifications or persisted last_error. Do not assume Printexc.to_string is content-free.

Preserve explicitly requested transcript output from foreground commands and the intended wtype/clipboard delivery sink; diagnostic stdout is not that sink for the background service. Preserve approved recovery content under PIE005/008 ownership rather than duplicating it into logs.

Use metadata-only subprocess failure projection and avoid content in argv where the selected helper supports stdin. Existing argv vectors and -- separation are good defenses against shell/option interpretation; the demonstrated issue is disclosure, not shell injection. Raw provider bodies/remote messages are not safe merely because they are errors.

## Acceptance

Synthetic unique markers through successful delivery, provider error, malformed response, helper nonzero exit and notification failure. Capture stdout/stderr/structured diagnostic attributes and assert markers absent, while intended delivery/output remains correct. Test debug environment unset,0,false and1 after removing the content-dump path. Do not inspect real journals or credentials for this regression suite.

## Eta prerequisites

Keep useful Eta observability metadata. PIE owns data classification and safe error projection; incoming runtime fixes do not remove application prints or raw body strings.
