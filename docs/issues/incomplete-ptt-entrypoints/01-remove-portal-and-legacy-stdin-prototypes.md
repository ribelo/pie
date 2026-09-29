# Remove portal and legacy stdin PTT prototypes

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Obsolete portal, PTT, and interactive entrypoints are removed.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete on packages/pie. Integrated suite pending. No deployment. Does not claim the whole audit. PIE022.

Deleted Commands.Ptt, Commands.Ptt_portal, Commands.Stt_interactive, Wayland.Portal and the five CLI/help registrations (ptt-portal, ptt, ptt-transcribe, ptt-translate, stt-interactive). Standalone stdin PTT convenience is removed per this decision; the real assistant/service transcribe/translate path is retained (no extra assistant flag cleanup, no replacement aliases). Legacy parse_busctl_json_event and its test_pie coverage removed; native binary parser kept. Focused gates: removed-entrypoints 2, cli_status 9, keyboard_ownership 14, keyboard_bus_boundary 32. lib/pie install artifacts untouched. No --stdin_ptt existed in packages/pie.

## Requirements

- PIE shall remove the incomplete ptt-portal, ptt, ptt-transcribe, ptt-translate and stt-interactive entrypoints rather than advertise them alongside the actual service dictation workflow. ^pieentry-omwa

## Evidence

Commands.Ptt38–64 acquires a recording stream before waiting for the first Enter. Buffered audio can therefore precede the advertised start boundary. The selected mode only affects a printed label; both modes call Openrouter.transcribe with hard-coded whisper-1/en, not configured translation. This is source-backed, not a provider/microphone reproduction.

Wayland.Portal75 and Commands.Ptt_portal19 only issue separate busctl calls and run a stdout monitor. They do not dispatch Activated/Deactivated events into recording, own a continuing session interaction or close it. create_session ignores command status and extracts a quoted substring; BindShortcuts constructs a literal1 before the nominal object-path argument. No live portal operation or API-conformance trial was performed. CLI describes portal binding registration, not full dictation; this prototype cannot justify replacing the real keyboard monitor.

## Architecture decision

Remove these modules and Commands.Stt_interactive (235 lines total), with their five CLI registrations/help entries. Stt_interactive is the same pre-start stream/hard-coded transcription path, and calls exit0 inside capture on empty audio instead of settling through its effect owner. Preserve the actual Service capture/provider/delivery implementation. Standalone stdin PTT convenience is lost. The separate stt-interactive review found a duplicate, not a valid replacement. Do not keep aliases, repair only the translation label, or introduce a second parallel dictation engine.

Keep the native keyboard-monitor seam for the presently implemented service input. Apply PIE021 ownership and finish its narrow D-Bus address/framing/identity contract. Replacing it with a maintained client requires actual OxCaml/Eio compatibility evidence; no such replacement is asserted here. Remove the legacy parse_busctl_json_event helper and its test-only coverage after caller reconciliation; it does not exercise the live binary parser.

## Acceptance

Command tree/help/bindings no longer advertise removed entrypoints; unknown command errors occur before microphone or provider access. Service transcribe/translate retains configured model/language and real press/release behavior. Remove dead exports/imports and tests protecting only removed prototypes. No portal permission prompt or real dictation is necessary for these checks.

## Ox prerequisites

None for deletion. Actual service ownership uses the existing effect/resource contracts; do not invent an Ox application framework to rescue prototypes.
