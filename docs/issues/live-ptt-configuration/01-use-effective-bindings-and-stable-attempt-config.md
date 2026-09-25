# Use effective bindings and stable attempt configuration

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Effective configuration reloads retain last-good settings and preserve physical key ownership.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete and integrated. PIE006. Whole-suite pending. Not deployed; no historical crash causality claimed.

One published `Config.Stt.t` with CLI keysym overrides applied (`with_cli_overrides`) on startup and each successful reload; colliding/invalid reload keeps last-good. Sole recognizer is `make_live_recognizer`. `down` tracks every recognized physically-down PTT key, including a busy second key and Mod5 latch key, without making those owner. Only the capture owner's keyup emits Release (RPC Release stays explicit control). Latch toggles only on a fresh physical press. Autorepeat Press only while that key owns an active admission. `recognize_robust` removed. Attempt RMS/peak sampled with `on_press` in one `Effect.sync` thunk. Paste policy remains independently unresolved.

## Requirements

- When a CLI PTT keysym override is supplied, PIE shall use that override for actual recognition and effective-configuration reporting, including after config reload. ^piebindings-v4jg
- When the physical key that started a capture is released, PIE shall release that capture even if its configured binding changed while the key was held. ^piebindings-2dn2
- While a capture is active, PIE shall evaluate its capture thresholds using the configuration selected when that attempt began. ^piebindings-4a2t

## Evidence

Actual workspace control check-recognizer-reload.sh exits0: live_release_after_reload=lost; tested_old_helper_release=preserved. It calls Service.recognize_robust and Commands.Assistant.make_live_recognizer with identical synthetic press/reload/release, plus a private temporary coordinator state file. No real keyboard events, microphone or service interaction.

Service1137–1145 supplies current config keysyms to recognize_robust. Its active-mode release checks still compare against those new keysyms. Commands.Assistant.make_live_recognizer44–62 remembers physical keycode; tests2285/2309 target that helper, but Commands.Assistant.run579–583 delegates to the separate Service implementation.

CLI pie.ml475–500 advertises and forwards overrides. Service699–703 computes effective values, but its real recognizer1137 onward and on_ready1183 onward use current config rather than those values. Effective-binding unit tests of the old command helper do not establish actual command behavior. This override finding is source-backed, not a launched-service reproduction.

Service1194 captures config for provider/delivery setup, while capture_thresholds1176–1179 reads mutable current config on each call; an active recording can be admitted and finalized under different thresholds.

## Implementation direction

Resolve config plus CLI overrides once per validated publication. Give each capture its relevant configuration snapshot and physical-key ownership. Keep the intentional Mod5 latch behavior while handling ordinary press/release consistently. Update reporting from the same effective state. Do not maintain both a correct test-only recognizer and a different production recognizer.

Remove unused duplicated Commands.Assistant logic after checking consumers; transfer needed behavior/tests to the actual application seam. Coordinate with PIE003 removal of shadow component configuration/replacement. Do not introduce compatibility recognizers or a second config manager.

## Acceptance

Exercise the real command-to-service wiring with synthetic events and config: override before/after reload; press old key then reload then release; modifier changes while held; Mod5 latch; invalid reload; config change during threshold evaluation. Assert effective reporting matches actual recognition. Ensure a failed reload leaves the last valid published state.

## Eta prerequisites

No new Eta configuration abstraction or component replacement contract is needed. This is application-owned state; ordinary scoped lifecycle from PIE004 remains applicable.
