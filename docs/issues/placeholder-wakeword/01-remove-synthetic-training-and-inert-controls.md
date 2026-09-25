# Remove synthetic wakeword training and inert controls

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

The inert wakeword feature and its unsupported configuration are removed.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete on packages/pie. Integrated suite pending. No deployment. Does not claim the whole audit. PIE024.

Deleted Wakeword.Pipeline/Trigger, three wakeword command modules, CLI wakeword/wakeword-tune/wakeword-train, Pie.Wakeword, wakeword Paths declarations, and Coordinator.Wakeword. Config.Stt dropped only wakeword_enabled and the three inert dictation floats; leftover keys/tables raise Config_error unsupported wakeword. Native PTT thresholds/keysyms and the prompt loader are unchanged. Genuine meter CLI retained as RMS/peak diagnostic. No user clips/models/directories deleted. This does not claim a wakeword runtime ever existed. Focused gates: removed-wakeword 4, config-boundary 24, cli_status 9.

## Requirements

- PIE shall remove placeholder wakeword commands, inactive wakeword settings and model/training claims that have no speech-trigger implementation. ^piewake-bq2j

## Evidence

Wakeword.Pipeline5–7 derives its score only from RMS; its model contains threshold and smoothing, not learned word features. Trigger.update ignores its previous state and compares against0.5. Telemetry explicitly labels its scoring synthetic. Training creates directories and writes zero-sample/onnx_ready=false metadata, but CLI help says Train a custom wakeword model. Tuning reads no training samples and writes a threshold derived from hard-coded score lists.

Assistant.Wake builds read_loop32–41 then discards it57. Actual Service only includes wakeword fields in config/status/reconciliation; no capture trigger consumes them. Authoritative reference census found no alternative wakeword worker; tracked Python/JS/TS/shell/ONNX inventory found no external engine artifacts. This concerns reviewed current implementation, not every installed/untracked historical asset.

## Disposition

Delete Wakeword.Pipeline/Trigger and the three wakeword command modules (135 lines), CLI registrations/help, obsolete config fields/slices, unused coordinator mode, paths and public exports. Assistant.Wake and graph wiring are already deleted by PIE003; do not double-count them. Preserve a genuine audio meter only if its separate review shows useful diagnostic behavior.

No new model training or wakeword engine is authorized by this cleanup. Existing training clips, models, user directories and historical tuning files must remain untouched. Remove tests that merely certify fake calibration/loudness as word recognition, and update example configuration/help honestly.

## Acceptance

No command claims model training or hands-free activation; unsupported removed commands fail before microphone or file writes. Ordinary PTT transcribe/translate remains functional. Source/caller census contains no orphan wakeword configuration/modes/exports. No deletion of user assets.

## Eta prerequisites

None. There is no actual wakeword effect lifecycle to preserve beyond removing the already-inert graph. Do not add Eta components to implement an unrequested feature.
