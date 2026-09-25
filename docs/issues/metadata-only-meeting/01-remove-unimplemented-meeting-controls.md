# Remove metadata-only meeting controls

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Metadata-only Meeting mode and unimplemented meeting controls are removed; unsupported requests are rejected.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete (coordinated with PIE014). Integrated suite pending. Does not claim the whole audit. PIE016.

Meeting RPC types, CLI terms, `paths.meetings_dir`, and Assistant_service server cases are removed. Removed names fail as unknown method without admission change. Coordinator `Meeting_transcribe` and service named meeting actions were deleted by the coordinated worker. Remaining coordinator comment at `recording/coordinator.ml:199` is not a public advertisement. No compatibility fallback.

## Requirements

- PIE shall omit metadata-only meeting controls from its CLI, RPC request types and action bindings. ^piemeeting-zdhp

## Evidence

Both RPC servers implement Meeting_start/stop/toggle solely through coordinator mutation. Real Service849–883 does the same for named actions; it does not launch capture or a provider operation. Recording.Coordinator is mutable admission metadata, not an audio worker. Mode-reference census finds these mutation paths, duplicate Commands.Assistant helpers and the coordinator definitions; meetings_dir has no implementation use outside its declaration.

Actual isolated handler control: Meeting_start returns Started meeting-transcribe, then ordinary Ptt_transcribe admission returns Busy Meeting_transcribe. Probe uses only a synthetic state file, not a socket or microphone. Absence of capture is established by the source/caller trace, not inferred merely from the probe output.

## Disposition

Delete meeting CLI/RPC/action entries, mode-specific coordinator mutation and unused path/helper definitions after final reference reconciliation. Remove tests that certify metadata-only start as a real feature; retain regression coverage that removed operations cannot claim admission or block dictation. Update help/docs/bindings consistently.

Do not implement a meeting recorder as part of this cleanup, keep a success-shaped stub, or add compatibility routes. Ordinary transcribe/translate and real wakeword duties remain. Existing recordings and historical files are not authorized for deletion.

## Acceptance

Removed methods/actions are rejected through the normal unknown-operation path without state changes. CLI help/bindings no longer advertise them. Normal dictation remains admissible; no dead meeting mode or unused meetings_dir remains in authoritative code.

## Eta prerequisites

None. Removing nonexistent application work does not require a framework feature.
