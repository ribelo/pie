# Distinguish readiness and terminal status

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Readiness and successful terminal outcomes are explicit; unverified dedicated Translation completion fails closed.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete on packages/pie. Whole-suite pending. No deployment. Does not claim the whole audit. PIE011.

Readiness requires `session.updated` matching the sent `session.update`; `session.created` and `transcription_session.updated` are not equivalent acks. Completion is per selected mode: transcription only `input_audio_transcription.completed`; conversation only `response.done` with `status=completed` (failed/cancelled/incomplete/missing/malformed never succeed from partials).

Verified integration limit: the dedicated Translation path currently has no established provider completion contract. Local drain expiry and foreign `response.done` cannot produce `Ok`. Conversation-mode translation still uses completed `response.done`. This does not claim the provider Translation feature as a whole works, nor that the API universally has no completion event.

Focused gates: realtime-outcomes 14 (`E6QOM055`), streaming_resampling 16 (`WCW75DAQ`). Codex_realtime source freeze for this ticket.

## Requirements

- When PIE begins sending session audio, PIE shall have received acknowledgement of the requested session configuration. ^pierealtime-i92i
- If the provider reports a failed, cancelled or incomplete terminal response, then PIE shall expose that outcome rather than report successful recognition from partial text. ^pierealtime-00da

- If a local translation drain ends before provider completion is established, then PIE shall report an incomplete outcome rather than completed translation. ^pierealtime-c2va

## Evidence

Actual helper control check-realtime-semantics.sh passes: session.created stops the initial readiness reader; parse_event converts a response.done containing response.status=failed into the status-free Response_done constructor. receive_result815 leaves state unchanged on that constructor; finish_result916–926 returns stored final/delta text when state.error is absent. The probe verifies decoder/readiness/finish helper behavior, not a complete network session.

session_event_types88–94 and initial_receive_should_stop826–833 conflate creation with update acknowledgement. parse_event332 discards the entire terminal response status. Broad event aliases and parsing twice in predicate/fold do not establish operation state integrity.

## Implementation direction

Decode each message once into the relevant typed provider event, preserving terminal status and safe failure details. Separate connection creation, requested configuration acknowledgement, active output and terminal outcome. Match the selected provider protocol’s actual acknowledgement rather than any Session event. Keep partial text distinguishable from completed recognition; do not automatically inject it as success.

The fixed2000ms close in buffered/streaming Translation send paths is a local heuristic, not a provider completion acknowledgement. Use the selected protocol’s supported completion signal; if none can be established, expose incomplete output rather than invent a successful terminal event. Do not merely increase the sleep.

Use the existing streaming scope and PIE002 ownership correction. Do not add a generic event bus or provider-state framework. Preserve the established-stream deadline exemption; readiness/setup deadline policy must be explicit rather than inferred from receiving session.created.

## Acceptance

Local scripted peer: created without updated; configuration rejected; valid update acknowledgement; successful terminal response; failed/cancelled/incomplete terminal status after partial output; malformed terminal payload. Assert audio is not sent before acknowledgement and partial failed output is not reported as successful delivery. Cover relevant transcription/translation/conversation modes without assuming all aliases have identical semantics.

## Ox prerequisites

No new Ox event abstraction is needed. Use settled typed causes, stream termination and PIE002 physical cleanup. This is provider-protocol interpretation owned by PIE.
