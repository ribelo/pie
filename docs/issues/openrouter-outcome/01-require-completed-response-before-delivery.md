# Require a completed OpenRouter response before delivery

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

OpenRouter incomplete and empty outcomes are not treated as successful transcripts.

## Earlier implementation checkpoints (superseded)

Parent-reviewed complete (PIE030). Outcome gate in stt/openrouter.ml plus 8 public-path tests. Integrated suite pending. debug_response_content unchanged (PIE019).

## Requirement

- When PIE accepts an OpenRouter result for delivery, PIE shall require a successful completed outcome as well as valid nonempty text. ^pieoutcome-i9ej

## Evidence

Openrouter249–254 extracts response.message and discards response.finish_reasons before parsing the structured text.

A pure public-codec composition preserves Stop for completed, Length for incomplete and Other cancelled for cancelled. PIE extraction and structured decoding accept the same synthetic text in all three cases. The installed upstream decoder rejects failed status, which is a positive control.

This probe executes the public decoder and PIE helpers, not the whole HTTP operation or a real provider. Source inspection establishes that run_audio_responses uses those helpers without checking the preserved marker.

## Direction

Check the typed terminal outcome before discarding the response envelope. Reject incomplete, cancelled, missing or unrecognized outcomes and unexpected tool continuation. Do not infer completion from valid JSON alone. Preserve errors for stage-aware recovery under PIE008.

Keep protocol decoding in Eta. This application check does not require a new generic result framework or copying the provider decoder.

## Acceptance

Use a private HTTP provider fixture to exercise the public transcription and translation paths. Completed valid text succeeds; incomplete/cancelled text, absent markers, malformed content and failed responses must not reach delivery. Preserve typed provider failures.

## Eta prerequisites

The selected Eta artifact must preserve terminal outcome distinctions. Installed d06548b already preserves the demonstrated markers. Verify the same consumer contract after incoming dependency changes.
