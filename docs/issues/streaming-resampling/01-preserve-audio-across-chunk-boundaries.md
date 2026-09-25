# Preserve audio across chunk boundaries

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Streaming resampling has bounded progress and sample handling.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete. PIE012. Integrated full-suite still pending; this is not whole-audit acceptance.

Standalone fixture `packages/pie/test/streaming_resampling` (own dune stanza, `test_pie` untouched) built with `--build-dir` `/home/ribelo/projects/ribelo/pie/.scratch/resampling-build/dune` and staged Eta `OCAMLPATH` `/home/ribelo/projects/ribelo/ocaml/Eta/.scratch/ws-reader-settle-build/install/default/lib` plus matching stublibs. No shared install. Alcotest `-o` logs under `.scratch/resampling-build/alcotest`.

Green after parent review corrections and bounded-advance follow-up: pie-linked 16/16 in 0.026s, run `BP6HJD6P`; pie-free bounds 2/2 in 0.000s, run `A82EMFTR`. Helper rejects odd PCM (including equal-rate) with `Invalid_argument`; buffered and streaming convert framing/rate errors to typed `Stt_error` (no Die, no silent odd-byte drop). Checked int multiply/add; unrepresentable rates/count math return `Resample_error` rather than `max_int` length clamps. Advance shifts only retained samples then jumps `q` in O(retained), not O(ratio). Push checks whole sample-count before mutation; arithmetic errors are sticky like format errors. Translation 20ms drain is a tested configuration only — drain 0 still races this fake WS and is not a universal tail-delivery claim. Repeat finish after odd-byte error stays failed.

## Requirements

- When the same valid PCM input is partitioned into different stream chunks, PIE shall produce the same resampled sample sequence and total sample count after end-of-stream settlement. ^pieresample-bydg

## Evidence

Codex resample_s16le_mono347–367 starts interpolation phase at0 and floors output length for each call. Streaming send_chunk987–992 applies it independently to each chunk. Actual-helper probe: four16kHz samples resampled to24kHz produce six samples as one buffer, but five when split1+3. A2+2 split keeps count but changes interpolated values. Source and synthetic control establish chunk dependence, not a measured live microphone distortion rate.

## Implementation direction

Own resampling phase and boundary sample/lookahead across the session; finalize the tail once. Alternatively negotiate and propagate a consistent native capture format if all real callers can use it, but do not change a global sample rate while leaving hard-coded duration/WAV assumptions behind. Do not build another generic audio framework.

Validate sample alignment at the PCM seam. Preserve identical-rate passthrough and define rounding/end behavior once. Keep typed format/rate errors separate from cancellation.

## Acceptance

Compare whole-buffer reference with every partition of short synthetic PCM sequences, including single-sample chunks, empty chunks, up/down/equal rates and stream end. Check exact output count and samples under the selected rounding rule. Exercise the actual streaming sender, not only a standalone resampler.

## Eta prerequisites

None beyond per-session state/resource ownership. Sample phase belongs to the application audio adapter.
