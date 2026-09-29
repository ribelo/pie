# Record faithful PCM with bounded publication

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Diagnostic recording owns private files, bounds RIFF output, and cleans up on every exit.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete (PIE025). Integrated suite pending.

## Design notes (accepted)

- Local file IO faults in the capture write path are deliberately `Cause.Die` via `Recording_io`, not invented `Pulse.Protocol_error` values. Typed Pulse failures stay Pulse; STT/record validation stays `Stt_error`.
- Capture audio remains a Pulse-error stream (`Pulse_error` only). There is no `%identity` / unsafe Stream error-row cast.
- Ox `Stream.map_error` is absent from `ox_stream.mli`. That is not a blocker after this composition (write defects + outer Pulse `map_error` + `catch_some` Timer_elapsed). Do not claim an upstream Stream API was added.

## Requirements

- When PIE records a diagnostic WAV file, PIE shall preserve captured PCM samples without packet-dependent automatic gain changes. ^pierecord-sjw3
- When PIE records to an explicit output path, PIE shall validate a finite positive duration before capture, keep memory bounded independently of duration and publish success only after verified file finalization. ^pierecord-zk7e

## Evidence

Commands.Record11–13 normalizes each received packet by default. Actual PCM helper control: identical four-sample audio has first sample1000 under whole-buffer normalization versus3932 when partitioned into two packets. This is a synthetic helper composition and source call trace, not a microphone/file end-to-end trial.

Record6–18 retains every packet, then40–43 manually opens/truncates/writes/closes the destination without all-exit cleanup. CLI duration is an unrestricted float; conversion to milliseconds occurs after acquiring Pulse recording. Header rate is hard-coded16000 rather than checked against negotiated format. Default CLI description says raw PCM despite default normalization.

## Disposition

Keep record as a faithful input diagnostic. Remove packet-local gain processing and the redundant --raw mode switch; record raw PCM consistently. Keep STT normalization where the real recognition path requires it—do not delete the shared normalization implementation merely because record no longer calls it.

Stream to a private owned output candidate instead of retaining all audio. Finalize the WAV header from validated format and actual byte count, check representable RIFF lengths and write/flush/close results, then publish to the requested path. Retain the command’s explicit replacement semantics but preserve a previous output on failed candidate creation/finalization. Close resources and remove only the owned temporary candidate on error/cancellation. Do not add a generic audio/file framework or partial-recording feature.

Validate duration/range before microphone acquisition or output mutation. An explicit file recording is not the dictation recovery buffer: enforce WAV/file-format bounds without silently borrowing the35-minute dictation cap.

## Acceptance

Synthetic PCM partitions produce identical raw WAV data; actual length/rate/header agree. Long streams keep resident buffering flat. Invalid/nonfinite/overflow duration fails before capture. Simulate provider-independent read/write/close failures and cancellation, asserting previous-output preservation and no FD/temp-file leak. Positive completion produces one valid WAV.

## Ox prerequisites

Use settled stream cancellation/finalizer contracts and real Pulse ownership. File publication and sample fidelity are PIE duties; no new Ox audio layer is needed.
