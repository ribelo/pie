# Use monotonic elapsed time for capture

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Capture timing uses monotonic time.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete. PIE026. Integrated full-suite still pending; this is not whole-audit acceptance.

Standalone fixture `packages/pie/test/capture_clock` (own dune stanza) plus 21 existing `ptt_capture` cases in `test_pie` (clock argument only). Built with `--build-dir` `/home/ribelo/projects/ribelo/pie/.scratch/capture-clock-build/eta-current`, `DUNE_CACHE=disabled`, staged Eta `OCAMLPATH` `/home/ribelo/projects/ribelo/ocaml/Eta/.scratch/pie-audit-current-build/install/default/lib` plus matching stublibs first. No shared install. Alcotest `-o` logs under `.scratch/capture-clock-build/alcotest`.

Parent accepted unmasked 6/6 in 3.082s, run `4I0CL74T`, after 21/21 `ptt_capture` in 106.013s. Hold/post-roll elapsed and post-roll sleep use required `~mono_clock:Eio.Time.Mono.t`. `timestamp()` remains `Unix.gettimeofday` wall time. Tests never change the host clock: Eio mock Mono vs independent fake wall ±60s; ineligible wall jump does not finish. Explicit `with_background` terminal watcher + private `capture_clock.stop` `catch_some`; 7s real deadline always fails; other errors fail. Mock pump/ticker live on the outer switch and join in `finally`. `ptt_loop` source ownership handed to bc9 for the typed Pulse_error union; no further clock edits from this slice.

## Requirements

- While PIE measures capture hold duration or post-roll, PIE shall use monotonic elapsed time so wall-clock adjustments do not change capture eligibility or extend/shorten recording. ^pieclock-d6dn

## Evidence

Ptt_loop32 uses Unix.gettimeofday for now_ms, feeding capture start/release, held duration107–113, total duration166 and post-roll remaining201–213. Capture25–28 computes release_at+2000-now. A backward wall step of60 seconds immediately after release therefore gives62000ms remaining instead of2000ms (source calculation, not a live clock experiment). A forward step can terminate the post-roll early.

No system clock was changed; no historical clock jump or resulting live failure is claimed.

## Implementation direction

Pass a monotonic time source into the real input loop and use one time domain for elapsed calculations and remaining-duration scheduling. Preserve the existing two-second post-roll behavior and minimum-hold policy. Installed Eio.Time.Mono.now provides Mtime.t; no new Eta clock package is necessary.

Keep wall time for human timestamps, coordinator status dates and absolute credential-expiry comparisons. Do not globally replace every Unix.gettimeofday call. Remove duplicate timestamp formatting only where it has no remaining consumer after the other deletions.

## Acceptance

Synthetic monotonic time controls hold/post-roll tests. Independently move a fake wall clock forward/backward and verify unchanged eligibility, post-roll duration and nonnegative elapsed capture time. Cover release/repress during post-roll. Never change the host clock for tests.

## Eta prerequisites

Use existing Eio monotonic time and relative delays, verifying their selected bridge during implementation. This does not add total deadlines to established realtime streams.
