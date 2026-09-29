# Update capture eligibility statistics incrementally

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Incremental PCM eligibility avoids repeated capture concatenation.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete and integrated. PIE007. Whole-suite pending. Not deployed; no historical crash causality claimed. Ptt_loop frozen pending PIE013 budget design.

Per-attempt PCM16LE accumulator (count, sum-squares, peak, odd-byte carry). Terminal odd byte is typed `pcm:framing` (no pad/drop, no successful finish). Eligibility uses the accumulator; concat only for `Buffered` finish. `Streaming` finish is duration only. Does not claim bounded whole-capture retention (PIE013).

## Requirements

- When a PCM chunk arrives for an active capture, PIE shall update eligibility statistics without copying previously accumulated PCM. ^piecapture-nblv

## Evidence

Ptt_loop237–256 repeatedly concatenates all accumulated chunks and scans RMS/peak until eligibility succeeds. Dead_input treats any nonzero chunk as input; therefore quiet nonzero audio can remain on this path for the entire hold.

Bounded actual-helper composition check-quiet-capture-copy.sh exited0:128 chunks/524288 input bytes copied33816576 prefix bytes, allocated34034256 bytes;256 chunks/1048576 input bytes copied134742016 prefix bytes, allocated135570512 bytes. Fixture asserts input is nonzero and remains below eligibility thresholds. Copy count equals4096*n*(n+1)/2.

This composes the same production helpers and branch conditions, not the complete PTT event loop. It establishes the path’s algorithmic cost, not a measured live-daemon OOM or whole-pipeline benchmark.

## Implementation direction

Maintain sample count, sum of squares and peak per attempt. Preserve current threshold decisions within an explicitly tested floating-point tolerance. Concatenate only when a buffered provider truly needs a contiguous clip. Streaming finish currently ignores its clip payload while the generic loop still creates it; simplify that interface so unused whole-recording copies are not mandatory. Do not introduce another generic audio-processing framework.

Keep bytes needed for recovery/initial streaming handoff under an explicit owner; statistics are not a replacement for the audio itself. Memory/retention limits remain a separate policy disposition. Validate odd-byte/sample framing at the actual transport seam rather than silently changing audio format.

## Acceptance

Drive the actual PTT loop with quiet nonzero audio, silence, threshold crossings, post-roll continuation and cancellation. Compare decisions with a whole-clip reference. Demonstrate linear eligibility work/copy growth as input doubles, and ensure streaming finish does not force an unused complete clip allocation. Use synthetic PCM only.

## Ox prerequisites

None beyond the existing effect/stream lifecycle contracts. Eligibility statistics are application-owned state, not a new Ox package.
