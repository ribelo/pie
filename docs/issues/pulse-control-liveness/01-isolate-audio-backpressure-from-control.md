# Isolate audio backpressure from control replies

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Pulse liveness, bounded queues, and required-input failure propagation are explicit.

## Earlier implementation checkpoints (superseded)

Parent-reviewed bounded adapter implemented in Pulse.Client (PIE009). Service-level active-attempt handling remains PIE003/004; not claimed by the six focused tests.

## Requirements

- While an audio consumer is stalled, PIE shall continue processing Pulse control replies within the configured request timeout when those replies have arrived. ^piepulse-y1ho
- If a recording stream exhausts its bounded audio queue, then PIE shall report an explicit recording-overload failure instead of silently dropping audio or blocking the shared control reader. ^piepulse-s6fm

## Evidence

Actual workspace control check-pulse-backpressure.sh exits0. Synthetic Unix server, supplied zero cookie,250ms request timeout, five-second outer deadline. With63 queued unconsumed chunks, server-info succeeds. With65, the exact typed request timeout is observed although the server sent its response. Capacity is64. Record/client scope cleanup completes in both cases; fixtures are removed. No live Pulse, audio or provider access.

Client119–127 previously performed Queue.send in the single packet-dispatch reader. Full audio admission therefore stopped parsing unrelated replies.

Parent-reviewed adapter: `try_offer_now` in `Effect.sync`; `Full | Dropped` detaches and `close_with_error Recording_overload`; already-closed queues detach only. Capacity remains 64. Focused `pulse_liveness` 6/6 (63/64 server-info+delete; 65 overload not disconnect, 64 ordered then error; cancel; broken connection). Service active-attempt handling is PIE003/004.

## Implementation direction

Use nonblocking audio admission. On full, fail the affected recording explicitly and keep parsing control traffic. Preserve order for admitted chunks and never deliver a truncated recording as successful recognition. Do not create one worker per packet, silently drop chunks, switch to an unbounded queue, or increase capacity to conceal the problem.

Keep stream overload distinct from a broken Pulse connection. Integrate its visible operation failure with PIE003/004. Clean up the failed stream with the existing finite control timeout and maintain application shutdown ownership.

## Eta prerequisites

Installed Queue.try_offer already returns Full without waiting for bounded queues (queue.mli132–136); try_offer_now is the synchronous counterpart. Ordinary Queue.offer on bounded queues can wait and is not a substitute. Use the settled atomic admission/close contract and verify overload behavior against the target artifact. No new Eta queue or dispatcher abstraction is needed.

## Acceptance

Promote63/64/65-capacity boundary controls, then assert that overload is visible on the recording stream while server-info and delete replies still complete. Cover consumer cancellation, queued tail ordering, concurrent command replies and connection failure. Count retained workers/bytes and ensure no per-packet task growth.
