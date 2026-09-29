# Bound dictation buffering and report admission failure

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Capture, queues, result text, and recovery storage are bounded; the 100-attempt plateau passes.

## Earlier implementation checkpoints (superseded)

Provider slice parent-reviewed on packages/pie (Codex/OpenRouter/Dictation_limits frozen). Pulse/PTT/provider individual cap gates exist. Retained-store/physical plateau has a private e2e 100-attempt CHECKPOINT (run 0067DLRM, fd 15→15) against current Service; bc9 retry loop still open, so whole PIE013 is not accepted until a fresh rebuild/rerun after Service freeze. No deployment. Does not claim the whole audit.

Provider: 1MiB queue PCM + 1024-item cap (empty PCM is not enqueued), 64MiB capture before base64, 1MiB UTF-8 result with final replacing partials. Offers after failed/closed are visible Stt_error. Publish mutations run only in the executing finalizer. Focused gates: provider-budgets 19 (`PPWI43RE`); prior realtime-outcomes 14 and streaming_resampling 16 retained.

## Requirements

- While a dictation attempt is active, PIE shall enforce the queued-audio, captured-audio and retained-output byte budgets defined below. ^piebudget-o7ws
- If a session cannot accept offered audio because it has failed, closed or exhausted its budget, then PIE shall report that outcome to the attempt owner rather than acknowledge successful admission. ^piebudget-nf4j

## Target budgets and tradeoffs

For the ordinary dictation path:1MiB PCM per staging queue,64MiB total captured PCM and1MiB retained UTF-8 result text. Apply the queue limit to Pulse audio staging, Ptt_loop input and provider submission, not only the final unsent queue. These are explicit proposed engineering limits, not measured performance optima or new user-facing tuning flags. At16kHz mono S16LE, the capture limit is roughly35 minutes. PIE016 removes the unimplemented meeting route. Explicit diagnostic file recording has the separate PIE025 contract.

Enforce limits before growing buffers/files or constructing expanded base64/JSON bodies. A bounded item count alone does not establish a byte bound. Ptt_loop126 holds1024 variable-size events; apply audio-byte admission before retaining payloads there. Bound incoming packet allocation too. Keep control delivery live under the PIE009/004 ownership rules. Keep only the latest recovery candidate per PIE008, subject to the same audio/text bounds. Historical files are not authorized for deletion.

## Evidence

Codex with_stream_session1080 uses Queue.unbounded; offer_chunk1106 ignores send errors. receive_result809–814 accumulates deltas and final text without a total byte limit. Ptt_loop retains the full capture, and Retry_store reads a complete file. These are source-backed paths, not a live memory-exhaustion reproduction. HTTP response/body limits elsewhere do not bound this WebSocket/application state.

## Implementation direction

Keep a bounded producer/consumer path and explicit overload/cancel/closed outcomes. Do not block the only keyboard/control reader indefinitely behind provider backpressure (see PIE009’s equivalent Pulse failure). Do not silently drop PCM, spawn per-chunk workers or replace one unbounded queue with another.

Allocate attempt queue/promise/state inside its execution owner. Replace Async_completion1013–1069 with the existing Ox promise where its real duties fit; preserve queue/worker cancellation in the owning scope, not a custom register/resume state machine. Guard duplicate operation finish at the attempt boundary if needed, not through a second generic promise implementation.

## Acceptance

Use synthetic stalled and failed providers, variable-size chunks, boundary byte counts and cancellation while full. Assert retained bytes/worker count plateau and rejected offers remain visible. Exercise large retained-file selection before loading payload, base64 expansion and output deltas/final replacement without double accounting. Verify no rejected/partial recording reaches successful delivery.

## Ox prerequisites

Use settled Queue admission/closure, Promise completion and structured cancellation, plus PIE002 physical retirement. Existing primitives suffice; no new buffering or completion package is needed.

Pulse slice (this worker): Dictation_limits.queue_pcm_bytes=1048576 applied as per-stream PCM queue cap before try_offer_now/Bytes.to_string; item cap 64 unchanged. Declared native-frame length above Protocol.max_frame_bytes (same 1MiB) is Protocol_error and closes the connection without reading the body. Ordinary Recording_overload still detaches only that stream so delete/control remain. Ptt_loop/provider byte caps are other workers. Capture/result UTF-8 budgets not in this slice.
