# Own recording admission through attempt settlement

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Identity-owned admission lasts through provider settlement; native cancellation and compound failures are preserved.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete and integrated. PIE004. Whole-suite pending. Not deployed; no historical crash causality claimed.

Identity-only admission release (`Coordinator.stop` removed). One attempt owner: pause latch, acquired-attempt scope racing `await_cancel` vs body, structural `Pulse_error | Ptt_error` on `Ptt_loop.run`, exact Exit replay with status observation in `finally`, `settle_all` finally-chain that rethrows Cancelled/unknown defects and maps only Unix/Sys IO to `recording:<duty>`.

Focused evidence: assistant_owner 16/16 including three live Service gates (pre-handle then fresh press; delayed settle vs same-mode id; busy keyboard+RPC then fresh cycle), PTT 21/21, capture_clock 6/6, production Codex stream fixture green. Clipboard paste policy remains unresolved independently and is not PIE001.

## Requirements

- When a recording attempt ends, PIE shall release that attempt’s admission and close its retained-audio writer on success, failure and cancellation. ^pieattempt-ch9l
- If operation setup fails before a recording handle is available, then PIE shall report that failure and remain able to admit a subsequent recording attempt. ^pieattempt-zc59
- When an attempt performs delayed cleanup, PIE shall preserve admission owned by a different attempt. ^pieattempt-s64m
- When a press arrives while an operation owns admission, PIE shall reject that press for the busy operation instead of replaying it as a fresh attempt after settlement. ^pieattempt-wsly

## Evidence

Actual workspace Ptt_loop with synthetic Scoped setup failure stops after one of two queued Press requests. The raw cause contains the setup failure and sibling cancellation. Current Eta.Exit.render_error omits it; wrapping with ignore_errors removes the typed failure but retains Error(Interrupts). See .scratch/research/core-audit/ptt-admission-r11.md. This is not a full Service/systemd reproduction.

Service.on_press658–665 reserves Coordinator admission before constructing/entering the handle. Ptt_loop319–324 catches only failures after a handle is available; Scoped setup and on_press sit outside that handler. Coordinator.stop149–163 matches only the mode, not an attempt identity. Service clip/stream finish releases the mode before provider/delivery completion479–594; retry has a separate worker1212 onward and shares that coordinator. Follow cancellation/retry interleavings rather than assuming one event loop makes all attempt ownership identical.

Service.retain_handle610–633 closes the writer in finish/cancel, not an all-exit finalizer. Its cancel sequences file closure before underlying cancellation. Ptt_loop typed-error recovery calls cancel, but interruption/defect and failure before handle establishment require independent settlement. Retry_store26–45 lazily opens an output channel and relies on explicit close. These are source-level cleanup gaps, not a measured regular-file leak in the live daemon.

Round36 actual Ptt_loop control blocks handle.finish, forwards a second press/release pair while blocked, then releases finish. on_press is called again after settlement. Promise/rendezvous barriers establish ordering without scheduling sleeps. Ptt_loop192 awaits finish inside its sole state consumer while134–152 continue forwarding into the1024-item input queue. This proves stale callback admission, not a second provider invocation or historical crash attribution.

## Implementation direction

Use one attempt owner for admission, optional retained writer, recording/provider handle and final settlement. Return admission on pre-handle failure. Run required cleanup for every exit; one failing cleanup action must not skip the other. Stop matching cleanup solely by transcribe/translate mode where a later attempt may own that mode. Distinguish recording stopped from the operation still processing. Keep input/admission decisions responsive while provider/delivery completion runs in an owned worker. Reject busy press cycles when received; retain their release semantics without creating a later attempt. Do not blindly drain the queue after completion, which could discard genuinely fresh input. Admission needs one authoritative owner, not a second unsynchronized busy flag.

Avoid parallel independent handle protocols and duplicated keyboard/control press branches. Factor the common press/attempt lifecycle rather than adding a generic state-machine framework. Keep recoverable operation failure inside the PTT loop; required input-worker failure belongs to the application owner and must remain visible under PIE003. Do not turn cancellation into a generic success or erase a real failure from a composite cause.

## Acceptance

Exercise the actual Service admission seam with synthetic prompt/provider failures before a handle exists; assert the first reservation is released and a second press works. Test offer, finish, delivery, writer-close and cancellation failures. Keep a writer reachable while checking actual closure, so GC cannot masquerade as a finalizer. Interleave delayed cleanup with a new same-mode attempt/retry and assert ownership is preserved. Cover keyboard and RPC triggers through the shared path. Gate provider/finish completion and submit a full press/release cycle during it: no later attempt may be created from that cycle. Then submit a fresh cycle after settlement and prove it is accepted. Verify failure/cancellation and queue-pressure variants with synthetic inputs; do not turn delayed control into fresh microphone/provider acquisition.

Retain useful recovery material on failed delivery under the separate retry policy; closing an output channel is not permission to delete it. Do not read real retained recordings in tests.

## Eta prerequisites

Use the settled all-exit finalizer and structured-worker contracts, plus PIE002 transport retirement. Confirm mixed Fail+Interrupt rendering and observer behavior against the target Eta artifact: current installed behavior hides the primary message. Do not rely on ignore_errors to define operation recovery. Do not duplicate Cause logic throughout PIE; select a verified central boundary for visible error reporting.
