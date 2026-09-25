# Retire realtime session workers and connections

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Owned realtime sessions settle connections and workers; descriptor-identity and verified WS/WSS regressions pass.

## Earlier implementation checkpoints (superseded)

Parent-reviewed complete for the workspace owned-operation implementation: 12 held-peer identity gates plus 216 WS and 216 verified WSS on current production Codex entrypoints against reviewed Eta pie-audit-current hashes. Whole-suite / final artifact record pending. Not deployed. Not a proof of historical crash causality. Decision PIE002.

## Requirements

- When a realtime operation terminates, PIE shall settle its connection workers and release its connection before reporting operation completion. ^piesession-ml14
- If a realtime operation is cancelled before session readiness, then PIE shall complete cancellation without waiting for the provider to send session readiness or close its connection. ^piesession-mp2y
- When successive realtime operations have settled, PIE shall retain no connection or worker belonging exclusively to those completed operations. ^piesession-y37k

## Evidence and scope

The assistant passes its long-lived switch into buffered STT and streaming handles: assistant/service402–439,506–518,677 onward,1194 and1219. codex_realtime928–1011 forwards that switch to WebSocket connect. receive_result_marking_done908–914 installs a close finalizer, but only after await_session_ready has succeeded. The pre-ready failure interval is not covered by that finalizer.

Streaming with_stream_session1077–1120 owns a background session with Effect.with_background, but continues to pass the externally supplied switch to the transport. A consumer/background scope does not automatically prove that transport reader daemons share its lifetime. Audit all actual ownership edges, not names.

Paired loopback controls in .scratch/research/core-audit/ws-retention-control-r08.md observed adjusted descriptor counts11..22 over12 sessions both for explicit direct WebSocket close and for PIE pre-ready errors. Per-session runtime plus connection scope retirement made both sequences flat10; see ws-scope-counterfactual-r08.md. Thus a missing close alone is an insufficient fix. These probes use current workspace PIE and installed Eta, not the unidentified dependency image embedded in the live binary.

## Implementation direction

Give each realtime operation one owner for acquisition, readiness, send/receive, cancellation and final settlement. Install cleanup immediately after acquisition, not after readiness. A typed result, closed incoming queue, scheduled close frame or logical Eio close is not proof that a pending transport read has retired. Preserve original failure information while settling cleanup.

Do not turn the counterfactual into nested production runtimes. Select the smallest resource/worker scope supported by the settled Eta contract. Remove redundant completion/close bookkeeping only after its duties are demonstrably owned there. Do not raise descriptor limits as the correction, add periodic restarts, or add an arbitrary deadline to established streams. Existing speech-operation-deadlines policy intentionally leaves established streams without a total-session deadline.

## Eta prerequisite

Incoming D25, http-ws-close-execution/01-defer-incoming-shutdown-and-connect-work.md, makes close/connect/send work deferred. It is necessary for safely composing cleanup blueprints, but its stated requirements do not alone establish physical reader retirement.

Before integrating, demonstrate that Eta WebSocket operation ownership can interrupt and settle outstanding reader/writer work and physically release its flow, including when the peer remains open or never acknowledges readiness. Installed Eio Rcfd defers physical close while users retain the descriptor. This may require strengthening Eta WebSocket ownership; existing acceptance status of D25 must not be used as proof that this additional contract is implemented. Keep that prerequisite in PIE until verified upstream; do not fork a second transport into PIE.

## Acceptance

Promote paired local controls with held-open peers: normal completion, pre-ready server error, malformed readiness event, stalled readiness cancellation, send failure and mid-stream cancellation. Observe worker settlement and actual descriptors, not only close invocations. Exercise buffered and streaming entry points and repeated operations inside the same long-lived application runtime. Bound the local test wait; do not make it a new provider deadline. Include TLS after the plain WebSocket control and prove the selected deployed artifact contains the corrected dependencies.

The original root-scope controls reproduce a defect class. The scoped variants are explanatory controls, not implementation acceptance tests or proof of the exact historical crash chain.

## Workspace implementation (parent-reviewed)

Private fixture `packages/pie/test/session_ownership/` exercises public `run_realtime_session` (buffered) and `run_realtime_stream_session` (streaming) with Conversation protocol only (`session.update` → `session.updated`; terminal `response.done` `status=completed`). One long-lived runtime. Held-open peers. No Service/Config/PTT/Codex production edits. No production API change for tests. Codex not exposing `ca_file` is a Pie caller choice; Eta `Ws.Client.connect` already has `?ca_file` — not a new Eta API gap and not a new upstream ticket.

Evidence: `pie/.scratch/session-ownership-build/RESULTS.md`. Eta unassisted graceful-close contract `^wssettle-b6wb` remains open.
