---
kind: requirement
---
# Codex Realtime callback lifecycle

## Intent

Bridge Codex Realtime callback completion into Ox's structured asynchronous lifecycle so completion, failure, and interruption follow Ox semantics. This capability covers callback registration and cancellation; it does not define WebSocket protocol behavior.

## Requirements

- While Codex Realtime awaits callback-driven completion, the Codex Realtime integration shall represent the pending operation with `Ox.Effect.async`. ^codex-m71s
- If a pending Codex Realtime callback operation is interrupted, then the Codex Realtime integration shall report an Ox typed interruption through its typed failure channel. ^codex-w1jx
- While a Codex Realtime callback operation is pending, if interruption wins its completion race, then the Codex Realtime integration shall invoke its registered canceler at most once. ^codex-3y5k

## Verification seams

- Exercise callback success, typed failure, interruption-before-completion, completion-before-interruption, and duplicate completion through the Ox runtime seam.
- Instrument the registered canceler and assert that interruption invokes it no more than once.
