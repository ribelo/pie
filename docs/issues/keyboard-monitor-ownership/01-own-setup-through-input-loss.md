# Own keyboard setup through input loss

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Native keyboard monitoring has explicit ownership and lifecycle handling.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete on packages/pie. Integrated suite pending. No deployment. Does not claim the whole audit. PIE021.

`connect_and_watch` owns the flow immediately and closes on every AUTH/Hello/AddMatch/Watch failure or cancellation, combining primary+cleanup via `Eio.Exn.combine` (Cancelled kept observable). Unwatch write is advisory (ordinary I/O only); physical close is unconditional, with a 250ms production write budget then close. Unexpected reader EOF is `Input_lost` (`Cause.Die`); cancellation stays interruption. Focused gates: keyboard_ownership 14 green (`29G3S3YR`) on pie-audit-current Ox. bc9 service input-loss/Pause integration quick tests 10 green on the same stage. `with_monitor ~sw ~net ~process_mgr` unchanged.

## Requirements

- If keyboard acquisition or shutdown signalling fails, then PIE shall retire the acquired connection independently of that failure. ^piekeyboard-6r41
- When the keyboard event reader terminates unexpectedly, PIE shall report input loss to the owning service and settle affected recording attempts instead of continuing as if keyboard input were available. ^piekeyboard-8q2w

## Evidence

Actual connect_and_watch against a private fake Unix bus: four rejected AUTH exchanges yield FD counts8,9,10,11 after each fake peer scope has retired. Each connection is registered to the shared outer switch, but setup failure occurs before with_resource installs release. All probe resources retire when its process/root scope ends. No real session bus or keystroke was accessed.

Monitor372–379 acquires before authentication/Hello/AddMatch/WatchKeyboard without rollback. unwatch_and_close381–387 puts close after write inside the same try: failed write skips close. read_loop398–402 treats EOF and other errors as successful termination, so with_background alone does not establish usable input for its consumer. The latter paths are source-backed; no live disconnect was induced.

Current Service creates the real monitor once outside its shadow component graph. The probe does not demonstrate repeated keyboard acquisition per dictation or attribute historical EMFILE to it.

## Implementation direction

Own the connection before authentication and complete registration; release it on every failed stage. Make UnwatchKeyboard best effort but connection close unconditional, with physical retirement and cancellation settlement verified. Propagate unexpected reader loss through the actual service/attempt owner under PIE003/004. Do not add another supervisor/component framework or make a fake healthy fallback monitor.

Keep the fixed-size event queue and explicit single-consumer semantics where appropriate. Finish native-monitor versus portal input review before committing to preserve the hand-written wire implementation. Any retained adapter needs finite handshake/message bounds and correct address/protocol parsing; those are not supplied by adding a finalizer alone.

## Acceptance

Fake bus: rejected auth, failed Hello/WatchKeyboard, cancellation during each stage, failed unwatch write, normal disconnect, unexpected reader EOF and malformed packet. Repeated setup/release must keep descriptors flat within a long-lived parent scope. Active attempts must settle without fabricated key releases or silent continuation.

## Ox prerequisites

Use settled scope/typed failure/cancellation ownership and the existing queue. A background worker returning unit is not a capability-loss signal; PIE must express that outcome. Do not change generic with_background semantics to compensate.
