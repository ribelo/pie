# Own the control endpoint and bound peers

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Control sockets and stable locks are private and identity-owned, with bounded handlers and deadlines.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete. Integrated suite pending. Does not claim the whole audit. PIE015. No deployment. No further server code changes. Staged Eta (`pie-audit-current-build`) unchanged.

Canonical listener is a native Unix socket imported with `Eio_unix.Net.import_socket_listening ~sw ~close_unix:true` (Fd-only cleanup; Eio.Net.listen is not used because it unlinks the path on switch exit). Admission: 16 active handlers, listen backlog 16, 1s peer read+reply deadline on the supplied mono clock. Process-local registry then a persistent 0600 lockfile (existing open `O_RDWR|O_CLOEXEC` only; create `O_CREAT|O_EXCL|O_CLOEXEC`); the lockfile is never unlinked (intentional split-lock-inode). This Unix.open_flag set has no `O_NOFOLLOW`; lock identity is lstat/fstat (and `st_nlink = 1`) in a same-uid cooperating namespace, not a kernel nofollow guarantee. Focused gates: control_socket_ownership 22.

## Requirements

- When PIE starts its control listener, PIE shall preserve an existing non-socket file or live listener at the endpoint and report an ownership conflict instead of replacing it. ^piesocket-y8ja
- When PIE retires a control listener, PIE shall remove only the endpoint owned by that listener and release its accepted connections. ^piesocket-s6cn
- While the control server is running, PIE shall admit at most16 active peer handlers and expire an incomplete request line after one second. ^piesocket-8pag

## Evidence and boundary

Actual Assistant_service.Server.start probe uses a private fixture, synthetic coordinator and no live requests. It replaces a regular sentinel file with a socket; a second start replaces the first active listener pathname; root scope cleanup raises Unix.ENOENT from unlink. First run exited2 after the ownership assertions; the corrected observation harness explicitly asserts the cleanup error and exits0. This is a two-listener/same-scope control, not a cross-process or historical-crash reproduction.

Both servers explicitly unlink before listen with reuse_addr:true. Accepted handlers have a65536-byte line cap but no incomplete-line deadline or concurrency ceiling. Backlog10 limits waiting connections, not all accepted handlers. This peer-resource lead is source-backed, not stress-tested this round.

Threat boundary: endpoint files and local control availability. A second legitimate PIE instance is sufficient for the reproduced conflict; no malicious actor is needed. Other-user access depends on parent/socket permissions, which were not inspected on the live service. No CVE attribution is made for this application logic.

## Implementation direction

Use one listener after PIE014 consolidation. Establish exclusive ownership before binding and retain it until listener/peer retirement finishes. A lifetime lock in a verified private runtime directory is a candidate; stale-socket recovery must be conditioned on ownership and socket type, not blanket unlink. Do not retain Unix reuse semantics that still replace a live endpoint after removing only the explicit unlink.

Verify runtime-directory ownership/privacy and use owner-only socket access. Do not chmod an arbitrary shared directory or follow an unverified runtime-path symlink as a substitute for ownership. Cleanup must not unlink a replacement endpoint belonging to another lifetime.

Keep the existing request byte cap. Bound admission before spawning/waiting handlers, so a semaphore does not merely move the unbounded backlog into fibers. The16-peer/one-second incomplete-line limits are proposed local-control policy, not load-tuned performance results. Cancellation must retire the handler; do not turn it into an error response and continue work.

## Acceptance

Private fixtures: pre-existing regular file; active listener; stale owned socket; two processes racing startup; old-owner retirement after a replacement attempt; inaccessible/unsafe runtime directory; peer that sends no newline; capacity saturation; client cancellation; server shutdown. Assert file identity, surviving listener reachability, bounded descriptors/workers and cleanup outcome.

## Eta prerequisites

No new RPC framework. Use settled scope/cancellation/admission contracts with Eio listener ownership. Application endpoint identity remains PIE’s duty; ignoring unlink errors does not solve ownership.
