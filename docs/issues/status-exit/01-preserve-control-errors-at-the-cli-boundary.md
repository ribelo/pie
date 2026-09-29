# Preserve control errors at the CLI boundary

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

CLI failures return nonzero statuses without false-success output.

## Earlier implementation checkpoints (superseded)

Implemented. PIE031.

Isolated CLI fixture `packages/pie/test/cli_status` (registered `dune runtest` stanza, no change to `packages/pie/test/dune` or `test_pie.ml`) built `pie.exe` with `--build-dir .scratch/cli-status-build` against the installed switch. Real executable vs private Unix socket: valid status stdout exactly `armed\n` / WEXITED 0; RPC error and malformed JSON WEXITED nonzero (not signal), stdout exactly empty, stderr contains `synthetic protocol error` / `Json_error`; absent socket stdout `off\n` / 0; `msg status` same RPC-error channel; rejected retry stderr contains `synthetic rejection`. Harness: 4s child deadline, concurrent select drain, kill/reap only the spawned child, close listen socket before join, propagate unexpected server-thread errors. Control: unknown command returns before deadline without hanging accept. Red: status RPC/malformed still WEXITED 0. Green: 8/8 after handler + fixture hardening.

## Requirement

- If a status request fails with a control-protocol error, then PIE shall report the error with a nonzero exit status rather than return successful status output. ^piecli-gw3m

## Evidence

The isolated current-source executable received an explicit synthetic RPC error from a private Unix server. `pie status` printed error=synthetic protocol error and exited0. `pie msg status` received the same reply and exited1.

CLI153–160 consumes Daemon_error into a successful printing effect. The client already preserves the error.

The fixture also checked rejected actions: both msg action retry and retry exit1. Client.action56–67 rejects accepted=false before the CLI sees it. Do not report action rejection as an unfixed CLI bug.

## Direction

Preserve Daemon_error through Process.run. Keep stdout for successful status data and stderr for errors. The existing explicit off status for an absent daemon may remain documented behavior; do not conflate absence with malformed or failed replies.

## Acceptance

Exercise the real executable against private successful, explicit-error, malformed and absent-server fixtures. Assert exit code and output channel, not only message text. Retain the successful action-rejection controls.

## Ox prerequisites

No new abstraction. Existing typed failure and process exit handling suffice.
