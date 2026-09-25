# Reject malformed replies and remove duplicate codecs

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Control requests and status use the canonical contract.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete on packages/pie. Integrated suite pending. Does not claim the whole audit. PIE014.

Canonical control surface is `Pie.Assistant_service`. The public `Pie.Daemon` alias and unused `daemon/{client,contract,server}.ml` were removed intentionally. CLI and tests retarget to Assistant_service. `response_of_json` returns `(response, protocol_error) result`; Action replies must echo the requested action string or fail with `action-mismatch`. Focused gates: control_protocol 22 and cli_status 9 green. Eta staged provenance unchanged.

## Requirements

- If a control response does not satisfy the schema for the requested operation, then PIE shall report a protocol error instead of fabricating successful acknowledgement or default recording state. ^pierpc-cpa7

## Evidence

check-rpc-decode.sh uses actual workspace contract modules, without a socket. The public Pie.Daemon and Pie.Assistant_service paths both decode Null, Int, String and List as successful Pause and Toggle false. Round35 established that these paths alias the same implementation (pie.ml/mli12); the probe is not independent coverage of two decoders. Its type witness confirms that identity. Explicit error objects correctly become Error_resp: an old test comment alleging swallowed Pause errors is not a current-source finding.

Daemon.Contract224–305 and Assistant_service.Contract306–398 repeat request-dependent permissive decoding. Missing fields fabricate idle/disabled snapshots and meeting outcomes. Both clients connect to the same Paths.daemon_socket_path, close their flow in Fun.protect, cap response lines at65536 and apply a one-second call timeout. These positive guards do not validate the response schema.

## Architecture disposition

Keep Assistant_service as the canonical actual control implementation. Public Pie.Daemon already aliases it, so CLI and Service are not using different codecs on the wire. Delete unused internal daemon/{client,contract,server}.ml after removing obsolete Commands.Assistant helpers, remove the public Daemon compatibility alias, and retarget CLI/tests to the canonical name. Do not introduce another adapter or wire-version fallback. Remove component status variants/projection helpers with PIE003. Finish server lifecycle review before discarding server behavior that the real workflow needs.

Use strict decoding for required fields and tagged outcomes. Preserve explicit server errors separately from malformed messages and transport/cancellation failures. Empty-object acknowledgement may remain valid if it is the chosen schema, but arbitrary JSON is not equivalent to acknowledgement. Optional fields may be absent only where explicitly optional.

## Acceptance

For every operation: valid reply; explicit server error; non-object JSON; missing required field; wrong field type; unknown outcome tag. Verify client-visible typed protocol failure and no fabricated successful state. Include actual local round-trip tests after consolidation. No live service socket is required.

## Eta prerequisites

No RPC framework or generic schema machinery is needed. Keep the existing newline/size/timeout guards and use settled cancellation ownership.
