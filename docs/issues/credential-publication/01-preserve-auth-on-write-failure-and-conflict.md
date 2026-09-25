# Preserve credentials on write failure and conflict

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Shared Codex credentials are read-only; expired credentials require their owner to refresh them.

## Earlier implementation checkpoints (superseded)

Parent-reviewed. Implementation complete on packages/pie. Integrated suite pending. No deployment. Does not claim the whole audit. PIE010.

Architecture decision: no verified cooperative Codex writer protocol exists in the current Pie integration, so shared `CODEX_HOME/auth.json` is read-only. OPENAI_API_KEY and a fresh shared access token are still accepted. A JWT with a known-expired `exp` is a stable `Stt_error` requiring the Codex credential owner to run `codex login`. Opaque or unparseable expiry is left to the provider; this is not JWT validation. Pie never calls the token-refresh endpoint and never writes or rotates the shared file. Write/flush/replace tests are superseded by no-write invariants. This is an explicit integration limit, not a claim that an atomic shared credential publisher or transparent auto-refresh is implemented. Focused gates: credential-ownership 7 (`5T3A52KH`).

## Requirements

- If credential publication fails, then PIE shall report that failure while preserving the previous valid credential file. ^pieauth-um4n
- When another credential owner updates shared credentials after PIE reads them, PIE shall preserve that update rather than publish its stale snapshot over it. ^pieauth-kpm1

## Evidence

Actual workspace Codex_auth.write probe with synthetic tokens: concurrent_update_overwritten=true. A second child-only RLIMIT_FSIZE0 control returns write_exit=Ok; persisted_bytes=0. No real credentials, auth endpoint or live service were accessed.

Codex_auth.write615–635 opens the shared auth.path with Open_trunc, buffers output and returns Ok before close_out_noerr. The final flush failure is suppressed. persisted_json589–613 merges against the previously read auth.json, not the current shared file. Refresh642–705 has no shared-owner coordination around read/refresh/publication.

## Implementation direction

Publish through a private temporary file, check write/flush/close results, and replace only a fully written candidate. Clean up temporary output on every failure. Choose and document required crash durability; do not call close alone a durable commit. Maintain0600 permissions.

Separately establish credential ownership. Prefer one owner for shared refresh-token rotation and file publication. Atomic rename prevents a torn file but not a stale overwrite, and a PIE-only mutex cannot serialize a separate Codex process that ignores it. Do not claim a read/stat-then-rename check is a cross-process transaction.

Before implementation, establish the supported shared-owner protocol or delegate refresh/publication to the credential owner. If no cooperative contract exists, surface that integration limit rather than silently writing another application’s stale auth document. Any move to PIE-owned credentials must account for refresh-token rotation affecting Codex; copying tokens into another file is not independent ownership.

Do not add generic credential storage or fallback machinery. Retain only what the selected authentication route requires. Keep error messages free of tokens/provider response bodies.

## Acceptance

Use temporary synthetic auth files: write/flush/close/replace failure; cancellation; stale snapshot; simultaneous PIE refreshes and a separate credential writer; preserved unrelated fields; final permissions; temporary-file cleanup. Assert actual persisted bytes and previous-file survival, not merely an Ok effect. Test rotation conflicts through a fake local auth owner, not real token refreshes.

## Eta prerequisites

No new Eta auth store is required. Use settled failure/finalizer contracts. The shared-writer protocol is a PIE/Codex integration prerequisite and is not supplied by incoming Eta fixes.
