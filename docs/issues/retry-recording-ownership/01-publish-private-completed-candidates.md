# Publish private completed retry candidates

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Private completed recovery candidates replace historical scanning and the legacy retry-store API.

## Earlier implementation checkpoints (superseded)

Active storage frozen (f33 12/12). Service+owner Quick census green 34/34 pending whole-34: one Active.store per workflow, private Draft per attempt, seal before provider, publish Audio/Text on non-abort failure, nested missing parents 0700 without chmod of existing 0755. Production settlement is only outermost `run_acquired_attempt` on_settle. Legacy Retry_store bridge removed. Parent review before accept. Not deployed.

## Requirements

- When PIE persists captured audio for recovery, PIE shall restrict file access to the owning user. ^pieretry-en38
- When PIE selects a retry recording, PIE shall select a completed recovery candidate published by PIE rather than a recording still being written. ^pieretry-mqpz
- If a retry-directory entry is a symbolic link or is not a valid recovery candidate, then PIE shall exclude it without reading its payload. ^pieretry-cusl

## Evidence

Active store gates frozen (f33 12/12). Service uses Active only (legacy create/latest/append callers gone). Nested missing recovery ancestors created 0700; existing 0755 parents unchanged (owner nested-parent gate). Draft is private until seal; publish Audio/Text replaces the single latest candidate. Busy/Disabled retry peeks metadata and does not load payload.

assistant_owner Quick 34/34 green pending whole-34 (see stage-aware-recovery evidence IDs). Stream Slow not in that census.

## Implementation direction

Use private creation permissions (0700 directory,0600 audio files) independent of permissive caller umask. Publish a candidate only after its writer is closed and its recovery status is known; exclude active temporary files. Validate owned entry type and format without following links, and bind validation to opening so replacement races do not bypass it. Do not treat changing only the filename suffix filter as sufficient ownership.

Check admission before loading an entire recording. Keep filenames and recovery metadata simple; this does not justify a database or generic filesystem abstraction. Preserve useful failed recordings; do not delete historical user files during this audit or silently purge them during migration. Retention bounds and recovery after successful recognition but failed delivery require separate policy disposition.

## Acceptance

Promote synthetic controls for permissive umask, open writer, completed writer, symlink, unrelated regular file, concurrent publication and replacement, Busy/Disabled admission, and failed creation/close. Observe candidate publication and payload reads rather than only filename parsing. Check successful recovery deletion and failed recovery preservation separately. Use temporary directories, never the real failed_recordings_dir.

## Ox prerequisites

No new Ox filesystem package is needed. Use ordinary explicit filesystem ownership and PIE004 all-exit writer settlement. Resource finalization must not depend on garbage collection.
