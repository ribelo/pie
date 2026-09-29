# Retry only the incomplete stage

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Audio/Text recovery retries only the incomplete stage; cancellation, publication, and clear failures remain visible.

## Earlier implementation checkpoints (superseded)

Service+assistant_owner Quick 34/34 green (reload 1, input 3, cancel 9, service_gates 15 incl. 013/014, abort_older 1, admission_hold 1, recover 4). Peek-then-admit-then-load RetryLatest; Audio retries provider; Text delivers without provider; abort does not publish; `recover_single_failure` recovers only exact `Cause.Fail`. Stream Slow not in this census. Parent review before accept. Whole-34 pending. Not deployed.

## Requirements

- When delivery of a recognized text result is retried, PIE shall deliver that retained result without invoking transcription or translation again. ^pierecovery-kxpe
- If a provider returns no transcript text, then PIE shall report a no-transcript outcome rather than completed text delivery. ^pierecovery-r590
- When PIE publishes a new recovery candidate, PIE shall retain only the latest candidate within its active recovery slot. ^pierecovery-6p6x

## Evidence

Fresh assistant_owner Quick (stage-recovery-build, DUNE_CACHE=disabled): reload RRJM8MK8 1/1; input 3CTM7I0V 3/3; cancel DXT5B3M6 9/9; service_gates QZDCKLTY 15/15 (013 success-preserves-older, 014 publication compound DU7RIT1N handoff); abort_older R9VPI2G9; admission_hold 7HU6UDGU; recover 8O2M2H4O 4/4. Total 34 Quick, 0 fail. Stream Slow not run.

Implementation: `recognize_then_deliver` shared; blank is `stt:no_transcript` then publish Audio; delivery failure publishes exact Text; RetryLatest peek→admit→load; cancel latch skips new publication; admission released only at outermost `run_acquired_attempt` on_settle; `Ptt_loop.recover_single_failure` (exact `Cause.Fail` only) in PTT, owned-pause, retry, and `commands/record` timer.

## Decision and tradeoff

Represent recovery at the incomplete stage: captured audio awaiting recognition, or recognized text awaiting delivery. Preserve the successful result rather than paying for or changing recognition again because a desktop action failed. Do not auto-retry ambiguous key dispatch: submitting Ctrl-V is not acknowledgement that the target accepted text.

Keep one latest recovery slot, matching the simple retry-latest command rather than building an archive browser. Publishing a newer failure replaces the older pending candidate. Make that policy visible; the tradeoff is loss of older retry history. This applies to the new active slot, not an audit-time purge or silent migration of existing user recordings. Historical files require a separate explicit migration/deletion decision.

Keep active capture separate from published recovery. Successful delivery clears its candidate; provider/delivery failure preserves the corresponding incomplete stage. Treat explicit user abort as abort rather than automatically publishing another failed recording. A no-transcript outcome preserves applicable recovery material instead of following the delivered-success deletion path. Bound active recording buffers/files separately at the capture boundary; a one-candidate count does not itself bound bytes.

Use one small application-owned tagged state, not a workflow engine, job database, retry framework or independent parallel audio/text stores. Preserve PIE004 attempt identity and PIE005 private completed publication.

## Acceptance

Use synthetic provider and injection seams with call counters: provider failure retries recognition; delivery failure retries the identical existing text with zero extra provider calls; empty result reports no transcript and does not invoke injection. Exercise ambiguous dispatch without automatic duplication, candidate replacement, successful removal, abort, startup recovery and private serialization. Verify old historical files are untouched by migration tests unless an explicit migration operation is under test.

Existing retain/discard tests are useful regression context but do not prove stage-aware recovery. No acceptance implementation was performed during this audit.

## Ox prerequisites

No new Ox retry abstraction is required. Use settled result/finalizer contracts; PIE002 owns transport retirement and PIE004 owns attempts. Keep recovery policy in PIE.
