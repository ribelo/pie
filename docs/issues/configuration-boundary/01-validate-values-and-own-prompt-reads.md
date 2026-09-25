# Validate configuration values and own prompt reads

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Configuration and prompt reads are bounded, validated, and physically settled with cleanup failures preserved.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete. PIE018. Integrated full-suite still pending; this is not whole-audit acceptance.

Standalone fixture `packages/pie/test/config_boundary` (own dune stanza, not `test_pie`). Current evidence: `--build-dir` `/home/ribelo/projects/ribelo/pie/.scratch/config-boundary-final-build`, `DUNE_CACHE=disabled`, staged Eta `OCAMLPATH` `/home/ribelo/projects/ribelo/ocaml/Eta/.scratch/pie-audit-current-build/install/default/lib` plus matching stublibs first. No shared install. Alcotest `-o` under `.scratch/config-boundary-final-build/alcotest`. Parent accepted 34/34 in 0.701s plus PIE006 `live_config` last-good / effective CLI / active-attempt evidence. `Config.Stt` and the reader fixture are frozen for ongoing integration. No Service API change, no new configuration framework, no deploy.

Historical provenance (first 24 gates, not current evidence): `--build-dir .scratch/config-boundary-build/dune` with Eta `OCAMLPATH` `/home/ribelo/projects/ribelo/ocaml/Eta/.scratch/ws-reader-settle-build/install/default/lib`; 24/24 in 0.060s after wakeword-range correction. PIE024 later deleted inert wakeword settings; this reader rejects leftover wakeword keys as unsupported rather than retaining or range-checking them.

## Requirements

- When PIE accepts configuration, PIE shall require finite normalized RMS/peak thresholds within0–1, and validate the effective key bindings before publication. ^pieconfig-6pyd
- If an existing prompt cannot be read successfully, then PIE shall close any acquired file handle and report configuration failure instead of silently replacing the prompt with default instructions. ^pieconfig-rxni
- When PIE reads configuration or prompts, PIE shall accept only bounded regular-file content and reject unsupported file types without waiting for stream producers. ^pieconfig-6jwr

## Evidence

`Config.Stt.load` uses a bounded regular-file reader (`Unix.openfile` `O_RDONLY|O_NONBLOCK|O_CLOEXEC`, close once never retried, `fstat` on the opened descriptor requires `S_REG`, 1MiB cap including a max+1 sentinel on the chunked read). TOML is parsed from that string via `Toml.Parser.from_string`, not a path reopen. Optional prompt `ENOENT` selects the built-in default; directory, FIFO, oversize, chmod 000 and empty existing prompts raise `Config_error` and are not swallowed. PTT `ptt_min_capture_rms` / `ptt_min_capture_peak` must be finite in 0.0-1.0; 0 remains a valid disable. Leftover wakeword keys are rejected as unsupported (PIE024). Existing keysym uniqueness is unchanged. Private HOME/XDG/TMPDIR fixtures only; no user config.

Close: typed `Config_error` plus Unix close stays one `Config_error`; unknown/cancel plus close raises `Compound` without rewriting either as `Config_error`. Close is called once per acquired descriptor.

Current 34/34 in 0.701s: the historical 24 numeric/type/binding/FIFO/1MiB gates plus mid-read EIO fd-flat, pathname regular→FIFO/dir swap while the fd is held (original inode still read), growing-file cap after fstat, close-once, combined read+close, `Test_cancel` not wrapped as `Config_error`, `Test_cancel`+close EIO and `Config_error`+close defect both present, and Service `run_with_inputs` reload cancel via existing `config_control.load` + `~read_io` barrier (not settled while the owned loader thread is held; fixture barrier released independently of cancel; then join + FD closed). This does not claim an arbitrary blocking syscall can be killed, nor a multi-file content transaction. Last-good / effective CLI / active-attempt remain accepted PIE006 `live_config` evidence. No Service API change.

Historical first red: directory FD leak, FIFO hang, unvalidated PTT RMS/peak. Historical 24/24 after wakeword-range correction used the ws-reader-settle Eta stage noted under Status.

## Implementation direction

Bracket prompt channels on every exit. Use a proposed1MiB limit per configuration/prompt file. Check the opened descriptor’s type and bound reads before allocation. Avoid a blocking FIFO open before that check, and avoid check-then-reopen races. A symlink to a valid regular file need not be rejected solely because it is a symlink. These are byte/type limits, not a promise that all filesystem I/O has bounded latency; do not replace one manual open/read/close with another. An absent optional prompt may select a documented built-in default; an existing invalid/unreadable file is not absence. Preserve the prior working configuration on reload failure and report safe diagnostic context.

Validate the merged effective configuration once, including CLI binding overrides under PIE006. Zero thresholds may intentionally disable a signal gate; negative, above-normalized-range and nonfinite values are invalid. PIE024 removed unused wakeword duration/threshold settings; leftover keys are rejected rather than range-checked. Keep the last-known-good reload behavior; remove component replacement machinery under PIE003 rather than adding configuration layers.

The reload thread must remain owned until its read completes. A system thread or logical timeout cannot safely kill an arbitrary blocking syscall. Retain last-good configuration after rejection. Keep the existing bounded metadata-stability retry, but do not call it a multi-file content transaction.

## Acceptance

Synthetic valid/boundary/out-of-range/overflow numbers, wrong types, duplicate effective bindings, absent prompt, existing directory, unreadable file and read failure after acquisition. Add FIFO-without-writer, oversized file, descriptor-type race and cancellation-during-reload cases. Repeated failures must leave FD count flat before GC. Check physical worker settlement rather than only logical timeout completion. Validate startup errors, unchanged last-good config after reload rejection and stable active-attempt config.

## Eta prerequisites

No new Eta configuration abstraction. This source leak is in synchronous channel ownership and is not repaired by incoming WebSocket fixes. Use settled failure/finalizer contracts at the application boundary.
