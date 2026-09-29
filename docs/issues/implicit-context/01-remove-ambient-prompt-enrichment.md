# Remove implicit ambient prompt enrichment

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Only configured prompts are sent; ambient desktop context is removed.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete (PIE017). Integrated suite pending.

Context.Project/Metadata modules and public reexports were removed. Configured transcription/translation prompts, model, and source/target language reach `production_stt` without ambient Niri/manifest/.pie discovery. Delivery focus (PIE027) is unchanged. No user context or prompt files were deleted.

## Requirements

- PIE shall build recognition prompts from explicitly configured instructions and operation parameters without automatically adding window titles, inferred project metadata or discovered context-file contents. ^piecontext-avnq

## Evidence

Service359–410 unconditionally enriches prompts through Niri focused-window metadata, process cwd inference, manifest inspection and ancestor .pie/context.md discovery. Streaming handle setup also calls this enrichment. No new provider call was made during this audit.

Actual isolated helpers: a context symlink inside the synthetic project loads a synthetic file outside that project; a FIFO passes Metadata.regular_file; Project.read_file_opt fails to obtain nonempty /proc/self/stat while a streaming read of that same probe process file succeeds. FIFO was classified only, never opened. No desktop title, real project context, user process stat or private credential was collected.

Project.read_file_opt20–26 uses in_channel_length for procfs data. Metadata.regular_file13 means exists-and-not-directory, not regular-file validation. File reads have no application byte limit and discovered symlinks are followed. These findings concern the existing enrichment path, not proof of a past disclosure or a remote prompt exploit.

## Architecture decision and tradeoff

Remove Context.Project181 lines and Context.Metadata317 lines, the process scan/descendant inference, hand-written manifest parsers and implicit prompt enrichment. Retain configured transcription/translation prompts and explicit language/model parameters. Users can place needed terminology in their configured prompts; this loses automatic per-window/project disambiguation.

Do not replace this with another context plugin, discovery framework or automatic opt-in based on a repository file’s presence. Do not repair the procfs reader solely to preserve code being removed. If contextual discovery is later explicitly requested, its disclosure and trust contract must be designed separately.

Keep Niri functionality actually required for text delivery/focus ownership. Remove only enrichment-only helpers, duplicate Commands.Assistant paths, public reexports and tests that no longer describe a supported capability. Do not delete any user context/prompt files.

## Acceptance

Synthetic window titles/manifests/context files never enter provider prompts automatically. Configured instructions and operation parameters still reach the selected backend. No process-tree scan or manifest read occurs on record/finish. Existing delivery focus behavior remains unchanged. Run caller/reference census and affected prompt/request tests after deleting the modules.

## Ox prerequisites

None. Implicit application data collection is not an effect-runtime capability to wrap.
