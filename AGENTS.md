# Repository Guidelines

## Landing: work is done when it is committed

You are the only agent working in this repository. Each session starts with no
memory of earlier ones. The git history is the only memory that lasts, so
uncommitted work is effectively lost work.

- A task is finished only when its verified change is committed to local
  `master`. A passing build or a green test run is not finished work.
- Commit each coherent, verified slice as soon as it is done. Do not batch a
  whole session into one commit at the end. The commit message names what
  changed and cites the issue or story IDs it covers.
- Never end a session with uncommitted changes. If something is unfinished,
  commit it anyway with a message that says what is unfinished and why, or
  record it in `docs/issues`.
- Uncommitted changes you find at the start of a session came from an earlier
  session of yours that did not land its work. There is no other author. Read
  the diff, verify it (build and tests), and then commit it or finish it. Do not
  work around it, stash it, or treat it as someone else's change.
- When subagents implement slices, the coordinating session reviews each result
  and commits it before starting dependent work.
- Push only when the user asks.
- The user dictates through the running `pie.service` all the time. Replace its binary or restart it only when asked, and only after `pie msg status` says `armed` and the journal shows no `PTT capture started` without a matching stop: a restart during a dictation ends it.
- Dangerous calls go through `Ox_caps.Cap_*` and require a capability in the signature; `scripts/caps-gate` checks production code.

## Gates (before any commit)

Run `scripts/test-local-ox.sh --shared-ox` for a build, the installed capability
gate, formatting, and every registered test (including Slow cases) with a private
HOME and fail-closed desktop helpers. This uses the same shared switch as Pie’s
`nix develop` without installing packages or changing the running Pie service.
The legacy no-argument mode instead installs Ox into ignored `_dev/ox-install`;
use it only when a private local-Ox build is wanted.

For a standalone capability check, run `nix develop -c scripts/caps-gate` after
updating the shared switch with Ox’s own installation workflow.
Do not run `pie-opam-install` just to gate: it also replaces the installed Pie
binary, which needs explicit authorization. The wrapper builds `@install` plus
`@packages/pie/bin/check` immediately before the gate and requires fresh typed
evidence; the scoped `@check` produces executable annotations, without unrelated
test-only probes.
A source-clean run with undecided modules is not green.
Finish with `git diff --check`.

## Agent skills

### Issue tracker

Issues and specs live as committed markdown files under `docs/issues/`; wayfinder maps under `docs/wayfinder/`. See `docs/agents/issue-tracker.md`.

### Triage labels

The five canonical triage roles, with label strings equal to their names. See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: one `GLOSSARY.md` at the repo root. See `docs/agents/domain.md`.
