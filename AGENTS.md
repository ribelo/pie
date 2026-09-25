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
