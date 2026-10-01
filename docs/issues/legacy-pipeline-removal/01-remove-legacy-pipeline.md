---
status: open
package: pie
consumer: pie
---

# Remove the legacy single-pass pipeline

## Problem

`pipeline = "legacy"` keeps the conversational gpt-realtime path (audio prompt files transcription.md and translation.md, the realtime conversation session, the single-pass recognizer) that produced the garbage the two-pass and one-pass work replaced. It was kept so the new binary could be installed before the configuration changed (dict-26tm, dict-ezcr). That migration happened on 2026-09-30. The user decided on 2026-10-01: legacy is removed; two-pass stays as a dormant alternative because future models may favour it.

## Solution

Remove the legacy pipeline value, its recognizer path, its prompt files and placeholders, its configuration mapping, its tests and its documentation. `pipeline` accepts `one-pass` (default) and `two-pass`. A configuration that still says `legacy` or relies on the legacy defaults is rejected at load with a message naming the key and the two accepted values, so the user sees it at startup rather than dictating into a silently changed path.

## User stories

1. As a dictation user, I want the legacy pipeline gone, so that the configuration, the tests and the code carry only the two approaches I may use. ^dict-r5lg
2. As a dictation user, I want a configuration that names the removed pipeline to be rejected with a clear message, so that nothing silently changes behaviour. ^dict-v2qd
3. As a maintainer, I want CONTEXT.md and the spec to stop describing the legacy path, so that the vocabulary matches the code. ^dict-p6mz

## Implementation decisions

- Delete, do not disable: no dormant provider, no dead configuration keys. Keys that only served legacy (the audio prompt file names, the conversational session settings) go, and the config suite proves they are rejected as unknown.
- The Codex realtime transcription session used by two-pass pass 1 stays; only the conversation session path goes.
- Record removed stories (dict-26tm, dict-ezcr) as retired in the two-pass spec with one line each.
