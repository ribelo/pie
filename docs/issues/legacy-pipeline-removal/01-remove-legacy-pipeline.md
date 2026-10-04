---
status: completed
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
3. As a maintainer, I want GLOSSARY.md and the spec to stop describing the legacy path, so that the vocabulary matches the code. ^dict-p6mz

## Implementation decisions

- Delete, do not disable: no dormant provider, no dead configuration keys. Keys that only served legacy (the audio prompt file names, the conversational session settings) go, and the config suite proves they are rejected as unknown.
- The Codex realtime transcription session used by two-pass pass 1 stays; only the conversation session path goes.
- Record removed stories (dict-26tm, dict-ezcr) as retired in the two-pass spec with one line each.

- The `ptt_*` keys remain as the supported no-`[[bindings]]` mapping for both pipelines. They do not select the removed recognizer or its defaults. Only the conversational migration promise in dict-ezcr is retired. No service installation, restart or user-configuration write is part of this change.

## Verification (2026-10-01)

- `./scripts/dev-env.sh dune build --root "$PWD" --build-dir "$PWD/_dev/b-all" --only-packages pie @all`: passed.
- Sixteen affected suites passed, 384 tests in total: root Pie, config boundary, dictation pipeline, correction, history, OpenRouter outcomes, realtime outcomes (including TLS peer close), provider budgets, session ownership, assistant owner, streaming resampling, lifecycle soak, end-to-end budgets, audio archive, live config and diagnostic content. Legacy-only tests and live executables were deleted; remaining fixtures use the supported protocols.
- `./scripts/dev-env.sh ./scripts/caps-gate`: passed. A transient Semgrep engine failure was reproduced as an infrastructure error, not a finding; a direct pinned scan and the final gate both passed.
- `ocamlformat` on changed OCaml files and `git diff --check`: passed.
- `pie config check` on a copy of `docs/examples/stt.one-pass.toml` and the read-only `/home/ribelo/.config/pie/stt.toml`: passed in private homes. No installed service, binary, user config or user data was changed.
- Removed pipeline values, `two_pass`, obsolete prompt keys and conversational GPT realtime model families fail loading. The error identifies the key and the accepted pipelines. The family check runs on the resolved model, not its provider prefix.
- Provider outcomes were exercised with local fixtures; no live-provider request was made.
