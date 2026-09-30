---
status: completed
package: pie
consumer: pie
---

# Two-pass dictation with glossary, history and correction switch

## Status

2026-09-30 afternoon: one pass (dict-dq3o and following) was added after a comparison showed Gemini hearing the audio got 7 of 8 hard spots right against 2 of 8 for two passes, at similar p50 latency (docs/research/2026-09-30-dictation-models). One pass through OpenRouter with Google AI Studio first becomes the recommended configuration (docs/examples/stt.one-pass.toml); two passes stay available. One pass is implemented on master; all suites pass except session_ownership (Ox TLS race). Installed 2026-09-30 16:18 at the user's request:

- `~/.cache/opam/5.4.0+ox/bin/pie` is built from master 90af250 against Ox 76d2596d (the `_dev/ox-install` the suites ran on), release profile, sha256 843370d7d0263ed7…, 13202176 bytes. Previous binary (sha256 7e2dd08e2b123725…) kept as `pie.pre-dict-20260930` beside it.
- `~/.config/pie/stt.toml` is docs/examples/stt.one-pass.toml plus the user's existing newline and text-entry rules; the previous file is `stt.toml.pre-one-pass-20260930`. A starter global glossary was created at `~/.config/pie/glossary.md`.
- Roll back: copy both `.pre-*` files back over `pie` and `stt.toml`, then `systemctl --user restart pie.service`.
- After one restart: active, NRestarts 0, Result success, startup config shows pipeline one-pass with Google AI Studio first, `pie msg status` armed.
- Two defects found on the first real dictation and fixed the same hour: every one-pass reply was rejected as "unsupported output" because Gemini adds a `reasoning` output item (fixed in the adapter, binary reinstalled, sha256 93e7ca4239dc4164…); and the installed config had taken the examples' F9/F10 bindings instead of the user's Right Ctrl (65508) with Shift for transcription and Right Ctrl alone for translation (restored; examples corrected). The failed attempt's audio is in the archive. A third: archives were always WAV because ffmpeg could not infer the container from the `.partial` temp name; fixed with `-f ogg` (installed sha256 50b0642904644f11…). Verified after the fixes: one translation and one transcription through Right Ctrl, delivered, one-pass 2.6 s and 4.1 s, both in `pie history`.

Implemented on master (2026-09-30); every suite passes except session_ownership, which fails on the Ox TLS buffer race recorded in docs/issues/session-ownership-wss-flake. Not installed: the running service still uses the previous binary and the user's legacy configuration. A proposed configuration is in docs/examples/stt.two-pass.toml. Streaming pass 1 still dies after about 11 minutes until Ox fixes its consecutive-ping limit (docs/issues/long-recording-cutoff); the recording then falls back to buffered pass 1 instead of being lost.

## Problem Statement

I dictate most of my work in Polish full of English programming terms, and Pie's output is often garbage: "Nema" comes out as "NEMa", "Pie" as "Pi", "Chrome CDP" as "Front Servera". The configured model, gpt-realtime-2.1, is a conversational voice model that is asked to transcribe and rewrite in one step, so it guesses at words it does not know. Nothing tells it which words I use: the old project context (`.pie/context.md`) was removed (piecontext-avnq) and my prompts still claim that window and project context is appended, which is no longer true.

When a text is lost, it is lost for good. The recovery store keeps only one failed attempt; a delivered text that I later delete, or that the target application swallows, is gone. I have already recovered transcripts by hand more than once.

I also cannot choose, per key, whether I want my stream of thought turned into a tidy message or left as I said it, and I cannot switch between streaming and buffered recognition.

## Solution

Dictation becomes two passes. Pass 1 is a dedicated speech-to-text model that writes down literally what I said, streaming audio while I hold the key when the provider supports it. Pass 2 is optional per key binding: a fast text model of my choice gets the raw transcript, my glossary and my own prompt, and returns the final text, which can be heavily rewritten if my prompt says so. Translation is pass 2 with a translation prompt.

A global glossary and a per-project `.pie/glossary.md` (found from the working directory of the focused window) tell both passes which words I use and how they get misheard. A bundled agent skill, installed with `pie skill install`, teaches an agent to write that project glossary.

Every attempt is appended to a local text history before delivery, with the raw transcript and the final text, and `pie history` gives it back.

If pass 2 fails or times out, Pie types the raw transcript and tells me so, instead of losing the text.

Nothing changes until I opt in: an existing configuration keeps today's single-pass behavior, so the new binary can be swapped in before the configuration is switched.

## User Stories

### History

1. As a dictation user, I want every transcript appended to a local history as soon as pass 1 returns it, so that a text I delete later or that never arrives can be recovered. ^dict-dozi
2. As a dictation user, I want each history entry to keep both the raw pass-1 transcript and the final text, so that I can see which pass made a mistake and fall back to what I actually said. ^dict-3dqh
3. As a dictation user, I want each history entry to record the time, mode, binding, focused application and title, pass-1 model, pass-2 model, glossary files used, outcome and the duration of each pass, so that I can see what happened and where the time went. ^dict-s0jv
4. As a dictation user, I want `pie history` to list the newest entries with time, mode, outcome and the first line of text, so that I can find a lost text quickly. ^dict-zzju
5. As a dictation user, I want to print the final text of the newest entry, or of a chosen entry, and optionally its raw transcript, so that I can pipe it into the clipboard. ^dict-5xmu
6. As an agent working for me, I want a JSON form of the history output, so that I can read and compare entries without parsing a table. ^dict-z87z
7. As a dictation user, I want the history kept only as private text (no audio) under Pie's data directory and only ever appended to, so that it is cheap and nothing is rewritten behind my back. ^dict-b18k
8. As a dictation user, I want a history write failure to be reported without blocking delivery, so that a full disk never costs me the text I am dictating. ^dict-6frx

### Pass 1: speech to text

9. As a dictation user, I want pass 1 to use a dedicated speech-to-text model that returns what I said literally, so that the recognizer does not invent or rewrite words. ^dict-dl3q
10. As a dictation user, I want to choose the pass-1 model in the configuration with a provider prefix, including `codex-realtime/gpt-live-transcribe` through my Codex login and OpenRouter transcription models such as `openrouter/microsoft/mai-transcribe-2`, so that I can pick speed, quality or price. ^dict-m2xr
11. As a dictation user, I want a configuration switch between streaming and buffered pass 1, so that audio goes out while I hold the key when I want low latency, and as one recording after release otherwise. ^dict-q6xd
12. As a dictation user, I want streaming requested for a provider that cannot stream to be rejected when the configuration loads, with a message naming the provider, so that I never believe I am streaming when I am not. ^dict-i28t
13. As a dictation user, I want the pass-1 request to carry my language and my glossary as recognition context where the provider accepts it, so that the recognizer already favors my words. ^dict-wha3

### Pass 2: correction and translation

14. As a dictation user, I want an optional pass 2 in which a text model I choose receives the raw transcript, my glossary and my prompt and returns the final text, so that jargon is fixed and my stream of thought becomes a sensible message. ^dict-ldo9
15. As a dictation user, I want to configure the pass-2 model, its reasoning effort and its time limit, so that I control the trade-off between quality and waiting. ^dict-l4zp
16. As a dictation user, I want the pass-2 prompt to be entirely mine, read from a file in my configuration, with placeholders for the glossary and the languages and the transcript sent as the message, so that Pie adds no instructions I did not write. ^dict-g52m
17. As a dictation user, I want a transcription binding whose pass 2 fails, times out or returns nothing to type the raw transcript and notify me that correction was skipped, so that I never lose the text. ^dict-bpuh
18. As a dictation user, I want a translation binding whose pass 2 fails to type nothing, notify me, and keep the source-language transcript in the history, so that a Polish text never lands in an English conversation. ^dict-swyn
19. As a dictation user, I want a translation binding to transcribe in the source language in pass 1 and translate in pass 2 with the same glossary, so that translation benefits from the same vocabulary as transcription. ^dict-vwco
20. As a dictation user, I want pass-2 output that exceeds the result budget to be treated as a pass-2 failure, so that a runaway model cannot type an unbounded text. ^dict-fk8i

### Bindings and compatibility

21. As a dictation user, I want to define several push-to-talk bindings, each with a key, a shift flag, a mode, whether pass 2 runs and which pass-2 prompt it uses, so that I can keep one key for corrected and one for raw dictation, or several prompt styles. ^dict-wrbh
22. As a dictation user, I want the existing transcribe and translate key settings to keep working as two bindings, so that my current configuration still loads. ^dict-ezcr
23. As a dictation user, I want a configuration without the new settings to behave exactly as today (single pass with the conversational model and my current audio prompts), so that installing the new binary changes nothing until I switch. ^dict-26tm
24. As a dictation user, I want `pie retry` of a retained failed recording to go through the same pass 1, pass 2 and history as a live attempt, so that retried text gets the same treatment. ^dict-ynad

### Glossary

25. As a dictation user, I want a global glossary file in Pie's configuration directory, with its path overridable through an environment variable of the systemd service, so that words I use everywhere are always known. ^dict-gnkq
26. As a dictation user, I want a project glossary at `.pie/glossary.md` in the nearest ancestor of the focused window's working directory at key press, so that dictating in a project terminal picks up that project's words without any manual step. ^dict-cods
27. As a dictation user, I want the focused window's working directory to come from the process running inside it (the shell or editor under the terminal), so that a terminal opened in a project resolves to that project. ^dict-c5j3
28. As a dictation user, I want only the global glossary used when the focused window is not in a project, such as a browser or Slack, so that dictation there still works. ^dict-cwri
29. As a dictation user, I want the global and project glossaries both passed, each labelled with its source, so that project words add to my general vocabulary. ^dict-eiod
30. As a dictation user, I want glossary files read at every attempt, so that an edit takes effect at the next key press without a restart. ^dict-60is
31. As a dictation user, I want glossary files followed through symlinks and read in full, so that I decide what goes in them. ^dict-zwjf
32. As a dictation user, I want a missing or unreadable glossary to leave the attempt running without it and to be reported, so that a glossary problem never blocks dictation. ^dict-5lsq

### Agent skill

33. As a dictation user, I want `pie skill install [PATH]` to install a bundled glossary skill into my agents' skill directories the same way `nema skill install` does, so that any agent can build a glossary for me. ^dict-a52r
34. As an agent, I want the skill to tell me how to create or update `.pie/glossary.md` for a directory (which names, identifiers, tools and commands to collect, and how to list the ways speech recognition mishears them), so that the glossary actually helps both passes. ^dict-5w57
35. As a dictation user, I want the skill install to report each target as installed, replaced or current, support dry runs and forced replacement, and never wait for input without a terminal, so that it is safe in scripts. ^dict-pguq

### Audio retention and long recordings

39. As a dictation user, I want the audio of every attempt kept for 24 hours by default, with the retention configurable and 0 disabling it, so that I can re-run a recording that went wrong. ^dict-c0yv
40. As a dictation user, I want retained audio stored compactly (Opus when an encoder is available, WAV otherwise), so that keeping it costs almost no disk space. ^dict-agvw
41. As a dictation user, I want each history entry to name its audio file and `pie retry` to accept a history ID, so that I can run a past recording through the current passes again. ^dict-z1ax
42. As a dictation user, I want Pie to keep recording locally when the pass-1 stream closes or fails while I am still speaking, and to transcribe the whole recording buffered after I release the key, so that a long dictation is never cut off. ^dict-oj7u
43. As a dictation user, I want the captured audio retained for `pie retry` after any failure, including a pass-1 stream that closed during capture, so that speech is never discarded. ^dict-u5xj
44. As a dictation user, I want expired audio removed at service start and at each new attempt, so that the archive never grows past the retention window. ^dict-g3ap

### One pass

45. As a dictation user, I want a one-pass mode in which an audio-capable model through OpenRouter hears the recording and receives my prompt and the glossary in one request, so that the words it is unsure of are resolved from the audio rather than guessed from text. ^dict-dq3o
46. As a dictation user, I want the configuration to choose between the legacy single pass, one pass and two passes with one `pipeline` key, so that I can switch approaches without losing the others. ^dict-rtdw
47. As a dictation user, I want one-pass prompts of my own for tidy transcription, raw transcription and translation, with the same placeholders as pass 2, so that a key without correction still gets a literal transcript with my spellings. ^dict-gi5u
48. As a dictation user, I want to set the OpenRouter provider order (for example Google AI Studio first), so that I can pick the route with the best latency. ^dict-4y4r
49. As a dictation user, I want one-pass attempts written to the history and the audio archive like two-pass attempts, so that nothing I say is lost whichever mode I use. ^dict-msdh
50. As a dictation user, I want a failed one-pass request to keep the audio for `pie retry` and notify me, so that a provider error never costs me the dictation. ^dict-pna4

### Prompts, vocabulary and diagnostics

36. As a dictation user, I want default pass-2 prompts for correction and translation shipped with Pie, and no prompt that claims context is appended when it is not, so that the documentation matches the behavior. ^dict-4faf
37. As a maintainer, I want the domain glossary to retire "Project context" and define glossary, pass 1 and pass 2, so that code and conversations use one vocabulary. ^dict-ewqw
38. As a dictation user, I want `pie status` to show the pass-1 model, the streaming switch, the pass-2 model and the bindings, and the logs to record each pass's latency, so that I can check what is configured and where time goes. ^dict-oagf

## Implementation Decisions

- **Vocabulary.** "Pass 1" (transcription) and "pass 2" (correction; translation is a pass-2 prompt) are the domain terms. "Glossary" replaces "project context"; the global glossary and the project glossary are its two sources.
- **Opt-in by configuration.** New STT configuration keys: the pass-1 model (`transcription_model` keeps its provider-prefix syntax), a `streaming` flag, a pass-2 model, a pass-2 reasoning effort and a pass-2 timeout, and a `[[bindings]]` array. A binding has a keysym, a shift flag, a mode (transcribe or translate), a pass-2 flag and an optional pass-2 prompt file name. Without `[[bindings]]`, the legacy `ptt_*` keys produce the two bindings they produce today. Without a pass-2 model and with a conversational pass-1 model, Pie runs today's single-pass path with the audio prompts unchanged. A translate binding requires a pass-2 model; a configuration that has one without the other is rejected.
- **Prompt files.** Pass-2 prompts live in the same prompts directory as today's audio prompts, under new names, so the old files keep serving the legacy path. Defaults: `correction.md` for transcribe bindings and `correction-translate.md` for translate bindings; a binding may name another file there. A correction prompt must contain `{{glossary}}`; a translation prompt must contain `{{glossary}}`, `{{source_language}}` and `{{target_language}}`; `{{language}}` is optional. The transcript is sent as the user message. Pie ships defaults and a missing file falls back to them, as today.
- **Pass-1 adapters.** Codex realtime: the existing transcription session (not the conversation session) with the chosen model; the glossary goes into the session's transcription prompt. Streaming uses the existing incremental stream handle; buffered sends the sealed recording after release. OpenRouter: the existing `/api/v1/audio/transcriptions` client with language and the glossary as prompt; buffered only. Measured on 2026-09-30 (docs/research/2026-09-30-dictation-models): gpt-live-transcribe through the Codex login answers about 0.6 s after release when streaming; MAI-Transcribe-2 through OpenRouter about 1.0 s p50 buffered and most accurate.
- **Pass-2 adapter.** One OpenRouter chat request (system = rendered prompt, user = transcript), temperature from the configuration default 0.2, reasoning effort passed through, bounded by the pass-2 timeout (default 8 s) and the existing result budget. Any non-completed outcome, blank or oversized text is a pass-2 failure. If Ox's OpenRouter client lacks a text-only request, record the gap in the missing-Ox-API document and use the smallest local request following the existing HTTP capability pattern.
- **Pipeline.** The attempt's recognition step becomes: pass 1 → append history entry (raw) → pass 2 when the binding asks for it → append history entry (final, outcome, durations) → delivery. It keeps today's recovery rules: a pass-1 failure or blank result keeps the audio for `pie retry`; a delivery failure publishes the final text as today. A pass-2 failure on a transcribe binding delivers the raw transcript and notifies; on a translate binding it delivers nothing, notifies, and ends like a delivery failure that retains the raw text.
- **Glossary resolution** happens at key press, before pass 1 starts, so streaming sessions can include it. Take the focused window's PID from niri; among that process and its descendants, use the working directory of the most recently started leaf, falling back to the window process itself; walk up from there to `$HOME` or the root for `.pie/glossary.md`. Files are read at every attempt, symlinks followed, no size cap. The global glossary is `glossary.md` in Pie's configuration directory unless `PIE_GLOSSARY` names another file. The rendered glossary is the global file then the project file, each under a heading naming its path.
- **History.** Append-only JSON Lines at `$XDG_DATA_HOME/pie/history.jsonl` (default `~/.local/share/pie/history.jsonl`), file 0600, directory created 0700 like the recovery store. One line per event keyed by an attempt ID: `transcribed` (raw text, pass-1 model, pass-1 duration, mode, binding, window, glossary paths) and `finished` (final text, pass-2 model, pass-2 duration, outcome: delivered, delivered-raw-after-correction-failure, correction-failed, delivery-failed). Writes are synchronous appends of complete lines; a failure logs a warning and does not change the attempt's outcome.
- **CLI.** `pie history [--limit N] [--json]` lists entries newest first; `pie history show [ID|last] [--raw] [--json]` prints one entry's final (or raw) text. `pie skill install [PATH] [--force] [--dry-run] [--no-input]` follows Nema's skill-installation contract (targets `~/.claude/skills`, `~/.codex/skills`, `~/.pi/agent/skills`, `~/.agents/skills` when their roots exist; one JSON result per target). The skill tree is compiled into the binary with a Dune generator, as Nema does.
- **Audio archive.** Every attempt's captured PCM (the recovery draft Pie already writes while recording) is copied at settlement to `$XDG_DATA_HOME/pie/audio/<attempt-id>.opus`, encoded with `ffmpeg` (found on PATH or through `PIE_FFMPEG`) at speech bitrate, or `.wav` when no encoder is available. Encoding runs after delivery and never delays or fails the attempt. `audio_retention_hours` (default 24) bounds the archive. `0` disables archiving of new attempts and does not delete existing files. A positive value deletes expired regular archive files at service start and when an attempt starts; symbolic links are not followed. History entries carry the audio path; `pie retry ID` decodes it (through the same encoder) and reruns pass 1 and pass 2 with the current configuration.
- **Stream failure during capture.** A pass-1 streaming failure no longer ends the capture: the attempt keeps appending to its draft until release, then runs buffered pass 1 on the whole draft (Codex through the websocket, OpenRouter through its endpoint). Only if that also fails does the attempt end as a pass-1 failure, and the sealed audio is published for `pie retry` as today. This applies to the legacy single-pass path too, because the draft is shared.
- **Pipeline switch.** `pipeline = "legacy" | "one-pass" | "two-pass"` (default legacy) replaces the `two_pass` flag, which was never deployed. One pass uses `transcription_model` (OpenRouter provider, audio-capable model) for transcribe bindings and `translation_model` for translate bindings, with `thinking_effort` as its reasoning effort; it is buffered (no audio streaming) and `streaming = true` is rejected with it.
- **One-pass request.** One OpenRouter Responses call: `instructions` = the rendered one-pass prompt, input = one user message with a short text part and the WAV as `input_audio`; optional `provider.order` from `openrouter_provider_order` (list of provider names, fallbacks allowed). Same bounds, typed failures and sanitized diagnostics as the pass-2 adapter, which it shares. Measured: Responses and chat completions have the same latency (p50 about 3.0 s, p90 about 5.5-5.9 s in an interleaved run of 30 each).
- **One-pass prompts.** `one-pass.md` (transcribe bindings with correction), `one-pass-verbatim.md` (transcribe bindings with `correct = false`) and `one-pass-translate.md` (translate bindings), embedded defaults, binding `prompt` override; required placeholders as for pass 2.
- **One-pass history.** The result is recorded as the attempt's final text, and the `transcribed` event carries the same text with the one-pass model as its model, so `pie history show --raw` still prints what the model heard. A failure publishes the audio for `pie retry`, notifies, and records outcome `failed`.
- **Bindings in the keyboard path.** The recognizer maps key events to a binding instead of to one of two modes; the recording mode stays transcribe or translate, and the binding travels with the attempt to the pipeline and history.
- **Status.** The status JSON and `pie status` include the pass-1 model, streaming flag, pass-2 model and bindings; the per-pass durations are logged with the existing structured logger.

## Testing Decisions

- A good test drives Pie through its public seams and asserts what the user observes: the typed text, notifications, history lines, CLI output, and requests the provider fixture received. It does not assert internal function calls.
- **Main seam: the assistant attempt pipeline** in the assistant service, which tests already drive with injected recognition and delivery (test_assistant_owner, test_explicit_prompts, test_retry via recovery_store). Extend the injection with pass-2 and history dependencies, and test with fakes: raw and corrected text reach history and delivery (dict-dozi, dict-3dqh, dict-s0jv, dict-ldo9); pass-2 failure, timeout and blank on transcribe deliver raw and notify (dict-bpuh); translate failure delivers nothing and keeps text (dict-swyn, dict-vwco); oversized pass-2 output (dict-fk8i); history failure does not change delivery (dict-6frx); a binding without pass 2 skips it (dict-wrbh); retry goes through the same flow (dict-ynad); legacy configuration still takes the single-pass path (dict-26tm).
- **Provider adapters** against local fixtures, following test_realtime_outcomes (websocket fixture) and test_openrouter_outcomes (HTTP fixture): the Codex transcription session carries the model, language and glossary prompt (dict-dl3q, dict-m2xr, dict-wha3, dict-q6xd); the OpenRouter transcription request carries language and prompt; the pass-2 request carries the rendered prompt, transcript, model, reasoning effort, and respects the timeout (dict-l4zp, dict-g52m).
- **Configuration** through the existing config-boundary and live-config suites: bindings parsing, legacy key mapping, required placeholders, streaming rejected for OpenRouter, translate without pass-2 model rejected (dict-wrbh, dict-ezcr, dict-i28t, dict-g52m, dict-4faf).
- **Glossary** with real directories and real child processes: a child started in a project directory resolves to that project's glossary; outside a project only the global file is used; symlinked files are read; missing files report and continue; edits are seen on the next resolution (dict-gnkq, dict-cods, dict-c5j3, dict-cwri, dict-eiod, dict-60is, dict-zwjf, dict-5lsq).
- **CLI** in the style of test_cli_status: `pie history` listing, `show last`, `--raw`, `--json` on a synthetic history file (dict-zzju, dict-5xmu, dict-z87z, dict-b18k); `pie skill install` into temporary roots: fresh install, current, replacement with and without `--force`, `--dry-run`, no input without a TTY (dict-a52r, dict-5w57, dict-pguq); status output (dict-oagf).
- Live provider checks stay out of the default suite, as the existing live_* executables do.

## Out of Scope

- Evaluation datasets or any quality-measurement tooling (audio is retained for recovery only).
- A live on-screen preview of partial text, or typing text while I am still speaking.
- A pass 2 that receives audio.
- Changes to the installed binary, the systemd unit or my configuration files; switching the running service is a separate decision after this lands.
- New speech-to-text providers beyond Codex realtime and OpenRouter (ElevenLabs, xAI or Soniox APIs directly).

## Further Notes

- On 2026-09-30 two long dictations (11:12:32 and 11:24:54) failed after exactly 11 min 6 s each with "Codex realtime stream closed" on the legacy gpt-realtime-2.1 path, and their audio was discarded because a capture-time stream failure aborts the draft. dict-oj7u and dict-u5xj address the loss; the cause of the cutoff (server session limit or Pie) is being probed separately.
- The seams above were not reviewed with the user before writing; the user asked for autonomous work.
- Which pass-2 model to configure is the user's choice. Benchmarks (docs/research/2026-09-30-dictation-models): gemini-3.8-flash at low reasoning is the most accurate (about 1.8-2.0 s p50, under 3 s p90); gemini-3.1-flash-lite is about 1.0 s p50 / 1.4 s p90 with weaker term recovery. Qwen3-ASR, Grok STT and Whisper are not usable for Polish as pass 1.
- The Codex ChatGPT token is refused by the plain OpenAI transcription endpoint ("no credits") but accepted by the realtime transcription websocket, so free gpt-transcribe/gpt-live-transcribe must go through the websocket, even in buffered mode.
- If one terminal process owns several windows (for example Alacritty in daemon mode), the most-recently-started-leaf rule may pick the wrong window's directory; the user's terminal setup decides whether this matters.
- The user's current prompt files still describe appended window and project context. They are the user's files and are not edited by this work; the new default pass-2 prompts are shipped instead.