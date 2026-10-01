---
status: completed
package: pie
consumer: pie
---

# Two-pass dictation with glossary, history and correction switch

## Status

2026-10-01: default prompts now remove incidental profanity, dict-x9pf.

- 2026-10-01 12:02: installed master 2134e89 (sha256 4406a46a…, previous kept as `pie.pre-leakfix-20261001`) with `audio_retention_hours = 168` and the merged glossary (`glossary.md.pre-merge-20261001` kept). The install script did not stop when Pie was busy and restarted the service during a 5-minute dictation; the audio was retained and recovered with `pie retry`, which pasted into the then-focused window (fixed in the overlapping-attempts slice: retry delivers to the clipboard only). Installs now wait for `armed` and 60 s without capture or processing, and abort otherwise.
- 2026-10-01 13:4x: installed master 781eab8 (sha256 8d059edc…, previous kept as `pie.pre-install2-20261001`): legacy pipeline removed, actionable notifications, ignored outcome, build identity, schema language framing, prompt_rules. The user's one-pass prompts now remove incidental profanity (`*.pre-profanity-20261001` kept). `pie status` reports build 781eab8e545a, effort low, schema dictation/text-v1.

2026-10-01 glossary revision: one global glossary now serves every application, replacing process-tree/project discovery (used 0 times in 87 attempts). `pie glossary` reports the file and documents maintenance; the bundled agent skill and `pie skill install` are retired. This work does not install the binary, restart the service, or replace the active glossary.

2026-10-01: incident 3808-30 returned HTTP 200 with a choice-level provider error (`finish_reason: error`, native `OTHER`, code 500, `unmapped`) and truncated content. HTTP success is not dictation success. Controlled replay reproduced the error with WAV at low reasoning effort but not at medium, so the single automatic retry uses WAV and at least medium reasoning. MP3 remains 32 kbit/s mono with Xing and ID3v2 headers disabled. Full provider bodies now remain private beside audio for seven days by default; history names the bodies and records sanitized diagnostics plus retry format and effort. This change is verified on the upload worktree only; the live service and user configuration have not been changed. A non-delivering replay through the new encoder and adapter succeeded on the first MP3/low request in 6.869 s (bundled prompt, empty glossary); this does not establish transcript quality or the cause of the earlier failure. Existing response-size bounds remain: no complete body can be retained when transport fails before returning one.

2026-09-30 afternoon: one pass (dict-dq3o and following) was added after a comparison showed Gemini hearing the audio got 7 of 8 hard spots right against 2 of 8 for two passes, at similar p50 latency (docs/research/2026-09-30-dictation-models). One pass through OpenRouter with Google AI Studio first becomes the recommended configuration (docs/examples/stt.one-pass.toml); two passes stay available. One pass is implemented on master; all suites pass except session_ownership (Ox TLS race). Installed 2026-09-30 16:18 at the user's request:

- `~/.cache/opam/5.4.0+ox/bin/pie` is built from master 90af250 against Ox 76d2596d (the `_dev/ox-install` the suites ran on), release profile, sha256 843370d7d0263ed7…, 13202176 bytes. Previous binary (sha256 7e2dd08e2b123725…) kept as `pie.pre-dict-20260930` beside it.
- `~/.config/pie/stt.toml` is docs/examples/stt.one-pass.toml plus the user's existing newline and text-entry rules; the previous file is `stt.toml.pre-one-pass-20260930`. A starter global glossary was created at `~/.config/pie/glossary.md`.
- Roll back: copy both `.pre-*` files back over `pie` and `stt.toml`, then `systemctl --user restart pie.service`.
- After one restart: active, NRestarts 0, Result success, startup config shows pipeline one-pass with Google AI Studio first, `pie msg status` armed.
- Two defects found on the first real dictation and fixed the same hour: every one-pass reply was rejected as "unsupported output" because Gemini adds a `reasoning` output item (fixed in the adapter, binary reinstalled, sha256 93e7ca4239dc4164…); and the installed config had taken the examples' F9/F10 bindings instead of the user's Right Ctrl (65508) with Shift for transcription and Right Ctrl alone for translation (restored; examples corrected). The failed attempt's audio is in the archive. A third: archives were always WAV because ffmpeg could not infer the container from the `.partial` temp name; fixed with `-f ogg` (installed sha256 50b0642904644f11…). Verified after the fixes: one translation and one transcription through Right Ctrl, delivered, one-pass 2.6 s and 4.1 s, both in `pie history`.
- 2026-09-30 evening: a 4-minute dictation failed locally as "input too large" (Ox's 4 MiB request default); its audio was kept and transcribed by hand. One pass now uploads MP3 and sizes the request to the capture budget; prompts forbid analysis in the output; `one_pass_timeout_seconds = 120` in the user config. Then Pie adopted ox ffe57e78 (rows 57 and 59): full suite 693 tests in 42 suites, 0 failed (session_ownership included); installed binary sha256 c6148d01b1d989e9…; live_one_pass on the 4-minute recording returned in 3.6 s.
- 20:15: that install restarted the service 32 s into a dictation, and the audio was discarded because a cancellation the user did not ask for aborted the draft. Fixed: such cancellations now publish the captured audio for `pie retry` (dict-u5xj, red-checked test); installed at 20:31 after waiting for Pie to be idle, sha256 13a25820d9bca1f6…. Two captures at 20:12 were ignored by design (held 2 s and 0.16 s, near-silent).

Implemented on master (2026-09-30); every suite passes except session_ownership, which fails on the Ox TLS buffer race recorded in docs/issues/session-ownership-wss-flake. Not installed: the running service still uses the previous binary and the user's legacy configuration. A proposed configuration is in docs/examples/stt.two-pass.toml. Streaming pass 1 still dies after about 11 minutes until Ox fixes its consecutive-ping limit (docs/issues/long-recording-cutoff); the recording then falls back to buffered pass 1 instead of being lost.

## Problem Statement

I dictate most of my work in Polish full of English programming terms, and Pie's output is often garbage: "Nema" comes out as "NEMa", "Pie" as "Pi", "Chrome CDP" as "Front Servera". The configured model, gpt-realtime-2.1, is a conversational voice model that is asked to transcribe and rewrite in one step, so it guesses at words it does not know. Nothing tells it which words I use: the old project context (`.pie/context.md`) was removed (piecontext-avnq) and my prompts still claim that window and project context is appended, which is no longer true.

When a text is lost, it is lost for good. The recovery store keeps only one failed attempt; a delivered text that I later delete, or that the target application swallows, is gone. I have already recovered transcripts by hand more than once.

I also cannot choose, per key, whether I want my stream of thought turned into a tidy message or left as I said it, and I cannot switch between streaming and buffered recognition.

## Solution

Dictation becomes two passes. Pass 1 is a dedicated speech-to-text model that writes down literally what I said, streaming audio while I hold the key when the provider supports it. Pass 2 is optional per key binding: a fast text model of my choice gets the raw transcript, my glossary and my own prompt, and returns the final text, which can be heavily rewritten if my prompt says so. Translation is pass 2 with a translation prompt.

One global glossary tells each selected pipeline which words I use and how they get misheard, including project terms in topic/project sections. `pie glossary --help` teaches its format and gives a coding-agent maintenance recipe; no project resolver or bundled skill is needed.

Every attempt is appended to a local text history before delivery, with the raw transcript and the final text, and `pie history` gives it back.

If pass 2 fails or times out, Pie types the raw transcript and tells me so, instead of losing the text.

One-pass is the default; two-pass remains a configured alternative. Removed conversational configurations fail loading rather than selecting a different recognizer.

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
22. Retired by dict-r5lg: the conversational-default migration is removed; supported pipelines retain PTT key-to-binding compatibility. ^dict-ezcr
23. Retired by dict-r5lg: conversational single-pass compatibility is removed; an absent pipeline selects one-pass. ^dict-26tm
24. As a dictation user, I want `pie retry` of a retained failed recording to go through the same pass 1, pass 2 and history as a live attempt, so that retried text gets the same treatment. ^dict-ynad

### Glossary

25. As a dictation user, I want a global glossary file in Pie's configuration directory, with its path overridable through an environment variable of the systemd service, so that words I use everywhere are always known. ^dict-gnkq
26. As a dictation user, I want a project glossary at `.pie/glossary.md` in the nearest ancestor of the focused window's working directory at key press, so that dictating in a project terminal picks up that project's words without any manual step. ^dict-cods
   Retired (2026-10-01): one global glossary replaces project discovery and the bundled skill; `pie glossary --help` documents maintenance.
27. As a dictation user, I want the focused window's working directory to come from the process running inside it (the shell or editor under the terminal), so that a terminal opened in a project resolves to that project. ^dict-c5j3
   Retired (2026-10-01): one global glossary replaces project discovery and the bundled skill; `pie glossary --help` documents maintenance.
28. As a dictation user, I want only the global glossary used when the focused window is not in a project, such as a browser or Slack, so that dictation there still works. ^dict-cwri
   Retired (2026-10-01): one global glossary replaces project discovery and the bundled skill; `pie glossary --help` documents maintenance.
29. As a dictation user, I want the global and project glossaries both passed, each labelled with its source, so that project words add to my general vocabulary. ^dict-eiod
   Retired (2026-10-01): one global glossary replaces project discovery and the bundled skill; `pie glossary --help` documents maintenance.
30. As a dictation user, I want glossary files read at every attempt, so that an edit takes effect at the next key press without a restart. ^dict-60is
31. As a dictation user, I want glossary files followed through symlinks and read in full, so that I decide what goes in them. ^dict-zwjf
32. As a dictation user, I want a missing or unreadable glossary to leave the attempt running without it and to be reported, so that a glossary problem never blocks dictation. ^dict-5lsq

### Agent skill

33. As a dictation user, I want `pie skill install [PATH]` to install a bundled glossary skill into my agents' skill directories the same way `nema skill install` does, so that any agent can build a glossary for me. ^dict-a52r
   Retired (2026-10-01): one global glossary replaces project discovery and the bundled skill; `pie glossary --help` documents maintenance.
34. As an agent, I want the skill to tell me how to create or update `.pie/glossary.md` for a directory (which names, identifiers, tools and commands to collect, and how to list the ways speech recognition mishears them), so that the glossary actually helps both passes. ^dict-5w57
   Retired (2026-10-01): one global glossary replaces project discovery and the bundled skill; `pie glossary --help` documents maintenance.
35. As a dictation user, I want the skill install to report each target as installed, replaced or current, support dry runs and forced replacement, and never wait for input without a terminal, so that it is safe in scripts. ^dict-pguq
   Retired (2026-10-01): one global glossary replaces project discovery and the bundled skill; `pie glossary --help` documents maintenance.

### Audio retention and long recordings

39. The system shall retain attempt audio and private provider responses for 168 hours by default, with configurable retention and 0 disabling both archives. ^dict-1y3i
40. As a dictation user, I want retained audio stored compactly (Opus when an encoder is available, WAV otherwise), so that keeping it costs almost no disk space. ^dict-agvw
41. As a dictation user, I want each history entry to name its audio file and `pie retry` to accept a history ID, so that I can run a past recording through the current passes again. ^dict-z1ax
42. As a dictation user, I want Pie to keep recording locally when the pass-1 stream closes or fails while I am still speaking, and to transcribe the whole recording buffered after I release the key, so that a long dictation is never cut off. ^dict-oj7u
43. As a dictation user, I want the captured audio retained for `pie retry` after any failure, including a pass-1 stream that closed during capture, so that speech is never discarded. ^dict-u5xj
44. As a dictation user, I want expired audio removed at service start and at each new attempt, so that the archive never grows past the retention window. ^dict-g3ap

### One pass

45. As a dictation user, I want a one-pass mode in which an audio-capable model through OpenRouter hears the recording and receives my prompt and the glossary in one request, so that the words it is unsure of are resolved from the audio rather than guessed from text. ^dict-dq3o
46. As a dictation user, I want `pipeline` to choose one-pass or two-pass, so that I can switch between the supported approaches. ^dict-rtdw
47. As a dictation user, I want one-pass prompts of my own for tidy transcription, raw transcription and translation, with the same placeholders as pass 2, so that a key without correction still gets a literal transcript with my spellings. ^dict-gi5u
48. As a dictation user, I want to set the OpenRouter provider order (for example Google AI Studio first), so that I can pick the route with the best latency. ^dict-4y4r
49. As a dictation user, I want one-pass attempts written to the history and the audio archive like two-pass attempts, so that nothing I say is lost whichever mode I use. ^dict-msdh
50. As a dictation user, I want a failed one-pass request to keep the audio for `pie retry` and notify me, so that a provider error never costs me the dictation. ^dict-pna4

### Bounded provider recovery and private evidence

51. If a one-pass or pass-2 request fails with a typed provider, transport, timeout, invalid-response, unsupported-output or malformed-output failure, then the system shall retry exactly once through the same path for live attempts and `pie retry`, using WAV when the first upload was MP3, otherwise preserving the input, and setting reasoning effort to medium unless it was already medium or higher. The second outcome shall be final; cancellation shall propagate without a retry. ^dict-6wq3
52. When a provider exchange returns a complete HTTP response body, the system shall retain its full bytes as `<attempt-id>.response.json`, or `<attempt-id>.response-1.json` for the failed first call, with mode 0600 beside the audio; history shall name the retained paths and record sanitized first/final diagnostics plus the retry container and reasoning effort. Archive write failure shall only warn and shall not alter delivery. Provider bodies shall not appear in logs or notifications. ^dict-x6e9
53. When preparing an MP3 upload, the system shall encode 32 kbit/s mono without Xing or ID3v2 headers (`-write_xing 0 -id3v2_version 0`). ^dict-4c5v

### Safe delivery

54. As a dictation user, I want Pie to paste only into the niri window captured at key press, so that changing focus during processing never puts my words into another window. WHEN the delivery-time focused id equals the key-press id, the system shall offer the delivery text to the clipboard before each paste and apply the configured newline rules without character typing. ^dict-snot

55. As a dictation user, I want my text kept in the clipboard when its original window is no longer focused, so that I can paste it myself without losing it. WHEN focus is changed, missing or unavailable (or the key-press id is unknown), the system shall copy the text without sending any key or changing focus, send exactly one notification `Pie: text for <title> is in the clipboard` using the original title and no transcript, and record a delivered outcome with `delivery = "clipboard"`; a paste records `delivery = "pasted"`. The system shall retain `window.id` with the key-press app id and title; older history may omit both new fields. ^dict-ib9n

56. As a dictation user, I want every delivery to use the clipboard rather than simulated character typing, so that my clipboard manager always has the text. WHERE a legacy text-entry rule says `method = "type"`, the system shall load it as paste and warn at startup with the rule index and matcher. The system shall preserve existing paste-key selection rather than invent terminal shortcuts. ^dict-mg0p

### Prompts, vocabulary and diagnostics

36. As a dictation user, I want default pass-2 prompts for correction and translation shipped with Pie, and no prompt that claims context is appended when it is not, so that the documentation matches the behavior. ^dict-4faf
37. As a maintainer, I want the domain glossary to retire "Project context" and define glossary, pass 1 and pass 2, so that code and conversations use one vocabulary. ^dict-ewqw
38. As a dictation user, I want `pie status` to show the pass-1 model, the streaming switch, the pass-2 model and the bindings, and the logs to record each pass's latency, so that I can check what is configured and where time goes. ^dict-oagf

## Implementation Decisions

- **Delivery (2026-10-01, dict-snot / dict-ib9n / dict-mg0p).** Capture the niri id alongside app id/title at key press, reuse that snapshot for glossary/history, and compare ids at delivery. Clipboard-only is success, not a retry/error, with one title-only notification; never focus a window. Paste remains best-effort Ctrl+V: no terminal-specific key policy existed, so none is invented. Matching-window newline policies collapse text or paste line segments with the configured newline keys, then restore the full clipboard text. Clipboard-only preserves the normalized multiline transcript. Retire the typing backend and `pie type`; old `method = "type"` rules become paste with a startup warning naming the rule (also warned after a stable hot reload). Unknown targets, including retained-text retries, stay clipboard-only. Optional history `window.id` and `delivery` keep old records readable. The focus check and paste cannot be atomic across external niri/clipboard/key processes: `pasted` records key dispatch, not proof of application receipt.

- **Vocabulary.** "Pass 1" (transcription) and "pass 2" (correction; translation is a pass-2 prompt) are the domain terms. "Glossary" replaces "project context"; the global glossary and the project glossary are its two sources.
- **Configuration.** `pipeline` accepts `one-pass` (default) and `two-pass`. Dedicated pass-1 settings, optional pass-2 settings and `[[bindings]]` configure two-pass. Without `[[bindings]]`, the `ptt_*` keys produce transcribe and translate bindings. Two-pass translation requires a pass-2 model. Removed conversational defaults fail at load.
- **Prompt files.** Two-pass prompts are `correction.md` and `correction-translate.md`; one-pass prompts are `one-pass.md`, `one-pass-verbatim.md` and `one-pass-translate.md`. Bindings may name other files in the prompts directory. Prompts require `{{glossary}}`; translation also requires `{{source_language}}` and `{{target_language}}`. `{{language}}` is optional. Missing default files use embedded prompts. The old audio prompt keys and files are removed.
- **Pass-1 adapters.** Codex realtime: the existing transcription session (not the conversation session) with the chosen model; the glossary goes into the session's transcription prompt. Streaming uses the existing incremental stream handle; buffered sends the sealed recording after release. OpenRouter: the existing `/api/v1/audio/transcriptions` client with language and the glossary as prompt; buffered only. Measured on 2026-09-30 (docs/research/2026-09-30-dictation-models): gpt-live-transcribe through the Codex login answers about 0.6 s after release when streaming; MAI-Transcribe-2 through OpenRouter about 1.0 s p50 buffered and most accurate.
- **Pass-2 adapter.** One OpenRouter chat request (system = rendered prompt, user = transcript), temperature from the configuration default 0.2, reasoning effort passed through, bounded by the pass-2 timeout (default 8 s) and the existing result budget. Any non-completed outcome, blank or oversized text is a pass-2 failure. If Ox's OpenRouter client lacks a text-only request, record the gap in the missing-Ox-API document and use the smallest local request following the existing HTTP capability pattern.
- **Pipeline.** The attempt's recognition step becomes: pass 1 → append history entry (raw) → pass 2 when the binding asks for it → append history entry (final, outcome, durations) → delivery. It keeps today's recovery rules: a pass-1 failure or blank result keeps the audio for `pie retry`; a delivery failure publishes the final text as today. A pass-2 failure on a transcribe binding delivers the raw transcript and notifies; on a translate binding it delivers nothing, notifies, and ends like a delivery failure that retains the raw text.
- **Glossary resolution.** At key press, read only `$XDG_CONFIG_HOME/pie/glossary.md` (default `~/.config/pie/glossary.md`) unless `PIE_GLOSSARY` names a replacement. Read in full at every attempt, follow symlinks, impose no size cap, and warn on missing or unreadable files without blocking dictation. Render the file under a heading naming its absolute configured path. `glossaryPaths` has zero or one path. Niri still supplies optional history window metadata, independently of glossary resolution.
- **Retired glossary resolution (2026-09-30 design).** Resolution happened at key press, before pass 1 starts, so streaming sessions can include it. Take the focused window's PID from niri; among that process and its descendants, use the working directory of the most recently started leaf, falling back to the window process itself; walk up from there to `$HOME` or the root for `.pie/glossary.md`. Files are read at every attempt, symlinks followed, no size cap. The global glossary is `glossary.md` in Pie's configuration directory unless `PIE_GLOSSARY` names another file. The rendered glossary is the global file then the project file, each under a heading naming its path.
- **History.** Append-only JSON Lines at `$XDG_DATA_HOME/pie/history.jsonl` (default `~/.local/share/pie/history.jsonl`), file 0600, directory created 0700 like the recovery store. One line per event keyed by an attempt ID: `transcribed` (raw text, pass-1 model, pass-1 duration, mode, binding, window, glossary paths) and `finished` (final text, pass-2 model, pass-2 duration, outcome: delivered, delivered-raw-after-correction-failure, correction-failed, delivery-failed). Writes are synchronous appends of complete lines; a failure logs a warning and does not change the attempt's outcome.
- **Glossary CLI.** `pie glossary` prints the resolved path, existence and physical line count. `--help` documents term lines, `##` section headings, the override, next-key-press reload, and a recipe for a coding agent to collect project names, identifiers, tools, commands and mishearings into the global file. No installation command or bundled skill remains.
- **Retired skill CLI (2026-09-30 design; history CLI remains).** `pie history [--limit N] [--json]` lists entries newest first; `pie history show [ID|last] [--raw] [--json]` prints one entry's final (or raw) text. `pie skill install [PATH] [--force] [--dry-run] [--no-input]` follows Nema's skill-installation contract (targets `~/.claude/skills`, `~/.codex/skills`, `~/.pi/agent/skills`, `~/.agents/skills` when their roots exist; one JSON result per target). The skill tree is compiled into the binary with a Dune generator, as Nema does.
- **Audio archive.** Every attempt's captured PCM (the recovery draft Pie already writes while recording) is copied at settlement to `$XDG_DATA_HOME/pie/audio/<attempt-id>.opus`, encoded with `ffmpeg` (found on PATH or through `PIE_FFMPEG`) at speech bitrate, or `.wav` when no encoder is available. Encoding runs after delivery and never delays or fails the attempt. `audio_retention_hours` (default 168) bounds the archive. `0` disables archiving of new attempts and does not delete existing files. A positive value deletes expired regular archive files at service start and when an attempt starts; symbolic links are not followed. History entries carry the audio path; `pie retry ID` decodes it (through the same encoder) and reruns pass 1 and pass 2 with the current configuration.
- **Stream failure during capture.** A pass-1 streaming failure no longer ends the capture: the attempt keeps appending to its draft until release, then runs buffered pass 1 on the whole draft (Codex through the websocket, OpenRouter through its endpoint). Only if that also fails does the attempt end as a pass-1 failure, and the sealed audio is published for `pie retry` as today.
- **Pipeline switch.** `pipeline = "one-pass" | "two-pass"` (default one-pass) replaces the `two_pass` flag, which was never deployed. One pass uses `transcription_model` (OpenRouter provider, audio-capable model) for transcribe bindings and `translation_model` for translate bindings, with `thinking_effort` as its reasoning effort; it is buffered (no audio streaming) and `streaming = true` is rejected with it.
- **One-pass request.** One OpenRouter Responses call: `instructions` = the rendered one-pass prompt, input = one user message with a short text part and the WAV as `input_audio`; optional `provider.order` from `openrouter_provider_order` (list of provider names, fallbacks allowed). Same bounds, typed failures and sanitized diagnostics as the pass-2 adapter, which it shares. Measured: Responses and chat completions have the same latency (p50 about 3.0 s, p90 about 5.5-5.9 s in an interleaved run of 30 each).
- **One-pass prompts.** `one-pass.md` (transcribe bindings with correction), `one-pass-verbatim.md` (transcribe bindings with `correct = false`) and `one-pass-translate.md` (translate bindings), embedded defaults, binding `prompt` override; required placeholders as for pass 2.
- **One-pass history.** The result is recorded as the attempt's final text, and the `transcribed` event carries the same text with the one-pass model as its model, so `pie history show --raw` still prints what the model heard. A failure publishes the audio for `pie retry`, notifies, and records outcome `failed`.
- **Bindings in the keyboard path.** The recognizer maps key events to a binding instead of to one of two modes; the recording mode stays transcribe or translate, and the binding travels with the attempt to the pipeline and history.
- **Status.** The status JSON and `pie status` include the pass-1 model, streaming flag, pass-2 model and bindings; the per-pass durations are logged with the existing structured logger.

## Testing Decisions

- A good test drives Pie through its public seams and asserts what the user observes: the typed text, notifications, history lines, CLI output, and requests the provider fixture received. It does not assert internal function calls.
- **Main seam: the assistant attempt pipeline** in the assistant service, which tests already drive with injected recognition and delivery (test_assistant_owner, test_retry via recovery_store). Extend the injection with pass-2 and history dependencies, and test with fakes: raw and corrected text reach history and delivery (dict-dozi, dict-3dqh, dict-s0jv, dict-ldo9); pass-2 failure, timeout and blank on transcribe deliver raw and notify (dict-bpuh); translate failure delivers nothing and keeps text (dict-swyn, dict-vwco); oversized pass-2 output (dict-fk8i); history failure does not change delivery (dict-6frx); a binding without pass 2 skips it (dict-wrbh); retry goes through the same flow (dict-ynad); removed pipeline values and conversational configurations fail loading (dict-v2qd).
- **Provider adapters** against local fixtures, following test_realtime_outcomes (websocket fixture) and test_openrouter_outcomes (HTTP fixture): the Codex transcription session carries the model, language and glossary prompt (dict-dl3q, dict-m2xr, dict-wha3, dict-q6xd); the OpenRouter transcription request carries language and prompt; the pass-2 request carries the rendered prompt, transcript, model, reasoning effort, and respects the timeout (dict-l4zp, dict-g52m).
- **Configuration** through the existing config-boundary and live-config suites: bindings parsing, supported PTT key mapping, required placeholders, streaming rejected for OpenRouter, translate without pass-2 model rejected (dict-wrbh, dict-r5lg, dict-i28t, dict-g52m, dict-4faf).
- **Glossary** with real files: default HOME/XDG path, replacement/relative override, project files ignored, edits and symlink retargeting observed on the next read, full large-file reads, missing/unreadable/non-regular files warning without blocking, and physical line counts (dict-gnkq, dict-60is, dict-zwjf, dict-5lsq).
- **Retired glossary tests (2026-09-30 design)** with real directories and real child processes: a child started in a project directory resolves to that project's glossary; outside a project only the global file is used; symlinked files are read; missing files report and continue; edits are seen on the next resolution (dict-gnkq, dict-cods, dict-c5j3, dict-cwri, dict-eiod, dict-60is, dict-zwjf, dict-5lsq).
- **Glossary CLI** in the style of test_cli_status: resolved path/existence/line count, override and next read, missing/unreadable warnings, symlinks, maintenance help, and rejection of the retired skill command.
- **Retired skill CLI tests (2026-09-30 design; history/status tests remain)** in the style of test_cli_status: `pie history` listing, `show last`, `--raw`, `--json` on a synthetic history file (dict-zzju, dict-5xmu, dict-z87z, dict-b18k); `pie skill install` into temporary roots: fresh install, current, replacement with and without `--force`, `--dry-run`, no input without a TTY (dict-a52r, dict-5w57, dict-pguq); status output (dict-oagf).
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
- Historical project-resolver limitation (retired 2026-10-01): if one terminal process owns several windows (for example Alacritty in daemon mode), the most-recently-started-leaf rule may pick the wrong window's directory; the user's terminal setup decides whether this matters.
- The user's current prompt files still describe appended window and project context. They are the user's files and are not edited by this work; the new default pass-2 prompts are shipped instead.