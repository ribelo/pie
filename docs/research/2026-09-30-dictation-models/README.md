# Dictation models: STT and correction benchmark (2026-09-30)

Evidence for the two-pass dictation spec (docs/issues/two-pass-dictation). Measured from this machine (Warsaw) on 2026-09-30 between 10:05 and 10:40 local time. The scripts are in `scripts/`; they read the OpenRouter key from `/run/agenix/openrouter-api-key` and the Codex login from `~/.codex/auth.json`.

## Speech to text

Input: 10 real Pie recordings from `~/.local/share/pie/failed-recordings` (Polish with English jargon, 16-46 s, 16 kHz mono), 2 runs each (n=20) through OpenRouter `/api/v1/audio/transcriptions` (multipart, `language=pl`), concurrency 3. Latency is request start to full response, which is what a buffered (non-streaming) attempt waits after key release. No reference transcripts exist; quality is my reading of all outputs side by side.

| Model (OpenRouter) | p50 ms | p90 ms | max ms | Quality on Polish + jargon |
|---|---|---|---|---|
| microsoft/mai-transcribe-2 | 1010 | 1688 | 1783 | Best. "Muse Spark 1.3", "satysfakcjonuje", "cron", "NixOS" right |
| deepgram/nova-3 | 773 | 1725 | 1991 | Good but more slips ("nogach" for "logach", "crohna", "NIX, Os") |
| openai/gpt-transcribe | 1455 | 2091 | 2125 | Very good, close to MAI |
| google/gemini-3.5-transcribe | 3252 | 4010 | 4095 | Very good, close to MAI, but slow |
| qwen/qwen3-asr-0.6b | 1396 | 2217 | 2639 | Unusable: Chinese and Russian fragments inserted, words mangled |
| x-ai/grok-stt-1.0 | 1269 | 2620 | 3296 | Poor: garbled sentences, one empty result |
| openai/whisper-large-v3-turbo | 1838 | 2908 | 3457 | Poor: garbled, hallucinated "Dziękuję." |
| qwen/qwen3-asr-1.7b | 2409 | 2977 | 3854 | Poor: foreign-script insertions |
| qwen/qwen3-asr-flash-2026-02-10 | 2488 | 3515 | 3762 | Medium, no punctuation |
| fish-audio/transcribe-1-pro | 2009 | 2505 | 4749 | Good words but emits `<|speaker:0|>`, `[cisza]`, `[click]` tags |
| nvidia/nemotron-3.5-asr-streaming-multilingual-0.6b | 3156 | 4406 | 4558 | Unusable |
| meta/muse-voice-transcribe-1.0 | 7737 | 11024 | 11904 | Good words, far too slow |
| assemblyai/universal-3-5-pro | - | - | - | 400 from provider on every request |

### Streaming through the Codex login (free)

The Codex ChatGPT token is rejected by `api.openai.com/v1/audio/transcriptions` ("no credits"), but accepted by the realtime transcription websocket (`wss://api.openai.com/v1/realtime?intent=transcription`, the path Pie already uses for non-conversation models). Audio was sent in 100 ms chunks at real-time pace, as Pie does while the key is held; the number below is commit (key release) to the final transcript, n=10.

| Model (Codex realtime) | after-release ms, sorted |
|---|---|
| gpt-live-transcribe | 440 449 494 579 587 598 605 617 636 666 (p50 ~590, max 666) |
| gpt-transcribe | 995 1061 1138 1185 1192 1285 1361 1466 1507 1516 (p50 ~1190) |

gpt-live-transcribe quality is close to gpt-transcribe and slightly below MAI-Transcribe-2 (for example "Mi niestety fakcjonuje" for "Mnie nie satysfakcjonuje", "wyspać" for "wysłać").

## Correction (pass 2)

Input: 8 synthetic Polish transcripts with misrecognised jargon (`scripts/llm-cases.json`), a 28-term glossary with mishearing variants, and a prompt asking for glossary-grounded correction plus full tidying. Streaming chat completions through OpenRouter, temperature 0.2, concurrency 2-3. Latency is request start to last token. "Terms" is the share of expected glossary spellings present in the output (it undercounts Polish inflection such as "commicie"; `Pie` versus `Pi` is ambiguous in one case because Pi is also a real tool).

| Model / setting | n | p50 ms | p90 ms | max ms | TTFT p50 | Terms | Notes |
|---|---|---|---|---|---|---|---|
| google/gemini-3.1-flash-lite, reasoning minimal | 24+40 | 983-1009 | 1268-1363 | 1562 | 617-640 | 0.87 | Clean Polish, good tidying, keeps "Pi" instead of "Pie" |
| google/gemini-3.1-flash-lite, default | 24 | 1037 | 1464 | 2068 | 631 | 0.87 | Same as minimal |
| google/gemini-3.8-flash, reasoning low | 24+40+40 | 1680-1977 | 2296-2918 | 4188 | 1221-1431 | 0.97 | Best quality: every glossary term, natural Polish |
| google/gemini-3.8-flash, reasoning minimal | 24 | 1909 | 3171 | 11940 | 1412 | 0.97 | No faster than low, worse tail |
| qwen/qwen3.6-flash, reasoning disabled | 24 | 851 | 1297 | 1513 | 402 | 0.75 | Fastest; leaves "eio", "oks", "Kodeksie" |
| qwen/qwen3.6-flash, default (thinks) | 24 | 12104 | 14184 | 14785 | 10667 | 0.31 | Unusable latency |
| inception/mercury-2.5, reasoning disabled | 24 | 673 | 1145 | 8304 | 660 | 0.76 | Fast median, bad tail, invented words ("skł", "Możebyś") |
| inception/mercury-2.5, default | 24 | 3614 | 5515 | 20887 | 3249 | 0.70 | Reasons by default |
| cohere/command-a-plus, default | 24 | 4422 | 6415 | 7480 | 4276 | 0.81 | Reasons by default; over-shortens |
| cohere/command-a-plus, reasoning low | 40 | 4885 | 7568 | 8856 | 4567 | - | Still slow |
| cohere/command-a-plus, reasoning disabled | 24 | - | - | - | - | - | 422 INVALID_TOOL_GENERATION on every request |

Cost for all correction runs together was below USD 0.30; for all STT runs below USD 0.30.

## Conclusions

- Pass 1: gpt-live-transcribe over the Codex realtime websocket is free and adds about 0.6 s after key release when audio streams during capture. MAI-Transcribe-2 through OpenRouter is the most accurate, at about 1.0 s p50 / 1.7 s p90 buffered. qwen3-asr-0.6b, Grok STT and Whisper (Groq) are not usable for Polish.
- Pass 2: gemini-3.8-flash at low reasoning is clearly the most accurate and stays under 3 s at p90 (worst seen 4.2 s). gemini-3.1-flash-lite is about 1.0 s p50 / 1.4 s p90 with somewhat weaker term recovery.
- Expected end to end after key release: gpt-live-transcribe + gemini-3.1-flash-lite ≈ 1.6 s p50 / 2.0 s p90; gpt-live-transcribe + gemini-3.8-flash low ≈ 2.5 s p50 / 3.5 s p90.
## One pass (Gemini hears the audio) versus two passes

Added the same afternoon at the user's request. Same 10 recordings, 3 runs each (n=30 per row), same glossary. Two passes: gpt-live-transcribe streamed at real-time pace through the Codex login, then google/gemini-3.8-flash (reasoning low) through OpenRouter with the default correction prompt. One pass: google/gemini-3.8-flash (reasoning low) receives the WAV and the same prompt reworded for audio. Latency is from key release to the full final text. Scripts: `scripts/cmp.mjs`, `scripts/cmp-prov.mjs`.

| Pipeline | p50 ms | p90 ms | max ms |
|---|---|---|---|
| Two passes: pass 1 alone (after release) | 604 | 656 | 669 |
| Two passes: pass 2 alone | 1873 | 2523 | 23257 |
| Two passes: total | 2542 | 3127 | 23687 |
| One pass, OpenRouter (default routing) | 2782 | 6522 | 10756 |
| One pass, OpenRouter pinned to Google AI Studio | 2248 | 5047 | 7404 |
| One pass, OpenRouter pinned to Google (Vertex) | 2811 | 4705 | 4789 |
| One pass, Google API direct (latency only) | 2475 | 3812 | 7062 |

The two 17.7 s and 23.3 s pass-2 responses were both on the same recording (s4, where the speaker talks about transcription mistakes); the text-only corrector also invented "odmiana słów z glossary" there.

Quality at the eight hardest spots, judged against what the best recognizers agree was said:

| Spot | One pass | Two passes |
|---|---|---|
| s1 "Contributor" | right | "kontrybutor" |
| s2 "Zero zdziwienia" | right | paraphrased away |
| s3 "Mnie nie satysfakcjonuje" | right | "Co z tego" (guessed, meaning changed) |
| s4 "odmiana grillowania" | right | "odmiana słów z glossary" (invented) |
| s5 opening clause | "minąłem się z rekomendacji" (wrong) | "dałeś mi rekomendacje" (plausible) |
| s6 "mam problem ewentualnie z pisaniem" | right | inverted ("nie mam problemu ... z pisaniem") |
| s9 "Cała konfiguracja" | right | "Co, jeśli" (guessed) |
| s10 "wysłać", "one-shot cron" | right | right |

One pass got 7 of 8, two passes 2 of 8 (s5 counted for two passes). The text-only corrector cannot hear, so where pass 1 garbles a phrase it guesses, and with a tidy prompt the guess reads fluently and hides the error. One pass is also the simpler path. Its costs: no audio streaming during capture, so latency grows with recording length (upload plus audio tokens), and OpenRouter's tail is worse than Google's API direct.
