# Gemini Flash dictation leak experiment

## Result and decision

**Choose low effort + schema-text under the requested rule.** It had **0 leaks in 20 delivered outputs**, zero in each repeat, and **2.190 s p50 on audio shorter than 60 s**. It is the simplest eligible format and had the lowest reported experiment cost, $0.094730 for 24 attempts. **Runner-up: low + schema-verbatim**, also 0/20 leaks, short p50 3.623 s, cost $0.121795.

This is a **conditional experiment recommendation, not a clean production pass**. Both candidates failed 4/24 calls (16.7%), on the same two recordings in both repeats. Schema-text also produced one Polish answer where English translation was required. Failures are not leaks and were not silently added to the selection rule. If zero failures is an additional requirement, **none of the six conditions qualifies**: all medium conditions succeeded without leaks, but their short-recording p50 exceeded 5 s. Medium + raw is the cheapest failure-free option, at 6.166 s short p50.

Adding a sanctioned verbatim field was not necessary to remove the observed leaks: the one-field schema also had none. The two-field format costs more and has a higher typical short latency, although its short maximum was lower. No Pie code, configuration, service, or installed packages were changed. Nothing was committed.

## Protocol and scope

Run: **2026-10-01, 11:12:40–11:19:51 Europe/Warsaw (UTC+02:00)**. Exactly **144 client POSTs**: 12 recordings × six conditions × two repeats. Four workers, 180 s timeout per call, no client retries. A fixed Fisher–Yates shuffle with xorshift seed 20261001 interleaved conditions and repeats.

The private corpus contains four Polish-to-English translations and eight Polish transcriptions, with English IT jargon. Ten recordings have a historical leak label and two are clean. Actual decoded WAV durations are **10–310 s**, with **seven recordings below 60 s**. The planned rough range extending to about nine minutes is therefore not covered by this supplied corpus. Duration means the entire recording, not active speech time.

Each source Opus was converted to WAV, 16000 Hz, mono, pcm_s16le, and verified with ffprobe before call timing. One WAV per recording was reused, with identical audio bytes across conditions. Every request used:

- OpenRouter chat completions, model `google/gemini-3.8-flash`, temperature 0.2.
- Provider preference `{"order":["Google AI Studio"]}`, reasoning `{"effort":"low","exclude":true}` or the identical object with effort set to medium.
- The live [transcription prompt](</home/ribelo/.config/pie/prompts/one-pass.md>) or [translation prompt](</home/ribelo/.config/pie/prompts/one-pass-translate.md>), with Polish/English placeholders and the full [glossary](</home/ribelo/.config/pie/glossary.md>) rendered unchanged.
- Exactly one user text part, "Transcribe this recording.", followed by base64 WAV input_audio.
- Raw: no response_format. Schema-text: strict object with only required string text. Schema-verbatim: strict object with required strings verbatim first, then text. Both schemas prohibit additional properties. Only text is delivered from a schema response.

The two-field descriptions were exactly:

> What the speaker said, written down literally in the order spoken, before any tidying. Working notes belong here.
>
> The final message to type, exactly as the typist instructions require. Nothing else.

**Routing caveat:** provider order is a preference, not an exclusive pin. Responses reported Google AI Studio for 133 attempts (123 successes and all ten failures), and Google for eleven successful attempts. The latter were distributed 0/1/0/1/4/5 across the six conditions in table order. These are results for the requested routing configuration, not an isolated Google AI Studio-only comparison. All responses reported the requested model.

## Measurement and review

Latency starts immediately before fetch and ends after the entire HTTP body has been read. WAV conversion, base64 encoding, and JSON payload construction are excluded. It includes connection, routing, inference, download, and any provider-side fallback. Every attempt, including failures, enters latency statistics. Percentiles use linear interpolation at index (n−1)p.

A single agent reviewer read every one of the **134 deliverable texts** in full and recorded a private yes/no judgment and one-line reason. A leak means model deliberation, commentary, drafts, alternative translations, or unsolicited framing in the delivered message. Dictated commands remain content, and spoken scene timestamps are not automatically leaks. Incorrect language, uncertain transcription, or retained false starts are quality issues, not deliberation leaks.

HTTP/API/choice errors, non-stop finish reasons, empty content, malformed JSON, and schema violations are failures, with leak judgment null. Partial error content is not treated as delivered text. No automatic model judge was used.

Reasoning-token medians use the provider-reported completion_tokens_details.reasoning_tokens for all attempts, including reported zeros on failures. All 144 attempts supplied this field. Rough jargon counts cover clear glossary identity/stem errors and English phonetic substitutions, not ordinary Polish ASR errors, capitalization, or uncertain names without a reference. They are **not WER**.

## All recordings

Leaks show count / deliverable outputs. Times are seconds. Cost is the sum of reported usage.cost, in USD, across 24 attempts.

| Condition | n | Failures | Leaks | p50 | p90 | Max | Median reasoning tokens | Cost |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| low-raw | 24 | 2 | 3 / 22 | 2.837 | 36.380 | 66.481 | 0 | $0.098156 |
| low-schema-text | 24 | 4 | 0 / 20 | 2.900 | 29.814 | 36.242 | 0 | $0.094730 |
| low-schema-verbatim | 24 | 4 | 0 / 20 | 4.398 | 9.166 | 44.167 | 0 | $0.121795 |
| medium-raw | 24 | 0 | 0 / 24 | 7.543 | 22.785 | 82.006 | 1122.5 | $0.233434 |
| medium-schema-text | 24 | 0 | 0 / 24 | 8.085 | 38.889 | 46.382 | 1095.5 | $0.273468 |
| medium-schema-verbatim | 24 | 0 | 0 / 24 | 8.302 | 34.862 | 67.205 | 1187 | $0.271935 |

Total reported cost: **$1.093517**. Failed choices reported zero tokens/cost, so low-condition totals are not a fair estimate for 24 successful outputs. Cost per delivered output was about $0.004736 for schema-text and $0.006090 for schema-verbatim, without any retries. Low effort did not always mean zero reasoning: eleven low-effort calls reported nonzero reasoning usage, including eight in schema-verbatim.

## Audio shorter than 60 seconds

Seven recordings × two repeats = 14 calls per condition. No failures in this subset.

| Condition | n | Failures | Leaks | p50 | p90 | Max | Median reasoning tokens |
|---|---:|---:|---:|---:|---:|---:|---:|
| low-raw | 14 | 0 | 3 / 14 | 2.114 | 4.379 | 5.436 | 0 |
| low-schema-text | 14 | 0 | 0 / 14 | 2.190 | 3.176 | 16.850 | 0 |
| low-schema-verbatim | 14 | 0 | 0 / 14 | 3.623 | 6.232 | 7.931 | 0 |
| medium-raw | 14 | 0 | 0 / 14 | 6.166 | 12.090 | 32.283 | 883 |
| medium-schema-text | 14 | 0 | 0 / 14 | 5.407 | 31.207 | 40.190 | 747 |
| medium-schema-verbatim | 14 | 0 | 0 / 14 | 6.469 | 23.321 | 67.205 | 1113.5 |

## Repeats, failures, and quality

| Condition | Delivered run 1 / run 2 | Leaks run 1 / run 2 | Rough jargon errors | Quality notes |
|---|---:|---:|---:|---|
| low-raw | 11 / 11 | 2 / 1 | 2 | Analysis leaks on two recordings; one short translation omits an opening idea; Nema segmentation and wtype stem errors. |
| low-schema-text | 10 / 10 | 0 / 0 | 0 | One wrong-language translation; more retained false starts/corrections; two recordings fail both repeats. |
| low-schema-verbatim | 10 / 10 | 0 / 0 | 0 | Better correction cleanup on some clips, but an implausible Polish phrase remains; same repeated provider failures. |
| medium-raw | 12 / 12 | 0 / 0 | 3 | Nema becomes NameError once, Shorts stem loses s once, phonetic garbage stem once; no uniform lexical improvement. |
| medium-schema-text | 12 / 12 | 0 / 0 | 2 | Noncanonical chrome-cdp once, phonetic garbage stem once; one translation retains corrected CLI wording. |
| medium-schema-verbatim | 12 / 12 | 0 / 0 | 1 | Phonetic garbage stem once; an opening orchestration request is omitted once. |

Term examples above are diagnostic labels, not copied dictated passages. An unclear technical phrase and a project name vary widely among conditions; those were left out of the rough count because no reference transcript resolves them.

The three leaks were all low-raw:

| Recording ID | Mode / duration | Run | One-line reason |
|---|---|---:|---|
| 3808-7-1790839733515335 | translate / 14 s | 1 | Polish quotation and bullet translation analysis precede and duplicate the final English message. |
| 2265263-20-1790800292160649 | transcribe / 54 s | 1 | English speaker analysis, instruction checking, and multiple drafts precede the Polish message. |
| 2265263-20-1790800292160649 | transcribe / 54 s | 2 | English analysis checks the instruction marker and timestamps, then repeats Polish drafts. |

All ten failures were **HTTP 200 with a choice-level error code 500, finish_reason error, native reason OTHER**, rather than a successful transcript. Recording 3808-29-1790844484648839 (86 s) failed low-raw run 1 and both runs of each low schema. Recording 3808-30-1790844706369562 (310 s) failed low-raw run 2 and both runs of each low schema. Both succeeded in every medium condition. WAV therefore did **not** eliminate the provider-error issue in this run. There were no transport timeouts, HTTP-status failures, or successful-response JSON/schema violations.

## Per-recording scaling for the winner

Seconds per audio minute = latency / (decoded audio seconds / 60). The median of two repeats is their arithmetic midpoint. **F** denotes a failed attempt: its timing is retained, but it does not measure successful transcription speed.

| Recording ID | Mode | Audio s | Latency run 1 / run 2, s | Seconds per audio minute run 1 / run 2 | Median s/min |
|---|---|---:|---:|---:|---:|
| 3808-14-1790841075983053 | translate | 10 | 1.753 / 1.570 | 10.52 / 9.42 | 9.97 |
| 3808-7-1790839733515335 | translate | 14 | 1.761 / 1.755 | 7.55 / 7.52 | 7.53 |
| 3808-21-1790843029815214 | transcribe | 32 | 2.196 / 2.553 | 4.12 / 4.79 | 4.45 |
| 3808-15-1790841119225280 | translate | 36 | 2.063 / 2.219 | 3.44 / 3.70 | 3.57 |
| 2265263-32-1790806273079818 | transcribe | 38 | 2.185 / 2.047 | 3.45 / 3.23 | 3.34 |
| 3808-17-1790841340056274 | translate | 40 | 2.847 / 3.271 | 4.27 / 4.91 | 4.59 |
| 2265263-20-1790800292160649 | transcribe | 54 | 16.850 / 2.954 | 18.72 / 3.28 | 11.00 |
| 3808-29-1790844484648839 | transcribe | 86 | 2.478 F / 3.859 F | 1.73 F / 2.69 F | 2.21 F |
| 2265263-22-1790804090951831 | transcribe | 108 | 12.987 / 3.714 | 7.21 / 2.06 | 4.64 |
| 3808-0-1790836250052130 | transcribe | 204 | 36.242 / 6.512 | 10.66 / 1.92 | 6.29 |
| 2265263-4-1790794734155593 | transcribe | 278 | 5.685 / 35.370 | 1.23 / 7.63 | 4.43 |
| 3808-30-1790844706369562 | transcribe | 310 | 6.743 F / 36.110 F | 1.31 F / 6.99 F | 4.15 F |

Length alone is a poor latency predictor here. The 54 s winner recording varied 16.850 versus 2.954 s, and the 278 s recording varied 5.685 versus 35.370 s. Fixed prompt cost, speech density, inference variability, and routing prevent a reliable linear extrapolation. Do not extend this table to nine-minute audio.

## Evidence and reproduction

Repository deliverables are [the runner](</home/ribelo/projects/ribelo/pie/docs/research/2026-10-01-leak-experiment/scripts/run.mjs>), [request/schema and parsing library](</home/ribelo/projects/ribelo/pie/docs/research/2026-10-01-leak-experiment/scripts/lib.mjs>), [private-review generator](</home/ribelo/projects/ribelo/pie/docs/research/2026-10-01-leak-experiment/scripts/inspect.mjs>), [aggregator](</home/ribelo/projects/ribelo/pie/docs/research/2026-10-01-leak-experiment/scripts/summarize.mjs>), and [synthetic checks](</home/ribelo/projects/ribelo/pie/docs/research/2026-10-01-leak-experiment/scripts/test.mjs>). They use Node built-ins and existing ffmpeg/ffprobe binaries, with no dependency installation.

Private evidence is outside Git: [plan and hashes](</home/ribelo/.local/share/pie/corpus/experiment/plan.json>), [144 manual judgments](</home/ribelo/.local/share/pie/corpus/experiment/judgments.json>), [computed summary](</home/ribelo/.local/share/pie/corpus/experiment/summary.json>), [full review texts](</home/ribelo/.local/share/pie/corpus/experiment/review/>), [converted WAVs](</home/ribelo/.local/share/pie/corpus/experiment/wav/>), and [exact raw bodies plus sibling timing metadata](</home/ribelo/.local/share/pie/corpus/experiment/>). Raw paths follow `experiment/<condition>/<id>.<run>.json`; metadata adds `.meta` before `.json`. Files are private to the user (0700 directories, 0600 files). The API key is read from the existing runtime secret only and never copied or logged.

The saved plan hashes the source and WAV audio, glossary, and rendered prompts. Per-call metadata hashes the exact request payload. Prompts and glossary must match those hashes for a reproduction. Resume preserves completed calls and refuses a changed plan or an orphan raw response; it does not retry failed calls.

Review and aggregation with no new paid calls (review files and summary are regenerated privately):

~~~sh
/etc/profiles/per-user/ribelo/bin/node docs/research/2026-10-01-leak-experiment/scripts/test.mjs
/etc/profiles/per-user/ribelo/bin/node docs/research/2026-10-01-leak-experiment/scripts/inspect.mjs
/etc/profiles/per-user/ribelo/bin/node docs/research/2026-10-01-leak-experiment/scripts/summarize.mjs
~~~

The runner accepts corpus, output root, and concurrency as positional arguments. A new run incurs API charges and needs authorization; the following is a preparation-only example:

~~~sh
/etc/profiles/per-user/ribelo/bin/node docs/research/2026-10-01-leak-experiment/scripts/run.mjs /home/ribelo/.local/share/pie/corpus /home/ribelo/.local/share/pie/corpus/experiment 4 --prepare-only
~~~

Validation passed: synthetic request/schema/extraction/percentile/selection checks, 144 unique calls and judgments, waveform codec/rate/channel checks, and repository privacy review. No corpus audio, dictated passages, raw responses, or secret was added to the repository.

## Limits and next decision

Zero observed leaks is not a guarantee. This is a deliberately leak-heavy, twelve-recording sample with two correlated repeats, one reviewer, no reference transcripts, a small short subset, and mixed backend routing. Four missing outputs in each low schema weaken the leak comparison. Different prompts, audio formats, providers, or future model revisions may change it.

The evidence supports **trying the one-field schema before paying for a verbatim scratchpad**, while separately treating provider failures and language fidelity as unresolved. It does not support silently pasting partial error content, ignoring the wrong-language output, or claiming the lowest-latency candidate is fully reliable. A follow-up focused on the two repeated failures and translation fidelity would be justified, but no additional calls or implementation changes were made in this task.
