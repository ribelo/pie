---
status: open
package: pie
consumer: pie
---

# Give the model a channel for its deliberation, and pick the prompt by destination

## Problem

In 9 of 75 delivered one-pass attempts on 2026-09-30 and 2026-10-01 (12%; 4 of 26 translations, 5 of 49 transcriptions) the delivered text was the model's deliberation ("The speaker says:", "Wait, let's listen closely around 0:23", "Let's refine:", bullet analyses) followed by, or instead of, the message. The installed prompt already says "Output only the final typed text, once, with no notes, timestamps, analysis". Hand replays of attempt 3808-30-1790844706369562 show the mechanism: at reasoning effort low OpenRouter reports 0 reasoning tokens and the model deliberates inside the answer; at medium it used 1,891 reasoning tokens and the answer was clean, but took 55 s for 5 minutes of audio. The strict single-field schema on master (57abe32) constrains shape, not content, and is untested on the leaking recordings.

The user also decided that profanity is removed from the typed message (the default prompts say "swearing stays"), and that the register depends on the destination: a coworker chat gets neutral wording, a coding agent gets direct instructions, an unknown window gets clean direct language.

## Solution

The one-pass and pass-2 requests use the configuration that docs/research/2026-10-01-leak-experiment recommends: the least costly of effort low or medium and of raw, schema `{text}` or schema `{verbatim, text}` that produced zero leaks on the corpus with p50 under 5 s on short recordings. With the two-field schema the `verbatim` (transcribe) or `source` (translate) field is recorded as the history raw transcript and only `text` is delivered. Field descriptions are protocol framing; the user's prompt remains the only writing instruction.

Default prompts drop incidental profanity (keep stance, force and bluntness), and the spec story that said swearing stays is amended. A `[[prompt_rules]]` array (app_id, title_regex, prompt file name) selects the prompt file for a binding by the key-press window, like text_entry_rules; without a matching rule the binding's prompt applies.

## User stories

1. As a dictation user, I want the model's deliberation to have a place that is never typed, so that the delivered text is only the message. ^dict-7k2q
2. As a dictation user, I want the history raw transcript of a one-pass attempt to be what the model heard (the verbatim or source field), not a copy of the final text, so that I can recover what I actually said. ^dict-m4ws
3. As a dictation user, I want incidental profanity removed from the typed message while my stance, force and bluntness stay, so that the message is sendable as typed. ^dict-x9pf
4. As a dictation user, I want the prompt file chosen by the destination window (app id and title), with my binding's prompt as the default, so that a coworker chat gets neutral wording and a coding agent gets direct instructions without me switching keys. ^dict-c3nt
5. As a dictation user, I want the chosen effort and schema recorded in the startup config JSON and in the history entry, so that I can see which configuration produced a text. ^dict-h8rd

## Implementation decisions

- Experiment first: docs/research/2026-10-01-leak-experiment decides effort and schema; this issue implements the recommendation and records it here.
- Schema descriptions are allowed; prompt text added by Pie is not (dict-g52m).
- The user's own prompt files under ~/.config/pie/prompts are updated by the coordinator at install, not by the slice; the slice updates the embedded defaults and docs/examples.
- prompt_rules are evaluated against the key-press window (the same window delivery targets), never the delivery-time window.
