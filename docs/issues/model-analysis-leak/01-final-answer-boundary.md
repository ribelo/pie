---
status: completed
package: pie
consumer: pie
---

# Deliver only the framed dictation answer

## Problem

A 2026-09-30 transcription pasted the model's English deliberation about Polish wording, repeatedly beginning with "Wait", followed by one short Polish sentence. This is not the separate Responses reasoning item that Ox already ignores: the analysis was inside message output_text, which Pie previously accepted without any framing. Adding more "output only the transcript" words to the prompt did not prevent it.

## Evidence

- History attempt 2265263-22-1790804090951831: 107-second captured recording, 8,312 ms recognition, 4,286 UTF-8 bytes delivered (4,112 characters). Its archived audio and original history were copied outside the 24-hour archive to the user's private recovery directory. The final sentence is recoverable.
- The installed service was responsive, not permanently deadlocked. Earlier attempts at 20:53:38 and 20:55:25 took about 80 seconds and failed as unavailable; a subsequent retry succeeded. That delay and the leaked-answer incident are separate symptoms.
- The old adapter delivered both a JSON wrapper instead of its contained answer and an unframed analysis blob in local HTTP-fixture regressions. Both tests failed before the repair.
- Live Responses and chat calls on the same preserved audio sometimes returned clean text. The original provider JSON was not stored, so native-model generation versus OpenRouter conversion cannot be distinguished from this evidence. We do not claim the converter caused it.
- Disabling reasoning on Gemini 3.8 Flash is refused by the provider. reasoning.exclude alone is not an answer-content validator. JSON-object mode did not reliably produce an object. Strict schema requests returned the final-text object on the incident recording and the saved four-minute recording; one older sample received a failed, truncated provider response. That is refused, not silently delivered.

## Repair (dict-dq3o, dict-ldo9)

- One-pass and pass-2 requests ask for a strict JSON schema: exactly one text string. The user's prompt remains unchanged and governs its contents, including Markdown and spoken formatting instructions.
- Explicitly exclude reasoning when a reasoning effort is configured. Ox still makes the provider-status, refusal, tool-call and other-output delivery verdict first.
- Return only the decoded text string, checking blank and decoded UTF-8 budgets. Reject unframed text, invalid or truncated JSON, wrong types, duplicate keys and extra fields. Never guess the final sentence by removing a prefix or accept raw text as a fallback.
- Yojson accepts comments, so an envelope-specific lexical check rejects commentary outside strings, invalid escapes and raw control characters before parsing. Literal comment markers, URLs and code inside the text string are preserved.
- Existing failed-one-pass retention and notification behavior keeps the audio available for retry if the provider fails to satisfy the contract.

## Verification

- HTTP-fixture correction suite: 20 tests, including the red-checked incident regression, Markdown/Unicode preservation, separate reasoning items, invalid frames (including comment syntax), status/tool/refusal rejection, budgets and large MP3 requests.
- Full suite before the lexical-tightening review adjustment: 697 passing tests in 42 suites. After that narrow adjustment, correction was rerun green, and all targets were rebuilt.
- Actual Pie live_one_pass replay of the affected recording: clean one-sentence output, 1.6–2.7 seconds in three runs. Saved four-minute MP3: 4.2 seconds. These checks did not type into a focused window.

## Limits and deployment

A schema enforces framing, not the meaning of the text string. A model can still put incorrect wording or commentary inside that field; the test evidence is not a universal semantic guarantee.

No timeout/retry change is claimed to fix the earlier 80-second provider failures. The audit also found upload MP3 encoding is outside the HTTP deadline and unbounded; it was not demonstrated as this incident's cause and is not changed in this slice.

The running service and the user's prompt/config files were left untouched. Installation needs the user's confirmation and a genuinely idle recording snapshot; armed alone is not sufficient to prove idle.
