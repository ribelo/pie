---
status: implemented
package: pie
consumer: pie
---

# Capture is exclusive, processing and delivery overlap

## Problem

One attempt owns the whole operation from key press to settlement ("Busy" in CONTEXT.md), so a second press while a 2 to 16 s one-pass request is in flight is ignored. The user has ADHD: waiting for the previous text before starting the next sentence, or before moving to another window, is the real cost of latency. Also, a finger slipping off the push-to-talk key ends the recording, and the immediate re-press is ignored or starts a new attempt.

## Solution

The microphone and the key are exclusive to one capturing attempt; processing (encoding, provider calls, retry) and delivery of earlier attempts run concurrently with the next capture. Deliveries happen in attempt order through one queue, so a long attempt never lands after a shorter later one in the same window. With target-window delivery (dict-* in the delivery slice) a text whose window is no longer focused goes to the clipboard, so overlapping never types into the wrong window.

Releasing the push-to-talk key starts a release grace of 1 s (configurable) inside the existing finishing tail: a press of the same binding within the grace cancels the stop and the same recording continues. Toggle bindings: a press while a previous attempt is processing starts a new capture instead of being ignored. Disabling the assistant still cancels the capturing attempt and lets processing attempts finish.

## User stories

1. As a dictation user, I want to start the next dictation while the previous one is still being transcribed, so that I never wait for the model. ^dict-q7we
2. As a dictation user, I want earlier texts delivered before later ones, so that overlapping attempts never scramble a message. ^dict-b1yt
3. As a dictation user, I want a push-to-talk release followed by a press within one second to continue the same recording, so that a slipped finger does not split or lose a sentence. ^dict-n3kf
4. As a dictation user, I want a toggle press during processing to begin a new capture, so that latching dictation flows like push-to-talk. ^dict-z8hc
5. As a maintainer, I want "Busy", "Recording" and "Processing" in CONTEXT.md to describe the new ownership, so that the vocabulary matches the code. ^dict-d4jm

## Implementation decisions

- One capturing attempt at a time, including setup and the finishing tail. Four processing/delivery slots are allowed; a capture admitted with three processing attempts reserves the fourth. At four processing attempts a fresh press is refused once through `Notification_policy.Capacity_reached` (Notify), so a provider outage cannot pile up requests. Key repeats do not repeat the notice.
- The delivery queue is FIFO by attempt start; a failed attempt leaves the queue without blocking later ones.
- Each attempt retains its own audio writer, archive path, history ID and retry payload. A real archive/history regression holds the first request, completes the second first, and verifies distinct IDs, text and audio files without bypassing FIFO delivery.
- `ptt_release_grace_ms` is a non-negative integer, measured on the monotonic clock, default 1,000 ms; 0 disables continuation. Grace is sampled at capture start. The existing 2,000 ms finishing tail remains (extended when grace exceeds it). A same-binding press outside grace or a different binding during the tail ends the old capture immediately and admits a new attempt after handoff. Binding identity is keysym plus shift, not merely transcription mode.
- Processing fibers stay within a supervised child scope of the service switch and retain their scoped resources until settlement. They outlive capture handoff but not the PTT loop: cancellation or typed loop failure cancels detached workers before the child scope closes. The existing stalled-finish budget regression guards this lifetime boundary. Only the capturing fiber reads microphone/input events. Fatal typed/composite processing errors return to the service supervisor; ordinary per-attempt failures still recover after publishing retention.
- Normal and recovery deliveries use the same FIFO position, retained through delivery and cleanup. Retained and archived `pie retry` always use clipboard-only delivery: recovery has no press-time target, and retry-time focus must never authorize paste.
- Status remains additive: `active` and `phase` describe microphone ownership; `processing` counts detached attempts including FIFO wait, delivery and cleanup. Settlement persists the updated count even when no capture state changes and never clears a newer capture. Older status without `processing` decodes as zero. An enabled assistant without capture still reports armed while processing continues.
- Regression coverage exercises blocked first requests, reversed completion order, failed and ignored FIFO heads, four-slot refusal, toggle capture during processing, pause isolation, grace continuation/expiry/zero/different binding (including the same mode), and clipboard-only retained/archive recovery. Audio writers and history IDs remain per attempt; retention keeps its existing last-published-candidate policy. No live service installation or restart is part of this change.
