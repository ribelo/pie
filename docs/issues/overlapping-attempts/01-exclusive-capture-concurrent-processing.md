---
status: open
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

- One capturing attempt at a time; a bounded number of processing attempts (for example 4): beyond it a new press is refused with a notification, so a provider outage cannot pile up requests.
- The delivery queue is FIFO by attempt start; a failed attempt leaves the queue without blocking later ones.
- Audio archive, history and retry are per attempt and already keyed by attempt ID; nothing in them assumes one attempt at a time, verify in tests.
- The release grace is measured on the monotonic clock and defaults to 1,000 ms; 0 disables it.
