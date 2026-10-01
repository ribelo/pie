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
- The delivery queue is FIFO by attempt start; a failed attempt leaves the queue without blocking later ones. FIFO waiting uses a cancellable read lock, not the cancellation-protected mutation lock. Settlement releases its FIFO/processing slot in a finalizer around phase, dictation, writer and archive/publication cleanup, including exceptions; cleanup defects remain visible.
- Each attempt retains its own audio writer, archive path, history ID and retry payload. A real archive/history regression holds the first request, completes the second first, and verifies distinct IDs, text and audio files without bypassing FIFO delivery.
- `ptt_release_grace_ms` is a non-negative integer, measured on the monotonic clock, default 1,000 ms; 0 disables continuation. Grace is sampled at capture start. The existing 2,000 ms finishing tail remains (extended when grace exceeds it). A same-binding press outside grace or a different binding during the tail ends the old capture immediately and admits a new attempt after handoff. Binding identity is keysym plus shift, not merely transcription mode.
- Processing fibers stay within a supervised child scope of the service switch and retain their scoped resources until settlement. They outlive capture handoff but not the PTT loop: cancellation or typed loop failure cancels detached workers before the child scope closes. The existing stalled-finish budget regression guards this lifetime boundary. Only the capturing fiber reads microphone/input events. Fatal typed/composite processing errors return to the service supervisor; ordinary per-attempt failures still recover after publishing retention.
- Normal and recovery deliveries use the same FIFO position, retained through delivery and cleanup. Retained and archived `pie retry` always use clipboard-only delivery: recovery has no press-time target, and retry-time focus must never authorize paste.
- Retry admission reserves a processing-only FIFO slot immediately; a queued retry never owns capture. A removable service-switch settlement hook is registered before enqueueing and releases queued admissions even when shutdown or failure prevents dequeueing. Enqueue failure releases immediately; dequeued settlement removes the hook. Existing retry refusal while paused or capturing remains, and queued retries count toward the same four-slot cap.
- Status remains additive: `active` and `phase` describe microphone ownership; `processing` counts detached attempts including FIFO wait, delivery and cleanup. Settlement persists the updated count even when no capture state changes and never clears a newer capture. Older status without `processing` decodes as zero. An enabled assistant without capture still reports armed while processing continues.
- Regression coverage exercises blocked first requests, reversed completion order, failed and ignored FIFO heads, four-slot refusal, toggle capture during processing, pause isolation, grace continuation/expiry/zero/different binding (including the same mode), and clipboard-only retained/archive recovery. Audio writers and history IDs remain per attempt; retention keeps its existing last-published-candidate policy. No live service installation or restart is part of this change.

## Verification (2026-10-01)

- Whole pie `@all` passed using `./scripts/dev-env.sh dune build --root "$PWD" --build-dir "$PWD/_dev/b-all" --only-packages pie @all`.
- All 42 offline test executables passed (701 Alcotest cases), including the root suite, mocked OpenRouter/realtime outcomes, full assistant ownership, archive/history isolation, and the 100-settled-attempt resource plateau. The inventory contains 43 `test_*.exe` files; only `build_record` was excluded. No live-provider probes ran. Each executable used `./scripts/dev-env.sh --private-home`.
- Four review regressions were red-checked before production fixes: FIFO wait ignored cancellation until its head settled; simultaneous writer fsync/close defects left two processing slots instead of one; a queued retry owned capture; shutdown left queued capture ownership behind. All four pass after the fixes. The queued-retry regression also checks fresh capture, capture-only pause, reservation of all four processing slots and fifth-retry refusal.
- The legacy held-retry budget edges now assert capture-only pause, fresh capture admission, clipboard-only retry, and full settlement (`active = false`, `processing = 0`). Assertion guards release held cleanup even on failure.
- Existing PTT budget regressions passed after introducing the supervised child scope, including stalled finish, root cancellation, typed budget failures, and retained cleanup defects.
- `scripts/caps-gate`, formatting checks on changed OCaml files, and `git diff --check` passed. The complete post-review matrix passed on stable artifacts without interruptions; do not rebuild binaries while tests that spawn themselves are running.
