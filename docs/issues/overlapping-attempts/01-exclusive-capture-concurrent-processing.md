---
status: implemented
package: pie
consumer: pie
---

# Capture is exclusive, processing and delivery overlap

## Problem

One attempt owns the whole operation from key press to settlement ("Busy" in GLOSSARY.md), so a second press while a 2 to 16 s one-pass request is in flight is ignored. The user has ADHD: waiting for the previous text before starting the next sentence, or before moving to another window, is the real cost of latency. Also, a finger slipping off the push-to-talk key ends the recording, and the immediate re-press is ignored or starts a new attempt.

## Solution

The microphone and the key are exclusive to one capturing attempt; processing (encoding, provider calls, retry) and delivery of earlier attempts run concurrently with the next capture. Deliveries happen in attempt order through one queue, so a long attempt never lands after a shorter later one in the same window. With target-window delivery (dict-* in the delivery slice) a text whose window is no longer focused goes to the clipboard, so overlapping never types into the wrong window.

Releasing the push-to-talk key starts a release grace of 1 s (configurable) inside the existing finishing tail: a press of the same binding within the grace cancels the stop and the same recording continues. Toggle bindings: a press while a previous attempt is processing starts a new capture instead of being ignored. Disabling the assistant still cancels the capturing attempt and lets processing attempts finish.

## User stories

1. As a dictation user, I want to start the next dictation while the previous one is still being transcribed, so that I never wait for the model. ^dict-q7we
2. As a dictation user, I want earlier texts delivered before later ones, so that overlapping attempts never scramble a message. ^dict-b1yt
3. As a dictation user, I want a push-to-talk release followed by a press within one second to continue the same recording, so that a slipped finger does not split or lose a sentence. ^dict-n3kf
4. As a dictation user, I want a toggle press during processing to begin a new capture, so that latching dictation flows like push-to-talk. ^dict-z8hc
5. As a maintainer, I want "Busy", "Recording" and "Processing" in GLOSSARY.md to describe the new ownership, so that the vocabulary matches the code. ^dict-d4jm

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

## Status EOF responsiveness follow-up (2026-10-01)

Native filesystem work on the Eio scheduler domain could expire the local IPC
listener budgets (peer 1 s, ingress 300 ms, handler 400 ms, egress 300 ms) and
close a healthy Status connection. Traces captured fsync at 418.620 ms,
recording-state close at 328.706 ms and 525.101 ms, and archive partial-file unlink
at 457.155 ms. Later isolated traces saw fsync at 670.782 ms, 1,545.260 ms and
975.004 ms for retained payload, manifest and index, and recording-state close
at 1,257.162 ms. `findmnt -T ~/.local/share/pie` and `findmnt -T /tmp` both
identify ext4 on the same LUKS-mapped root filesystem. These are measured native
operation latencies, not a claim about ext4 or encryption generally. No IPC
timeout was increased and no Status retry was added.

Writer, retained-publication/retry, archive and history native operations now use
`Fs.blocking`, backed by `Ox_blocking` with a protected child scope and physical
completion drain. Eio process/network/clock effects stay on the scheduler side.
Retained candidates remain durable before publication is reported; archive and
history completion and ordering guarantees are unchanged.

Offloading persistence alone exposed a second dependency: Status waited on the
coordinator mutation mutex while a native projection write was pending. The
coordinator now publishes one coherent immutable atomic snapshot per mutation;
Status and active-state reads do not take that mutex. A native-held persistence
regression was red before this change and green after it.

**Approved projection decision:** `recording.json` is current state, not a
per-mutation durability contract. The service uses one serial latest-state
worker: the first write completes synchronously and fails startup on error;
later submissions replace one pending immutable snapshot without yielding;
switch shutdown drains the latest state. Worker errors are logged with state
path and exception, matching previous synchronous runtime failure reporting.
This applies only to that projection, not retained candidates, the audio archive
or history appends. Native-held Pause/Resume regressions verify responsiveness,
exact writes `[1; 2; 4]`, final enabled state, asynchronous failure reporting,
and fail-fast initial persistence.

Offloading also exposed fixture assumptions hidden by synchronous stalls:

- A sealed older nonempty draft does not prove a new capture consumed PCM; the
  overlap helper requires actual draft growth. The plateau waits for recorded
  PCM before release or Pause instead of racing native writer admission.
- Fixed 2,000 ms post-roll and physical sealing belong to capture lifecycle,
  not the unchanged 2 s publication allowance. Publication fixtures wait for
  actual recognizer entry within the existing 6 s lifecycle bound, then begin
  that publication wait. FIFO-head admission uses the same lifecycle bound.
- Listener readiness replaces blind 200 ms startup sleeps before the two
  immediate-RPC fixtures; the initial fail-fast state write may yield.
- Delivery callback completion precedes asynchronous retained clearing; the
  startup retry fixture waits for settled processing before its unchanged
  exact candidate-cleared assertion.
- Clipboard stdin becomes visible only when the helper atomically publishes
  its completed file, within the original 100 ms bound; the exact stdin
  assertion is unchanged.
- Readiness/lifetime pipes prove the child bound and remains owner in the
  two-process socket fixture, replacing the blind 200 ms readiness delay and
  fixed 2 s child lifetime.

The approved offline-fixture environment uses the private runtime tmpfs under
`/run/user/$(id -u)` when writable, with `/tmp` as a fallback and the selected
root printed. Both `scripts/dev-env.sh --private-home` and
`scripts/test-local-ox.sh` use this documented default. The plateau fixture
also honors `TMPDIR` rather than hard-coding `/tmp`. This does not alter
production filesystem paths, timeouts, assertions or durability operations.
The explicit native-held regressions prove responsiveness independently of
filesystem speed; ext4 measurements above are separate from fixture timing.

### Follow-up acceptance

- Ten consecutive complete `assistant_owner` runs passed: 52 tests per run,
  520 total, including overlap 0 and 3. Individual runs took 80.109–85.830 s
  on the documented private tmpfs root.
- All 42 offline executables passed, including all 100 settled plateau
  attempts, all 25 control-socket tests and all 26 clipboard-owner tests.
  The build-record executable is the separate opt-in build-log inspection
  check and is not part of this 42-executable offline inventory.
- Local-Ox `@all`, regenerated `pie.opam`, changed-file `ocamlformat --check`,
  shell syntax and `git diff --check` passed. Capability gate passed with
  `EIO_BACKEND=posix SEMGREP_JOBS=2 ./scripts/caps-gate` (the default backend
  encountered an io_uring allocation failure in the scanner environment).
- The private-home `/tmp` fallback and empty, fail-closed environment were
  exercised separately. The local-Ox reinstall harness was syntax-checked;
  the equivalent already-installed local-Ox build and offline executables
  above were used for acceptance, without reinstalling during stable runs.

The native-held persistence regression was confirmed red before snapshot
publication was made lock-free, then green with the final implementation. No
production RPC retry or timeout increase was added. No live binary, configuration
or data was modified; `pie.service` was not restarted, and
`packages/pie/input/text_injection.ml` was left untouched.
