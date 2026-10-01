---
status: completed
package: pie
consumer: pie
---

# Notify only when the user has to act, tell ignored from failed, show what is running

## Problem

The low-volume warning ("microphone input has stayed very quiet for 3 seconds") fires during dictation on the user's quiet microphone, when nothing can be done about it, and every attempt produces notifications the user has learned to ignore. History lists near-silent or very short captures ("PTT capture ignored") as `failed`: 8 of 12 "failures" on 2026-09-30 and 2026-10-01 were ignored presses, so the failed list lies. And to learn which build the service runs, the coordinator had to hash the binary and search an issue document.

## Solution

Notifications are sent only for events that need the user's action: a text left in the clipboard, a failed attempt with its audio kept, a configuration rejected at startup, a provider outage after the automatic retry. The low-volume warning goes; the ignored-capture decision (near silent or too short) is logged, and a notification is sent only if the whole capture was ignored and lasted longer than a threshold (so a real dictation that was too quiet is reported after the fact, an accidental tap is not).

History records `ignored` as its own outcome with the reason (duration, peak, rms), and `pie history` lists it as such. `pie status` and the startup config JSON show the build's git commit and build time, embedded at build time.

## User stories

1. As a dictation user, I want a notification only when I have to do something, so that I read the ones I get. ^dict-f2vx
2. As a dictation user, I want a too-quiet dictation reported once after it ends, not while I am speaking, so that I can repeat it instead of being interrupted. ^dict-j9sw
3. As a dictation user, I want ignored presses listed as ignored, not failed, so that `pie history` shows real failures. ^dict-t6ab
4. As a dictation user, I want `pie status` to show the commit the running binary was built from, so that I know what is installed. ^dict-w1pe

## Implementation decisions

- Notification policy lives in one place (a small module deciding notify or log per event), tested through the pipeline seams.
- The ignored notification threshold defaults to 3 s of held key; shorter ignored captures are only logged.
- Build identity comes from a Dune rule that embeds the git commit (or "unknown" outside a checkout) and is part of the provenance record.

## Implementation and verification (2026-10-01)

- `dict-f2vx`: one pure policy controls actionable notifications. Clipboard-only delivery, rejected configuration, exhausted provider outages, and exhausted attempts with retained audio notify; low/dead input, empty transcripts, delivery failure, and ordinary correction fallback log. Startup validation and daemon reload rejection use the same policy.
- `dict-j9sw`: ignored captures settle once after release. Only holds strictly longer than 3000 ms notify; exactly 3000 ms and shorter holds log. Digital silence does not cancel while the key is held, and later speech in the same hold can still stream.
- `dict-t6ab`: history records `ignored`, with optional held/duration milliseconds and normalized peak/RMS. Old records remain readable; human and JSON listings expose available metrics. Ignored captures neither retain retry audio nor become unfinished failures.
- `dict-w1pe`: builds embed a 12-character Git commit with a dirty suffix and actual UTC rule execution time. Archives without `.git` use `unknown`. `SOURCE_DATE_EPOCH` is intentionally ignored. Startup, daemon IPC/status, CLI version, and binary provenance expose this identity; a differing client build is reported separately from the daemon.

Verified in the isolated worktree (no installation, live daemon restart, or provider calls):

- `dune build --only-packages pie @all` and `scripts/caps-gate`.
- Suites: `notification_policy`, `build_identity`, `build_record`, `control_protocol`, `audio_archive`, `dictation_pipeline`, `assistant_owner`, `correction`, `history`, and `config_boundary`; the full root suite also passed.
- Follow-up root `ptt_dead_input` cases verify no pre-release ignored decision/cancellation, one ignored settlement, and recovered speech after initial flatline.
- Private CLI smoke checks verified one notification for invalid startup configuration, preserved error diagnostics, authoritative daemon identity in human/JSON status, separate differing client identity, and compatibility with an older daemon response.
- Private test runners stub `notify-send` as well as desktop/provider helpers to prevent accidental desktop effects.
