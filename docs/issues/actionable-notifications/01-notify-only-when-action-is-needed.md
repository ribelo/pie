---
status: open
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
