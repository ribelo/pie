---
status: ready-for-agent
category: enhancement
package: pie
---

# Screen capture: show an agent a problem

## Problem

Explaining a visual problem to a coding agent in words is slow and lossy: "this button here, after I click there". Kit Langton's scrawl (github.com/kitlangton/scrawl, macOS and Windows) solves this: one shortcut records the screen, the voice, clicks and ink drawn on the screen, and compiles them into a small bundle an agent can read (contact sheet, keyframes, a time-aligned transcript). Nothing like it exists for Linux and Wayland. Pie already owns the global key, the microphone, the speech model and delivery into the focused window, so the missing part is the screen, the ink and the bundle.

Dictation also has two entry shortcuts (translate and transcribe) although the recording is the same; what happens to a recording should be decided when it ends, not when it starts. Holding the key (push-to-talk) did not work in practice.

## Solution

One recording model for dictation and screen capture: a start key chooses what is recorded (voice, or screen plus voice plus events), and the finish key chooses what is done with it (keep Polish, or translate to English). Holding a key is removed.

A screen capture records the focused monitor, the microphone, click moments and red ink drawn while Shift is held. At the finish pie compiles a bundle with the core of scrawl's algorithm (keyframes cut only on real events, words attached to each keyframe) and delivers the text of `summary.md`, with absolute paths to the images, the same way dictation delivers its text.

Only scrawl's core is ported. No GUI, no review window, no capsule animation, no onboarding or permission screens, no iPhone, no OpenCode session routing (Rafał, 2026-11-03).

## User stories

1. As Rafał, I want one key to start dictation and the finish key to decide transcription or translation, so that I do not choose before I speak. ^scap-k1ey
2. As Rafał, I want hold-to-talk and the Mod5 latches removed, so that there is one way to dictate. ^scap-h0ld
3. As Rafał, I want Shift + Right Ctrl to start a screen capture of the monitor I am on, so that I can show a problem instead of describing it. ^scap-st4r
4. As Rafał, I want to draw red ink over any application while I hold Shift, without clicks reaching the application, so that I can point at things. ^scap-1nk0
5. As Rafał, I want the ink to stay visible in the recording and to be kept as timed strokes, so that the agent sees what I marked and when. ^scap-1nkd
6. As Rafał, I want the moment of each click recorded, so that keyframes follow what I did. ^scap-cl1k
7. As Rafał, I want a visible cue while the screen is recorded, so that I know it is on. ^scap-cue0
8. As an agent, I want 6 to 12 keyframes cut only on marks, clicks and settled scene changes, never on speech pauses over a static screen, so that my context is spent where the signal is. ^scap-kf01
9. As an agent, I want each keyframe paired with the words said around it, so that I know what each image is about. ^scap-kf02
10. As an agent, I want a contact sheet, full keyframes, `summary.md` and a JSON manifest in one directory, so that I can read the capture from a terminal. ^scap-bndl
11. As Rafał, I want images and video scaled down from 4K, so that bundles stay small and providers do not downscale them anyway. ^scap-sz01
12. As Rafał, I want the summary pasted into the window that was focused when I started, only if it is still focused, otherwise copied to the clipboard, so that capture delivery works like dictation. ^scap-dlvr
13. As an agent, I want `pie capture inspect`, `frames` and `zoom`, so that I can look closer at a moment. ^scap-cli0
14. As Rafał, I want bundles removed after the audio retention period, so that captures do not fill the disk. ^scap-ret0

## Implementation decisions

### Keys (decided 2026-11-04, from Rafał's 2026-11-03 direction)

- The keyboard monitor only watches keys (org.freedesktop.a11y.KeyboardMonitor), it does not grab them, so every key pie uses also reaches the focused application. Only lone taps of modifier keys are used: a press and release of the key with no other key in between. Right Ctrl used in a chord (Right Ctrl + C) is not a tap.
- Idle: a tap of Right Ctrl starts dictation; Shift + tap of Right Ctrl starts a screen capture.
- Recording (either kind): a tap of Right Ctrl finishes and keeps the language (Polish transcription); Shift + tap of Right Ctrl finishes with translation to English.
- No cancel key (Rafał, 2026-11-03).
- The target window is the one focused at start, and prompt rules match it, as today. Delivery pastes only when that window is still focused at delivery; otherwise the text goes to the clipboard (Rafał, 2026-11-03).

### Capture

- Source: the whole focused monitor (`niri msg --json focused-output`). Window capture is not done: niri does not report window positions on screen (Rafał accepted the whole monitor, 2026-11-03).
- Video: `wf-recorder -o <output>` scaled to 1920 px wide at capture time, as a subprocess through `Ox_caps` like `wtype` and `wl-copy`.
- Audio: pie's existing capture path, so the microphone is shared and owned by one attempt.
- Clicks: moments only, read from evdev (`/dev/input`, the user is in group `input`). Wayland gives no click position to other clients; capturing positions would need the overlay to take every click and replay it through a virtual pointer, which risks drags and latency (Rafał accepted moments only, 2026-11-03).
- One monotonic clock for video start, audio, clicks, ink and finish.

### Ink

- A `zwlr_layer_shell_v1` overlay surface (niri 26.04 provides it) on the captured output, fully transparent, with an empty input region so clicks pass through.
- While Shift is held during a capture, the input region covers the output; pointer motion adds points to the current stroke; release of Shift ends it and empties the input region again.
- Strokes are rasterised by pie into a `wl_shm` buffer (no graphics library dependency). Being part of the composited screen, they appear in the recording.
- A thin red frame on the output edge is the recording cue.
- Library: the pure-OCaml `wayland` package (2.3, Eio based; needs `xmlm` and `logs` in the switch). Its build under OxCaml is checked first (ticket 02). Drawing is required: if the library cannot work, the feature is dropped, not shipped without ink (Rafał, 2026-11-03).

### Transcript times

- scrawl uses measured word times from Apple's recogniser; with a text-only recogniser (Hex) it cuts the audio into speech islands at silences and spreads each island's words by length, marked as estimated.
- Pie computes speech islands from its own PCM (exact boundaries). The one-pass Gemini request returns the text as segments with approximate start times. Each segment is snapped to the nearest island; words are spread inside it by length. Times are marked estimated, so spoken cues ("here", "now") never cut keyframes. Model times alone are not trusted.

### Keyframes and text (port of scrawl `src/keyframes.rs`, `docs/keyframes-and-text.md`)

- Cut only on marks (ink strokes), click moments and settled scene changes (scene diff on a small luma grid). Never on speech pauses.
- Budget: 6 up to 40 s, then one more per 20 s, at most 12. Ordinary events fill the first 6; the rest are for marks and coverage.
- Marks are never dropped; marks on the same screen share one keyframe at the last of them.
- Coverage: a stretch longer than 100 words or 35 s gets a frame from inside it when one qualifies.
- Each keyframe owns the words from the start of the clause in progress at its anchor until the next keyframe's anchor.

### Bundle

- Directory: `~/.local/share/pie/captures/<id>/` with `recording.mp4`, `audio.wav`, `frames/frame-NN.png` (max 1920 px long edge), `contact-sheet.png` (max 1568 px long edge, the size Claude reads without downscaling), `summary.md`, `capture.json`.
- Images are visual only: screenshot, ink, a small click ring and the keyframe number chip. Text carries meaning: `summary.md` has `[#N · M:SS]` per keyframe with its words, and absolute paths to the contact sheet, frames and video.
- Retention: `audio_retention_hours`, the same as dictation audio.

### Agent commands

- `pie capture inspect <bundle|latest> [--json]`, `pie capture frames <bundle> --from --to`, `pie capture zoom <bundle> --at <M:SS|phrase|mark>`. Frame extraction uses ffmpeg.

### Not ported

GUI, review window, capsule, onboarding, permissions, settings window, iPhone, ScrawlKit, OpenCode routing, Sparkle updates, experiments and the Accessibility tree (no equivalent on Wayland).

### Installation

No pie binary is installed and `pie.service` is not restarted as part of these tickets without Rafał's explicit word.
