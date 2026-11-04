---
status: ready-for-agent
category: enhancement
blocked_by:
  - "[[02-wayland-layer-shell-spike]]"
  - "[[04-capture-session]]"
---

# Ink overlay

Spec: [[01-screen-capture]]

**Covers:** scap-1nk0, scap-1nkd, scap-cue0

Layer-shell overlay with an empty input region; Shift held during a capture makes it take pointer input and draw red strokes into a wl_shm buffer; strokes are recorded as timed points. Red edge frame as the recording cue.
