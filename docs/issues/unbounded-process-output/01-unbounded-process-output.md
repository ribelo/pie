---
status: ready-for-agent
category: bug
blocked_by: []
---

# Bound the three whole-process output reads

## User story

As a Pie user, I want subprocess output acquisition to be bounded, so a faulty
producer or malformed archive cannot exhaust the resident service's memory. ^pieout-j4q1

## Requirements

- When Pie captures shell, encoded-audio or decoded-PCM output, it shall enforce a finite byte budget before accumulating the entire output. ^pieout-zxt7
- If a producer exceeds that budget, Pie shall return a named output-size error and release the subprocess resources. ^pieout-6noh

## Evidence — 2026-10-06

Exactly three production sites pass Eio.Buf_read.take_all to
Eio.Process.parse_out, whose internal reader uses max_size:max_int:

| Site | Output |
|---|---|
| [Sh.run_capture:67](<../../../packages/pie/sh.ml#L64-L69>) | Command stdout |
| [Audio_archive encoding:200](<../../../packages/pie/recording/audio_archive.ml#L191-L208>) | Encoded Opus bytes |
| [Audio_archive decoding:278](<../../../packages/pie/recording/audio_archive.ml#L272-L287>) | Decoded PCM bytes |

The resolved-identity Ox read visitor reports 3 production and 3 test sites;
84/99 production/test files are decided, 1/0 lack typed annotations, and 0/0
have visitor-unknown evidence. No memory-exhaustion incident was reproduced.
The separately bounded assistant reader is not part of this issue.

## Acceptance and delivery boundary

A finite-output control succeeds; over-budget output returns the named failure
without retaining partial output or leaving the child alive. Archive budgets
reflect the supported audio contract rather than an arbitrary max_int.

This commit records the risk only. It changes no Pie code, installed binary,
shared switch, or running service. The remedy is a later authorized slice.
