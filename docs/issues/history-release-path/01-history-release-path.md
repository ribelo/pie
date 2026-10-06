---
status: ready-for-agent
category: bug
blocked_by: []
---

# Close history descriptors on exceptional validation

## User story

As a Pie user, I want failed history validation to release its descriptor, so
repeated failures do not exhaust descriptors in the resident service. ^piehist-vt0j

## Requirements

- When history acquires a descriptor for validation, it shall establish an exceptional release path before calling operations that can raise. ^piehist-1og3
- If descriptor validation fails, history shall close the descriptor before returning its existing error result; a successfully returned descriptor shall retain its caller-owned lifetime. ^piehist-6j14

## Evidence — 2026-10-06

[History validation:902–918](<../../../packages/pie/history.ml#L902-L918>)
acquires a descriptor through Cap_unix.openfile, then calls Unix.fstat.
If fstat raises, the Unix_error/Sys_error handlers return Error without closing
the acquired descriptor. The ordinary invalid-file branch does close it;
a successful branch deliberately transfers ownership.

This is a concrete structural exceptional-cleanup gap, not a reproduced runtime
leak. A blanket ban on returned raw descriptors would misclassify the success
path and is not the requested remedy.

## Acceptance and delivery boundary

Inject failure immediately after acquisition and observe exactly one close;
success transfers the still-open descriptor to its existing owner. Preserve the
current public error mapping and history-file security checks.

This commit records the risk only. It changes no Pie code, installed binary,
shared switch, or running service. The remedy is a later authorized slice.
