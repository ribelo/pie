---
status: todo
category: bug
package: pie
consumer: Pie
blocked_by: []
---

# Clipboard confirmation must not follow wall-clock jumps

1. As a dictating user, I want clipboard confirmation to have a monotonic elapsed deadline, so that a system-clock correction neither extends a stalled confirmation nor reports failure prematurely. ^pieclip-c8t2

**Observed defect:** [text injection:216–227](</home/ribelo/projects/ribelo/pie/packages/pie/input/text_injection.ml#L216-L227>) sets the deadline from Eio.Time.now at216;222 subtracts a second wall reading;227 interprets nonadvance as failure. These are three gate occurrences of one defect, not three incidents. A backward correction increases remaining time or triggers false failure; a forward correction exhausts the budget immediately. No actual host clock was changed and no historical incident is claimed.

**Replacement:** pass Eio.Time.Mono.t into this confirmation path; compute elapsed spans and remaining budget from Mono/Mtime. Keep wall clocks for timestamps and absolute civil deadlines.

**Acceptance:** a fake wall clock stepping forward/backward must not change successful confirmation or its fixed timeout; a nonadvancing monotonic test clock must not spin. Use deterministic fake clocks, never mutate the host clock.

**Evidence:** Ox ox-monotonic-elapsed-time audit at SHA256558783c1c933c4f812ae3e7bd4f80dd76a0acbd29f703e4978e15584cd19a8a7 emitted216,222,227; the opaque call at225 remains disclosed. This is a new clipboard path defect, not the completed capture-clock issue. Ticket only: no Pie code, binary or service change. The mandatory shared-Ox verification passed build, formatting and all727 tests/43 suites, but its shared-switch engine ox_lint@2206becfc5cd3a64811078003d74f33281e1e150 emitted130 existing findings and zero typed unknowns. Its launcher SHA256 is7909d27adfc9469f681ecdf9fc737a95faaa28ec1736214c4aa559007741ab4b; this was neither the flake-locked engine nor current Ox master. This documentation-only issue changes none of those call sites. The architect authorized this issue-only baseline exception; no hook is bypassed.
