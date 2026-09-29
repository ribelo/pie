# Pie prepared-text hang: dependency-level lost SIGCHLD wakeup

## Cause and evidence

Pie master 074eab3, EIO_BACKEND=posix, installed OxCaml 5.4.0-ox2, Eio 1.3+ox and iomux 0.4. No Pie production source was changed for the diagnosis. Pie.Sh.run calls Eio.Process.run directly, not Ox_process. Eio_posix.Process.await waits on an exit-status promise; low_level.Process.reap resolves it after waitpid(WNOHANG), driven by Eio_unix.Process.sigchld. There is one reaper per child, not two competing waitpid owners.

A naturally stalled run had parent PID 2554338 sleeping in poll_schedule_timeout and fake wtype PID 2554358 already a zombie. GDB attached from the ancestor process (normal sibling attach is denied by ptrace_scope=1):

```text
#2 ppoll()
#4 caml_iomux_ppoll (...) at iomux_stubs.c:89
#6 camlIomux__Poll__ppoll_5_19_code () at lib/poll.ml:83
#7 camlEio_posix__Sched__next_13_54_code ()
(gdb) p caml_pending_signals
$1 = {65536}
```

The runtime stores signal n in bit n-1: 65536 is SIGCHLD (17). The OS signal has been recorded, but its OCaml handler has not run. Another hung run (2538926) attached with strace showed an infinite ppoll with only Eio wakeup fd 6; every stdout/stderr/exec-error pipe had already been drained/removed. Thus it is not a child waiting for stdin, output backpressure, or awaiting before drain. The scheduler is asleep while the child-reaping notification remains pending in OCaml.

The iomux 0.4 ppoll primitive calls caml_enter_blocking_section(), then ppoll(..., NULL signal mask). OxCaml checks pending signals before releasing the runtime lock. A SIGCHLD arriving after that check but before the syscall is recorded without interrupting the not-yet-started syscall. ppoll can then sleep forever. The OCaml SIGCHLD callback would broadcast the condition and wake the scheduler, but cannot execute until native ppoll returns.

Tracing at startup changes timing: forty strace runs passed while untraced executions hung. Attaching only after the stall avoids that perturbation.

## Experimental cause-level repair

[iomux-ppoll-experiment.patch](./iomux-ppoll-experiment.patch) changes the native ppoll boundary, not Pie injection logic:

1. Block signal delivery on this thread.
2. Enter the runtime blocking section without executing callbacks under the temporary mask.
3. If an OCaml signal is already recorded, return EINTR rather than sleep.
4. Otherwise ppoll atomically restores the requested/original mask while sleeping.
5. Restore the original mask and errno before re-entering OCaml.

This closes the observed single-domain race. The patch is an upstream diagnostic candidate, NOT a reviewed portable production dependency release: it uses runtime-internal caml_check_pending_signals, fixes the Linux ppoll path only, needs OCaml-version compatibility review, and needs multidomain/masked-signal/fallback-poll tests. A signal recorded on another thread between check and poll still needs a runtime-wide wake mechanism.

Dune refused a source-vendored iomux because installed eio_posix refers to the installed library. Therefore the experiment linked the same changed ppoll primitive using GNU ld --wrap=caml_iomux_ppoll and a local C-only poll_fix library. The full source copy remained isolated under /tmp/pie-hang/vendor/iomux. Nothing was installed. The fixed strace confirms block -> ppoll with an explicit empty mask -> restore.

## Reproducer

[repro/reap.ml](./repro/reap.ml) needs only eio_main, no Pie or Ox. It spawns 1,000 immediately exiting children in one Eio run. /bin/sh -c "exit 0" is used because NixOS has no /bin/true; a normal system can substitute /bin/true. Build with dune, then run 50 separate processes, each with an external timeout:

```sh
for i in $(seq 1 50); do
  timeout --kill-after=1 5 env EIO_BACKEND=posix _build/default/reap.exe || exit $?
done
```

## Stress results

Initial untouched Pie: 0/5 passes, 5/5 timeouts (3-second external timeout). Later measured 50-run baseline, each bounded by 2 seconds:

- Standalone reproducer: 28 passed, 22 timed out, no other failures.
- Prepared-text suite: 49 passed, 1 timed out, no other failures.

Failure frequency is load/timing sensitive; do not interpret 49/50 as reliability.

Patched native primitive: standalone 50/50 passed (50,000 child processes), each externally bounded by 5 seconds.

The installed Ox API changed during the investigation, temporarily preventing untouched 074eab3 from rebuilding. At the lead's direction, the worktree moved to compatible Pie master 489f473; no compatibility adapters were added. On that commit:

- Unpatched prepared-text suite: **37/50 passed, 13/50 timed out**, no other failures (2-second external deadlines).
- Patched prepared-text suite: **50/50 passed**, no hangs or other failures (5-second external deadlines). This ran through the delivered Dune stress alias.

Build/check commands (all foreground):

```sh
timeout 900 nix develop -c env DUNE_BUILD_DIR=_build_hang dune build repro/reap.exe packages/pie/test/prepared_text/test_prepared_text.exe
timeout 900 nix develop -c env DUNE_BUILD_DIR=_build_hang dune build @packages/pie/test/prepared_text/stress
sh -n packages/pie/test/prepared_text/stress.sh
git diff --check
```

The stress alias initially exposed a harness pathname error (status 127: executable basename not on PATH); the script now prefixes relative executables with ./, and the alias passed 50/50. Existing Unix.putenv unsafe_multidomain warnings remain. No full unrelated Pie suite was run.

[pie-stress.patch](./pie-stress.patch) is the clean regression-only diff applicable to Pie master: adds a stress alias and a shell runner, not the experimental native wrapper. The dependency repair is separate; applying only the stress patch does not fix production. To reproduce the fixed experiment, copy poll_fix/ into a Pie worktree, add poll_fix to the prepared-text executable libraries and `(link_flags -cclib -Wl,--wrap=caml_iomux_ppoll)` to that executable stanza. The same wrapper can be linked into the standalone repro. Only Linux native linking was exercised.

## Safe mitigation options without patched dependencies (not implemented)

**Recommended short-term: scoped periodic Eio clock wake while a child operation is outstanding.** A 10-100 ms timer causes ppoll to have a finite deadline, allowing pending OCaml SIGCHLD handlers to run. Cost is 10-100 scheduler wakeups/second only while active; race-hit completion latency is bounded by that interval, subject to normal scheduling. Keep it alive around spawn, output drain, await AND process-switch cleanup. A naive Fiber.first(run, ticker) or daemon under the same cancelling switch is insufficient: cancellation can stop the ticker before protected process cleanup waits for the child. The ticker must be cancellation-protected until a done promise is resolved in a finally after the whole process scope exits; then terminate it promptly. No competing waitpid, no signal-handler replacement, no global timer. Put a private `with_child_reaping_wakeup` helper in Pie.Sh and wrap both run and run_capture around the entire Eio.Process operation (including its internal switch cleanup). The existing public signatures can stay unchanged using Eio_unix.sleep 0.05, which the installed API explicitly provides without clock plumbing; add an explicit eio.unix dependency. This yields at most roughly 20 wakeups/second per outstanding operation, one temporary fiber and a completion promise, no extra OS thread, and approximately <=50 ms race-hit latency absent scheduler starvation. Prefer an explicitly supplied monotonic clock if deterministic capability tracking is required, but that entails plumbing clock through Sh and its callers. The helper must be armed before spawn; cancellation, spawn failure, nonzero exit, output flood and no leaked ticker after completion need dedicated tests. Never use blocking Unix.sleep.

Longer term the same helper could live at the shared subprocess ownership layer in ox, with Pie.Sh routed through it. Changing Ox_process alone does not cover Pie.Sh today because Sh bypasses it. Other direct Eio.Process paths (e.g. clipboard owner) must be audited separately. A global process-wide periodic timer is simpler but imposes idle wakeups and changes unrelated behavior; not recommended.

**Linux pidfd readiness:** readiness is level-triggered and survives a missed SIGCHLD. But merely awaiting a pidfd and then Eio.Process.await does not resolve Eio's promise; another waitpid would race Eio's reaper. A proper pidfd-backed process manager needs ownership of spawn, reaping, cancellation, signal delivery and fd lifetime. This is a larger dependency/backend change, plus Linux >=5.3 support and fallback policy. eio_linux already uses pidfds, but switching production backend is a separate operational decision.

**Dedicated blocking waitpid in an Eio system thread:** only safe if replacing, not competing with, Eio's existing reaper. It costs a system thread per outstanding wait and requires cancellation/cleanup design. Not the smallest Pie mitigation.

A single overall timeout is not a correctness repair: it can delay successful child completion until the deadline and then itself hang during cleanup if the timer has disappeared.

## Cleanup

The detached /tmp/pie-hang worktree was removed with git worktree remove after preserving these deliverables. The Pie checkout, ox checkout, installed libraries/binaries and running services were not modified by this worker. No commits were made. Only the report, patches, standalone reproducer and diagnostic wrapper remain under /tmp/pie-hang-report. Temporary native wrapper changes are not part of the Pie regression patch.
