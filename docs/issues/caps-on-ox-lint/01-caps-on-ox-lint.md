---
status: completed
category: enhancement
package: pie (development gates)
consumer: Pie maintainers
---

# Run Pie’s capability gate on compiler-backed ox-lint

## Problem Statement

Ox row 90 replaces Semgrep with the compiler-backed `ox_lint` build tool. Pie’s
locked Ox input and package lists must include that tool, and typed checks need
fresh annotations from Pie’s own build and matching dependency fingerprints.

## Solution

Lock Ox at `d798268590c1b8f81924a7ecc1663dbe33ff3772` (row 90 plus executable
installation, artifact discovery, and authority-provenance fixes), install
`ox_lint` into the same prefix as
`ox_caps`, and build immediately before invoking the installed gate. The fixed
package installs `bin/ox-lint` executable and keeps rule pages and fixtures
under `share/ox_lint`; the installed capability launcher finds that binary.
Keep Pie’s empty allow-list and run the complete gate set without changing the live service.
Pie declares no Semgrep dependency; shell packages otherwise stay unchanged.
The development-shell change also implements Ox story `^ncapslint-ebh6`.

## User Stories

1. As a Pie maintainer, I want the locked input and both Ox installation paths to
   supply the compiler-backed gate, so that capability checks use the pinned
   compiler without Semgrep. ^pcapslint-h9x2
2. As a Pie maintainer, I want the capability wrapper and full test runner to
   build before checking and require fresh typed evidence, so that undecided
   modules cannot look like a green gate. ^pcapslint-t4bn

## Implementation Decisions

The default development shell loads a mutable shared opam switch, not the locked
flake package. The existing private-prefix runner installs `ox_caps` and `ox_lint`
together and mirrors the tool’s installed rule pages and fixtures read-only.
The standalone wrapper builds `@install @packages/pie/bin/check` in `_build`,
where the tool discovers annotations, before invoking the installed launcher with
`--require-typed`. `@check` produces executables’ typed evidence without
reshaping their targets. Two existing formatting failures in the
build-identity Dune rule and test support call are normalized without behavior
changes; these gate-only fixes were explicitly authorized.
The full runner keeps its existing private build for all tests and also runs
that wrapper and formatting. Its `--shared-ox` mode enters Pie’s normal shell
and uses the shared installation without modifying it; the existing no-argument
private installation remains available. No production code or allowance
changes belong to this migration.

## Testing Decisions

Record an installed-launcher run before building (findings and undecided count),
then run `scripts/test-local-ox.sh --shared-ox`: build, installed capability gate,
formatting, and every registered private-HOME test including Slow cases. Record
the exit code of each gate directly, plus `git diff --check`. Any capability finding
requires human review before application code or allowances change.

## Known test-only check gap

The unscoped `dune build @install @check` returned 1, even after a successful
`dune build @all`. Both [live_one_pass.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/live_one_pass.ml>)
and [ws_settlement_probe.ml](</home/ribelo/projects/ribelo/pie/packages/pie/test/ws_settlement_probe.ml>)
report `Error: Could not find the .cmi file for interface` followed by,
respectively, `packages/pie/test/live_one_pass.mli.` and
`packages/pie/test/ws_settlement_probe.mli.` These interfaces are Dune-generated.
The unscoped alias also reported `Error: Unbound module Pie_test_runtime` in
[test_pie.ml:7](</home/ribelo/projects/ribelo/pie/packages/pie/test/test_pie.ml#L7>).
This is a Pie check-alias gap, not an Ox capability finding. No test source was
changed to conceal it. Scope the gate’s `@check` to production bin modules;
retain the full `@all` build and every registered safe test suite.

## Recorded verification

Against the installed `d798268590c1b8f81924a7ecc1663dbe33ff3772`:

| Gate | Exit | Findings | Typed undecided |
| --- | ---: | ---: | ---: |
| Installed launcher before rebuilding | 2 | 0 | 58 |
| Full `dune build @all` | 0 | — | — |
| `dune build @install @packages/pie/bin/check` | 0 | — | — |
| Installed launcher after rebuilding | 0 | 0 | 0 |
| Normal capability wrapper | 0 | 0 | 0 |
| Shared runner build (`@all`) | 0 | — | — |
| Shared runner capability gate | 0 | 0 | 0 |
| Shared runner formatting (`@fmt`) | 0 | — | — |
| Every registered test (720 passes in 43 suites) | 0 | — | — |
| Complete `scripts/test-local-ox.sh --shared-ox` | 0 | — | — |

The prebuild failures were imported-CMI fingerprint mismatches left from the
previous installation. Both installed-launcher runs explicitly classify
[skill_cli.mli](</home/ribelo/projects/ribelo/pie/packages/pie/bin/skill_cli.mli>)
as `not built (not in any Dune target)`; the orphan is unchanged. The first
`c03cbf9a` diagnostic runs had 79 undecided before `@install` and 65 afterward
(both exit 2, no findings); the upstream artifact lookup fix eliminated them.
The original formatting gate returned 1 for the two formatting-only fixes
above; after those explicitly authorized fixes it returned 0.
The complete shared runner captured build, capability and formatting exits
directly, and every one of the 43 registered suites returned 0, including Slow
cases with private HOME and fail-closed desktop helpers.
Shell syntax and `git diff --check` both return 0. No allow-list, behavior,
shared installation, or running service was changed by this worker.
