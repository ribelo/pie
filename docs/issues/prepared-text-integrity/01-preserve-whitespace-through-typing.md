# Preserve prepared text through typing

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

Text injection preserves verbatim text.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete. PIE020. Final cross-slice suite still pending service refactor; this is not whole-audit acceptance.

Standalone fixture `packages/pie/test/prepared_text` (own dune stanza, `test_pie` untouched) built with `--build-dir` `/home/ribelo/projects/ribelo/pie/.scratch/prepared-text-build/dune` and staged Ox `OCAMLPATH` `/home/ribelo/projects/ribelo/ocaml/ox/.scratch/ws-reader-settle-build/install/default/lib` plus matching stublibs. No shared install. Alcotest `-o` logs under `.scratch/prepared-text-build/alcotest`.

Parent reviewed leaf changes, Niri.type_text removal, and fake emitted argv/prepared-result/failure assertions. Green: 21/21 in 0.058s, run `4IOPSP8Q`. Red first: 14/21, `top\n  indented` emitted as `indented` on wtype and xdotool for Shift_enter and Ctrl_j including inject. Collapse-to-space unchanged. Later helper failure remains Error with the earlier segment captured and no replay.

## Requirements

- When PIE types text prepared for a line-preserving newline action, PIE shall preserve the prepared segments’ whitespace through backend emission. ^pietext-pl6f

## Evidence

Actual prepare_text/type_text composition with a private fake wtype: top followed by two-space-indented indented remains unchanged after prepare_text, but emitted segments lose the two spaces. Effect returns Ok. No real wtype, Niri, keyboard or clipboard operation occurred.

Input.Text_injection131–141 splits the already prepared text; type_segment invokes Wtype.type_text or Xdotool.type_text, which String.trim each segment. inject reports prepared text in result214 even though backend emission can differ. Xdotool has the same trimming source pattern but was not dynamically exercised in this control.

## Implementation direction

Normalize at one explicit application boundary; emit already-prepared segments verbatim. Preserve the deliberate collapse-to-space policy and current outer normalization contract rather than changing all whitespace semantics. Remove duplicate leaf normalization. Keep literal argv separation and safe diagnostic projection under PIE019.

Delete unused Niri.type_text: authoritative-source reference search finds no caller. Its compositor-spawned wtype path duplicates the direct backend and cannot justify another delivery route. Retain Niri focused-window lookup only for actual local routing rules after PIE017 removal; do not imply it pins focus during typing.

## Acceptance

Capture fake wtype and xdotool argv for both Shift_enter and Ctrl_j: indentation, trailing spaces inside a multiline value, empty lines, CRLF and Unicode. Compare actual emitted segments/key events with prepared text and reported result. Preserve explicit collapse behavior.

Delivery is not a transaction: a later helper can fail after an earlier segment was typed. Keep that failure visible and do not automatically replay the entire result as if nothing was emitted (PIE008). Do not promise atomic target focus or rollback from these helpers.

## Ox prerequisites

None beyond existing sequential failure/cancellation semantics. Text normalization is application policy, not another framework seam.
