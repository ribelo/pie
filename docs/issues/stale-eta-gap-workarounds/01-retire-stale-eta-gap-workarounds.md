# Retire Eta gap workarounds that Eta has already fixed or is fixing

## Status

Ready for agent. Documentation and two small code changes; no behaviour change intended.

## Requirements

- When Pie is built against an Eta pin that contains the completed `background-stop-provenance` fix, Pie's PulseAudio reader shall use a named `Eta.Effect.with_background` and `docs/missing-eta-api.md` shall no longer describe the named-background workaround. ^piegap-eg9u
- Before removing that note, the implementer shall run Pie's existing named-background regression tests (`packages/pie/test/test_pie.ml`, the cases around named background clean cancellation) against the current pin and record the result in this ticket's Status. ^piegap-2uhs
- When Pie is built against an Eta pin that contains the completed `main-door-cause-diagnostics` change, Pie's process boundary (`packages/pie/process.ml`) shall stop formatting the full cause itself before the door and rely on `Eta_eio.run_main`. ^piegap-fdeu
- When Pie is built against an Eta pin that contains `openrouter-response-idle-timeout`, Pie's OpenRouter transcription and response calls shall pass a response-idle timeout instead of the 60- and 120-second total timeouts. ^piegap-piegap-gsqw
- When Pie is built against an Eta pin that contains `ws-connect-timeout`, Pie's Codex Realtime connection shall use the client's connect timeout instead of the ten-second total `timeout_as`. ^piegap-piegap-lwro
- Each removed workaround shall be deleted from `docs/missing-eta-api.md` in the same commit that removes the code, with the Eta ticket named in the commit message. ^piegap-4c4p

## Evidence

Eta ticket `background-stop-provenance` (`/home/ribelo/projects/ribelo/ocaml/Eta/docs/issues/background-stop-provenance/`) is completed on Eta master with regressions Supervisor 13–22; Pie's note in `docs/missing-eta-api.md` ("Named background cancellation") predates it. Eta ticket `main-door-cause-diagnostics` is ready for agent; until it lands, requirement piegap-fdeu waits.

## Architecture disposition

Four independent slices, one per Eta ticket. The named-background slice can be done now against the current pin (`flake.lock` updated 2026-09-24, after the fix). The other three each wait for their Eta ticket to land and for Pie's pin to move; none may be started early. Also in this ticket, as slice 0: remove the stale `eta_component` declarations from `dune-project`, `pie.opam`, `flake.nix` (`ETA_OPAM_PACKAGES`) and `scripts/test-local-eta.sh`; Pie has no code using it.

## Acceptance

Pie's full test gate passes; `docs/missing-eta-api.md` lists only gaps that still exist in the pinned Eta.

## Eta prerequisites

`background-stop-provenance` (done); `main-door-cause-diagnostics`, `openrouter-response-idle-timeout`, `ws-connect-timeout` (open) for the later slices.
