# Remove shadow component orchestration

## Status

Parent-reviewed complete in the workspace. Final verification passed all 554 tests across 34 registered private suites, including Slow cases, and a fresh artifact was built with matching source/native-dependency attestations. The verified executable was subsequently installed and the service restarted by explicit user request; no historical crash causality is claimed.

One canonical assistant workflow replaces the shadow component graph.

## Earlier implementation checkpoints (superseded)

Parent-reviewed implementation complete and integrated. PIE003. Whole-suite pending. Not deployed; no historical crash causality claimed.

Shadow graph deleted (12 assistant modules, Commands.Assistant, assistant_shim). `eta_component` dropped from packages/pie/dune. Single owner: Pulse.with_client → with_record_stream → Keyboard.with_monitor → Clipboard.create → `run_with_inputs` (PTT + retry + 500ms stamp watcher). Watcher publishes Config.Stt.t+stamp only after systhread load succeeds; failures log and keep last-good. Required audio failure sets coordinator lastError (typed category) and fails the par. Components fields removed from assistant_service contract/server/client. bin/pie assistant retargeted to Service.run.

Focused assistant_owner: last-good then successful reload, required input lastError, Recording_overload, keyboard Input_lost, cancel waits, Pause FD release, production Codex streaming via private-child WS fixture. Fake STT covers Openrouter clip only; Codex uses production_stt. PIE006 effective-binding/physical-key design is next and is not started.

## Requirements

- When PIE reports assistant health, PIE shall derive that report from the state of the workers that perform the requested workflow. ^pieowner-5fja
- If a required input worker stops unexpectedly, then PIE shall expose that failure instead of reporting an operational recording workflow. ^pieowner-k9u9

## Decision

Delete the assistant component graph rather than completing its placeholders. Keep one explicit application owner for real input, recording/session state, provider operations, delivery and necessary command/config control. Pass typed resources directly; use existing Eta/Eio resource and worker scopes where they actually provide lifecycle guarantees. Do not replace Eta_component with a homegrown generic component framework.

## Evidence

All12 assistant support modules (excluding service.ml) were read in full across rounds3/10. Stt_backend19–20 returns literal transcribed text/translated text. Ptt24–46 ignores all six dependencies and owns a dummy unit. Wake31–57 builds and discards read_loop, despite its ownership comment. Pulse_client and Keyboard_monitor forward Env streams with no-op releases, or create inert substitutes. Recording_coordinator forwards a global or allocates a spare coordinator. Rpc_server returns a control stream and ignores its coordinator; it does not start a server.

Env stores six capabilities/streams in Obj.t refs and exposes unchecked casts. Supervision retries activation of this graph rather than owning actual input restart. Config_slices contains unit configurations and a large provider-only comparison. Real input is opened in Service903–905; actual STT dispatch is402–439 and the PTT loop runs1207. The provider-only route965 onward constructs replacement revisions/candidates for the dummy STT component. This is shadow state, not the operative data path.

## Deletion and migration

Remove assistant/{coeffects,config_slices,env,keyboard_monitor,ptt,pulse_client,recording_coordinator,rpc_server,stt_backend,supervision,tree,wake}.ml after transferring any real duty identified by callers. These12 modules contain772 source lines at audit observation, excluding the larger service integration. Remove graph construction/reconciliation, replacement fences, component diagnostic caches and their global providers from Service.

Update Assistant_service.Contract components_field/Status_with_components codecs and Server components_provider consumers to truthful application state or remove those fields. Do not keep fabricated health as compatibility output. Preserve functioning command routing, validated config reload, current-session configuration consistency, PTT recognition and resource cleanup. These are actual application duties, not justification for retaining the dummy graph. Separate wakeword review is complete under PIE024: the standalone routes are synthetic placeholders, not a speech-trigger workflow. Remove them explicitly with their settings/help; preserve user training/model assets.

Remove Commands.Assistant (583 lines) and Assistant_shim (1 line): the only production command entrypoint forwards to Assistant.Service, already exported as Pie.Assistant.Service. Retarget that caller directly. Preserve necessary corrected recognition behavior under PIE006 in the real input seam, and retarget helper tests; do not keep a duplicate implementation merely because tests call it. The other helper bodies have no in-repository production caller.

Remove eta_component dependency only after a repository-wide consumer census confirms all remaining consumers are gone. Remove tests that merely assert placeholder activation; replace them with behavior tests of real workflow ownership. No implementation during this audit.

## Acceptance

With synthetic input/provider/delivery adapters, prove record-to-delivery uses the configured provider result; required input failure is visible; cancellation settles actual workers; failed reload preserves the last valid config; successful reload affects subsequent operations without corrupting an in-flight operation. Status must follow actual worker failure, not graph activation. Exercise CLI/RPC users of removed status fields. No Obj-based resource globals or inert fallback streams remain in assistant wiring.

## Eta prerequisites

No Eta_component reconciliation/replacement fixes are prerequisites for the deleted graph. Keep the PIE002 physical session retirement dependency and only the ordinary scopes/typed-error/cancellation contracts the real workflow uses. Do not expand application architecture to exercise optional Eta features.
