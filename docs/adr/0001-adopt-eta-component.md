# Adopt the Eta component runtime for the assistant service

The assistant service ran as one nested-callback pyramid: resources were acquired in `with_*` scopes, nothing had an explicit stop, and teardown depended on Eio switch unwind order. We decided to rebuild the service lifecycle on the `eta_component` runtime. All subsystems become entries in one desired-state tree, submitted to one component context. The component runtime is new, and Pie develops Eta, so Pie also serves as its proving ground.

## Considered Options

- **Keep the callback pyramid.** Rejected: no single lifecycle authority, no dependency validation, and no restart granularity. Runtime reconfiguration was limited to swapping configuration refs.
- **Strangler migration.** Rejected: `Context.run` owns its body, so the pyramid and the component tree cannot share one process without two competing switch hierarchies. We do one flag-day rewrite of `Assistant.run`.
- **One-to-one component mirroring.** Rejected: wrapping each scoped resource as an opaque component gives teardown order but no admission checking. We declare typed coeffects for the real exchanges (`audio_stream`, `key_events`, `recording_state`, `stt`, `wake_events`, `control_events`) so admission validates the wiring.

## Consequences

- The Eta opam pin moves to a master commit that contains `eta_component`, and `eta_component` joins the pinned package list.
- `Daemon.*` modules are renamed to `Assistant_service.*`. The RPC wire format stays compatible; the `Status` response gains an additive `components` field.
- Feature toggles for providers use dormant providers, not entry-level `enabled` flags. A disabled entry publishes no provisions, so its consumers would wait forever.
- The STT backend provides a factory coeffect. Per-press sessions stay as they are. A provider-field change in the configuration makes the watcher submit a replacement batch, so a failed candidate rolls back to the working backend.
- OTLP observability stays outside the component tree so it can report context shutdown.
- The generic loader and native HMR packages remain spec-only in Eta. Pie watches its own configuration files and calls `reconcile` or `replace` directly.
