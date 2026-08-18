---
kind: issue
status: ready-for-agent
---

# Adopt the Eta API-boundary doors in Pie

## Problem Statement

Pie copies process startup, exit codes, OpenTelemetry bootstrap, HTTP fetch,
and component diagnostics handling that Eta now owns. Those copies disagree
with the Eta doors: a sidecar thread runs a second event loop, logs have empty
trace identity, typed failures can render as placeholders, expected foreign
raises become defects, HTTP clients match status numbers and concatenate query
strings, and the assistant service uses `Obj.magic` when a target revision is
missing. Eta added the shared doors because Pie needed them. Pie still does
not use them.

## Solution

Every Pie process runs its command through the Eta Eio process door and
installs OpenTelemetry on that one runtime, outside the assistant component
tree. Production logs go through the runtime logger. A small Pie tee writes
logfmt to stderr and forwards the same record to OTLP when export is enabled.
Speech HTTP uses Eta fetch, error categories, and URL query combinators on one
process-scoped client. Expected foreign raises become typed failures. The
assistant service uses Eta printers, fused settlement waits, a non-optional
target revision, and the diagnostics change stream for the components cache.
The configuration file watcher stays as a 500 millisecond stamp poll.

## Requirements

### Package and build

- The Pie opam package dependencies shall include `eta_otel_eio`. ^pieeta-1ng8
- The Pie development installer package list shall include `eta_otel_eio`. ^pieeta-hwis
- When a developer builds Pie through the Nix package or the development shell, the build shall resolve an Eta source that provides the Eio process door, HTTP fetch, Eio OpenTelemetry scope, and component printers. ^pieeta-v17g

### Process door

- When a Pie command process starts, the process shall run the command effect through the Eta Eio process door. ^pieeta-g8ep
- When the command effect succeeds, the process shall terminate with status 0. ^pieeta-94ds
- If the command effect fails with a typed failure, then the process shall write the rendered failure to stderr and terminate with status 1. ^pieeta-np5l
- If the command effect is interrupted, then the process shall terminate with status 130. ^pieeta-yr8l
- The process shall render typed failures with the Pie command error printer. ^pieeta-y2r7
- While a Pie command process runs, the process shall use one Eta runtime. ^pieeta-u2ve
- When the command effect completes, the process shall drain and shut down that runtime. ^pieeta-q7xg

### Observability

- When a Pie command process starts, the process shall install OpenTelemetry on the process runtime through the Eta Eio OpenTelemetry scope. ^pieeta-lbfz
- While the assistant service runs, OpenTelemetry shall remain outside the assistant component tree. ^pieeta-ze7s
- When production code writes a log record, Pie shall write the record through the Eta observability logger on the process runtime. ^pieeta-al2f
- When Pie writes a log record, the process shall write that record as logfmt to stderr. ^pieeta-bal1
- Where OpenTelemetry export is enabled, when Pie writes a log record, the process shall also export that record through the OTLP logger. ^pieeta-mhid
- While a span is active on the process runtime, when Pie writes a log record, the record shall carry that span's trace identity. ^pieeta-l92v
- Where OpenTelemetry export is disabled, the process shall run the command effect and send no telemetry. ^pieeta-w728
- The process shall read exporter host, port, and disable flags from the standard OTEL environment variables. ^pieeta-llib

### HTTP

- When the OpenRouter speech client sends an HTTP request that Pie owns, the client shall send the request through Eta HTTP fetch. ^pieeta-4c9u
- When the Codex Realtime client sends an HTTP request that Pie owns, the client shall send the request through Eta HTTP fetch. ^pieeta-sq47
- If a fetched response has a non-success status, then the speech client shall fail with a status error that includes the buffered body. ^pieeta-ebw5
- If an HTTP request that Pie owns fails, then the speech client shall classify the failure by Eta HTTP error category. ^pieeta-io0n
- If a Codex token refresh response has status 401 or 403, then the Codex Realtime client shall report an authentication failure that tells the operator to run `codex login`. ^pieeta-5a8b
- If a Codex token refresh response has a non-success status other than 401 or 403, then the Codex Realtime client shall report a request failure that includes a short body excerpt. ^pieeta-mfzo
- When a speech client builds a request URL that includes a query, the client shall set the query through the Eta URL query combinators. ^pieeta-omim
- While a Pie process that performs HTTP requests runs, the process shall reuse one HTTP client for those requests. ^pieeta-8mrm

### Expected raises

- When Pie calls a foreign function whose raise is an expected failure, Pie shall lift that raise as a typed failure in the existing error variant. ^pieeta-nrn6
- If a computation that must not raise does raise, then Pie shall treat that raise as a defect. ^pieeta-42n6
- If configuration file reload raises while the assistant service reads the file, then the assistant service shall keep the previous configuration and write the failure through the process logger. ^pieeta-116s

### Component diagnostics

- When the assistant service reads a component instance's target revision, the service shall use the target revision value supplied by diagnostics. ^pieeta-ff8i
- When the assistant service writes an admission error, phase, integrity, lifecycle, progress, or settlement fence outcome, the service shall format that value with the Eta component printer for that value. ^pieeta-a2lb
- When the assistant service submits a reconcile, retry, or replace and waits for settlement, the service shall wait through the fused Eta await operation for that submission. ^pieeta-fhob
- While the assistant service runs, the components cache updater shall consume the diagnostics change stream: the current snapshot first, then updates in revision order, until the stream closes. ^pieeta-yqcn
- While the assistant service runs, the configuration watcher shall poll the configuration file stamp every 500 milliseconds. ^pieeta-em0a
- When the configuration watcher detects a stamp change and loads the file, the watcher shall submit a replace for an STT-provider-only change and a reconcile for every other accepted change. ^pieeta-izts
- If the component context rejects a submitted desired state at admission, then the assistant service shall keep the previously accepted state and report the admission error through the log and the Status response. ^pieeta-4dcj

### Tests

- When a unit test runs an Eta effect, the test shall run that effect through the Eta Eio one-shot door. ^pieeta-nyk8
- When a live test runs an Eta effect, the test shall run that effect through the Eta Eio one-shot door. ^pieeta-w88k

## Implementation Decisions

- Land the work on one branch as five commits, one completed door each: process
  and OpenTelemetry (including the ADR sentence), HTTP, expected raises,
  component diagnostics, then tests and leftover deletion.
- Replace the custom command runner with `Eta_eio.run_main` and the existing
  Pie command error printer. The door returns the status; the process
  terminates with that integer. Do not call `Stdlib.exit` inside the door.
- Install OpenTelemetry with `with_otlp` around the command effect. Keep it
  outside the assistant component tree so it can report context shutdown.
- Delete the Pie observability module and its sidecar thread. Call sites use
  `Eta_observability` log operations. Remove the eleven `Effect.sync` wrappers
  whose only job was to reach the old global logger.
- Install a Pie-local composite logger at process start: format every record as
  logfmt to stderr, and forward the same record to the OTLP logger that
  `with_otlp` installs. Do not add a fan-out API to Eta in this work.
- Drop `PIE_OTLP` and `PIE_OTLP_*`. Use `OTEL_*` and `OTEL_SDK_DISABLED`.
- Update ADR 0001 so “OTLP outside the component tree” means the process-door
  `with_otlp` scope, not a second event loop.
- Production has no extra `with_runner`. The process door owns the only
  runtime. Unit tests do not share a runner across cases.
- Both speech providers that Pie owns switch to `Client.fetch`,
  `Error.category`, and `Url.with_query` / `Url.with_query_param`. Keep the
  Codex 401/403 re-authenticate remedy as a status-code branch on a status
  error, not a parallel fetch wrapper.
- Create the HTTP client once in the process scope and pass it to speech
  operations.
- Audit every `Effect.sync` that wraps Unix, Yojson, Sys, protocol parse, or
  other foreign code whose raise is expected. Those sites become
  `Effect.sync_exn ~catch` into the existing error variants. Leave `sync` only
  where a raise is a defect. The configuration reload systhread read is one
  such expected-raise site; do not change the watcher shape.
- Use non-optional `target_revision` (this removes the `Obj.magic` fallback).
  Replace local admission, phase, integrity, and fence formatters with Eta
  printers. Use `reconcile_and_await`, `retry_and_await`, and
  `replace_and_await` where the service currently reconciles or replaces and
  then awaits the settlement fence.
- Switch only the components cache updater from `snapshot` plus `await_change`
  to `Diagnostics.changes`. The supervision loop may keep coalesced
  `await_change`. The 500 millisecond configuration stamp poll stays.
- Keep the current admission-failure policy: log, retain previous accepted
  state, surface the error on Status.
- Keep the `Activation.own` no-teardown recipe. Do not add `own_no_release`.

## Testing Decisions

Good tests verify external behavior at a seam, not which helper was deleted.

Two seams:

1. **The command process.** Drive a Pie process (or the process door with the
   same printer and logger installation the binary uses). Gates: success
   terminates with status 0; a typed failure writes a rendered error to stderr
   and terminates with status 1; an interrupt-only cause terminates with
   status 130; a log record appears as logfmt on stderr; with
   `OTEL_SDK_DISABLED` the command still runs and sends no telemetry; log
   records emitted under a span carry that span's trace identity. Cite
   `pieeta-g8ep` through `pieeta-llib`. Prior art: `packages/pie/bin` command
   runner and the existing alcotest suite.

2. **The Eta effect seam.** Assistant service and speech-client effects run
   through `Eta_eio.run_once`. Gates: a missing target revision is impossible
   at the type; replace and reconcile wait through the fused await; the
   components cache follows the diagnostics change stream through snapshot,
   update, and close; a config stamp change still reloads; an STT-provider-only
   change still replaces; other accepted changes still reconcile; admission
   rejection keeps previous state; expected foreign raises become typed
   failures; a Codex token refresh 401/403 reports the re-authenticate remedy;
   URL queries are applied through the URL combinators; HTTP failures classify
   by category. Cite `pieeta-4c9u` through `pieeta-4dcj`. Prior art: the
   alcotest suite under `packages/pie/test/` and the component-runtime gates
   that already use the Eta effect seam.

Do not introduce a shared runner in unit tests. Identity reset on the one-shot
door is the intended test isolation.

Live PulseAudio, D-Bus, and provider WebSocket behavior stays dogfood, except
the existing live STT tests must themselves use the one-shot door
(`pieeta-w88k`).

## Out of Scope

- The other Eta consumers (nema, taumel, inn, grip, exergy).
- js_of_ocaml and `Eta_jsoo`.
- New Eta combinators: annotate or sync-span sugar, `own_no_release`,
  `Entry_id.Map` / `Set`, a cross-runtime `Effect.fresh` misuse guard.
- A cookbook or extra per-package READMEs.
- Replacing the 500 millisecond configuration file poll with inotify or another
  filesystem watch.
- Rewriting `Activation.own` call sites.
- Changing assistant topology, coeffect contracts, or the RPC wire format.
- Dual Eta versions, compatibility aliases, or shims.
- A production `with_runner` layer above the process door.
- Promoting the stderr-plus-OTLP tee into Eta.

## Further Notes

- There is no pull-request workflow. Cut the five commits on one branch.
- Pie follows the local Eta master checkout. The build must pick a revision
  that already contains these doors.
- `REVIEW.md` still names some raise-through-`sync` defects. The expected-raise
  audit should close those that this spec covers. A full `REVIEW.md` rewrite is
  not part of this spec.
- The archived component-runtime spec still defines the tree shape. This spec
  only changes how Pie talks to the process, HTTP, effect, observability, and
  diagnostics doors.
