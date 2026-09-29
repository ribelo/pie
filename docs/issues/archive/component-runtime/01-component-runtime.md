---
kind: issue
status: ready-for-agent
---

# Adopt the Ox component runtime in the assistant service

## Problem Statement

The assistant service is one nested-callback pyramid. Resources are acquired in
`with_*` scopes, no subsystem has an explicit stop, and teardown depends on Eio
switch unwind order. Runtime reconfiguration is limited to swapping
configuration refs: a changed prompt file takes effect on the next key press,
but nothing can restart one subsystem, swap one backend with rollback, or report
which subsystem is unhealthy. The `wakeword_enabled` configuration field is
loaded and never used. Meanwhile Ox's new `ox_component` runtime has no real
adopter, and Pie is its proving ground.

## Solution

Run the whole assistant service as one desired-state tree on one component
context. Each subsystem becomes an entry with a typed configuration slice and
typed coeffect requirements and provisions, so admission validates the wiring
and reconciliation restarts only what a change affects. The existing
configuration watcher drives runtime reconfiguration: ordinary changes submit a
reconcile, an STT provider change submits a replacement batch that rolls back
to the working backend on failure. A supervision loop retries failed
infrastructure entries. The `Status` RPC reports component phases. The
wakeword becomes a real, config-gated participant in the tree.

## Requirements

### Package and build

- The Pie opam package dependencies shall include `ox_component`. ^piecomp-5v7r
- The Pie development installer package list shall include `ox_component`. ^piecomp-7ywj
- When a developer builds Pie through the Nix package or the development shell, the build shall resolve an Ox source that contains the `ox_component` package. ^piecomp-haad

### Component tree

- While the assistant service runs, the service shall operate its subsystems as entries in one desired-state tree admitted to one component context. ^piecomp-akvh
- The assistant service desired state shall contain exactly these entries: `pulse-client`, `keyboard-monitor`, `recording-coordinator`, `stt-backend`, `wakeword`, `rpc-server`, and `ptt-loop`. ^piecomp-vj3k
- The `pulse-client` entry shall provide the `audio_stream` coeffect. ^piecomp-5wy5
- The `keyboard-monitor` entry shall provide the `key_events` coeffect. ^piecomp-2coc
- The `recording-coordinator` entry shall provide the `recording_state` coeffect. ^piecomp-cufj
- The `stt-backend` entry shall provide the `stt` coeffect. ^piecomp-7ksk
- The `wakeword` entry shall provide the `wake_events` coeffect and require the `audio_stream` coeffect. ^piecomp-ibsx
- The `rpc-server` entry shall provide the `control_events` coeffect and require the `recording_state` coeffect. ^piecomp-pfq5
- The `ptt-loop` entry shall require the `audio_stream`, `key_events`, `recording_state`, `stt`, `wake_events`, and `control_events` coeffects. ^piecomp-mk6e
- The assistant service shall give each entry only the configuration slice that the entry consumes. ^piecomp-t085

### Reconciliation and reconfiguration

- When the configuration watcher reloads `stt.toml` or a prompt file successfully, the assistant service shall build a new desired state from the reloaded configuration and submit it to the component context as one reconcile. ^piecomp-q8qx
- When a configuration change alters only the configuration slice of one entry, the reconcile shall restart only the entries whose configuration slice changed. ^piecomp-cto1
- When a configuration change alters only the STT provider field, the configuration watcher shall submit a one-candidate replacement batch for the `stt-backend` entry instead of a reconcile. ^piecomp-bl7w
- If candidate activation fails during an `stt-backend` replacement, then the component context shall restore the previous `stt-backend` declaration and configuration and complete the operation as rolled back. ^piecomp-suy0
- If the component context rejects a submitted desired state at admission, then the assistant service shall keep the previously accepted state and report the admission error through the log and the `Status` response. ^piecomp-cktz
- When the user presses a PTT key, the assistant service shall create the capture's STT session from the `stt` factory coeffect of the currently active `stt-backend` instance. ^piecomp-exmz

### Wakeword

- The `wakeword` entry shall be enabled in the desired state regardless of the `wakeword_enabled` configuration value. ^piecomp-gmd2
- While `wakeword_enabled` is false, the active `wakeword` instance shall provide an inert `wake_events` value that never publishes a wake event. ^piecomp-o1qg
- While `wakeword_enabled` is true, when the wakeword detector scores the wake phrase above its configured threshold, the `wakeword` instance shall publish one wake event. ^piecomp-q40a
- When a wake event arrives while no capture is active, the `ptt-loop` shall start a transcribe capture. ^piecomp-rgs5
- When a wake event or a PTT key release arrives while a wake-started capture is active, the `ptt-loop` shall stop the capture. ^piecomp-jhu3

### Supervision and failure

- When the `pulse-client` or `keyboard-monitor` instance reaches the activation-failed phase, the supervision loop shall retry the entry through the component context with a backoff that doubles from one second to a thirty-second cap. ^piecomp-1d0u
- The supervision loop shall not automatically retry the `stt-backend` entry. ^piecomp-5gj9
- While any entry is outside the active phase, the `Status` response shall report the phase of that entry. ^piecomp-nzo2
- The supervision loop shall observe the component context through coalesced diagnostics change waits. ^piecomp-j5l4

### RPC contract

- The `Status` response shall contain a `components` field that reports the phase of each entry, the context integrity, and the latest settlement fence outcome. ^piecomp-q3t4
- The `components` field shall be additive, so clients that predate the field continue to operate without change. ^piecomp-0utm
- The RPC contract shall contain no operation that mutates component lifecycle state. ^piecomp-gn5u

### Process structure

- The assistant service shall initialize OTLP observability before the component context starts and keep it running after the component context stops. ^piecomp-4l78
- The modules previously named `Daemon.*` shall be named `Assistant_service.*`. ^piecomp-xn99
- The assistant service RPC wire format shall remain compatible with the pre-rename format except for the additive `components` field. ^piecomp-lfnz
- The `ptt`, `wakeword`, `stt-interactive`, and other standalone CLI commands shall not use the component runtime. ^piecomp-1t7v
- When the assistant service stops, the component context shall withdraw consumers before their providers and await all owned scopes before the process exits. ^piecomp-ah76

## Implementation Decisions

- New library modules under the `Pie` umbrella: one module for the six coeffect
  contracts, one module per entry that binds the existing subsystem code to a
  component declaration, one module that builds the desired-state tree from a
  loaded `Config.Stt.t`, and one supervision-loop module.
- Coeffect value types: `audio_stream` is a bounded PCM chunk stream,
  `key_events` is a keyboard event stream, `recording_state` is the recording
  coordinator handle, `stt` is a factory record of transcribe and translate
  session functions, `wake_events` is an event stream, `control_events` is a
  trigger stream.
- Configuration slices are per-entry records projected from `Config.Stt.t`, each
  with its own `config_equal`. `stt-backend` receives provider, models,
  languages, and prompts. `ptt-loop` receives thresholds and keysyms.
  `wakeword` receives `wakeword_enabled` and its thresholds.
- The configuration watcher keeps its 500 ms polling mechanism and gains a
  router: a change to only the STT provider field builds a one-candidate
  replacement batch (target instance and target revision read from the
  diagnostics snapshot, source revision from a watcher-owned counter); any
  other accepted change submits a reconcile.
- The STT backend keeps per-press sessions. The component activation builds
  the factory from the configuration slice; reconcile and replacement swap the
  factory.
- Dormant provider pattern: the wakeword component always activates. When its
  configuration slice disables it, activation returns an inert provision. The
  entry-level `enabled` flag is reserved for sink entries.
- The `Daemon.Server`, `Daemon.Client`, and `Daemon.Contract` modules move to
  `Assistant_service.*` in the same pass. The JSON-lines codec gains the
  additive `components` field on `Status`.
- The supervision loop runs inside the component context body alongside the
  watcher, classifies entries as infrastructure (`pulse-client`,
  `keyboard-monitor`) or configuration-driven (`stt-backend`), and applies
  retry with bounded backoff to infrastructure entries only.
- OTLP observability is initialized by the command entry point before
  `Context.run` and shut down after it returns.

## Testing Decisions

Good tests verify external behavior at a seam, never implementation details.
Two seams:

1. **The Ox effect seam.** The component tree, watcher router, and
   supervision loop are one Ox effect, interpreted deterministically by
   `ox_test`. Tests use fake component declarations with the same tree shape
   and coeffect contracts. Gates: admission rejects a duplicate provider,
   admission rejects a dependency cycle, an accepted reconcile settles
   quiescent with every enabled entry active, a failed `stt-backend` candidate
   completes rolled back with the previous declaration restored, a
   config-disabled wakeword leaves `ptt-loop` active with an inert
   `wake_events`, an infrastructure activation failure triggers retry with the
   specified backoff, and a config slice change restarts only the affected
   entries. Each gate cites its requirement ID. Prior art: Ox's own component
   gates under `test/component/`.
2. **The RPC wire seam.** Codec-level tests over the JSON-lines contract: a
   `Status` response carries the `components` field, and a client that ignores
   unknown fields decodes it without error. Prior art: the existing alcotest
   suite under `packages/pie/test/`.

Real PulseAudio, D-Bus, and STT WebSocket behavior is covered by daily
dogfooding, not by live integration tests.

## Out of Scope

- The generic `ox_component_loader` and native HMR loader packages, which are
  specification-only in Ox. Pie watches its own configuration files and calls
  reconcile or replace directly.
- Voice activity detection or capture-until-silence.
- A meeting transcription pipeline. The meeting mode remains a flag on the
  recording coordinator.
- A warm, persistent Codex realtime session owned by the component.
- Changes to the standalone CLI commands.
- Live integration tests against real PulseAudio, D-Bus, or STT services.
- Automatic state migration of component-local state across replacement.

## Further Notes

- The meeting-mode flag rides inside `recording_state` unchanged.
- `REVIEW.md` is stale in places (it predates current switch and fiber usage).
  Reconciling it is not part of this spec.
- The Ox pin moves to master commit `06edeafb`, the first commit containing
  the component runtime and the fallible-codec gates.
