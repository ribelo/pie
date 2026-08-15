# Pie

Pie is an OCaml voice assistant. It listens for push-to-talk keys and wake words, transcribes or translates speech, and injects text into the focused window.

## Language

### Processes

**Assistant service**:
The long-running Pie process that hosts the RPC server and all audio and input pipelines.
_Avoid_: daemon

**Project context**:
The project that a voice command applies to, found through the `.pie/context.md` file of an ancestor directory.
_Avoid_: context (unqualified)

### Component runtime

**Component context**:
The single lifecycle authority that admits desired state, activates component instances, and settles all lifecycle work. Supplied by `eta_component`.
_Avoid_: context (unqualified), service registry, container

**Component**:
A reusable declaration with a typed configuration, typed requirements, typed provisions, and one activation effect.

**Entry**:
One application-owned request in a desired-state tree: a stable ID, a component, a configuration, an enabled flag, and a context specification.

**Desired state**:
The immutable tree of entries that the application submits to the component context.

**Coeffect**:
A typed dependency contract with a generative identity. A component declares coeffects as requirements or provisions.

**Provider / consumer**:
Runtime roles for one coeffect. A provider supplies the coeffect value; a consumer requires it.

**Reconcile**:
The submission of one complete desired-state snapshot to the component context.

**Settlement fence**:
The handle of one accepted component-context operation. Its terminal report is immutable and repeatable.

**Dormant provider**:
A provider component that activates normally but publishes an inert coeffect value because its configuration disables its behavior. Use dormancy, not an entry-level `enabled` flag, to disable any component that others require.

**Supervision loop**:
The fiber that watches component diagnostics and retries failed infrastructure entries with bounded backoff.

### Voice

**Wake event**:
The signal that the wakeword component publishes when it detects the wake phrase. A wake event toggles a transcribe capture: the first event starts the capture, the next event or any PTT key stops it.

**PTT**:
Push-to-talk. The user holds a key, speaks, and releases the key to capture one utterance.

**Wakeword**:
A spoken trigger phrase that starts a capture without a key press.
