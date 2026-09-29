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
The single lifecycle authority that admits desired state, activates component instances, and settles all lifecycle work. Supplied by `ox_component`.
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

**PTT**:
Push-to-talk. The user holds a key, speaks, and releases the key to capture one utterance.

**Enabled**:
The assistant accepts new recording attempts. Disabling it cancels the current attempt.

**Recording**:
The current utterance is accepting microphone audio. A stop request begins a short finishing-capture tail, after which no more audio belongs to that utterance.

**Processing**:
Capture has ended and the assistant is obtaining the transcription or translation. The microphone recording indicator is off.

**Delivery**:
The assistant is attempting to enter the resulting text into the target application. Dispatch does not prove receipt.

**Busy**:
One attempt still owns the operation, including setup, recording, processing, delivery and cleanup. A second attempt is not admitted until the first has settled. Busy does not mean recording.

**Low-volume warning**:
An advisory that nonzero microphone audio has remained too quiet for a sustained interval. It does not stop recording and is distinct from the existing digital-silence warning.
