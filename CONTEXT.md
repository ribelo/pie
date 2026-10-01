# Pie

Pie is an OCaml voice assistant. It listens for push-to-talk keys and wake words, transcribes or translates speech, and injects text into the focused window.

## Language

### Processes

**Assistant service**:
The long-running Pie process that hosts the RPC server and all audio and input pipelines.
_Avoid_: daemon


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
The assistant offers the resulting text to the clipboard. It pastes only when the currently focused niri window id matches the id captured with the app id and title at key press. Changed, missing or unavailable focus means clipboard-only delivery: no keystroke, no focus change, and one notice naming the original title without the transcript. Both paths are delivered; history distinguishes `pasted` from `clipboard`. Application newline rules still apply when pasting. Character typing is not a delivery method, and paste dispatch does not prove receipt.

**Busy**:
One attempt still owns the operation, including setup, recording, processing, delivery and cleanup. A second attempt is not admitted until the first has settled. Busy does not mean recording.

**Pass 1**:
Speech to text: a dedicated recognition model turns the captured audio into a literal transcript, streaming audio during recording when configured.
_Avoid_: STT pass (unqualified), realtime

**Pass 2**:
The optional text step after pass 1: a text model rewrites the raw transcript with the user's prompt and the glossary. Correction and translation are both pass-2 prompts.
_Avoid_: post-processing, fixer

**Raw transcript**:
The text pass 1 returned, before pass 2. It is kept in the history and delivered when pass 2 fails on a transcription binding.

**Glossary**:
The words the user dictates and how recognition mishears them, read from one global glossary at each key press and given to the selected dictation pipeline. Terms from projects live in sections of this same file.
_Avoid_: context, project context, vocabulary

**Global glossary**:
The only glossary file Pie resolves: `glossary.md` in Pie's XDG configuration directory (normally `~/.config/pie/glossary.md`), or the replacement file named by `PIE_GLOSSARY`. Symlinks are followed and the full file is reread for every attempt; missing or unreadable files warn without blocking dictation. `pie glossary` shows its path, existence and line count, and its help explains maintenance.

**Project glossary** (retired):
The former per-project `.pie/glossary.md` source discovered from the focused process tree. Pie no longer discovers or reads it; project terms belong in the global glossary. Its bundled agent skill and `pie skill install` are retired too.

**Binding**:
One push-to-talk key: a keysym, a shift flag, a mode (transcribe or translate), whether pass 2 runs, and which pass-2 prompt it uses.

**History**:
The append-only local record of every attempt's raw transcript, final text and outcome, read with `pie history`.

**Low-volume warning**:
An advisory that nonzero microphone audio has remained too quiet for a sustained interval. It does not stop recording and is distinct from the existing digital-silence warning.
