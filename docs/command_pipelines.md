# Command Pipelines

PIE commands are expressed as YAML pipelines stored in `~/.config/pie/commands/*.yml`.
Each pipeline describes the narrative for a desktop automation task while making the
processing graph explicit.

## YAML Shape (P0 baseline)

```yaml
name: translate-and-type
summary: "Capture speech, translate it, and stage text for typing"
input:
  kind: audio
  source: listen            # `pie listen` session
  capture:
    mode: hybrid
    route: microphone
    max_duration_secs: 8
    min_duration_secs: 1
    silence_timeout_ms: 1200
    silence_threshold: 0.015
stages:
  - id: stt
    kind: stt
    uses: ai-ox:groq/whisper-large-v3
    after: [input]
    with:
      format: simple
  - id: translate
    kind: llm
    uses: ai-ox:gemini/flash
    after: [stt]
    with:
      prompt: |
        Translate the user text to Polish and keep it concise.
output:
  kind: text
  from: translate
```

* `after` defines dependencies and enables fan-out/fan-in for non-linear flows.
* Every node returns a typed artifact (audio, text, structured data) captured in
  the schema so downstream steps know how to consume it.
* `kind` acts as a high-level discriminator for executor dispatch (STT, LLM,
  tool, shell, keyboard, etc.).
* `capture` supports `mode` values `timed`, `silence`, `manual`, `hybrid`, or `wake_word` plus safety knobs
  (`max_duration_secs`, `min_duration_secs`, `silence_timeout_ms`, `silence_threshold`).
* `capture.route` selects the audio source (`microphone` or `system`), and `capture.device` can target a specific PulseAudio monitor/input name.
* When `mode: wake_word`, include a `wake_word` block with at least `keyword_path` (and optionally `model_path`, `sensitivity`, `phrase`); PIE validates the Picovoice assets at startup and does not begin recording until the keyword fires.

```yaml
capture:
  mode: wake_word
  route: microphone
  wake_word:
    keyword_path: /opt/picovoice/ok_pie.ppn
    model_path: /opt/picovoice/porcupine_params.pv
    sensitivity: 0.55
  max_duration_secs: 6
```

## Rust Representation Sketch

```rust
use serde::Deserialize;

#[derive(Debug, Deserialize)]
pub struct CommandPipeline {
    pub name: String,
    pub summary: Option<String>,
    pub input: PipelineInput,
    #[serde(default)]
    pub stages: Vec<PipelineStage>,
    pub output: PipelineOutput,
}

#[derive(Debug, Deserialize)]
pub struct PipelineInput {
    pub kind: InputKind,      // audio (P0), text, etc.
    pub source: String,       // e.g. "listen", "file", "stdin"
}

#[derive(Debug, Deserialize)]
#[serde(tag = "kind")]
pub enum PipelineStage {
    #[serde(rename = "stt")]
    Stt(SttStage),
    #[serde(rename = "llm")]
    Llm(LlmStage),
    #[serde(other)]
    Unsupported,
}

#[derive(Debug, Deserialize)]
pub struct StageBase {
    pub id: String,
    #[serde(default)]
    pub after: Vec<String>,   // dependency ids (or "input")
}

#[derive(Debug, Deserialize)]
pub struct SttStage {
    #[serde(flatten)]
    pub base: StageBase,
    pub uses: ModelRef,       // ai-ox provider identifier
    #[serde(default)]
    pub with: SttOptions,
}

#[derive(Debug, Deserialize)]
pub struct LlmStage {
    #[serde(flatten)]
    pub base: StageBase,
    pub uses: ModelRef,
    #[serde(default)]
    pub with: LlmOptions,
}

#[derive(Debug, Deserialize)]
pub struct PipelineOutput {
    pub kind: OutputKind,     // text, keyboard, file, noop…
    pub from: String,         // stage id supplying the payload
}
```

### Execution Notes

1. Build a DAG keyed by `id` and perform a topological walk that respects `after`.
2. Cache each stage result by id so later stages can read named inputs.
3. Fail fast if required dependencies are missing or produce an incompatible
   artifact type.
4. Emit structured status (JSON) so the CLI/Waybar can surface active pipeline
   state and currently running node.

This sketch keeps P0 focused on `audio -> stt -> text` flows while giving us a
clear extension point for translation, improvement, and desktop outputs in the
next iterations.

## CLI Integration

- Start the daemon with pipelines loaded via `pie server`. The process runs in
  the foreground (ideal for development) and fails fast if any YAML file is
  invalid.
- Execute a pipeline with `pie run <pipeline-name>`; the CLI will wait for the
  daemon to finish and print the resulting artifact (e.g., the transcribed text
  for the `transcribe` pipeline).
