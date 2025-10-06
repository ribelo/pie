use anyhow::{Context, Result};
use serde::Deserialize;
use std::collections::{hash_map::Values, HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use tracing::info;

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct CommandPipeline {
    pub name: String,
    pub summary: Option<String>,
    pub input: PipelineInput,
    #[serde(default)]
    pub stages: Vec<PipelineStage>,
    pub output: PipelineOutput,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct PipelineInput {
    pub kind: InputKind,
    #[serde(default = "default_source")]
    pub source: String,
    #[serde(default)]
    pub capture: CaptureConfig,
}

fn default_source() -> String {
    "listen".to_string()
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum InputKind {
    Audio,
    Text,
    #[serde(other)]
    Unsupported,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum PipelineStage {
    Stt(SttStage),
    Llm(LlmStage),
    #[serde(other)]
    Unsupported,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct StageBase {
    pub id: String,
    #[serde(default)]
    pub after: Vec<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CaptureMode {
    Timed,
    Silence,
    Manual,
    Hybrid,
    WakeWord,
}

impl Default for CaptureMode {
    fn default() -> Self {
        CaptureMode::Timed
    }
}

#[cfg_attr(not(feature = "audio"), allow(dead_code))]
#[derive(Debug, Clone, Deserialize)]
pub struct CaptureConfig {
    #[serde(default)]
    pub mode: CaptureMode,
    pub max_duration_secs: Option<u32>,
    pub min_duration_secs: Option<u32>,
    pub silence_timeout_ms: Option<u32>,
    pub silence_threshold: Option<f32>,
    #[serde(default)]
    pub route: AudioRoute,
    pub device: Option<String>,
    #[serde(default)]
    pub wake_word: Option<WakeWordCapture>,
    #[serde(default)]
    pub push_to_talk: Option<PushToTalkConfig>,
}

impl Default for CaptureConfig {
    fn default() -> Self {
        Self {
            mode: CaptureMode::Timed,
            max_duration_secs: None,
            min_duration_secs: None,
            silence_timeout_ms: None,
            silence_threshold: None,
            route: AudioRoute::Microphone,
            device: None,
            wake_word: None,
            push_to_talk: None,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AudioRoute {
    Microphone,
    System,
}

#[cfg_attr(not(feature = "audio"), allow(dead_code))]
#[derive(Debug, Clone, Deserialize)]
pub struct WakeWordCapture {
    pub keyword_path: String,
    #[serde(default)]
    pub model_path: Option<String>,
    #[serde(default = "default_wake_sensitivity")]
    pub sensitivity: f32,
    #[serde(default)]
    pub phrase: Option<String>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct PushToTalkConfig {
    pub hotkey: String,
}

fn default_wake_sensitivity() -> f32 {
    0.5
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manual_capture_rejects_silence_fields() {
        let yaml = r#"
name: test
input:
  kind: audio
  capture:
    mode: manual
    route: microphone
    silence_timeout_ms: 1000
stages:
  - id: stt
    kind: stt
    uses: ai-ox:groq/whisper-large-v3
    after: [input]
output:
  kind: keyboard
  from: stt
"#;

        let pipeline: CommandPipeline = serde_yaml::from_str(yaml).expect("valid yaml");
        assert!(validate_pipeline(&pipeline).is_err());
    }

    #[test]
    fn wake_word_requires_block() {
        let yaml = r#"
name: test
input:
  kind: audio
  capture:
    mode: wake_word
    route: microphone
stages:
  - id: stt
    kind: stt
    uses: ai-ox:groq/whisper-large-v3
    after: [input]
output:
  kind: keyboard
  from: stt
"#;

        let pipeline: CommandPipeline = serde_yaml::from_str(yaml).expect("valid yaml");
        assert!(validate_pipeline(&pipeline).is_err());
    }
}

impl Default for AudioRoute {
    fn default() -> Self {
        AudioRoute::Microphone
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct SttStage {
    #[serde(flatten)]
    pub base: StageBase,
    pub uses: String,
    #[serde(default)]
    pub with: serde_json::Value,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct LlmStage {
    #[serde(flatten)]
    pub base: StageBase,
    pub uses: String,
    #[serde(default)]
    pub with: serde_json::Value,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
pub struct PipelineOutput {
    pub kind: OutputKind,
    pub from: String,
}

#[allow(dead_code)]
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OutputKind {
    Text,
    Keyboard,
    File,
    Noop,
}

#[derive(Debug, Default, Clone)]
pub struct PipelineRegistry {
    pipelines: HashMap<String, CommandPipeline>,
}

impl PipelineRegistry {
    pub fn insert(&mut self, pipeline: CommandPipeline) -> Result<()> {
        if self.pipelines.contains_key(&pipeline.name) {
            anyhow::bail!("Duplicate pipeline name: {}", pipeline.name);
        }
        self.pipelines.insert(pipeline.name.clone(), pipeline);
        Ok(())
    }

    pub fn len(&self) -> usize {
        self.pipelines.len()
    }

    pub fn get(&self, name: &str) -> Option<&CommandPipeline> {
        self.pipelines.get(name)
    }

    pub fn values(&self) -> Values<'_, String, CommandPipeline> {
        self.pipelines.values()
    }
}

pub fn load_pipelines() -> Result<PipelineRegistry> {
    let command_dir = command_directory();

    if !command_dir.exists() {
        fs::create_dir_all(&command_dir).context("Failed to create command pipeline directory")?;
        info!("No command pipelines found at {}", command_dir.display());
        return Ok(PipelineRegistry::default());
    }

    let mut registry = PipelineRegistry::default();

    for entry in fs::read_dir(&command_dir).context("Failed to read command directory")? {
        let entry = entry?;
        let path = entry.path();
        if !is_yaml(&path) {
            continue;
        }

        let contents = fs::read_to_string(&path)
            .with_context(|| format!("Failed to read pipeline file {}", path.display()))?;
        let pipeline: CommandPipeline = serde_yaml::from_str(&contents)
            .with_context(|| format!("Failed to parse pipeline YAML {}", path.display()))?;

        validate_pipeline(&pipeline)?;

        registry.insert(pipeline)?;
    }

    info!("Loaded {} command pipeline(s)", registry.len());

    Ok(registry)
}

fn command_directory() -> PathBuf {
    if let Ok(dir) = std::env::var("PIE_COMMANDS_DIR") {
        return PathBuf::from(dir);
    }

    let mut base = dirs::config_dir().unwrap_or_else(|| PathBuf::from("."));
    base.push("pie");
    base.push("commands");
    base
}

fn is_yaml(path: &Path) -> bool {
    matches!(
        path.extension().and_then(|ext| ext.to_str()).map(|s| s.to_ascii_lowercase()),
        Some(ref ext) if ext == "yaml" || ext == "yml"
    )
}

fn validate_pipeline(pipeline: &CommandPipeline) -> Result<()> {
    if pipeline.name.trim().is_empty() {
        anyhow::bail!("Pipeline name must not be empty");
    }

    if matches!(pipeline.input.kind, InputKind::Unsupported) {
        anyhow::bail!("Pipeline {} uses unsupported input kind", pipeline.name);
    }

    if matches!(pipeline.input.kind, InputKind::Audio) {
        validate_capture_config(&pipeline.input.capture, &pipeline.name)?;
    }

    if pipeline.stages.is_empty() {
        anyhow::bail!("Pipeline {} must declare at least one stage", pipeline.name);
    }

    let mut seen_ids: HashSet<String> = HashSet::new();
    seen_ids.insert("input".to_string());

    for stage in &pipeline.stages {
        let base = match stage {
            PipelineStage::Stt(stage) => &stage.base,
            PipelineStage::Llm(stage) => &stage.base,
            PipelineStage::Unsupported => {
                anyhow::bail!("Pipeline {} contains unsupported stage kind", pipeline.name);
            }
        };

        if base.id.trim().is_empty() {
            anyhow::bail!("Pipeline {} has a stage without an id", pipeline.name);
        }

        if !seen_ids.insert(base.id.clone()) {
            anyhow::bail!("Pipeline {} reuses stage id {}", pipeline.name, base.id);
        }

        for dep in &base.after {
            if dep != "input" && !seen_ids.contains(dep) {
                anyhow::bail!(
                    "Pipeline {} stage {} references unknown dependency {}",
                    pipeline.name,
                    base.id,
                    dep
                );
            }
        }
    }

    if !seen_ids.contains(&pipeline.output.from) {
        anyhow::bail!(
            "Pipeline {} output references unknown stage {}",
            pipeline.name,
            pipeline.output.from
        );
    }

    Ok(())
}

fn validate_capture_config(config: &CaptureConfig, pipeline_name: &str) -> Result<()> {
    match config.mode {
        CaptureMode::Timed => {}
        CaptureMode::Silence => {
            if config.silence_timeout_ms.is_none() {
                anyhow::bail!(
                    "Pipeline {} capture config (silence) requires silence_timeout_ms",
                    pipeline_name
                );
            }
        }
        CaptureMode::Manual => {
            if config.silence_timeout_ms.is_some() || config.silence_threshold.is_some() {
                anyhow::bail!(
                    "Pipeline {} capture config (manual) must not define silence_timeout_ms or silence_threshold",
                    pipeline_name
                );
            }
            if config.wake_word.is_some() {
                anyhow::bail!(
                    "Pipeline {} capture config (manual) must not include wake_word settings",
                    pipeline_name
                );
            }
            if let Some(ptt) = &config.push_to_talk {
                if ptt.hotkey.trim().is_empty() {
                    anyhow::bail!(
                        "Pipeline {} capture config (manual) push_to_talk.hotkey must not be empty",
                        pipeline_name
                    );
                }
            }
        }
        CaptureMode::Hybrid => {
            if config.max_duration_secs.is_none() {
                anyhow::bail!(
                    "Pipeline {} capture config (hybrid) requires max_duration_secs",
                    pipeline_name
                );
            }
            if config.silence_timeout_ms.is_none() {
                anyhow::bail!(
                    "Pipeline {} capture config (hybrid) requires silence_timeout_ms",
                    pipeline_name
                );
            }
            if config.wake_word.is_some() {
                anyhow::bail!(
                    "Pipeline {} capture config (hybrid) must not include wake_word settings",
                    pipeline_name
                );
            }
        }
        CaptureMode::WakeWord => {
            let wake = config.wake_word.as_ref().ok_or_else(|| {
                anyhow::anyhow!(
                    "Pipeline {} capture config (wake_word) requires wake_word block",
                    pipeline_name
                )
            })?;

            if let Some(route) = match config.route {
                AudioRoute::Microphone => None,
                AudioRoute::System => Some("system"),
            } {
                anyhow::bail!(
                    "Pipeline {} capture config (wake_word) cannot use {} route",
                    pipeline_name,
                    route
                );
            }

            if config.silence_timeout_ms.is_some() || config.silence_threshold.is_some() {
                anyhow::bail!(
                    "Pipeline {} capture config (wake_word) must not define silence thresholds",
                    pipeline_name
                );
            }

            if !std::path::Path::new(&wake.keyword_path).exists() {
                anyhow::bail!(
                    "Pipeline {} wake_word keyword file not found: {}",
                    pipeline_name,
                    wake.keyword_path
                );
            }

            if let Some(model_path) = &wake.model_path {
                if !std::path::Path::new(model_path).exists() {
                    anyhow::bail!(
                        "Pipeline {} wake_word model file not found: {}",
                        pipeline_name,
                        model_path
                    );
                }
            }
        }
    }

    if config.push_to_talk.is_some() && !matches!(config.mode, CaptureMode::Manual) {
        anyhow::bail!(
            "Pipeline {} push_to_talk configuration requires capture mode 'manual'",
            pipeline_name
        );
    }

    match config.route {
        AudioRoute::Microphone | AudioRoute::System => {}
    }

    Ok(())
}
