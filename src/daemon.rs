use ai_ox::stt::{
    builder::auto_stt, AudioSource, OutputFormat, SpeechToText, SttError, TranscriptionRequest,
};
use anyhow::{anyhow, Context, Result};
use chrono::Local;
#[cfg(feature = "audio")]
use chrono::Utc;
use daemonize::Daemonize;
#[cfg(feature = "audio")]
use hound::{SampleFormat, WavSpec, WavWriter};
#[cfg(feature = "audio")]
use libpulse_binding::sample::{Format as PulseFormat, Spec as PulseSpec};
#[cfg(feature = "audio")]
use libpulse_binding::stream::Direction;
#[cfg(feature = "audio")]
use libpulse_simple_binding::Simple;
use serde_json;
use tokio::fs as tokio_fs;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::UnixListener;
use tokio::signal;
use tokio::sync::RwLock;
use tokio::task::JoinHandle;
use tokio_util::sync::CancellationToken;
use tracing::{debug, error, info, warn};

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};

use crate::ai_router::AiRouter;
use crate::command_executor::CommandExecutor;
#[cfg_attr(not(feature = "audio"), allow(unused_imports))]
use crate::commands::{
    load_pipelines, AudioRoute, CaptureConfig, CaptureMode, InputKind, OutputKind,
    PipelineRegistry, PipelineStage,
};
#[cfg(feature = "audio")]
use crate::find_target_device;
use crate::ipc::{ControlCommand, ControlResponse};
use crate::keyboard::{detect_keyboard_backend, KeyboardOutput};
use crate::notification::NotificationManager;
use crate::push_to_talk;
#[cfg(feature = "audio")]
use crate::record_audio_without_ctrlc;
use crate::status::{DaemonStatus, ListeningStatus, WakeWordMode};
#[cfg(not(feature = "audio"))]
use crate::wake_word::WakeWordDetector;
#[cfg(feature = "audio")]
use crate::wake_word::{WakeWordDefinition, WakeWordDetector};
use std::sync::OnceLock;

#[derive(Clone)]
pub struct DaemonConfig {
    pub pid_file: PathBuf,
    pub log_file: PathBuf,
    pub data_dir: PathBuf,
    pub control_socket: PathBuf,
    pub detach: bool,
    pub listen_duration_secs: u32,
    pub wake_model_path: Option<PathBuf>,
    pub wake_keyword_path: Option<PathBuf>,
    pub wake_sensitivity: f32,
    pub debug_audio_dir: Option<PathBuf>,
    pub push_to_talk_tail_secs: f32,
}

impl Default for DaemonConfig {
    fn default() -> Self {
        let data_dir = dirs::data_local_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("pie");

        let pid_file = data_dir.join("pie.pid");
        let log_file = data_dir.join("daemon.log");
        let control_socket = data_dir.join("pie.sock");

        let mut debug_audio_dir = match std::env::var("PIE_DEBUG_AUDIO_DIR") {
            Ok(value) if value.trim().is_empty() => None,
            Ok(value) => Some(PathBuf::from(value)),
            Err(_) => Some(std::env::temp_dir().join("pie_pipeline_audio")),
        };

        if let Some(dir) = &debug_audio_dir {
            if let Err(err) = std::fs::create_dir_all(dir) {
                warn!(
                    "Failed to create default debug audio directory {}: {}",
                    dir.display(),
                    err
                );
                debug_audio_dir = None;
            }
        }

        let push_to_talk_tail_secs = std::env::var("PIE_PTT_TAIL_SECS")
            .ok()
            .and_then(|v| v.parse::<f32>().ok())
            .map(|v| v.max(0.0))
            .unwrap_or(1.0);

        Self {
            pid_file,
            log_file,
            data_dir,
            control_socket,
            detach: true,
            listen_duration_secs: 3,
            wake_model_path: None,
            wake_keyword_path: None,
            wake_sensitivity: 0.5,
            debug_audio_dir,
            push_to_talk_tail_secs,
        }
    }
}

pub struct PieDaemon {
    config: DaemonConfig,
    wake_detector: Arc<RwLock<WakeWordDetector>>,
    ai_router: Arc<AiRouter>,
    executor: Arc<CommandExecutor>,
    notifier: Arc<NotificationManager>,
    running: Arc<RwLock<bool>>,
    listening_state: Arc<RwLock<ListeningState>>,
    pipelines: Arc<PipelineRegistry>,
    stt_provider: Arc<RwLock<Option<Arc<dyn SpeechToText>>>>,
    keyboard: Arc<dyn KeyboardOutput>,
    pipeline_controls: Arc<RwLock<HashMap<String, PipelineTokens>>>,
    push_to_talk_cancel: CancellationToken,
    push_to_talk_tasks: Arc<RwLock<Vec<JoinHandle<()>>>>,
}

// Global registry to allow in-process cancellation without going through the control server.
#[derive(Clone)]
struct PipelineTokens {
    abort: CancellationToken,
    capture: CancellationToken,
}

static GLOBAL_PIPELINE_TOKENS: OnceLock<Arc<RwLock<HashMap<String, PipelineTokens>>>> =
    OnceLock::new();

fn global_tokens() -> Option<&'static Arc<RwLock<HashMap<String, PipelineTokens>>>> {
    GLOBAL_PIPELINE_TOKENS.get()
}

pub async fn stop_capture_by_name(name: &str) -> bool {
    if let Some(tokens) = global_tokens() {
        let map = tokens.read().await;
        if let Some(ctrl) = map.get(name) {
            ctrl.capture.cancel();
            return true;
        }
    }
    false
}

pub async fn abort_pipeline_by_name(name: &str) -> bool {
    if let Some(tokens) = global_tokens() {
        let map = tokens.read().await;
        if let Some(ctrl) = map.get(name) {
            ctrl.abort.cancel();
            return true;
        }
    }
    false
}

#[derive(Default)]
struct ListeningState {
    session: Option<ListeningSession>,
    status: ListeningStatus,
}

#[cfg_attr(not(feature = "audio"), allow(dead_code))]
struct ListeningSession {
    cancel: CancellationToken,
    handle: JoinHandle<Result<RecordedAudio>>,
}

#[cfg_attr(not(feature = "audio"), allow(dead_code))]
struct RecordedAudio {
    path: PathBuf,
    duration: Duration,
}

#[cfg_attr(not(feature = "audio"), allow(dead_code))]
enum ToggleResult {
    Started,
    Stopped {
        transcript: Option<String>,
        duration_seconds: Option<f64>,
    },
}

struct PipelineRunResult {
    text: Option<String>,
}

enum StageArtifact {
    Audio(PathBuf),
    Text(String),
}

struct CaptureResult {
    path: PathBuf,
    cancelled: bool,
}

enum PipelineOutcome {
    Completed(PipelineRunResult),
    Cancelled,
}

impl PieDaemon {
    pub fn new(config: DaemonConfig) -> Result<Self> {
        // Ensure data directory exists
        fs::create_dir_all(&config.data_dir).context("Failed to create data directory")?;

        if config.control_socket.exists() {
            if let Err(err) = fs::remove_file(&config.control_socket) {
                if err.kind() != std::io::ErrorKind::NotFound {
                    warn!("Failed to remove stale control socket: {}", err);
                }
            }
        }

        let pipelines = Arc::new(load_pipelines().context("Failed to load command pipelines")?);

        if pipelines.len() == 0 {
            info!("No command pipelines registered");
        } else {
            info!("Loaded {} command pipeline(s)", pipelines.len());
        }

        let wake_detector = Arc::new(RwLock::new(
            WakeWordDetector::with_config(
                config.wake_model_path.clone(),
                config.wake_keyword_path.clone(),
                config.wake_sensitivity,
            )
            .context("Failed to initialize wake word detector")?,
        ));

        let ai_router = Arc::new(AiRouter::new().context("Failed to initialize AI router")?);

        let executor = Arc::new(
            CommandExecutor::new(Arc::clone(&pipelines))
                .context("Failed to initialize command executor")?,
        );

        let notifier = Arc::new(NotificationManager::new());
        let keyboard = detect_keyboard_backend();
        let push_to_talk_cancel = CancellationToken::new();
        let push_to_talk_tasks = Arc::new(RwLock::new(Vec::new()));

        if let Some(dir) = &config.debug_audio_dir {
            if let Err(err) = std::fs::create_dir_all(dir) {
                warn!(
                    "Failed to ensure debug audio directory {}: {}",
                    dir.display(),
                    err
                );
            }
        }

        let pipeline_controls = if let Some(existing) = GLOBAL_PIPELINE_TOKENS.get() {
            existing.clone()
        } else {
            let arc = Arc::new(RwLock::new(HashMap::new()));
            let _ = GLOBAL_PIPELINE_TOKENS.set(arc.clone());
            arc
        };

        Ok(Self {
            config,
            wake_detector,
            ai_router,
            executor,
            notifier,
            running: Arc::new(RwLock::new(false)),
            listening_state: Arc::new(RwLock::new(ListeningState::default())),
            pipelines,
            stt_provider: Arc::new(RwLock::new(None)),
            keyboard,
            pipeline_controls,
            push_to_talk_cancel,
            push_to_talk_tasks,
        })
    }

    #[cfg_attr(not(feature = "audio"), allow(dead_code))]
    async fn ensure_stt_provider_from(
        store: &Arc<RwLock<Option<Arc<dyn SpeechToText>>>>,
    ) -> Result<Arc<dyn SpeechToText>> {
        if let Some(existing) = store.read().await.clone() {
            return Ok(existing);
        }

        let provider = auto_stt().map_err(|err| match err {
            SttError::MissingApiKey => {
                anyhow!(
                    "No speech-to-text provider configured. Set GROQ_API_KEY (or another provider key) to enable transcription."
                )
            }
            other => anyhow!(other),
        })?;
        info!("Initialized speech-to-text provider");

        let mut writer = store.write().await;
        let provider_arc = Arc::clone(&provider);
        writer.replace(provider);

        Ok(provider_arc)
    }

    #[cfg_attr(not(feature = "audio"), allow(dead_code))]
    async fn transcribe_audio_with_store(
        store: &Arc<RwLock<Option<Arc<dyn SpeechToText>>>>,
        file_path: &Path,
    ) -> Result<String> {
        let provider = Self::ensure_stt_provider_from(store).await?;
        info!(path = %file_path.display(), "Submitting audio for transcription");
        let request = TranscriptionRequest::builder()
            .audio(AudioSource::from_file(file_path.to_path_buf()))
            .output_format(OutputFormat::Simple)
            .build();

        let response = provider
            .transcribe(request)
            .await
            .map_err(|err| anyhow!("Speech-to-text transcription failed: {}", err))?;

        debug!(text = %response.text, "Received transcription result");

        Ok(response.text)
    }

    async fn run_pipeline(&self, name: &str) -> Result<PipelineOutcome> {
        let pipeline = self
            .pipelines
            .get(name)
            .ok_or_else(|| anyhow!("Pipeline '{}' not found", name))?
            .clone();

        let pipeline_start = Instant::now();

        let abort_token = CancellationToken::new();
        let capture_token = abort_token.child_token();
        {
            let mut controls = self.pipeline_controls.write().await;
            if controls.contains_key(name) {
                anyhow::bail!("Pipeline '{}' is already running", name);
            }
            controls.insert(
                name.to_string(),
                PipelineTokens {
                    abort: abort_token.clone(),
                    capture: capture_token.clone(),
                },
            );
        }

        info!(pipeline = %name, stages = pipeline.stages.len(), "Starting pipeline execution");

        let outcome = async {
            let mut artifacts: HashMap<String, StageArtifact> = HashMap::new();
            let mut cleanup_paths: Vec<PathBuf> = Vec::new();

            match pipeline.input.kind {
                InputKind::Audio => {
                    if let Err(err) = self
                        .notifier
                        .notify(
                            "Pipeline recording",
                            &format!(
                                "{} capturing {:?} ({:?} mode)",
                                pipeline.name, pipeline.input.capture.route, pipeline.input.capture.mode
                            ),
                        )
                        .await
                    {
                        warn!(
                            "Failed to send recording notification for pipeline {}: {}",
                            pipeline.name,
                            err
                        );
                    }

                    let capture = self
                        .capture_pipeline_audio(&pipeline.input.capture, &capture_token)
                        .await?;

                    if capture.cancelled {
                        if tokio_fs::remove_file(&capture.path).await.is_err() {
                            warn!(
                                "Failed to remove cancelled capture file {}",
                                capture.path.display()
                            );
                        }
                        return Ok(PipelineOutcome::Cancelled);
                    }

                    info!(
                        pipeline = %pipeline.name,
                        path = %capture.path.display(),
                        "Audio capture complete"
                    );
                    cleanup_paths.push(capture.path.clone());
                    self.persist_debug_audio(&capture.path);
                    artifacts.insert("input".to_string(), StageArtifact::Audio(capture.path));
                }
                _ => {
                    anyhow::bail!("Pipeline '{}' uses unsupported input kind", pipeline.name);
                }
            }

            for stage in &pipeline.stages {
                let stage_start = Instant::now();
                if abort_token.is_cancelled() {
                    let stage_id = match stage {
                        PipelineStage::Stt(stt) => stt.base.id.as_str(),
                        PipelineStage::Llm(llm) => llm.base.id.as_str(),
                        PipelineStage::Unsupported => "unsupported",
                    };
                    info!(
                        pipeline = %pipeline.name,
                        stage = stage_id,
                        "Pipeline cancelled before stage execution"
                    );
                    return Ok(PipelineOutcome::Cancelled);
                }
                match stage {
                    PipelineStage::Stt(stt) => {
                        let deps = if stt.base.after.is_empty() {
                            vec!["input".to_string()]
                        } else {
                            stt.base.after.clone()
                        };

                        let mut audio_source: Option<PathBuf> = None;
                        for dep in deps {
                            if let Some(artifact) = artifacts.get(&dep) {
                                if let StageArtifact::Audio(path) = artifact {
                                    audio_source = Some(path.clone());
                                    break;
                                }
                            }
                        }

                        let audio_path = audio_source.ok_or_else(|| {
                            anyhow!(
                                "Stage '{}' requires audio input but none of its dependencies produced audio",
                                stt.base.id
                            )
                        })?;

                        let text =
                            Self::transcribe_audio_with_store(&self.stt_provider, &audio_path)
                                .await?;
                        let stage_secs = stage_start.elapsed().as_secs_f32();
                        info!(
                            pipeline = %pipeline.name,
                            stage = %stt.base.id,
                            chars = text.chars().count(),
                            stage_secs,
                            "STT stage produced transcript"
                        );
                        artifacts.insert(stt.base.id.clone(), StageArtifact::Text(text));
                    }
                    PipelineStage::Llm(llm) => {
                        use ai_ox::content::{message::{Message, MessageRole}, part::Part};
                        use ai_ox::model::request::ModelRequest as OxModelRequest;
                        use ai_ox::GeminiModel;
                        use ai_ox::model::Model;

                        // Determine model provider from llm.uses.
                        let uses = llm.uses.trim();
                        let model_name = if let Some(rest) = uses.strip_prefix("ai-ox:google/") {
                            rest
                        } else if let Some(rest) = uses.strip_prefix("google/") {
                            rest
                        } else if let Some(rest) = uses.strip_prefix("gemini/") {
                            rest
                        } else if uses.contains("gemini") {
                            uses.split('/').last().unwrap_or("gemini-2.5-flash")
                        } else {
                            anyhow::bail!(
                                "LLM stage '{}' uses unsupported model reference '{}'; expected ai-ox:google/<model>",
                                llm.base.id, uses
                            );
                        };

                        // Build parts: instruction text + optional transcript + optional audio
                        let mut parts: Vec<Part> = Vec::new();

                        // Instruction/task
                        let task = llm
                            .with
                            .get("task")
                            .and_then(|v| v.as_str())
                            .unwrap_or("Translate the following audio into English at CEFR B2 level. Respond with the translation only.");
                        parts.push(Part::text(task));

                        // Optional transcript reference
                        if let Some(text_from) = llm
                            .with
                            .get("text_from")
                            .and_then(|v| v.as_str())
                        {
                            if let Some(StageArtifact::Text(t)) = artifacts.get(text_from) {
                                if !t.trim().is_empty() {
                                    parts.push(Part::text(format!("Transcript (may be imperfect): {}", t)));
                                }
                            }
                        }

                        // Optional audio include
                        let include_audio = llm
                            .with
                            .get("include_audio")
                            .and_then(|v| v.as_bool())
                            .unwrap_or(false);
                        if include_audio {
                            let audio_from = llm
                                .with
                                .get("audio_from")
                                .and_then(|v| v.as_str())
                                .unwrap_or("input");
                            let audio_path = match artifacts.get(audio_from) {
                                Some(StageArtifact::Audio(p)) => p,
                                _ => {
                                    anyhow::bail!(
                                        "LLM stage '{}' requested audio_from='{}' but no audio artifact was found",
                                        llm.base.id, audio_from
                                    )
                                }
                            };
                            let bytes = std::fs::read(audio_path).context("Failed to read audio file for LLM stage")?;
                            use base64::Engine;
                            let b64 = base64::engine::general_purpose::STANDARD.encode(&bytes);
                            // Our pipeline WAVs use 16-bit or 32-bit PCM; use generic WAV mimetype
                            parts.push(Part::blob_base64(b64, "audio/wav"));
                        }

                        let user_message = Message::new(MessageRole::User, parts);
                        let request = OxModelRequest {
                            messages: vec![user_message],
                            system_message: None,
                            tools: None,
                        };

                        let thinking_budget = llm
                            .with
                            .get("thinking_budget_tokens")
                            .and_then(|v| v.as_i64())
                            .map(|v| v.max(0).min(i32::MAX as i64) as i32);

                        let llm_start = Instant::now();
                        let model = if let Some(budget) = thinking_budget {
                            let api_key = std::env::var("GOOGLE_AI_API_KEY")
                                .or_else(|_| std::env::var("GEMINI_API_KEY"))
                                .map_err(|_| {
                                    anyhow::anyhow!(
                                        "Failed to initialize Gemini model: missing GOOGLE_AI_API_KEY or GEMINI_API_KEY"
                                    )
                                })?;

                            let generation_config = gemini_ox::generate_content::GenerationConfig {
                                thinking_config: Some(gemini_ox::generate_content::ThinkingConfig {
                                    include_thoughts: false,
                                    thinking_budget: budget,
                                }),
                                ..Default::default()
                            };

                            Ok(GeminiModel::builder()
                                .api_key(api_key)
                                .model(model_name.to_string())
                                .generation_config(generation_config)
                                .build())
                        } else {
                            GeminiModel::new(model_name).await
                        }
                        .map_err(|e| anyhow::anyhow!("Failed to initialize Gemini model: {}", e))?;
                        let response = model
                            .request(request)
                            .await
                            .map_err(|e| anyhow::anyhow!("LLM request failed: {}", e))?;
                        let text = response
                            .to_string()
                            .unwrap_or_else(|| String::from(""));
                        let llm_elapsed = llm_start.elapsed().as_secs_f32();
                        let char_count = text.chars().count();
                        let approx_tokens = ((char_count + 3) / 4).max(1);
                        let tokens_per_sec = if llm_elapsed > 0.0 {
                            approx_tokens as f32 / llm_elapsed
                        } else {
                            0.0
                        };

                        info!(
                            pipeline = %pipeline.name,
                            stage = %llm.base.id,
                            chars = char_count,
                            llm_secs = llm_elapsed,
                            approx_tokens = approx_tokens,
                            tokens_per_sec = tokens_per_sec,
                            stage_secs = stage_start.elapsed().as_secs_f32(),
                            "LLM stage produced text"
                        );
                        artifacts.insert(llm.base.id.clone(), StageArtifact::Text(text));
                    }
                    PipelineStage::Unsupported => {
                        anyhow::bail!("Pipeline '{}' contains unsupported stage", pipeline.name);
                    }
                }
            }

            if abort_token.is_cancelled() {
                info!(pipeline = %pipeline.name, "Pipeline cancelled before output stage");
                return Ok(PipelineOutcome::Cancelled);
            }

            let output_artifact = artifacts.get(&pipeline.output.from).ok_or_else(|| {
                anyhow!(
                    "Pipeline '{}' output references unknown stage '{}'",
                    pipeline.name,
                    pipeline.output.from
                )
            })?;

            let mut result = PipelineRunResult { text: None };

            match pipeline.output.kind {
                OutputKind::Keyboard => {
                    let text = match output_artifact {
                        StageArtifact::Text(text) => text.clone(),
                        StageArtifact::Audio(_) => {
                            anyhow::bail!(
                                "Pipeline '{}' output expects text but received audio",
                                pipeline.name
                            )
                        }
                    };
                    info!(
                        pipeline = %pipeline.name,
                        output_kind = ?pipeline.output.kind,
                        "Dispatching pipeline output"
                    );
                    self.keyboard.send_text(&text).await?;
                    result.text = Some(text);
                }
                OutputKind::Text | OutputKind::File | OutputKind::Noop => {
                    anyhow::bail!(
                        "Pipeline '{}' output kind '{:?}' not implemented",
                        pipeline.name,
                        pipeline.output.kind
                    );
                }
            }

            for path in cleanup_paths {
                if tokio_fs::remove_file(&path).await.is_err() {
                    warn!("Failed to remove temporary audio file {}", path.display());
                }
            }

            Ok(PipelineOutcome::Completed(result))
        }
        .await;

        self.pipeline_controls.write().await.remove(name);
        abort_token.cancel();

        match &outcome {
            Ok(PipelineOutcome::Completed(result)) => info!(
                pipeline = %name,
                has_text = result.text.as_ref().map(|t| !t.is_empty()).unwrap_or(false),
                pipeline_secs = pipeline_start.elapsed().as_secs_f32(),
                "Pipeline finished successfully"
            ),
            Ok(PipelineOutcome::Cancelled) => info!(
                pipeline = %name,
                pipeline_secs = pipeline_start.elapsed().as_secs_f32(),
                "Pipeline cancelled"
            ),
            Err(err) => error!(
                pipeline = %name,
                error = %err,
                pipeline_secs = pipeline_start.elapsed().as_secs_f32(),
                "Pipeline failed"
            ),
        }

        outcome
    }

    #[cfg(feature = "audio")]
    async fn capture_pipeline_audio(
        &self,
        capture: &CaptureConfig,
        stop: &CancellationToken,
    ) -> Result<CaptureResult> {
        use hound::{SampleFormat, WavSpec, WavWriter};

        let file_name = format!("pipeline_audio_{}.wav", Utc::now().timestamp_millis());
        let path = self.config.data_dir.join(file_name);

        let max_duration =
            capture
                .max_duration_secs
                .map(|v| v as f32)
                .or_else(|| match capture.mode {
                    CaptureMode::Timed | CaptureMode::Hybrid => {
                        Some(self.config.listen_duration_secs as f32)
                    }
                    _ => None,
                });
        let min_duration = capture.min_duration_secs.unwrap_or(0) as f32;
        let silence_timeout = capture
            .silence_timeout_ms
            .map(|ms| ms as f32 / 1000.0)
            .unwrap_or(1.5);
        let silence_threshold = capture.silence_threshold.unwrap_or(0.015);

        match capture.mode {
            CaptureMode::Timed => info!(
                "Pipeline audio capture (timed, {:?}) up to {:.1}s",
                capture.route,
                max_duration.unwrap_or(self.config.listen_duration_secs as f32)
            ),
            CaptureMode::Silence => info!(
                "Pipeline audio capture (silence, {:?}) timeout {}ms",
                capture.route,
                (silence_timeout * 1000.0) as u32
            ),
            CaptureMode::Manual => info!(
                "Pipeline audio capture (manual, {:?}) awaiting cancellation",
                capture.route
            ),
            CaptureMode::Hybrid => info!(
                "Pipeline audio capture (hybrid, {:?}) max {:.1}s, silence timeout {}ms",
                capture.route,
                max_duration.unwrap_or(self.config.listen_duration_secs as f32),
                (silence_timeout * 1000.0) as u32
            ),
            CaptureMode::WakeWord => info!(
                "Pipeline audio capture (wake_word, {:?}) max {:.1}s",
                capture.route,
                max_duration.unwrap_or(self.config.listen_duration_secs as f32)
            ),
        }

        let (mic_only, system_only) = match capture.route {
            AudioRoute::Microphone => (true, false),
            AudioRoute::System => (false, true),
        };

        let target_device = find_target_device(mic_only, system_only, &capture.device)
            .context("Failed to resolve capture audio device")?;

        if let Some(ref device) = target_device {
            info!("Using audio source device: {}", device);
        } else {
            info!("Using default audio source for route {:?}", capture.route);
        }

        let spec = PulseSpec {
            format: PulseFormat::F32le,
            channels: 2,
            rate: 48_000,
        };

        if !spec.is_valid() {
            anyhow::bail!("Invalid audio specification");
        }

        let simple = Simple::new(
            None,
            "pie",
            Direction::Record,
            target_device.as_deref(),
            "Pipeline Capture",
            &spec,
            None,
            None,
        )
        .context("Failed to open PulseAudio for pipeline capture")?;

        let wav_spec = WavSpec {
            channels: spec.channels as u16,
            sample_rate: spec.rate,
            bits_per_sample: 32,
            sample_format: SampleFormat::Float,
        };

        let mut writer = WavWriter::create(&path, wav_spec)
            .context("Failed to create pipeline capture WAV file")?;

        let mut buffer = vec![0u8; 1024 * 4 * spec.channels as usize];
        let mut start = std::time::Instant::now();
        let mut silence_elapsed = 0f32;
        let mut cancelled = false;
        let mut total_samples: u64 = 0;
        let tail_duration = if matches!(capture.mode, CaptureMode::Manual) {
            Some(self.config.push_to_talk_tail_secs.max(0.0))
        } else {
            None
        };
        let mut tail_remaining: Option<f32> = None;

        if matches!(capture.mode, CaptureMode::WakeWord) {
            let wake = capture
                .wake_word
                .as_ref()
                .ok_or_else(|| anyhow!("Wake word capture requires wake_word block"))?;

            let definition = WakeWordDefinition {
                keyword_path: PathBuf::from(&wake.keyword_path),
                model_path: wake.model_path.as_ref().map(PathBuf::from),
                sensitivity: wake.sensitivity,
            };

            info!(
                keyword = %definition.keyword_path.display(),
                "Waiting for wake word before recording"
            );

            let detector = WakeWordDetector::from_definition(&definition)?;
            match detector.wait_for_wake_word(stop).await {
                Ok(true) => {
                    info!(
                        keyword = %definition.keyword_path.display(),
                        "Wake word detected; starting capture"
                    );
                    if let Err(err) = self.notifier.notify_wake_word_detected().await {
                        warn!("Failed to send wake word notification: {}", err);
                    }
                    start = std::time::Instant::now();
                }
                Ok(false) => {
                    info!("Wake word wait cancelled before detection");
                    return Ok(CaptureResult {
                        path,
                        cancelled: true,
                    });
                }
                Err(err) => {
                    return Err(err.context("Wake word detection failed"));
                }
            }
        }

        loop {
            if stop.is_cancelled() && tail_remaining.is_none() {
                match tail_duration {
                    Some(tail) if tail > 0.0 => {
                        tail_remaining = Some(tail);
                    }
                    _ => {
                        cancelled = true;
                        break;
                    }
                }
            }

            if let Some(max) = max_duration {
                if start.elapsed().as_secs_f32() >= max {
                    break;
                }
            }

            if let Err(err) = simple.read(&mut buffer) {
                warn!("Error reading audio data for pipeline: {}", err);
                break;
            }

            let mut rms_accum = 0f64;
            let mut sample_count = 0u32;

            for chunk in buffer.chunks_exact(4) {
                let sample = f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
                rms_accum += (sample as f64) * (sample as f64);
                writer
                    .write_sample(sample)
                    .context("Failed to write pipeline audio sample")?;
                sample_count += 1;
            }

            if sample_count > 0 {
                let rms = (rms_accum / sample_count as f64).sqrt() as f32;
                let frame_duration =
                    sample_count as f32 / (spec.rate as f32 * spec.channels as f32);
                total_samples += sample_count as u64 / spec.channels as u64;

                if let Some(rem) = tail_remaining.as_mut() {
                    *rem -= frame_duration;
                    if *rem <= 0.0 {
                        break;
                    }
                }

                match capture.mode {
                    CaptureMode::Silence | CaptureMode::Hybrid => {
                        if rms < silence_threshold {
                            silence_elapsed += frame_duration;
                        } else {
                            silence_elapsed = 0.0;
                        }

                        if start.elapsed().as_secs_f32() >= min_duration
                            && silence_elapsed >= silence_timeout
                        {
                            break;
                        }
                    }
                    _ => {}
                }
            }

            if matches!(capture.mode, CaptureMode::Manual) {
                // Manual mode relies on cancellation; continue until cancel or optional max duration
            }
        }

        writer.finalize().ok();
        drop(simple);

        let duration_secs = start.elapsed().as_secs_f32();
        if cancelled
            && matches!(capture.mode, CaptureMode::Manual)
            && tail_duration.is_some()
            && total_samples > 0
        {
            cancelled = false;
        }

        info!(
            path = %path.display(),
            duration = duration_secs,
            frames = total_samples,
            cancelled,
            "Finished pipeline audio capture"
        );

        Ok(CaptureResult { path, cancelled })
    }

    #[cfg(not(feature = "audio"))]
    async fn capture_pipeline_audio(
        &self,
        _capture: &CaptureConfig,
        _stop: &CancellationToken,
    ) -> Result<CaptureResult> {
        Err(anyhow!(
            "Pipeline audio capture requires audio support. Build PIE with the 'audio' feature."
        ))
    }

    #[cfg(feature = "audio")]
    async fn is_listening_active(&self) -> bool {
        let state = self.listening_state.read().await;
        state.status.active
    }

    #[cfg(feature = "audio")]
    async fn start_listening(&self) -> Result<bool> {
        if self.is_listening_active().await {
            return Ok(false);
        }

        Self::ensure_stt_provider_from(&self.stt_provider).await?;

        let now = Local::now();
        let file_name = format!(
            "listening_{}_{}.wav",
            std::process::id(),
            now.format("%Y%m%d%H%M%S")
        );
        let temp_path = self.config.data_dir.join(file_name);

        let cancel = CancellationToken::new();
        let handle = Self::spawn_listening_task(temp_path.clone(), cancel.clone());

        let mut state = self.listening_state.write().await;
        state.session = Some(ListeningSession { cancel, handle });
        state.status.active = true;
        state.status.started_at = Some(now);
        state.status.duration_seconds = Some(0.0);
        state.status.last_error = None;
        state.status.last_transcript = None;

        if let Err(err) = self.notifier.notify_listening().await {
            warn!("Failed to send listening notification: {}", err);
        }

        Ok(true)
    }

    #[cfg(not(feature = "audio"))]
    async fn start_listening(&self) -> Result<bool> {
        Err(anyhow!(
            "Listening requires audio support. Build with --features audio to enable microphone capture."
        ))
    }

    #[cfg(feature = "audio")]
    async fn stop_listening(&self) -> Result<Option<(String, f64)>> {
        let session = {
            let mut state = self.listening_state.write().await;
            if !state.status.active {
                return Ok(None);
            }

            let session = state
                .session
                .take()
                .ok_or_else(|| anyhow!("Listening session state inconsistent"))?;
            state.status.active = false;
            state.status.duration_seconds = None;
            session
        };

        session.cancel.cancel();

        let recorded = session
            .handle
            .await
            .map_err(|err| anyhow!("Listening task join error: {}", err))??;

        let duration = recorded.duration.as_secs_f64();
        let transcript_result =
            Self::transcribe_audio_with_store(&self.stt_provider, &recorded.path).await;

        let _ = tokio_fs::remove_file(&recorded.path).await;

        let mut state = self.listening_state.write().await;
        state.status.active = false;
        state.status.started_at = None;
        state.status.duration_seconds = Some(duration);
        state.status.last_completed_at = Some(Local::now());

        match transcript_result {
            Ok(text) => {
                let trimmed = text.trim();
                if duration > 0.05 && !trimmed.is_empty() {
                    state.status.last_transcript = Some(text.clone());
                } else {
                    state.status.last_transcript = None;
                }
                state.status.last_error = None;
                Ok(Some((text, duration)))
            }
            Err(err) => {
                let message = err.to_string();
                state.status.last_error = Some(message.clone());
                state.status.last_transcript = None;
                Err(anyhow!(message))
            }
        }
    }

    #[cfg(not(feature = "audio"))]
    async fn stop_listening(&self) -> Result<Option<(String, f64)>> {
        Err(anyhow!(
            "Listening requires audio support. Build with --features audio to enable microphone capture."
        ))
    }

    #[cfg(feature = "audio")]
    async fn toggle_listening(&self) -> Result<ToggleResult> {
        if self.is_listening_active().await {
            match self.stop_listening().await? {
                Some((text, duration)) => {
                    let trimmed = text.trim();
                    if duration > 0.05 && !trimmed.is_empty() {
                        Ok(ToggleResult::Stopped {
                            transcript: Some(text),
                            duration_seconds: Some(duration),
                        })
                    } else {
                        Ok(ToggleResult::Stopped {
                            transcript: None,
                            duration_seconds: None,
                        })
                    }
                }
                None => Ok(ToggleResult::Stopped {
                    transcript: None,
                    duration_seconds: None,
                }),
            }
        } else {
            self.start_listening().await?;
            Ok(ToggleResult::Started)
        }
    }

    #[cfg(not(feature = "audio"))]
    async fn toggle_listening(&self) -> Result<ToggleResult> {
        Err(anyhow!(
            "Listening requires audio support. Build with --features audio to enable microphone capture."
        ))
    }

    #[cfg(feature = "audio")]
    fn spawn_listening_task(
        path: PathBuf,
        cancel: CancellationToken,
    ) -> JoinHandle<Result<RecordedAudio>> {
        tokio::task::spawn_blocking(move || Self::record_listening_audio(path, cancel))
    }

    #[cfg(feature = "audio")]
    fn record_listening_audio(path: PathBuf, cancel: CancellationToken) -> Result<RecordedAudio> {
        let spec = PulseSpec {
            format: PulseFormat::S16le,
            channels: 1,
            rate: 16_000,
        };

        if !spec.is_valid() {
            return Err(anyhow!("Invalid audio specification for listening capture"));
        }

        let simple = Simple::new(
            None,
            "pie",
            Direction::Record,
            None,
            "Listening Capture",
            &spec,
            None,
            None,
        )
        .context("Failed to open PulseAudio for listening capture")?;

        let wav_spec = WavSpec {
            channels: 1,
            sample_rate: spec.rate,
            bits_per_sample: 16,
            sample_format: SampleFormat::Int,
        };

        let mut writer = WavWriter::create(&path, wav_spec)
            .context("Failed to create listening capture WAV file")?;

        let mut buffer = vec![0u8; 512 * 2];
        let mut total_samples: u64 = 0;

        while !cancel.is_cancelled() {
            simple
                .read(&mut buffer)
                .context("Failed to read microphone audio for listening session")?;

            for chunk in buffer.chunks_exact(2) {
                let sample = i16::from_le_bytes([chunk[0], chunk[1]]);
                writer
                    .write_sample(sample)
                    .context("Failed to write microphone sample to WAV")?;
                total_samples += 1;
            }
        }

        if let Err(err) = simple.flush() {
            warn!("Failed to flush PulseAudio stream: {}", err);
        }

        let duration = if spec.rate > 0 {
            Duration::from_secs_f64(total_samples as f64 / spec.rate as f64)
        } else {
            Duration::from_secs(0)
        };

        writer
            .finalize()
            .context("Failed to finalize listening capture WAV file")?;

        Ok(RecordedAudio { path, duration })
    }

    pub async fn start(&self) -> Result<()> {
        // Check if already running
        if self.is_running().await? {
            anyhow::bail!("PIE daemon is already running");
        }

        info!("Starting PIE daemon");

        // Send startup notification
        self.notifier
            .notify("PIE daemon started", "Ready for voice commands")
            .await?;

        // Set running flag
        {
            let mut running = self.running.write().await;
            *running = true;
        }

        // Start main loop
        self.run_main_loop().await
    }

    pub async fn start_background(&self) -> Result<()> {
        if self.config.detach {
            // Create daemon process
            let daemonize = Daemonize::new()
                .pid_file(&self.config.pid_file)
                .working_directory(&self.config.data_dir)
                .stdout(fs::File::create(&self.config.log_file)?)
                .stderr(fs::File::create(&self.config.log_file)?);

            match daemonize.start() {
                Ok(_) => {
                    info!("Successfully daemonized");
                    self.start().await
                }
                Err(e) => {
                    error!("Failed to daemonize: {}", e);
                    anyhow::bail!("Failed to daemonize: {}", e)
                }
            }
        } else {
            // Run in foreground (for testing)
            self.start().await
        }
    }

    async fn run_main_loop(&self) -> Result<()> {
        info!("Starting main daemon loop");

        // Clone Arc references for async tasks
        let wake_detector = Arc::clone(&self.wake_detector);
        let ai_router = Arc::clone(&self.ai_router);
        let executor = Arc::clone(&self.executor);
        let notifier = Arc::clone(&self.notifier);
        let running = Arc::clone(&self.running);
        let listen_duration_secs = self.config.listen_duration_secs;
        let stt_provider = Arc::clone(&self.stt_provider);

        let mut control_server = Box::pin(self.run_control_server());

        let push_to_talk_bindings = push_to_talk::bindings_from_registry(self.pipelines.as_ref());
        if !push_to_talk_bindings.is_empty() {
            info!(
                count = push_to_talk_bindings.len(),
                "Starting push-to-talk hotkey watcher(s)"
            );
            let tasks = push_to_talk::spawn_daemon_watchers(
                push_to_talk_bindings,
                self.config.clone(),
                self.push_to_talk_cancel.clone(),
            );
            {
                let mut guard = self.push_to_talk_tasks.write().await;
                guard.extend(tasks);
            }
        }

        // Start wake word detection task
        let wake_task = tokio::spawn(async move {
            let cancel_token = tokio_util::sync::CancellationToken::new();
            let listen_duration = listen_duration_secs;

            loop {
                // Check if still running
                if !*running.read().await {
                    cancel_token.cancel();
                    break;
                }

                // Wait for wake word
                let detector = wake_detector.read().await;
                match detector.wait_for_wake_word(&cancel_token).await {
                    Ok(true) => {
                        info!("Wake word detected");
                        drop(detector); // Release read lock

                        // Notify user
                        if let Err(e) = notifier
                            .notify("PIE is listening...", "Say your command")
                            .await
                        {
                            warn!("Failed to send notification: {}", e);
                        }

                        // Get voice command
                        match Self::capture_voice_command(
                            listen_duration,
                            Arc::clone(&stt_provider),
                        )
                        .await
                        {
                            Ok(command_text) => {
                                info!("Captured command: {}", command_text);

                                // Route through AI
                                match ai_router.route_command(&command_text).await {
                                    Ok(command) => {
                                        info!("Routed command: {:?}", command);

                                        // Execute command
                                        if let Err(e) = executor.execute(command).await {
                                            error!("Failed to execute command: {}", e);
                                            let _ = notifier
                                                .notify("Command failed", &e.to_string())
                                                .await;
                                        }
                                    }
                                    Err(e) => {
                                        warn!("Failed to route command: {}", e);
                                        let _ = notifier
                                            .notify("I didn't understand", "Please try again")
                                            .await;
                                    }
                                }
                            }
                            Err(e) => {
                                warn!("Failed to capture voice command: {}", e);
                            }
                        }
                    }
                    Ok(false) => {
                        // No wake word detected, continue
                    }
                    Err(e) => {
                        error!("Wake word detection error: {}", e);
                        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
                    }
                }
            }
        });

        // Handle shutdown signals
        tokio::select! {
            _ = signal::ctrl_c() => {
                info!("Received Ctrl+C, shutting down");
            }
            _ = Self::wait_for_termination() => {
                info!("Received termination signal");
            }
            result = wake_task => {
                if let Err(e) = result {
                    error!("Wake word task failed: {}", e);
                }
            }
            result = &mut control_server => {
                if let Err(e) = result {
                    error!("Control server error: {}", e);
                }
            }
        }

        self.shutdown().await
    }

    #[cfg(unix)]
    async fn run_control_server(&self) -> Result<()> {
        if let Some(parent) = self.config.control_socket.parent() {
            tokio_fs::create_dir_all(parent)
                .await
                .context("Failed to create control socket directory")?;
        }

        if self.config.control_socket.exists() {
            let _ = tokio_fs::remove_file(&self.config.control_socket).await;
        }

        let listener = UnixListener::bind(&self.config.control_socket)
            .context("Failed to bind control socket")?;

        loop {
            let (mut stream, _) = listener
                .accept()
                .await
                .context("Failed to accept control connection")?;

            let mut buffer = Vec::new();
            stream
                .read_to_end(&mut buffer)
                .await
                .context("Failed to read control command")?;

            if buffer.is_empty() {
                continue;
            }

            let command: ControlCommand = match serde_json::from_slice(&buffer) {
                Ok(cmd) => cmd,
                Err(err) => {
                    let response = ControlResponse::Error {
                        message: format!("Invalid control payload: {}", err),
                    };
                    let payload = serde_json::to_vec(&response)?;
                    stream
                        .write_all(&payload)
                        .await
                        .context("Failed to write control error response")?;
                    continue;
                }
            };

            let response = self.handle_control_command(command).await;
            let payload = serde_json::to_vec(&response)?;
            stream
                .write_all(&payload)
                .await
                .context("Failed to write control response")?;
        }
    }

    #[cfg(not(unix))]
    async fn run_control_server(&self) -> Result<()> {
        Ok(())
    }

    async fn handle_control_command(&self, command: ControlCommand) -> ControlResponse {
        match command {
            ControlCommand::GetStatus => match self.get_status().await {
                Ok(status) => ControlResponse::Status { status },
                Err(err) => ControlResponse::Error {
                    message: err.to_string(),
                },
            },
            ControlCommand::ToggleListening => match self.toggle_listening().await {
                Ok(ToggleResult::Started) => ControlResponse::ListeningStarted,
                Ok(ToggleResult::Stopped {
                    transcript,
                    duration_seconds,
                }) => ControlResponse::ListeningStopped {
                    transcript,
                    duration_seconds,
                },
                Err(err) => ControlResponse::Error {
                    message: err.to_string(),
                },
            },
            ControlCommand::StartListening => match self.start_listening().await {
                Ok(_) => ControlResponse::ListeningStarted,
                Err(err) => ControlResponse::Error {
                    message: err.to_string(),
                },
            },
            ControlCommand::StopListening => match self.stop_listening().await {
                Ok(Some((text, duration))) => ControlResponse::ListeningStopped {
                    transcript: Some(text),
                    duration_seconds: Some(duration),
                },
                Ok(None) => ControlResponse::ListeningStopped {
                    transcript: None,
                    duration_seconds: None,
                },
                Err(err) => ControlResponse::Error {
                    message: err.to_string(),
                },
            },
            ControlCommand::RunPipeline { name } => match self.run_pipeline(&name).await {
                Ok(PipelineOutcome::Completed(result)) => ControlResponse::PipelineFinished {
                    name,
                    text: result.text,
                },
                Ok(PipelineOutcome::Cancelled) => ControlResponse::PipelineCancelled { name },
                Err(err) => ControlResponse::Error {
                    message: err.to_string(),
                },
            },
            ControlCommand::CancelPipeline { name } => {
                let tokens = {
                    let controls = self.pipeline_controls.read().await;
                    controls.get(&name).cloned()
                };
                if let Some(tokens) = tokens {
                    tokens.capture.cancel();
                    tokens.abort.cancel();
                    ControlResponse::PipelineCancelled { name }
                } else {
                    ControlResponse::Error {
                        message: format!("Pipeline '{}' is not running", name),
                    }
                }
            }
        }
    }

    pub(crate) async fn capture_voice_command(
        listen_duration_secs: u32,
        stt_provider: Arc<RwLock<Option<Arc<dyn SpeechToText>>>>,
    ) -> Result<String> {
        #[cfg(feature = "audio")]
        {
            info!(
                "Capturing voice command for {} seconds",
                listen_duration_secs
            );

            // Create a temporary file for recording
            let temp_file =
                std::env::temp_dir().join(format!("pie_voice_{}.wav", std::process::id()));
            let temp_path = temp_file.to_str().unwrap().to_string();

            // Record microphone audio for the specified duration
            record_audio_without_ctrlc(
                Some(listen_duration_secs as u32),
                temp_path.clone(),
                true,  // mic_only
                false, // system_only
                None,  // system_device
            )
            .await
            .context("Failed to record voice command")?;

            info!("Voice command recorded to: {}", temp_path);

            let transcript = Self::transcribe_audio_with_store(&stt_provider, &temp_file).await;

            // Clean up the temporary file
            if temp_file.exists() {
                let _ = std::fs::remove_file(&temp_file);
            }

            transcript
        }

        #[cfg(not(feature = "audio"))]
        {
            let _ = listen_duration_secs; // Parameter used in audio-enabled builds
            let _ = stt_provider;
            Err(anyhow::anyhow!(
                "Voice command capture requires audio support. Build with --features audio to enable."
            ))
        }
    }

    async fn wait_for_termination() {
        #[cfg(unix)]
        {
            let mut stream = signal::unix::signal(signal::unix::SignalKind::terminate())
                .expect("Failed to setup SIGTERM handler");
            stream.recv().await;
        }

        #[cfg(not(unix))]
        {
            // On non-Unix systems, just wait forever
            std::future::pending::<()>().await;
        }
    }

    pub async fn stop(&self) -> Result<()> {
        info!("Stopping PIE daemon");

        // Set running flag to false
        {
            let mut running = self.running.write().await;
            *running = false;
        }

        self.push_to_talk_cancel.cancel();
        let watcher_tasks = {
            let mut guard = self.push_to_talk_tasks.write().await;
            std::mem::take(&mut *guard)
        };
        for task in watcher_tasks {
            if let Err(err) = task.await {
                if !err.is_cancelled() {
                    warn!("Push-to-talk watcher terminated with error: {}", err);
                }
            }
        }

        // Clean up PID file
        if self.config.pid_file.exists() {
            fs::remove_file(&self.config.pid_file).context("Failed to remove PID file")?;
        }

        // Send shutdown notification
        self.notifier
            .notify("PIE daemon stopped", "Goodbye")
            .await?;

        Ok(())
    }

    async fn shutdown(&self) -> Result<()> {
        info!("Shutting down daemon");

        self.abort_listening().await;

        // Stop all components
        self.stop().await?;

        // Save any pending data
        self.executor.save_pending().await?;

        if self.config.control_socket.exists() {
            let _ = tokio_fs::remove_file(&self.config.control_socket).await;
        }

        Ok(())
    }

    async fn abort_listening(&self) {
        let session = {
            let mut state = self.listening_state.write().await;
            if state.status.active {
                state.status.active = false;
                state.status.started_at = None;
                state.status.duration_seconds = None;
            }
            state.session.take()
        };

        if let Some(session) = session {
            session.cancel.cancel();
            if let Ok(result) = session.handle.await {
                if let Ok(recorded) = result {
                    let _ = tokio_fs::remove_file(&recorded.path).await;
                }
            }
        }
    }

    pub async fn is_running(&self) -> Result<bool> {
        if !self.config.pid_file.exists() {
            return Ok(false);
        }

        let pid_str =
            fs::read_to_string(&self.config.pid_file).context("Failed to read PID file")?;

        let pid: i32 = pid_str.trim().parse().context("Invalid PID in file")?;

        // Check if process is running
        #[cfg(unix)]
        {
            use nix::sys::signal::{self, Signal};
            use nix::unistd::Pid;

            // Send signal 0 to check if process exists
            match signal::kill(Pid::from_raw(pid), Signal::SIGCONT) {
                Ok(_) => Ok(true),
                Err(_) => {
                    // Process doesn't exist, clean up PID file
                    fs::remove_file(&self.config.pid_file)?;
                    Ok(false)
                }
            }
        }

        #[cfg(not(unix))]
        {
            // On non-Unix, just check if PID file exists
            Ok(true)
        }
    }

    pub async fn get_status(&self) -> Result<DaemonStatus> {
        let is_running = self.is_running().await?;

        let pid = if is_running && self.config.pid_file.exists() {
            Some(fs::read_to_string(&self.config.pid_file)?.trim().parse()?)
        } else {
            None
        };

        let audio_enabled = cfg!(feature = "audio");
        let wake_word_mode = if cfg!(feature = "porcupine") {
            WakeWordMode::Porcupine
        } else {
            WakeWordMode::Stub
        };

        let recording = self.executor.get_recording_info().await;

        let listening_status = {
            let mut status = self.listening_state.read().await.status.clone();
            if status.active {
                if let Some(started_at) = status.started_at {
                    status.duration_seconds =
                        Some((Local::now() - started_at).num_milliseconds().max(0) as f64 / 1000.0);
                }
            }
            status
        };

        Ok(DaemonStatus {
            running: is_running,
            pid,
            uptime_seconds: None,
            memory_usage_bytes: None,
            audio_enabled,
            wake_word_mode,
            wake_model_path: self.config.wake_model_path.clone(),
            wake_keyword_path: self.config.wake_keyword_path.clone(),
            wake_sensitivity: self.config.wake_sensitivity,
            recording,
            listening: listening_status,
        })
    }

    fn persist_debug_audio(&self, source: &Path) {
        #[cfg(not(feature = "audio"))]
        let _ = source;
        #[cfg(feature = "audio")]
        if let Some(dir) = &self.config.debug_audio_dir {
            match self.copy_audio_to_debug(dir, source) {
                Ok(target) => info!(
                    original = %source.display(),
                    copy = %target.display(),
                    "Copied pipeline audio to debug directory"
                ),
                Err(err) => warn!(
                    "Failed to copy pipeline audio {} to debug dir {}: {}",
                    source.display(),
                    dir.display(),
                    err
                ),
            }
        }
    }

    #[cfg(feature = "audio")]
    fn copy_audio_to_debug(&self, dir: &Path, source: &Path) -> Result<PathBuf> {
        let file_name = source
            .file_name()
            .ok_or_else(|| anyhow!("Audio capture path missing filename"))?;
        let target = dir.join(file_name);
        std::fs::create_dir_all(dir)
            .with_context(|| format!("Failed to create debug audio dir {}", dir.display()))?;
        std::fs::copy(source, &target).with_context(|| {
            format!(
                "Failed to copy audio from {} to {}",
                source.display(),
                target.display()
            )
        })?;
        Ok(target)
    }
}
