#[cfg(any(feature = "porcupine", feature = "audio"))]
use anyhow::Context;
use anyhow::Result;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{debug, info};

#[cfg(feature = "porcupine")]
use porcupine_sys::Object;

#[async_trait::async_trait]
pub trait WakeWordDetectorTrait {
    async fn wait_for_wake_word(
        &self,
        cancel: &tokio_util::sync::CancellationToken,
    ) -> Result<bool>;
    async fn start_continuous_detection(&self) -> Result<()>;
    async fn set_sensitivity(&mut self, sensitivity: f32);
    async fn shutdown(&self) -> Result<()>;
}

#[allow(dead_code)]
pub struct WakeWordDetector {
    detector: Arc<Mutex<Box<dyn WakeWordDetectorTrait + Send + Sync>>>,
    sensitivity: f32,
}

#[allow(dead_code)]
impl WakeWordDetector {
    pub fn new() -> Result<Self> {
        Self::with_sensitivity(0.5)
    }

    pub fn with_sensitivity(sensitivity: f32) -> Result<Self> {
        Self::with_config(None, None, sensitivity)
    }

    pub fn with_config(
        model_path: Option<std::path::PathBuf>,
        keyword_path: Option<std::path::PathBuf>,
        sensitivity: f32,
    ) -> Result<Self> {
        #[cfg(feature = "porcupine")]
        let detector: Arc<Mutex<Box<dyn WakeWordDetectorTrait + Send + Sync>>> =
            Arc::new(Mutex::new(Box::new(
                real_detector::RealWakeWordDetector::new(
                    model_path.clone(),
                    keyword_path.clone(),
                    sensitivity,
                )
                .context("Failed to initialize real wake word detector")?,
            )));

        #[cfg(not(feature = "porcupine"))]
        let detector: Arc<Mutex<Box<dyn WakeWordDetectorTrait + Send + Sync>>> = {
            let _ = (model_path, keyword_path);
            Arc::new(Mutex::new(Box::new(
                stub_detector::StubWakeWordDetector::new(sensitivity),
            )))
        };

        Ok(Self {
            detector,
            sensitivity,
        })
    }

    pub fn from_definition(definition: &WakeWordDefinition) -> Result<Self> {
        Self::from_definitions(std::slice::from_ref(definition))
    }

    pub fn from_definitions(definitions: &[WakeWordDefinition]) -> Result<Self> {
        #[cfg(feature = "porcupine")]
        let detector: Arc<Mutex<Box<dyn WakeWordDetectorTrait + Send + Sync>>> =
            Arc::new(Mutex::new(Box::new(
                real_detector::RealWakeWordDetector::from_definitions(definitions)
                    .context("Failed to initialize real wake word detector")?,
            )));

        #[cfg(not(feature = "porcupine"))]
        let detector: Arc<Mutex<Box<dyn WakeWordDetectorTrait + Send + Sync>>> = {
            let _ = definitions;
            Arc::new(Mutex::new(Box::new(
                stub_detector::StubWakeWordDetector::new_with_definitions(definitions),
            )))
        };

        let sensitivity = definitions
            .first()
            .map(|def| def.sensitivity)
            .unwrap_or(0.5);

        Ok(Self {
            detector,
            sensitivity,
        })
    }

    pub async fn wait_for_wake_word(
        &self,
        cancel: &tokio_util::sync::CancellationToken,
    ) -> Result<bool> {
        let detector = self.detector.lock().await;
        detector.wait_for_wake_word(cancel).await
    }

    pub async fn start_continuous_detection(&self) -> Result<()> {
        let detector = self.detector.lock().await;
        detector.start_continuous_detection().await
    }

    pub async fn set_sensitivity(&mut self, sensitivity: f32) {
        self.sensitivity = sensitivity.clamp(0.0, 1.0);
        let mut detector = self.detector.lock().await;
        let _ = detector.set_sensitivity(sensitivity).await;
    }

    pub async fn shutdown(&self) -> Result<()> {
        let detector = self.detector.lock().await;
        detector.shutdown().await
    }
}

// Custom wake word configuration
#[allow(dead_code)]
pub struct WakeWordConfig {
    pub keyword: String,
    pub sensitivity: f32,
    pub model_path: Option<String>,
}

impl Default for WakeWordConfig {
    fn default() -> Self {
        Self {
            keyword: "Ok PIE".to_string(),
            sensitivity: 0.5,
            model_path: None,
        }
    }
}

#[cfg_attr(not(feature = "porcupine"), allow(dead_code))]
#[derive(Debug, Clone)]
pub struct WakeWordDefinition {
    pub keyword_path: PathBuf,
    pub model_path: Option<PathBuf>,
    pub sensitivity: f32,
}

// Audio capture specifically configured for wake word detection
#[cfg(feature = "audio")]
mod audio {
    use super::*;
    use libpulse_binding::sample::{Format, Spec};
    use libpulse_binding::stream::Direction;
    use libpulse_simple_binding::Simple;

    #[allow(dead_code)]
    pub struct AudioCapture {
        simple: Simple,
        buffer_size: usize,
    }

    #[allow(dead_code)]
    impl AudioCapture {
        pub fn new_for_wake_word() -> Result<Self> {
            // Porcupine expects 16kHz audio
            let spec = Spec {
                format: Format::S16le,
                channels: 1,
                rate: 16000,
            };

            let simple = Simple::new(
                None,              // Use default server
                "pie",             // Application name
                Direction::Record, // Recording
                None,              // Use default device
                "wake_word",       // Stream name
                &spec,
                None, // Use default channel map
                None, // Use default buffer attributes
            )
            .context("Failed to create PulseAudio connection for wake word")?;

            Ok(Self {
                simple,
                buffer_size: 512, // Porcupine typically uses 512 sample frames
            })
        }

        pub async fn capture_frame(&self, frame_length: usize) -> Result<Vec<i16>> {
            let mut buffer = vec![0u8; frame_length * 2]; // 2 bytes per i16

            // Read audio data
            self.simple
                .read(&mut buffer)
                .context("Failed to read audio for wake word")?;

            // Convert bytes to i16
            let pcm: Vec<i16> = buffer
                .chunks_exact(2)
                .map(|chunk| i16::from_le_bytes([chunk[0], chunk[1]]))
                .collect();

            Ok(pcm)
        }

        pub async fn stop(&self) -> Result<()> {
            // Flush any remaining audio
            self.simple
                .flush()
                .context("Failed to flush audio capture")?;
            Ok(())
        }
    }
}

#[cfg(feature = "porcupine")]
mod real_detector {
    use super::*;

    pub struct RealWakeWordDetector {
        porcupine: Arc<Mutex<Object>>,
        audio_capture: AudioCapture,
        sensitivity: f32,
    }

    impl RealWakeWordDetector {
        pub fn from_definitions(defs: &[WakeWordDefinition]) -> Result<Self> {
            if defs.is_empty() {
                anyhow::bail!("At least one wake word definition is required");
            }

            let access_key = std::env::var("PICOVOICE_ACCESS_KEY")
                .context("PICOVOICE_ACCESS_KEY environment variable not set. Please set it to your Picovoice access key.")?;
            let _ = access_key; // Access key is consumed by the underlying library via global state.

            let first = &defs[0];
            let model_path = first
                .model_path
                .clone()
                .map(|p| p)
                .or_else(|| {
                    std::env::var("PICOVOICE_MODEL_PATH")
                        .ok()
                        .map(PathBuf::from)
                })
                .unwrap_or_else(|| {
                    PathBuf::from("/usr/local/lib/porcupine/lib/common/porcupine_params.pv")
                });

            if !model_path.exists() {
                anyhow::bail!(
                    "Porcupine model file not found at: {}. Please provide a valid path via capture.wake_word.model_path or PICOVOICE_MODEL_PATH",
                    model_path.display()
                );
            }

            for def in defs.iter().skip(1) {
                if let Some(other_model) = &def.model_path {
                    if other_model != &model_path {
                        anyhow::bail!(
                            "All wake words must share the same model file. Mismatch: {} vs {}",
                            other_model.display(),
                            model_path.display()
                        );
                    }
                }
            }

            let mut keyword_paths: Vec<String> = Vec::with_capacity(defs.len());
            let mut sensitivities: Vec<f32> = Vec::with_capacity(defs.len());

            for def in defs {
                if !def.keyword_path.exists() {
                    anyhow::bail!(
                        "Porcupine keyword file not found at: {}",
                        def.keyword_path.display()
                    );
                }
                keyword_paths.push(
                    def.keyword_path
                        .to_str()
                        .ok_or_else(|| {
                            anyhow::anyhow!(
                                "Keyword path contains invalid UTF-8: {}",
                                def.keyword_path.display()
                            )
                        })?
                        .to_owned(),
                );
                sensitivities.push(def.sensitivity);
            }

            let keyword_refs: Vec<&str> = keyword_paths.iter().map(|s| s.as_str()).collect();
            let model_str = model_path.to_str().ok_or_else(|| {
                anyhow::anyhow!(
                    "Model path contains invalid UTF-8: {}",
                    model_path.display()
                )
            })?;

            let porcupine = unsafe {
                Object::new_multiple_keywords(model_str, &keyword_refs, &sensitivities)
            }
            .map_err(|e| anyhow::anyhow!(
                "Failed to initialize Porcupine: {:?}. Check your access key and model/keyword files.",
                e
            ))?;

            let audio_capture =
                AudioCapture::new_for_wake_word().context("Failed to initialize audio capture. Ensure audio feature is enabled and PulseAudio/PipeWire is running.")?;

            Ok(Self {
                porcupine: Arc::new(Mutex::new(porcupine)),
                audio_capture,
                sensitivity: first.sensitivity,
            })
        }

        pub fn new(
            model_path: Option<PathBuf>,
            keyword_path: Option<PathBuf>,
            sensitivity: f32,
        ) -> Result<Self> {
            let definition = WakeWordDefinition {
                keyword_path: keyword_path.unwrap_or_else(|| {
                    PathBuf::from(
                        "/usr/local/lib/porcupine/resources/keyword_files/linux/computer_linux.ppn",
                    )
                }),
                model_path,
                sensitivity,
            };
            Self::from_definitions(std::slice::from_ref(&definition))
        }
    }

    #[async_trait::async_trait]
    impl WakeWordDetectorTrait for RealWakeWordDetector {
        async fn wait_for_wake_word(
            &self,
            cancel: &tokio_util::sync::CancellationToken,
        ) -> Result<bool> {
            debug!("Waiting for wake word (real detector)...");

            // Get frame size required by Porcupine
            let frame_length = unsafe { porcupine_sys::frame_length() };

            loop {
                // Check for cancellation
                if cancel.is_cancelled() {
                    return Ok(false);
                }

                // Capture audio frame
                let pcm_frame = self
                    .audio_capture
                    .capture_frame(frame_length)
                    .await
                    .context("Failed to capture audio frame")?;

                // Process frame with Porcupine
                let keyword_index = {
                    let porcupine = self.porcupine.lock().await;
                    unsafe { porcupine.process_multiple_keywords(&pcm_frame) }
                        .map_err(|e| anyhow::anyhow!("Failed to process audio frame: {:?}", e))?
                };

                // Check if wake word was detected
                if keyword_index >= 0 {
                    info!("Wake word detected (index: {})", keyword_index);
                    return Ok(true);
                }

                // Small delay to prevent busy looping
                tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
            }
        }

        async fn start_continuous_detection(&self) -> Result<()> {
            info!("Starting continuous wake word detection (real)");
            let cancel = tokio_util::sync::CancellationToken::new();
            loop {
                match self.wait_for_wake_word(&cancel).await {
                    Ok(detected) => {
                        if detected {
                            return Ok(());
                        }
                    }
                    Err(e) => {
                        warn!("Error during wake word detection: {}", e);
                        // Brief pause before retrying
                        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
                    }
                }
            }
        }

        async fn set_sensitivity(&mut self, sensitivity: f32) {
            self.sensitivity = sensitivity.clamp(0.0, 1.0);
            info!("Wake word sensitivity set to {}", self.sensitivity);
        }

        async fn shutdown(&self) -> Result<()> {
            info!("Shutting down wake word detector (real)");
            self.audio_capture.stop().await?;
            Ok(())
        }
    }

    impl RealWakeWordDetector {
        async fn start_continuous_detection(&self) -> Result<()> {
            info!("Starting continuous wake word detection (real)");
            let cancel = tokio_util::sync::CancellationToken::new();
            loop {
                match self.wait_for_wake_word(&cancel).await {
                    Ok(detected) => {
                        if detected {
                            return Ok(());
                        }
                    }
                    Err(e) => {
                        warn!("Wake word detection loop error: {}", e);
                        tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
                    }
                }
            }
        }
    }
}

mod stub_detector {
    use super::*;

    pub struct StubWakeWordDetector {
        sensitivity: f32,
    }

    #[async_trait::async_trait]
    impl WakeWordDetectorTrait for StubWakeWordDetector {
        async fn wait_for_wake_word(
            &self,
            _cancel: &tokio_util::sync::CancellationToken,
        ) -> Result<bool> {
            debug!("Waiting for wake word (stub detector - always returns false)");
            // Simulate some delay
            tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
            Ok(false)
        }

        async fn start_continuous_detection(&self) -> Result<()> {
            info!("Starting continuous wake word detection (stub - never detects)");
            // Never returns, just loop
            loop {
                tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
            }
        }

        async fn set_sensitivity(&mut self, sensitivity: f32) {
            self.sensitivity = sensitivity.clamp(0.0, 1.0);
            info!("Wake word sensitivity set to {} (stub)", self.sensitivity);
        }

        async fn shutdown(&self) -> Result<()> {
            info!("Shutting down wake word detector (stub)");
            Ok(())
        }
    }

    impl StubWakeWordDetector {
        pub fn new(sensitivity: f32) -> Self {
            info!("Using stub wake word detector (Picovoice not available)");
            Self { sensitivity }
        }

        pub fn new_with_definitions(defs: &[WakeWordDefinition]) -> Self {
            let sensitivity = defs.first().map(|def| def.sensitivity).unwrap_or(0.5);
            info!(
                "Using stub wake word detector for {} keyword(s); no wake events will fire",
                defs.len()
            );
            Self { sensitivity }
        }
    }
}
