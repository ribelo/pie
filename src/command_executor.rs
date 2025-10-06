use anyhow::{Context, Result};
use chrono::Local;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{info, warn};

use crate::ai_router::{PieCommand, RecordingSource};
use crate::commands::PipelineRegistry;
use crate::notification::NotificationManager;
use crate::status::RecordingInfo;

// Re-use the existing recording logic from main.rs, minus Ctrl+C handler
use crate::{list_devices, record_audio_without_ctrlc};

pub struct CommandExecutor {
    active_recording: Arc<RwLock<Option<RecordingSession>>>,
    notifier: Arc<NotificationManager>,
    output_dir: PathBuf,
}

struct RecordingSession {
    handle: tokio::task::JoinHandle<Result<()>>,
    output_file: PathBuf,
    started_at: chrono::DateTime<Local>,
}

impl CommandExecutor {
    pub fn new(_pipelines: Arc<PipelineRegistry>) -> Result<Self> {
        let output_dir = dirs::document_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("pie-recordings");

        // Ensure output directory exists
        std::fs::create_dir_all(&output_dir).context("Failed to create recordings directory")?;

        Ok(Self {
            active_recording: Arc::new(RwLock::new(None)),
            notifier: Arc::new(NotificationManager::new()),
            output_dir,
        })
    }

    pub async fn get_recording_info(&self) -> Option<RecordingInfo> {
        if let Some(session) = &*self.active_recording.read().await {
            let duration_so_far = chrono::Local::now() - session.started_at;
            let filename = session
                .output_file
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("unknown")
                .to_string();

            Some(RecordingInfo {
                filename,
                started_at: session.started_at,
                duration_seconds: duration_so_far.num_milliseconds().max(0) as f64 / 1000.0,
            })
        } else {
            None
        }
    }

    pub async fn execute(&self, command: PieCommand) -> Result<()> {
        info!("Executing command: {:?}", command);

        match command {
            PieCommand::StartRecording {
                duration,
                output,
                source,
            } => {
                self.start_recording(duration, output, source).await?;
            }
            PieCommand::StopRecording => {
                self.stop_recording().await?;
            }
            PieCommand::ListDevices => {
                self.list_devices().await?;
            }
            PieCommand::GetStatus => {
                self.get_status().await?;
            }
            PieCommand::SaveRecording { filename } => {
                self.save_recording(filename).await?;
            }
            PieCommand::Unknown { original_text } => {
                warn!("Unknown command: {}", original_text);
                self.notifier
                    .notify(
                        "Unknown command",
                        &format!("I didn't understand: {}", original_text),
                    )
                    .await?;
            }
        }

        Ok(())
    }

    async fn start_recording(
        &self,
        duration: Option<u32>,
        output: Option<String>,
        source: RecordingSource,
    ) -> Result<()> {
        // Check if already recording
        {
            let recording = self.active_recording.read().await;
            if recording.is_some() {
                self.notifier
                    .notify("Already recording", "Stop the current recording first")
                    .await?;
                return Ok(());
            }
        }

        // Generate output filename
        let timestamp = Local::now().format("%Y%m%d_%H%M%S");
        let filename = output.unwrap_or_else(|| format!("recording_{}.wav", timestamp));
        let output_path = self.output_dir.join(&filename);

        info!("Starting recording to: {:?}", output_path);

        // Determine recording options based on source
        let (mic_only, system_only) = match source {
            RecordingSource::Microphone => (true, false),
            RecordingSource::System => (false, true),
            RecordingSource::Both => (false, false),
        };

        // Clone values for the async task
        let output_path_clone = output_path.clone();
        let notifier = Arc::clone(&self.notifier);

        // Start recording task
        let handle = tokio::spawn(async move {
            // Send start notification
            notifier
                .notify("Recording started", &format!("Recording to {}", filename))
                .await?;

            // Call the existing record_audio function
            record_audio_without_ctrlc(
                duration,
                output_path_clone.to_str().unwrap().to_string(),
                mic_only,
                system_only,
                None, // system_device
            )
            .await?;

            // Send completion notification
            if let Some(dur) = duration {
                notifier
                    .notify(
                        "Recording completed",
                        &format!("Saved {} seconds to {}", dur, filename),
                    )
                    .await?;
            }

            Ok(())
        });

        // Store recording session
        let session = RecordingSession {
            handle,
            output_file: output_path,
            started_at: Local::now(),
        };

        {
            let mut recording = self.active_recording.write().await;
            *recording = Some(session);
        }

        Ok(())
    }

    async fn stop_recording(&self) -> Result<()> {
        let session = {
            let mut recording = self.active_recording.write().await;
            recording.take()
        };

        if let Some(session) = session {
            info!("Stopping recording: {:?}", session.output_file);

            // Cancel the recording task
            session.handle.abort();

            // Calculate duration
            let duration = Local::now() - session.started_at;
            let seconds = duration.num_seconds();

            // Send notification
            let filename = session
                .output_file
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("recording.wav");

            self.notifier
                .notify(
                    "Recording stopped",
                    &format!("Saved {} seconds to {}", seconds, filename),
                )
                .await?;
        } else {
            self.notifier
                .notify("No active recording", "Nothing to stop")
                .await?;
        }

        Ok(())
    }

    async fn list_devices(&self) -> Result<()> {
        info!("Listing audio devices");

        // Get device list
        let devices = list_devices().await?;

        // Format for notification
        let device_text = if devices.is_empty() {
            "No audio devices found".to_string()
        } else {
            devices.join("\n")
        };

        self.notifier.notify("Audio Devices", &device_text).await?;

        Ok(())
    }

    async fn get_status(&self) -> Result<()> {
        info!("Getting system status");

        let status = {
            let recording = self.active_recording.read().await;
            if let Some(ref session) = *recording {
                let duration = Local::now() - session.started_at;
                format!(
                    "Recording in progress\nDuration: {} seconds\nFile: {:?}",
                    duration.num_seconds(),
                    session.output_file.file_name().unwrap_or_default()
                )
            } else {
                "PIE is ready\nNo active recordings".to_string()
            }
        };

        self.notifier.notify("PIE Status", &status).await?;

        Ok(())
    }

    async fn save_recording(&self, filename: String) -> Result<()> {
        // Check if there's an active or recent recording to rename
        let session = {
            let recording = self.active_recording.read().await;
            recording.as_ref().map(|s| s.output_file.clone())
        };

        if let Some(current_file) = session {
            let new_path = self.output_dir.join(&filename);

            // Rename the file
            std::fs::rename(&current_file, &new_path).context("Failed to rename recording")?;

            info!("Renamed recording to: {:?}", new_path);

            self.notifier
                .notify("Recording saved", &format!("Saved as {}", filename))
                .await?;
        } else {
            // Look for the most recent recording
            let entries = std::fs::read_dir(&self.output_dir)?;
            let mut recordings: Vec<_> = entries
                .filter_map(|e| e.ok())
                .filter(|e| {
                    e.path()
                        .extension()
                        .and_then(|ext| ext.to_str())
                        .map(|ext| ext == "wav")
                        .unwrap_or(false)
                })
                .collect();

            // Sort by modification time
            recordings.sort_by_key(|e| {
                e.metadata()
                    .and_then(|m| m.modified())
                    .unwrap_or(std::time::SystemTime::UNIX_EPOCH)
            });

            if let Some(latest) = recordings.last() {
                let new_path = self.output_dir.join(&filename);
                std::fs::rename(latest.path(), &new_path).context("Failed to rename recording")?;

                self.notifier
                    .notify("Recording saved", &format!("Saved as {}", filename))
                    .await?;
            } else {
                self.notifier
                    .notify("No recordings found", "No recent recordings to save")
                    .await?;
            }
        }

        Ok(())
    }

    pub async fn save_pending(&self) -> Result<()> {
        // Stop any active recordings gracefully
        let session = {
            let mut recording = self.active_recording.write().await;
            recording.take()
        };

        if let Some(session) = session {
            warn!("Saving pending recording before shutdown");
            // Wait for recording to complete
            let _ = session.handle.await;
        }

        Ok(())
    }
}
