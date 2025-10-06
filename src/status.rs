use chrono::{DateTime, Local};
use serde::{Deserialize, Serialize};
use std::fmt;
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum WakeWordMode {
    Stub,
    Porcupine,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecordingInfo {
    pub filename: String,
    pub started_at: DateTime<Local>,
    pub duration_seconds: f64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ListeningStatus {
    pub active: bool,
    pub started_at: Option<DateTime<Local>>,
    pub duration_seconds: Option<f64>,
    pub last_completed_at: Option<DateTime<Local>>,
    pub last_transcript: Option<String>,
    pub last_error: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DaemonStatus {
    pub running: bool,
    pub pid: Option<i32>,
    pub uptime_seconds: Option<f64>,
    pub memory_usage_bytes: Option<u64>,
    pub audio_enabled: bool,
    pub wake_word_mode: WakeWordMode,
    pub wake_model_path: Option<PathBuf>,
    pub wake_keyword_path: Option<PathBuf>,
    pub wake_sensitivity: f32,
    pub recording: Option<RecordingInfo>,
    pub listening: ListeningStatus,
}

impl fmt::Display for DaemonStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "PIE Daemon Status")?;
        writeln!(f, "==================")?;

        if self.running {
            writeln!(f, "Status: Running")?;
            if let Some(pid) = self.pid {
                writeln!(f, "PID: {}", pid)?;
            }
        } else {
            writeln!(f, "Status: Not running")?;
        }

        writeln!(
            f,
            "Audio: {}",
            if self.audio_enabled {
                "Enabled"
            } else {
                "Disabled"
            }
        )?;

        match self.wake_word_mode {
            WakeWordMode::Stub => {
                writeln!(f, "Wake Word: Stub mode (no detection)")?;
            }
            WakeWordMode::Porcupine => {
                writeln!(f, "Wake Word: Porcupine (real detection)")?;
                if let Some(ref model_path) = self.wake_model_path {
                    writeln!(f, "  Model: {}", model_path.display())?;
                } else {
                    writeln!(f, "  Model: Default")?;
                }
                if let Some(ref keyword_path) = self.wake_keyword_path {
                    writeln!(f, "  Keyword: {}", keyword_path.display())?;
                } else {
                    writeln!(f, "  Keyword: Default")?;
                }
                writeln!(f, "  Sensitivity: {:.2}", self.wake_sensitivity)?;
            }
        }

        if let Some(recording) = &self.recording {
            writeln!(f, "Recording: Active")?;
            writeln!(f, "  File: {}", recording.filename)?;
            writeln!(
                f,
                "  Started: {}",
                recording.started_at.format("%Y-%m-%d %H:%M:%S")
            )?;
            writeln!(f, "  Duration: {:.1}s", recording.duration_seconds)?;
        } else {
            writeln!(f, "Recording: None")?;
        }

        if self.listening.active {
            writeln!(f, "Listening: Active")?;
            if let Some(started_at) = self.listening.started_at {
                writeln!(f, "  Started: {}", started_at.format("%Y-%m-%d %H:%M:%S"))?;
            }
            if let Some(duration) = self.listening.duration_seconds {
                writeln!(f, "  Duration: {:.1}s", duration)?;
            }
        } else {
            writeln!(f, "Listening: Idle")?;
        }

        if let Some(last_transcript) = &self.listening.last_transcript {
            writeln!(f, "Last Transcript: {}", last_transcript)?;
        }

        if let Some(err) = &self.listening.last_error {
            writeln!(f, "Last Error: {}", err)?;
        }

        if let Some(uptime) = self.uptime_seconds {
            writeln!(f, "Uptime: {:.1}s", uptime)?;
        }

        if let Some(mem) = self.memory_usage_bytes {
            writeln!(f, "Memory: {} MB", mem / 1_000_000)?;
        }

        Ok(())
    }
}
