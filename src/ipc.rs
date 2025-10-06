use serde::{Deserialize, Serialize};

use crate::status::DaemonStatus;

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ControlCommand {
    GetStatus,
    ToggleListening,
    StartListening,
    StopListening,
    RunPipeline { name: String },
    CancelPipeline { name: String },
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "snake_case")]
pub enum ControlResponse {
    Status {
        status: DaemonStatus,
    },
    ListeningStarted,
    ListeningStopped {
        transcript: Option<String>,
        duration_seconds: Option<f64>,
    },
    PipelineFinished {
        name: String,
        text: Option<String>,
    },
    PipelineCancelled {
        name: String,
    },
    Error {
        message: String,
    },
}
