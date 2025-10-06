use anyhow::Result;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use tracing::{debug, info, warn};

// This would integrate with ai-ox, but we'll create a simple interface for now
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(tag = "command", content = "params")]
pub enum PieCommand {
    StartRecording {
        #[serde(default)]
        duration: Option<u32>,
        #[serde(default)]
        output: Option<String>,
        #[serde(default)]
        source: RecordingSource,
    },
    StopRecording,
    ListDevices,
    GetStatus,
    SaveRecording {
        filename: String,
    },
    Unknown {
        original_text: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema, Default)]
pub enum RecordingSource {
    #[default]
    Microphone,
    System,
    Both,
}

pub struct AiRouter {
    // In a real implementation, this would hold the ai-ox Agent
    // For now, we'll use pattern matching as a placeholder
}

impl AiRouter {
    pub fn new() -> Result<Self> {
        // TODO: Initialize ai-ox Agent here
        // let agent = Agent::new()
        //     .with_model("claude-3-haiku")
        //     .with_system_prompt(ROUTER_PROMPT)
        //     .build()?;

        Ok(Self {})
    }

    pub async fn route_command(&self, text: &str) -> Result<PieCommand> {
        info!("Routing command: {}", text);

        // In production, this would use ai-ox to process the command
        // For now, we'll use simple pattern matching
        let command = self.parse_command_simple(text)?;

        debug!("Parsed command: {:?}", command);
        Ok(command)
    }

    fn parse_command_simple(&self, text: &str) -> Result<PieCommand> {
        let lower = text.to_lowercase();

        if lower.contains("stop") {
            return Ok(PieCommand::StopRecording);
        }

        // Check for recording commands
        if lower.contains("record") || lower.contains("start") {
            let duration = Self::extract_duration(&lower);
            let output = Self::extract_filename(&lower);
            let source = Self::extract_source(&lower);

            return Ok(PieCommand::StartRecording {
                duration,
                output,
                source,
            });
        }

        if lower.contains("save") {
            if let Some(filename) = Self::extract_save_name(&lower) {
                return Ok(PieCommand::SaveRecording { filename });
            }
        }

        if lower.contains("list") || lower.contains("devices") {
            return Ok(PieCommand::ListDevices);
        }

        if lower.contains("status") {
            return Ok(PieCommand::GetStatus);
        }

        // Unknown command
        warn!("Could not parse command: {}", text);
        Ok(PieCommand::Unknown {
            original_text: text.to_string(),
        })
    }

    fn extract_duration(text: &str) -> Option<u32> {
        // Look for patterns like "30 seconds", "1 minute", etc.
        if let Some(pos) = text.find("second") {
            // Extract number before "second"
            let before = &text[..pos];
            if let Some(num_str) = before.split_whitespace().last() {
                if let Ok(num) = num_str.parse::<u32>() {
                    return Some(num);
                }
            }
        }

        if let Some(pos) = text.find("minute") {
            let before = &text[..pos];
            if let Some(num_str) = before.split_whitespace().last() {
                if let Ok(num) = num_str.parse::<u32>() {
                    return Some(num * 60);
                }
            }
        }

        None
    }

    fn extract_filename(text: &str) -> Option<String> {
        // Look for "as <filename>" pattern
        if let Some(pos) = text.find(" as ") {
            let after = &text[pos + 4..];
            let filename = after.split_whitespace().next()?;

            // Add .wav extension if not present
            if filename.ends_with(".wav") {
                return Some(filename.to_string());
            } else {
                return Some(format!("{}.wav", filename));
            }
        }

        None
    }

    fn extract_save_name(text: &str) -> Option<String> {
        // Look for "save ... as <name>" pattern
        if let Some(pos) = text.rfind(" as ") {
            let after = &text[pos + 4..];
            let name = after.trim();

            if !name.is_empty() {
                // Clean up the name
                let clean_name = name
                    .replace(' ', "_")
                    .replace(|c: char| !c.is_alphanumeric() && c != '_' && c != '-', "");

                // Add .wav extension if not present
                if clean_name.ends_with(".wav") {
                    return Some(clean_name);
                } else {
                    return Some(format!("{}.wav", clean_name));
                }
            }
        }

        None
    }

    fn extract_source(text: &str) -> RecordingSource {
        if text.contains("system") || text.contains("computer") {
            RecordingSource::System
        } else if text.contains("microphone") || text.contains("mic") {
            RecordingSource::Microphone
        } else if text.contains("both") || text.contains("everything") {
            RecordingSource::Both
        } else {
            RecordingSource::default()
        }
    }
}

// System prompt for AI router (when using ai-ox)
#[allow(dead_code)]
const ROUTER_PROMPT: &str = r#"
You are PIE (Personal Intelligence Engine), a voice-activated assistant.
Your job is to parse natural language commands and convert them to structured commands.

Available commands:
1. StartRecording - Begin audio recording
   - duration: optional, in seconds
   - output: optional filename
   - source: Microphone, System, or Both

2. StopRecording - Stop any active recording

3. ListDevices - List available audio devices

4. GetStatus - Get current system status

5. SaveRecording - Save the current/last recording with a specific name
   - filename: required

When you receive a command, analyze it and return the appropriate structured command.
If you cannot understand the command, return Unknown with the original text.

Examples:
- "let's record" -> StartRecording with defaults
- "record for 30 seconds" -> StartRecording with duration: 30
- "record system audio" -> StartRecording with source: System
- "stop recording" -> StopRecording
- "save as meeting notes" -> SaveRecording with filename: "meeting_notes.wav"
"#;

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_simple_routing() {
        let router = AiRouter::new().unwrap();

        // Test start recording
        let cmd = router.route_command("let's record").await.unwrap();
        assert!(matches!(cmd, PieCommand::StartRecording { .. }));

        // Test stop recording
        let cmd = router.route_command("stop recording").await.unwrap();
        assert!(matches!(cmd, PieCommand::StopRecording));

        // Test list devices
        let cmd = router.route_command("show me the devices").await.unwrap();
        assert!(matches!(cmd, PieCommand::ListDevices));
    }

    #[tokio::test]
    async fn test_duration_extraction() {
        let router = AiRouter::new().unwrap();

        let cmd = router.route_command("record for 30 seconds").await.unwrap();
        if let PieCommand::StartRecording { duration, .. } = cmd {
            assert_eq!(duration, Some(30));
        } else {
            panic!("Expected StartRecording command");
        }

        let cmd = router.route_command("record for 2 minutes").await.unwrap();
        if let PieCommand::StartRecording { duration, .. } = cmd {
            assert_eq!(duration, Some(120));
        } else {
            panic!("Expected StartRecording command");
        }
    }
}
