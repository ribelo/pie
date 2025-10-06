use anyhow::{Context, Result};
use notify_rust::{Notification, Timeout};
use tracing::{debug, info, warn};

pub struct NotificationManager {
    timeout_ms: u32,
    enabled: bool,
}

#[allow(dead_code)]
impl NotificationManager {
    pub fn new() -> Self {
        Self {
            timeout_ms: 5000,
            enabled: true,
        }
    }

    pub fn with_timeout(mut self, timeout_ms: u32) -> Self {
        self.timeout_ms = timeout_ms;
        self
    }

    pub fn enabled(mut self, enabled: bool) -> Self {
        self.enabled = enabled;
        self
    }

    pub async fn notify(&self, summary: &str, body: &str) -> Result<()> {
        if !self.enabled {
            debug!("Notifications disabled, skipping: {} - {}", summary, body);
            return Ok(());
        }

        info!("Sending notification: {} - {}", summary, body);

        // Send desktop notification
        match Notification::new()
            .appname("PIE")
            .summary(summary)
            .body(body)
            .icon("audio-card")
            .timeout(Timeout::Milliseconds(self.timeout_ms))
            .show()
        {
            Ok(_) => {
                debug!("Notification sent successfully");
                Ok(())
            }
            Err(e) => {
                warn!("Failed to send desktop notification: {}", e);
                // Fall back to console output
                println!("🔔 {} - {}", summary, body);
                Ok(())
            }
        }
    }

    pub async fn notify_error(&self, summary: &str, error: &anyhow::Error) -> Result<()> {
        let body = format!("Error: {}", error);
        self.notify(summary, &body).await
    }

    pub async fn notify_with_actions(
        &self,
        summary: &str,
        body: &str,
        actions: Vec<(&str, &str)>,
    ) -> Result<Option<String>> {
        if !self.enabled {
            return Ok(None);
        }

        let mut notification = Notification::new();
        notification
            .appname("PIE")
            .summary(summary)
            .body(body)
            .icon("audio-card")
            .timeout(Timeout::Milliseconds(self.timeout_ms));

        // Add action buttons
        for (id, label) in actions {
            notification.action(id, label);
        }

        // Show and wait for response
        match notification.show() {
            Ok(handle) => {
                // Note: Action handling depends on the notification daemon
                // This is a simplified version
                handle.wait_for_action(|action| {
                    debug!("Notification action clicked: {}", action);
                });
                Ok(None)
            }
            Err(e) => {
                warn!("Failed to send notification with actions: {}", e);
                Ok(None)
            }
        }
    }

    pub async fn notify_progress(&self, summary: &str, progress: f32) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }

        let percentage = (progress * 100.0) as u32;
        let body = format!("Progress: {}%", percentage);

        Notification::new()
            .appname("PIE")
            .summary(summary)
            .body(&body)
            .icon("audio-card")
            .hint(notify_rust::Hint::Custom(
                "value".to_string(),
                percentage.to_string(),
            ))
            .timeout(Timeout::Milliseconds(1000))
            .show()
            .context("Failed to show progress notification")?;

        Ok(())
    }
}

// Notification presets for common scenarios
#[allow(dead_code)]
impl NotificationManager {
    pub async fn notify_listening(&self) -> Result<()> {
        self.notify("PIE is listening...", "Say your command").await
    }

    pub async fn notify_daemon_started(&self) -> Result<()> {
        self.notify("PIE daemon started", "Voice commands are now available")
            .await
    }

    pub async fn notify_daemon_stopped(&self) -> Result<()> {
        self.notify(
            "PIE daemon stopped",
            "Voice commands are no longer available",
        )
        .await
    }

    pub async fn notify_recording_started(&self, filename: &str) -> Result<()> {
        self.notify("Recording started", &format!("Recording to {}", filename))
            .await
    }

    pub async fn notify_recording_stopped(&self, filename: &str, duration_secs: u64) -> Result<()> {
        self.notify(
            "Recording stopped",
            &format!("Saved {} seconds to {}", duration_secs, filename),
        )
        .await
    }

    pub async fn notify_wake_word_detected(&self) -> Result<()> {
        self.notify("Wake word detected", "PIE is listening for your command...")
            .await
    }

    pub async fn notify_command_received(&self, command: &str) -> Result<()> {
        self.notify("Command received", &format!("Processing: {}", command))
            .await
    }

    pub async fn notify_unknown_command(&self, text: &str) -> Result<()> {
        self.notify("Unknown command", &format!("I didn't understand: {}", text))
            .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_notification_disabled() {
        let notifier = NotificationManager::new().enabled(false);

        // Should not error when disabled
        assert!(notifier.notify("Test", "Body").await.is_ok());
    }

    #[tokio::test]
    async fn test_notification_formats() {
        let notifier = NotificationManager::new();

        // Test various notification types
        assert!(notifier.notify_listening().await.is_ok());
        assert!(notifier.notify_recording_started("test.wav").await.is_ok());
        assert!(notifier
            .notify_recording_stopped("test.wav", 30)
            .await
            .is_ok());
    }
}
