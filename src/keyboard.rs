use anyhow::{Context, Result};
use async_trait::async_trait;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs::OpenOptions;
use tokio::io::AsyncWriteExt;
use tokio::sync::Mutex;
use tracing::{info, warn};

#[async_trait]
pub trait KeyboardOutput: Send + Sync {
    async fn send_text(&self, text: &str) -> Result<()>;
}

pub struct NoopKeyboard;

#[async_trait]
impl KeyboardOutput for NoopKeyboard {
    async fn send_text(&self, text: &str) -> Result<()> {
        warn!(
            "No keyboard backend configured; text output suppressed: {}",
            text
        );
        Ok(())
    }
}

#[cfg(feature = "wayland")]
mod virtual_keyboard {
    use super::KeyboardOutput;
    use anyhow::{Context, Result};
    use async_trait::async_trait;
    use enigo::{Enigo, Keyboard as EnigoKeyboard, Settings};
    use tokio::sync::Mutex;
    use tracing::info;

    pub struct VirtualKeyboard {
        inner: Mutex<Enigo>,
    }

    impl VirtualKeyboard {
        pub fn new() -> Result<Self> {
            let enigo = Enigo::new(&Settings::default())
                .context("Failed to establish virtual keyboard connection")?;

            Ok(Self {
                inner: Mutex::new(enigo),
            })
        }
    }

    #[async_trait]
    impl KeyboardOutput for VirtualKeyboard {
        async fn send_text(&self, text: &str) -> Result<()> {
            info!(
                "Virtual keyboard typing {} characters",
                text.chars().count()
            );
            let mut enigo = self.inner.lock().await;
            enigo
                .text(text)
                .context("Failed to send text through virtual keyboard")?;
            Ok(())
        }
    }
}

pub struct CaptureKeyboard {
    path: PathBuf,
    lock: Mutex<()>,
}

impl CaptureKeyboard {
    pub fn new(path: PathBuf) -> Result<Self> {
        if let Some(dir) = path.parent() {
            std::fs::create_dir_all(dir).with_context(|| {
                format!(
                    "Failed to create keyboard capture directory {}",
                    dir.display()
                )
            })?;
        }

        Ok(Self {
            path,
            lock: Mutex::new(()),
        })
    }
}

#[async_trait]
impl KeyboardOutput for CaptureKeyboard {
    async fn send_text(&self, text: &str) -> Result<()> {
        let _guard = self.lock.lock().await;
        let mut file = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)
            .await
            .with_context(|| {
                format!(
                    "Failed to open keyboard capture file {}",
                    self.path.display()
                )
            })?;
        file.write_all(text.as_bytes()).await.with_context(|| {
            format!(
                "Failed to write keyboard capture text to {}",
                self.path.display()
            )
        })?;
        file.write_all(b"\n").await.with_context(|| {
            format!(
                "Failed to append newline to keyboard capture {}",
                self.path.display()
            )
        })?;
        file.flush().await.with_context(|| {
            format!(
                "Failed to flush keyboard capture file {}",
                self.path.display()
            )
        })?;
        Ok(())
    }
}

pub enum KeyboardBackend {
    #[cfg(feature = "wayland")]
    Virtual(virtual_keyboard::VirtualKeyboard),
    Capture(CaptureKeyboard),
    Noop(NoopKeyboard),
}

impl KeyboardBackend {
    pub fn detect() -> Result<Self> {
        if let Ok(path) = std::env::var("PIE_KEYBOARD_CAPTURE_PATH") {
            return Ok(Self::Capture(CaptureKeyboard::new(path.into())?));
        }

        #[cfg(feature = "wayland")]
        {
            let wayland_display = std::env::var("WAYLAND_DISPLAY").is_ok();
            let x11_display = std::env::var("DISPLAY").is_ok();

            if wayland_display || x11_display {
                return Ok(Self::Virtual(
                    virtual_keyboard::VirtualKeyboard::new()
                        .context("Failed to initialize virtual keyboard backend")?,
                ));
            }
        }

        Ok(Self::Noop(NoopKeyboard))
    }
}

pub fn detect_keyboard_backend() -> Arc<dyn KeyboardOutput> {
    match KeyboardBackend::detect() {
        Ok(backend) => {
            info!("Keyboard backend selected: {}", backend.name());
            Arc::new(backend)
        }
        Err(err) => {
            warn!("Failed to initialize keyboard backend: {}", err);
            Arc::new(KeyboardBackend::Noop(NoopKeyboard))
        }
    }
}

#[async_trait]
impl KeyboardOutput for KeyboardBackend {
    async fn send_text(&self, text: &str) -> Result<()> {
        match self {
            #[cfg(feature = "wayland")]
            KeyboardBackend::Virtual(inner) => inner.send_text(text).await,
            KeyboardBackend::Capture(inner) => inner.send_text(text).await,
            KeyboardBackend::Noop(inner) => inner.send_text(text).await,
        }
    }
}

impl KeyboardBackend {
    fn name(&self) -> &'static str {
        match self {
            #[cfg(feature = "wayland")]
            KeyboardBackend::Virtual(_) => "virtual",
            KeyboardBackend::Capture(_) => "capture",
            KeyboardBackend::Noop(_) => "noop",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    #[tokio::test(flavor = "current_thread")]
    async fn capture_backend_writes_text() {
        let tmp = tempdir().expect("tempdir");
        let file_path = tmp.path().join("keyboard.log");
        std::env::set_var("PIE_KEYBOARD_CAPTURE_PATH", &file_path);

        let backend = detect_keyboard_backend();
        backend
            .send_text("hello world")
            .await
            .expect("capture send succeeds");
        backend
            .send_text("second line")
            .await
            .expect("capture send second");

        drop(backend);
        std::env::remove_var("PIE_KEYBOARD_CAPTURE_PATH");

        let contents = fs::read_to_string(&file_path).expect("capture file readable");
        assert!(contents.contains("hello world"));
        assert!(contents.contains("second line"));
    }
}
