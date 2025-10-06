use crate::commands::{CaptureMode, CommandPipeline, InputKind, PipelineRegistry};
use crate::daemon::{abort_pipeline_by_name, stop_capture_by_name, DaemonConfig};
use crate::ipc::{ControlCommand, ControlResponse};
use crate::send_control_command;
use anyhow::{anyhow, Context, Result};
use evdev::{Device, InputEventKind, Key};
use notify::{event::CreateKind, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::collections::HashMap;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time::Duration;
use tokio::sync::mpsc::{self, Sender};
use tokio::task::JoinHandle;
use tokio::time;
use tokio_util::sync::CancellationToken;
use tracing::{info, warn};

#[derive(Clone)]
pub struct PushToTalkBinding {
    pub name: String,
    pub hotkey: String,
}

#[derive(Debug)]
enum HotkeyEvent {
    Pressed,
    Released,
}

pub fn bindings_from_registry(registry: &PipelineRegistry) -> Vec<PushToTalkBinding> {
    registry
        .values()
        .filter_map(|pipeline| binding_from_pipeline(pipeline))
        .collect()
}

pub fn spawn_daemon_watchers(
    bindings: Vec<PushToTalkBinding>,
    config: DaemonConfig,
    cancel: CancellationToken,
) -> Vec<JoinHandle<()>> {
    bindings
        .into_iter()
        .map(|binding| {
            let cfg = config.clone();
            let cancel_clone = cancel.clone();
            tokio::spawn(async move {
                if let Err(err) =
                    run_binding(binding.clone(), cfg, cancel_clone.cancelled_owned()).await
                {
                    warn!(
                        "Push-to-talk watcher for '{}' stopped: {}",
                        binding.name, err
                    );
                }
            })
        })
        .collect()
}

fn binding_from_pipeline(pipeline: &CommandPipeline) -> Option<PushToTalkBinding> {
    if !matches!(pipeline.input.kind, InputKind::Audio) {
        return None;
    }

    if !matches!(pipeline.input.capture.mode, CaptureMode::Manual) {
        return None;
    }

    let hotkey = pipeline
        .input
        .capture
        .push_to_talk
        .as_ref()
        .map(|cfg| cfg.hotkey.trim().to_string())?;

    if hotkey.is_empty() {
        return None;
    }

    Some(PushToTalkBinding {
        name: pipeline.name.clone(),
        hotkey,
    })
}

async fn evdev_listen_device(sender: Sender<HotkeyEvent>, path: PathBuf, device: Device, key: Key) {
    let name = device.name().unwrap_or("Unnamed device");
    let descriptor = format!("{} ({})", name, path.display());

    info!("Listening for push-to-talk hotkey on {}", descriptor);

    let mut events = match device.into_event_stream() {
        Ok(stream) => stream,
        Err(err) => {
            warn!("Failed to start event stream on {}: {}", descriptor, err);
            return;
        }
    };

    loop {
        let ev = match events.next_event().await {
            Ok(event) => event,
            Err(err) => {
                warn!(
                    "Event stream closed on {} (device disconnected?): {}",
                    descriptor, err
                );
                return;
            }
        };

        if let InputEventKind::Key(k) = ev.kind() {
            if k == key {
                match ev.value() {
                    0 => {
                        let _ = sender.send(HotkeyEvent::Released).await;
                    }
                    1 => {
                        let _ = sender.send(HotkeyEvent::Pressed).await;
                    }
                    _ => {}
                }
            }
        }
    }
}

fn spawn_listener(sender: Sender<HotkeyEvent>, path: PathBuf, device: Device, key: Key) {
    tokio::spawn(evdev_listen_device(sender, path, device, key));
}

async fn register_and_watch(sender: Sender<HotkeyEvent>, hotkey: Key) -> Result<()> {
    evdev::enumerate()
        .filter(|(_, device)| {
            device
                .supported_keys()
                .map_or(false, |keys| keys.contains(hotkey))
        })
        .for_each(|(path, device)| {
            spawn_listener(sender.clone(), path, device, hotkey);
        });

    let (tx, mut rx) = mpsc::channel(1);
    let mut watcher = RecommendedWatcher::new(
        move |res| {
            let _ = tx.blocking_send(res);
        },
        notify::Config::default(),
    )
    .context("Failed to configure /dev/input watcher")?;

    watcher
        .watch(Path::new("/dev/input"), RecursiveMode::NonRecursive)
        .context("Failed to watch /dev/input for new devices")?;

    let mut wait_for_permissions: HashMap<PathBuf, u8> = HashMap::new();

    loop {
        let mut timeout = Box::pin(time::sleep(Duration::from_secs(1)));
        tokio::select! {
            res = rx.recv() => {
                match res {
                    Some(Ok(event)) => {
                        if let EventKind::Create(CreateKind::File) = event.kind {
                            for path in event.paths {
                                if let Err(err) = Device::open(&path)
                                    .map(|device| spawn_listener(sender.clone(), path.clone(), device, hotkey))
                                {
                                    warn!(
                                        "Push-to-talk hotkey waiting for permissions on {}: {}",
                                        path.display(),
                                        err
                                    );
                                    wait_for_permissions.insert(path, 5);
                                }
                            }
                        }
                    }
                    Some(Err(err)) => warn!("Hotkey watcher error: {}", err),
                    None => break,
                }
            }
            _ = &mut timeout => {
                wait_for_permissions.retain(|path, remaining| {
                    if *remaining == 0 {
                        return false;
                    }

                    match Device::open(path) {
                        Ok(device) => {
                            spawn_listener(sender.clone(), path.clone(), device, hotkey);
                            false
                        }
                        Err(_) => {
                            *remaining -= 1;
                            true
                        }
                    }
                });
            }
        }
    }

    Ok(())
}

async fn run_binding<Fut>(
    binding: PushToTalkBinding,
    config: DaemonConfig,
    shutdown: Fut,
) -> Result<()>
where
    Fut: Future<Output = ()> + Send,
{
    let key = Key::from_str(&binding.hotkey)
        .map_err(|_| anyhow!("Could not find key with name {}", binding.hotkey))?;

    let (event_tx, mut event_rx) = mpsc::channel(32);
    let watcher_sender = event_tx.clone();
    let hotkey_label = binding.hotkey.clone();
    let hotkey_for_spawn = hotkey_label.clone();
    tokio::spawn(async move {
        if let Err(err) = register_and_watch(watcher_sender, key).await {
            warn!(
                "Push-to-talk watcher for hotkey {} terminated: {}",
                hotkey_for_spawn, err
            );
        }
    });

    tokio::pin!(shutdown);

    let mut pressed = false;
    let mut pipeline_active = false;
    let (done_tx, mut done_rx) = mpsc::channel::<()>(4);
    let cancel_token = CancellationToken::new();

    loop {
        tokio::select! {
            _ = shutdown.as_mut() => {
                if pipeline_active {
                    let _ = send_control_command(
                        ControlCommand::CancelPipeline { name: binding.name.clone() },
                        &config
                    )
                    .await;
                }

                info!(pipeline = %binding.name, "Push-to-talk watcher shutting down");
                break;
            }
            Some(_) = done_rx.recv() => {
                pipeline_active = false;
            }
            maybe_event = event_rx.recv() => {
                match maybe_event {
                    Some(HotkeyEvent::Pressed) => {
                        info!(pipeline = %binding.name, hotkey = %hotkey_label, "PTT pressed");
                        if pressed || pipeline_active {
                            continue;
                        }
                        pressed = true;
                        pipeline_active = true;

                        let name = binding.name.clone();
                        let cfg_for_run = config.clone();
                        let done = done_tx.clone();
                        let run_cancel = cancel_token.child_token();
                        tokio::spawn(async move {
                            match send_control_command(
                                ControlCommand::RunPipeline { name: name.clone() },
                                &cfg_for_run
                            ).await {
                                Ok(ControlResponse::PipelineFinished { text, .. }) => {
                                    emit_transcript(&name, text);
                                }
                                Ok(ControlResponse::PipelineCancelled { .. }) => {}
                                Ok(ControlResponse::Error { message }) => {
                                    warn!("Push-to-talk pipeline '{}' failed: {}", name, message);
                                }
                                Ok(other) => {
                                    warn!("Unexpected push-to-talk response for '{}': {:?}", name, other);
                                }
                                Err(err) => {
                                    warn!("Failed to start push-to-talk pipeline '{}': {}", name, err);
                                }
                            }
                            let _ = done.send(()).await;
                            run_cancel.cancel();
                        });
                    }
                    Some(HotkeyEvent::Released) => {
                        info!(pipeline = %binding.name, hotkey = %hotkey_label, "PTT released");
                        if !pressed {
                            continue;
                        }
                        pressed = false;

                        if pipeline_active {
                            let mut attempts = 0;
                            let mut cancelled = false;
                            // Try local in-process cancel first to avoid control-server contention.
                            if stop_capture_by_name(&binding.name).await {
                                cancelled = true;
                            }
                            if !cancelled {
                            while attempts < 6 {
                                match send_control_command(
                                    ControlCommand::CancelPipeline { name: binding.name.clone() },
                                    &config
                                ).await {
                                    Ok(ControlResponse::PipelineCancelled { .. }) => {
                                        cancelled = true;
                                        break;
                                    }
                                    Ok(ControlResponse::PipelineFinished { text, .. }) => {
                                        emit_transcript(&binding.name, text);
                                        cancelled = true;
                                        break;
                                    }
                                    Ok(ControlResponse::Error { message }) => {
                                        if message.contains("not running") && attempts < 5 {
                                            attempts += 1;
                                            time::sleep(Duration::from_millis(30)).await;
                                            continue;
                                        }
                                        warn!(
                                            "Failed to cancel push-to-talk pipeline '{}': {}",
                                            binding.name, message
                                        );
                                        break;
                                    }
                                    Ok(other) => {
                                        warn!(
                                            "Unexpected cancellation response for '{}': {:?}",
                                            binding.name, other
                                        );
                                        break;
                                    }
                                    Err(err) => {
                                        warn!(
                                            "Push-to-talk cancellation request for '{}' failed: {}",
                                            binding.name, err
                                        );
                                        break;
                                    }
                                }
                            }
                            }

                            if cancelled {
                                pipeline_active = false;
                            }
                            // Ensure pipeline tokens are cleared if the control path was used.
                            if !cancelled {
                                abort_pipeline_by_name(&binding.name).await;
                            }
                        }
                        cancel_token.cancel();
                    }
                    None => {
                        warn!("Push-to-talk event stream ended for hotkey {}", hotkey_label);
                        break;
                    }
                }
            }
        }
    }

    Ok(())
}

fn emit_transcript(pipeline: &str, text: Option<String>) {
    let Some(text) = text else {
        return;
    };

    let trimmed = text.trim();
    if trimmed.is_empty() {
        return;
    }

    info!(
        pipeline = %pipeline,
        transcript = %trimmed,
        "Push-to-talk pipeline completed"
    );
}
