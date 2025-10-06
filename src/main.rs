use anyhow::{anyhow, Context, Result};
use clap::{ArgAction, Parser, Subcommand};
#[cfg(feature = "audio")]
use libpulse_binding as pulse;
#[cfg(feature = "audio")]
use libpulse_simple_binding as psimple;
#[cfg(feature = "audio")]
use psimple::Simple;
#[cfg(feature = "audio")]
use pulse::context::{Context as PulseContext, FlagSet as ContextFlagSet};
#[cfg(feature = "audio")]
use pulse::mainloop::standard::{IterateResult, Mainloop};
#[cfg(feature = "audio")]
use pulse::operation::State as OpState;
#[cfg(feature = "audio")]
use pulse::sample::{Format, Spec};
#[cfg(feature = "audio")]
use pulse::stream::Direction;
#[cfg(feature = "audio")]
use std::cell::RefCell;
#[cfg(feature = "audio")]
use std::rc::Rc;
#[cfg(feature = "audio")]
use std::sync::atomic::{AtomicBool, Ordering};
#[cfg(unix)]
use tokio::io::{AsyncReadExt, AsyncWriteExt};
#[cfg(unix)]
use tokio::net::UnixStream;

mod ai_router;
mod command_executor;
mod commands;
mod daemon;
mod ipc;
mod keyboard;
mod notification;
mod push_to_talk;
mod status;
mod wake_word;

use daemon::{DaemonConfig, PieDaemon};
use ipc::{ControlCommand, ControlResponse};

#[derive(Parser)]
#[command(name = "pie")]
#[command(about = "PIE - Personal Intelligence Engine: Voice-activated AI assistant")]
struct Cli {
    /// Increase log verbosity (-v for debug, -vv for trace)
    #[arg(short, long, global = true, action = ArgAction::Count)]
    verbose: u8,

    #[arg(
        long = "tail",
        global = true,
        help = "Seconds of audio to keep recording after releasing push-to-talk (default respects PIE_PTT_TAIL_SECS or 1.0)"
    )]
    push_to_talk_tail: Option<f32>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// List available audio devices
    List,

    /// Start audio recording
    Record {
        #[arg(
            short,
            long,
            help = "Recording duration in seconds (omit for continuous recording until Ctrl+C)"
        )]
        duration: Option<u64>,

        #[arg(short, long, default_value = "recording.wav")]
        output: String,

        #[arg(long)]
        mic_only: bool,

        #[arg(long)]
        system_only: bool,

        #[arg(long, help = "Specify system audio device name")]
        system_device: Option<String>,
    },

    /// Start PIE daemon for voice activation
    Daemon {
        #[arg(long, help = "Run in foreground (don't detach)")]
        foreground: bool,

        #[arg(
            long,
            help = "Voice capture duration in seconds after wake word (default: 3)"
        )]
        listen_duration: Option<u32>,

        #[arg(long, help = "Path to Porcupine wake word model file")]
        wake_model: Option<String>,

        #[arg(long, help = "Path to Porcupine wake word keyword file")]
        wake_keyword: Option<String>,

        #[arg(long, help = "Wake word detection sensitivity (0.0-1.0, default: 0.5)")]
        wake_sensitivity: Option<f32>,
    },

    /// Start PIE daemon in the foreground (loads command pipelines)
    Server,

    /// Stop the PIE daemon
    Stop,

    /// Check PIE daemon status
    Status,

    /// Toggle on-device listening capture via the daemon
    Listen,

    /// Execute a named command pipeline via the daemon
    Run {
        #[arg(help = "Pipeline name to execute")]
        name: String,
    },

    /// Configure PIE settings
    Config {
        #[arg(long, help = "Set wake word sensitivity (0.0-1.0)")]
        sensitivity: Option<f32>,

        #[arg(long, help = "Enable/disable notifications")]
        notifications: Option<bool>,
    },
}

#[cfg(unix)]
pub(crate) async fn send_control_command(
    command: ControlCommand,
    config: &DaemonConfig,
) -> Result<ControlResponse> {
    let socket_path = &config.control_socket;
    let mut stream = UnixStream::connect(socket_path)
        .await
        .context("Failed to connect to PIE daemon control socket")?;

    let payload = serde_json::to_vec(&command)?;
    stream
        .write_all(&payload)
        .await
        .context("Failed to write command to daemon")?;
    stream
        .shutdown()
        .await
        .context("Failed to shutdown control socket writer")?;

    let mut buffer = Vec::new();
    stream
        .read_to_end(&mut buffer)
        .await
        .context("Failed to read daemon response")?;

    if buffer.is_empty() {
        anyhow::bail!("Daemon returned empty response");
    }

    let response: ControlResponse =
        serde_json::from_slice(&buffer).context("Failed to deserialize daemon response")?;

    Ok(response)
}

#[cfg(not(unix))]
pub(crate) async fn send_control_command(
    _command: ControlCommand,
    _config: &DaemonConfig,
) -> Result<ControlResponse> {
    anyhow::bail!("Control commands are only supported on Unix-like systems");
}

#[cfg(feature = "audio")]
#[derive(Debug, Clone)]
struct AudioSource {
    name: String,
    description: String,
    is_monitor: bool,
}

#[cfg(feature = "audio")]
pub async fn list_devices() -> Result<Vec<String>> {
    let sources = get_audio_sources()?;
    let mut device_list = Vec::new();

    if sources.is_empty() {
        return Err(anyhow!(
            "No audio sources found. Check that PulseAudio/PipeWire is running and accessible."
        ));
    }

    println!("Available audio sources:");
    for source in sources.iter() {
        let source_type = if source.is_monitor {
            "Monitor"
        } else {
            "Input"
        };

        let line = format!(
            "  [{}] {} - {}",
            source_type, source.name, source.description
        );
        println!("{}", line);
        device_list.push(line);
    }

    Ok(device_list)
}

#[cfg(not(feature = "audio"))]
pub async fn list_devices() -> Result<Vec<String>> {
    Err(anyhow!(
        "Audio support not enabled. Enable the 'audio' feature to list devices."
    ))
}

#[cfg(feature = "audio")]
fn get_audio_sources() -> Result<Vec<AudioSource>> {
    let mut mainloop = Mainloop::new().context("Failed to create mainloop")?;
    let mut context =
        PulseContext::new(&mainloop, "audio_recorder_list").context("Failed to create context")?;

    context
        .connect(None, ContextFlagSet::NOFLAGS, None)
        .context("Failed to connect to PulseAudio")?;

    // Wait for connection
    loop {
        match mainloop.iterate(false) {
            IterateResult::Quit(_) | IterateResult::Err(_) => {
                return Err(anyhow!("Failed to connect to PulseAudio"));
            }
            IterateResult::Success(_) => {}
        }
        match context.get_state() {
            pulse::context::State::Ready => break,
            pulse::context::State::Failed | pulse::context::State::Terminated => {
                return Err(anyhow!("PulseAudio context failed"));
            }
            _ => {}
        }
    }

    let sources = Rc::new(RefCell::new(Vec::<AudioSource>::new()));
    let sources_clone = sources.clone();

    let introspect = context.introspect();
    let op = introspect.get_source_info_list(move |info| {
        if let pulse::callbacks::ListResult::Item(source_info) = info {
            let name = source_info
                .name
                .as_ref()
                .map_or("Unknown".to_string(), |n| n.to_string());
            let description = source_info
                .description
                .as_ref()
                .map_or("No description".to_string(), |d| d.to_string());
            let is_monitor = name.contains("monitor");

            sources_clone.borrow_mut().push(AudioSource {
                name,
                description,
                is_monitor,
            });
        }
    });

    // Wait for operation to complete
    while let OpState::Running = op.get_state() {
        if let IterateResult::Err(_) = mainloop.iterate(false) {
            break;
        }
    }

    let result = sources.borrow().clone();
    context.disconnect();
    Ok(result)
}

#[cfg(feature = "audio")]
pub(crate) fn find_target_device(
    mic_only: bool,
    system_only: bool,
    device_hint: &Option<String>,
) -> Result<Option<String>> {
    let mut mainloop = Mainloop::new().context("Failed to create mainloop")?;
    let mut context =
        PulseContext::new(&mainloop, "audio_recorder_find").context("Failed to create context")?;

    context
        .connect(None, ContextFlagSet::NOFLAGS, None)
        .context("Failed to connect to PulseAudio")?;

    // Wait for connection
    loop {
        match mainloop.iterate(false) {
            IterateResult::Quit(_) | IterateResult::Err(_) => {
                return Err(anyhow!("Failed to connect to PulseAudio"));
            }
            IterateResult::Success(_) => {}
        }
        match context.get_state() {
            pulse::context::State::Ready => break,
            pulse::context::State::Failed | pulse::context::State::Terminated => {
                return Err(anyhow!("PulseAudio context failed"));
            }
            _ => {}
        }
    }

    let sources = Rc::new(RefCell::new(Vec::<AudioSource>::new()));
    let sources_clone = sources.clone();

    let introspect = context.introspect();
    let op = introspect.get_source_info_list(move |info| {
        if let pulse::callbacks::ListResult::Item(source_info) = info {
            let name = source_info
                .name
                .as_ref()
                .map_or("Unknown".to_string(), |n| n.to_string());
            let description = source_info
                .description
                .as_ref()
                .map_or("No description".to_string(), |d| d.to_string());
            let is_monitor = name.contains("monitor");

            sources_clone.borrow_mut().push(AudioSource {
                name,
                description,
                is_monitor,
            });
        }
    });

    while let OpState::Running = op.get_state() {
        if let IterateResult::Err(_) = mainloop.iterate(false) {
            break;
        }
    }

    let sources_vec = sources.borrow().clone();

    let target_device = if system_only {
        let monitors: Vec<_> = sources_vec.iter().filter(|s| s.is_monitor).collect();

        if monitors.is_empty() {
            return Err(anyhow!(
                "No monitor sources found. System audio recording requires monitor sources to be available."
            ));
        }

        if let Some(ref device_name) = device_hint {
            // Specific device requested
            let matching_monitor = monitors.iter().find(|s| s.name.contains(device_name));
            match matching_monitor {
                Some(monitor) => Some(monitor.name.clone()),
                None => {
                    let available_monitors: Vec<_> =
                        monitors.iter().map(|s| s.name.as_str()).collect();
                    return Err(anyhow!(
                        "Monitor device '{}' not found. Available monitor devices: {}",
                        device_name,
                        available_monitors.join(", ")
                    ));
                }
            }
        } else {
            // Any monitor
            monitors.first().map(|s| s.name.clone())
        }
    } else if mic_only {
        let mics: Vec<_> = sources_vec.iter().filter(|s| !s.is_monitor).collect();

        if mics.is_empty() {
            return Err(anyhow!(
                "No microphone sources found. Microphone recording requires input sources to be available."
            ));
        }

        if let Some(ref device_name) = device_hint {
            let matching_mic = mics.iter().find(|s| s.name.contains(device_name));
            match matching_mic {
                Some(mic) => Some(mic.name.clone()),
                None => {
                    let available_mics: Vec<_> = mics.iter().map(|s| s.name.as_str()).collect();
                    return Err(anyhow!(
                        "Microphone device '{}' not found. Available microphone devices: {}",
                        device_name,
                        available_mics.join(", ")
                    ));
                }
            }
        } else {
            mics.first().map(|s| s.name.clone())
        }
    } else {
        // For mixed recording, we'll use default and add monitor separately later
        None
    };

    context.disconnect();
    Ok(target_device)
}

#[cfg(feature = "audio")]
pub async fn record_audio(
    duration: Option<u32>,
    output: String,
    mic_only: bool,
    system_only: bool,
    system_device: Option<String>,
) -> Result<()> {
    let duration = duration.map(|d| d as u64);
    record_sync(duration, output, mic_only, system_only, system_device, true)
}

#[cfg(feature = "audio")]
pub async fn record_audio_without_ctrlc(
    duration: Option<u32>,
    output: String,
    mic_only: bool,
    system_only: bool,
    system_device: Option<String>,
) -> Result<()> {
    let duration = duration.map(|d| d as u64);
    record_sync(
        duration,
        output,
        mic_only,
        system_only,
        system_device,
        false,
    )
}

#[cfg(not(feature = "audio"))]
pub async fn record_audio(
    _duration: Option<u32>,
    _output: String,
    _mic_only: bool,
    _system_only: bool,
    _system_device: Option<String>,
) -> Result<()> {
    Err(anyhow!(
        "Audio support not enabled. Enable the 'audio' feature to record audio."
    ))
}

#[cfg(not(feature = "audio"))]
pub async fn record_audio_without_ctrlc(
    _duration: Option<u32>,
    _output: String,
    _mic_only: bool,
    _system_only: bool,
    _system_device: Option<String>,
) -> Result<()> {
    Err(anyhow!(
        "Audio support not enabled. Enable the 'audio' feature to record audio."
    ))
}

#[cfg(feature = "audio")]
fn record_sync(
    duration: Option<u64>,
    output: String,
    mic_only: bool,
    system_only: bool,
    system_device: Option<String>,
    install_ctrlc: bool,
) -> Result<()> {
    // Set up Ctrl+C handler
    let running = std::sync::Arc::new(AtomicBool::new(true));
    if install_ctrlc {
        let r = running.clone();

        ctrlc::set_handler(move || {
            r.store(false, Ordering::SeqCst);
        })
        .context("Error setting Ctrl-C handler")?;
    }

    // Find the target device
    let target_device = find_target_device(mic_only, system_only, &system_device)?;

    println!("Recording devices:");
    if let Some(ref device_name) = target_device {
        let device_type = if device_name.contains("monitor") {
            "System"
        } else {
            "Input"
        };
        println!("  {}: {}", device_type, device_name);
    } else {
        println!("  Using default source");
    }

    // Create audio spec - matching CD quality
    let spec = Spec {
        format: Format::F32le,
        channels: 2,
        rate: 48000,
    };

    if !spec.is_valid() {
        return Err(anyhow!("Invalid audio specification"));
    }

    // Create simple recording connection
    let simple = Simple::new(
        None,                     // Use default server
        "pie",                    // Application name
        Direction::Record,        // We want to record
        target_device.as_deref(), // Device (None for default)
        "Audio Recording",        // Stream description
        &spec,                    // Sample format
        None,                     // Default channel map
        None,                     // Default buffering
    )
    .context("Failed to create PulseAudio simple connection")?;

    // Start recording
    if let Some(dur) = duration {
        println!(
            "Recording for {} seconds... (Press Ctrl+C to stop early)",
            dur
        );
    } else {
        println!("Recording... Press Ctrl+C to stop");
    }

    let start_time = std::time::Instant::now();
    let mut audio_data = Vec::<f32>::new();

    // Buffer for reading audio (1024 samples * 4 bytes per f32 sample * 2 channels)
    let mut buffer = vec![0u8; 1024 * 4 * 2];

    while running.load(Ordering::SeqCst) {
        if let Some(dur) = duration {
            if start_time.elapsed().as_secs() >= dur {
                break;
            }
        }

        // Read audio data - this blocks until data is available
        match simple.read(&mut buffer) {
            Ok(()) => {
                // Convert bytes to f32 samples
                for chunk in buffer.chunks_exact(4) {
                    let sample = f32::from_le_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
                    audio_data.push(sample);
                }
            }
            Err(e) => {
                eprintln!("Error reading audio data: {}", e);
                break;
            }
        }
    }

    println!("\nStopping recording...");
    drop(simple); // Explicitly close the connection

    // Save to WAV
    let wav_spec = hound::WavSpec {
        channels: 2,
        sample_rate: 48000,
        bits_per_sample: 32,
        sample_format: hound::SampleFormat::Float,
    };

    let mut writer = hound::WavWriter::create(&output, wav_spec)?;

    let sample_count = audio_data.len();
    for sample in audio_data {
        writer.write_sample(sample)?;
    }

    writer.finalize()?;
    println!("Recording saved to {} ({} samples)", output, sample_count);

    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    let Cli {
        verbose,
        push_to_talk_tail,
        command,
    } = Cli::parse();

    let tail_secs = push_to_talk_tail.map(|t| t.max(0.0));

    init_logging(verbose)?;

    match command {
        Commands::List => {
            list_devices().await?;
        }
        Commands::Record {
            duration,
            output,
            mic_only,
            system_only,
            system_device,
        } => {
            if mic_only && system_only {
                anyhow::bail!("Cannot use both --mic-only and --system-only");
            }
            let duration = duration.map(|d| d as u32);
            record_audio(duration, output, mic_only, system_only, system_device).await?;
        }
        Commands::Daemon {
            foreground,
            listen_duration,
            wake_model,
            wake_keyword,
            wake_sensitivity,
        } => {
            let listen_duration_secs = listen_duration.unwrap_or(3);

            // Warn if wake word options provided but porcupine not enabled
            if (wake_model.is_some() || wake_keyword.is_some() || wake_sensitivity.is_some())
                && !cfg!(feature = "porcupine")
            {
                eprintln!("Warning: Wake word configuration options provided but Porcupine feature is not enabled. These options will be ignored.");
            }

            let mut config = DaemonConfig::default();
            config.detach = !foreground;
            config.listen_duration_secs = listen_duration_secs;
            config.wake_model_path = wake_model.map(std::path::PathBuf::from);
            config.wake_keyword_path = wake_keyword.map(std::path::PathBuf::from);
            config.wake_sensitivity = wake_sensitivity.unwrap_or(0.5);
            if let Some(tail) = tail_secs {
                config.push_to_talk_tail_secs = tail;
            }

            let daemon = PieDaemon::new(config)?;
            if foreground {
                daemon.start().await?;
            } else {
                daemon.start_background().await?;
            }
        }
        Commands::Server => {
            let mut config = DaemonConfig::default();
            config.detach = false;
            if let Some(tail) = tail_secs {
                config.push_to_talk_tail_secs = tail;
            }
            let daemon = PieDaemon::new(config)?;
            daemon.start().await?;
        }
        Commands::Stop => {
            let mut config = DaemonConfig::default();
            if let Some(tail) = tail_secs {
                config.push_to_talk_tail_secs = tail;
            }
            let daemon = PieDaemon::new(config)?;

            if daemon.is_running().await? {
                daemon.stop().await?;
                println!("PIE daemon stopped");
            } else {
                println!("PIE daemon is not running");
            }
        }
        Commands::Status => {
            let mut config = DaemonConfig::default();
            if let Some(tail) = tail_secs {
                config.push_to_talk_tail_secs = tail;
            }
            match send_control_command(ControlCommand::GetStatus, &config).await {
                Ok(ControlResponse::Status { status }) => {
                    println!("{}", serde_json::to_string_pretty(&status)?);
                }
                Ok(ControlResponse::Error { message }) => {
                    anyhow::bail!(message);
                }
                Ok(other) => {
                    anyhow::bail!("Unexpected daemon response: {:?}", other);
                }
                Err(_) => {
                    // Fallback to local status if daemon not reachable
                    let mut fallback = DaemonConfig::default();
                    if let Some(tail) = tail_secs {
                        fallback.push_to_talk_tail_secs = tail;
                    }
                    let daemon = PieDaemon::new(fallback)?;
                    let status = daemon.get_status().await?;
                    println!("{}", serde_json::to_string_pretty(&status)?);
                }
            }
        }
        Commands::Listen => {
            let mut config = DaemonConfig::default();
            if let Some(tail) = tail_secs {
                config.push_to_talk_tail_secs = tail;
            }
            match send_control_command(ControlCommand::ToggleListening, &config).await? {
                ControlResponse::ListeningStarted => {
                    println!("Listening session started");
                }
                ControlResponse::ListeningStopped {
                    transcript,
                    duration_seconds,
                } => {
                    println!("Listening session stopped");
                    if let Some(duration) = duration_seconds {
                        if duration > 0.05 {
                            println!("Duration: {:.1}s", duration);
                        }
                    }
                    if let Some(text) = transcript {
                        let trimmed = text.trim();
                        if !trimmed.is_empty() && duration_seconds.unwrap_or_default() > 0.05 {
                            println!("Transcript: {}", trimmed);
                        }
                    }
                }
                ControlResponse::Error { message } => {
                    anyhow::bail!(message);
                }
                other => {
                    anyhow::bail!("Unexpected daemon response: {:?}", other);
                }
            }
        }
        Commands::Run { name } => {
            let mut config = DaemonConfig::default();
            if let Some(tail) = tail_secs {
                config.push_to_talk_tail_secs = tail;
            }
            #[cfg(unix)]
            {
                use tokio::signal;

                let mut run_fut = Box::pin(send_control_command(
                    ControlCommand::RunPipeline { name: name.clone() },
                    &config,
                ));

                let mut ctrl_c = Box::pin(signal::ctrl_c());
                let mut cancellation_requested = false;

                let resp = loop {
                    tokio::select! {
                        res = &mut run_fut => break res?,
                        _ = &mut ctrl_c => {
                            if !cancellation_requested {
                                cancellation_requested = true;
                                eprintln!("Cancelling pipeline '{}'...", name);
                                if let Err(err) = send_control_command(
                                    ControlCommand::CancelPipeline { name: name.clone() },
                                    &config,
                                ).await {
                                    eprintln!("Failed to request cancellation: {}", err);
                                }
                                ctrl_c = Box::pin(signal::ctrl_c());
                            } else {
                                eprintln!("Force exiting pipeline '{}' without waiting for confirmation", name);
                                return Ok(());
                            }
                        }
                    };
                };

                match resp {
                    ControlResponse::PipelineFinished { text, .. } => {
                        println!("Pipeline '{}' completed", name);
                        if let Some(text) = text {
                            println!("Output: {}", text);
                        }
                    }
                    ControlResponse::PipelineCancelled { .. } => {
                        println!("Pipeline '{}' cancelled", name);
                    }
                    ControlResponse::Error { message } => {
                        anyhow::bail!(message);
                    }
                    other => {
                        anyhow::bail!("Unexpected daemon response: {:?}", other);
                    }
                }
            }

            #[cfg(not(unix))]
            {
                match send_control_command(
                    ControlCommand::RunPipeline { name: name.clone() },
                    &config,
                )
                .await?
                {
                    ControlResponse::PipelineFinished { text, .. } => {
                        println!("Pipeline '{}' completed", name);
                        if let Some(text) = text {
                            println!("Output: {}", text);
                        }
                    }
                    ControlResponse::PipelineCancelled { .. } => {
                        println!("Pipeline '{}' cancelled", name);
                    }
                    ControlResponse::Error { message } => {
                        anyhow::bail!(message);
                    }
                    other => {
                        anyhow::bail!("Unexpected daemon response: {:?}", other);
                    }
                }
            }
        }
        Commands::Config {
            sensitivity,
            notifications,
        } => {
            println!("Configuration management coming soon");
            if let Some(s) = sensitivity {
                println!("  Would set sensitivity to: {}", s);
            }
            if let Some(n) = notifications {
                println!(
                    "  Would {} notifications",
                    if n { "enable" } else { "disable" }
                );
            }
        }
    }

    Ok(())
}

fn init_logging(verbosity: u8) -> Result<()> {
    use tracing::level_filters::LevelFilter;
    use tracing_subscriber::EnvFilter;

    let max_level = match verbosity {
        0 => LevelFilter::INFO,
        1 => LevelFilter::DEBUG,
        _ => LevelFilter::TRACE,
    };

    let env_filter = EnvFilter::builder()
        .with_default_directive(max_level.into())
        .from_env_lossy();

    tracing_subscriber::fmt()
        .with_env_filter(env_filter)
        .with_level(true)
        .with_target(true)
        .with_writer(std::io::stderr)
        .try_init()
        .map_err(|err| anyhow!("Failed to initialize logging: {}", err))?;

    Ok(())
}
