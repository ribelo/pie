# PIE - Personal Intelligence Engine

PIE is a voice-activated personal AI assistant for Linux with PulseAudio/PipeWire integration, providing intelligent command execution through natural language understanding.

## Features

- **Voice Activation**: Wake word detection using Picovoice Porcupine
- **System Audio Recording**: Capture what's playing on your computer from monitor devices
- **Microphone Recording**: Capture voice input from microphone devices  
- **Device Discovery**: List and select from available audio sources
- **Flexible Output**: Custom filenames and recording durations
- **High Quality**: 48kHz/32-bit float WAV output format
- **Signal Handling**: Graceful stop with Ctrl+C

## Installation

### Development Environment
This project uses Nix for reproducible builds:

```bash
nix develop
# or
nix-shell
```

### Building from Source
```bash
cargo build --release
```

## Usage

### List Available Audio Devices
```bash
pie list
```

### Record System Audio
```bash
# Record system audio only
pie record --system-only

# Record from specific system device
pie record --system-only --system-device hdmi-stereo

# Record for specific duration
pie record --system-only --duration 30 --output system_audio.wav
```

### Record Microphone
```bash
# Record microphone only
pie record --mic-only

# Record microphone with custom filename
pie record --mic-only --output my_voice.wav
```

### Continuous Recording
```bash
# Record until Ctrl+C
pie record

# Stop recording gracefully with Ctrl+C
```

### Start Voice Daemon
```bash
# Start daemon in background with default 3-second voice capture
pie daemon

# Start daemon in foreground with custom voice capture duration
pie daemon --foreground --listen-duration 5

# Start daemon with custom wake word model and keyword
pie daemon --wake-model /path/to/custom_model.pv --wake-keyword /path/to/custom_keyword.ppn --wake-sensitivity 0.7

# Stop the daemon
pie stop
```

### Check Daemon Status
```bash
# Show detailed daemon status including audio/wake word capabilities
pie status
```

Example output:
```
PIE Daemon Status
==================
Status: Not running
Audio: Disabled
Wake Word: Stub mode (no detection)
Recording: None
```

## Command Reference

```
pie <COMMAND>

Commands:
  list      List available audio devices
  record    Start audio recording
  daemon    Start PIE daemon for voice activation
  stop      Stop the PIE daemon
  status    Check PIE daemon status

pie record [OPTIONS]

Options:
  -d, --duration <SECONDS>      Recording duration in seconds
  -o, --output <FILE>          Output filename [default: recording.wav]
      --mic-only               Record microphone only
      --system-only            Record system audio only
      --system-device <NAME>   Specify system audio device name

pie daemon [OPTIONS]

Options:
      --foreground             Run in foreground (don't detach)
      --listen-duration <SECS> Voice capture duration after wake word (default: 3)
      --wake-model <PATH>      Path to Porcupine wake word model file
      --wake-keyword <PATH>    Path to Porcupine wake word keyword file
      --wake-sensitivity <VAL> Wake word detection sensitivity (0.0-1.0, default: 0.5)
```

## Architecture

PIE follows BDD (Behavior-Driven Development) practices:

- **Features**: Gherkin scenarios in `features/`
- **Specifications**: Technical specs in `specs/`  
- **Tests**: Integration tests in `tests/`
- **Implementation**: Source code in `src/`

### Key Dependencies

- **Wake Word Detection**: `porcupine-sys` (optional, Picovoice Porcupine)
- **PulseAudio Integration**: `libpulse-binding`, `libpulse-simple-binding`
- **Audio File Output**: `hound` for WAV files  
- **CLI Interface**: `clap` with derive API
- **Error Handling**: `anyhow` for context-rich errors
- **Signal Handling**: `ctrlc` for graceful interruption

## Development Workflow

This project follows spec-driven development:

1. **Plan with specs**: Review and update the root `SPEC` and `TODO` files to capture planned work
2. **Write tests**: Create BDD scenarios and unit tests  
3. **Implement**: Write code to make tests pass
4. **Validate**: Ensure `cargo fmt`, `cargo clippy -- -D warnings`, and `cargo test` pass; confirm SPEC/TODO entries reflect the delivered change set

### Running Tests
```bash
# Unit and integration tests
cargo test

# Check code quality  
cargo clippy -- -D warnings
cargo fmt --check
```

## Wake Word Configuration

PIE supports voice activation using Picovoice Porcupine for low-latency wake word detection. To enable real wake word detection:

1. **Install Porcupine Libraries**:
    ```bash
    # On Linux x86_64
    git clone https://github.com/Picovoice/Porcupine
    sudo cp lib/linux/x86_64/* /usr/local/lib/
    ```

2. **Set Environment Variables** (optional, can be overridden by CLI):
    ```bash
    export PICOVOICE_ACCESS_KEY="your_access_key_here"
    export PICOVOICE_MODEL_PATH="/usr/local/lib/porcupine/lib/common/porcupine_params.pv"
    export PICOVOICE_KEYWORD_PATH="/usr/local/lib/porcupine/resources/keyword_files/linux/computer_linux.ppn"
    ```

3. **Build with Wake Word Support**:
    ```bash
    cargo build --features porcupine,audio
    ```

4. **Start the Daemon**:
    ```bash
    # Default configuration (uses environment variables or defaults)
    pie daemon

    # Custom voice capture duration
    pie daemon --listen-duration 5

    # Custom wake word model and keyword files
    pie daemon --wake-model /path/to/custom_model.pv --wake-keyword /path/to/custom_keyword.ppn

    # Adjust wake word sensitivity (0.0-1.0, higher = more sensitive)
    pie daemon --wake-sensitivity 0.7
    ```

### Custom Wake Word Models

To use custom Porcupine models and keywords:

1. **Obtain Custom Models**: Use Picovoice Console to create custom wake words or download pre-built models
2. **Place Files**: Store model (`.pv`) and keyword (`.ppn`) files in accessible locations
3. **Configure PIE**: Use CLI flags or environment variables to specify paths:
   ```bash
   # CLI flags (recommended for custom deployments)
   pie daemon --wake-model /opt/pie/models/my_model.pv --wake-keyword /opt/pie/keywords/my_wake_word.ppn

   # Environment variables (fallback)
   export PICOVOICE_MODEL_PATH="/opt/pie/models/my_model.pv"
   export PICOVOICE_KEYWORD_PATH="/opt/pie/keywords/my_wake_word.ppn"
   pie daemon
   ```

When enabled, PIE continuously monitors audio input for the configured wake word with <200ms detection latency, then captures microphone audio for command processing. The `--listen-duration` option controls how long PIE listens for commands after wake word detection (default: 3 seconds). Without the `porcupine` feature, PIE runs in stub mode for testing (wake word detection always returns false).

Use `pie status` to verify the active wake word configuration:
```
PIE Daemon Status
==================
Status: Running
Audio: Enabled
Wake Word: Porcupine (real detection)
  Model: /opt/pie/models/my_model.pv
  Keyword: /opt/pie/keywords/my_wake_word.ppn
  Sensitivity: 0.70
Recording: None
```

## Audio System Requirements

- **Linux**: PulseAudio or PipeWire
- **Permissions**: Access to audio system (user in `audio` group)
- **Monitor Devices**: Available for system audio recording

## Contributing

1. Follow TDD/BDD practices
2. Write failing tests first  
3. Implement minimal code to pass tests
4. Refactor without changing behavior
5. Validate specs before committing

## License

[Add license information]
