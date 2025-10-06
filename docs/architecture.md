# PIE Architecture

## Overview
PIE (Personal Intelligence Engine) is an intelligent voice-activated assistant that runs as a background daemon, listening for wake words and executing commands through natural language understanding powered by AI.

## Architecture

### System Architecture
```
┌─────────────────┐
│   User Voice    │
└────────┬────────┘
         │
    ┌────▼────┐
    │ Wake    │ ◄── "Ok PIE"
    │ Word    │
    └────┬────┘
         │ Activated
    ┌────▼────┐
    │   AI    │ ◄── Natural Language
    │ Router  │
    └────┬────┘
         │ Structured Command
    ┌────▼────┐
    │Command  │
    │Executor │
    └────┬────┘
         │
    ┌────▼────┐
    │ System  │ ◄── User Feedback
    │ Notif.  │
    └─────────┘
```

### Core Components

#### 1. Background Daemon
- Runs continuously in the background
- Manages wake word detection thread
- Coordinates command execution
- Handles system signals gracefully

#### 2. Wake Word Detector
- Uses Porcupine engine for hot word detection
- Listens for "Ok PIE" trigger phrase
- Activates command listening mode
- Returns to passive listening after command execution

#### 3. AI Command Router
- Uses ai-ox Agent for natural language processing
- Converts spoken commands to structured actions
- Returns typed command enums for execution
- Handles ambiguous or unknown commands

#### 4. Command Executor
- Executes parsed commands asynchronously
- Manages recording sessions
- Controls system operations
- Provides status updates

#### 5. Notification System
- Sends desktop notifications for user feedback
- Indicates listening state
- Confirms command execution
- Reports errors gracefully

## Command Flow

1. **Passive State**: Daemon listens for wake word
2. **Activation**: Wake word detected → "PIE is listening..." notification
3. **Command Capture**: Record user's voice command
4. **AI Processing**: Send audio/text to AI router
5. **Command Parsing**: AI returns structured command
6. **Execution**: Execute the parsed command
7. **Feedback**: Send notification about action taken
8. **Return**: Go back to passive listening state

## Supported Commands

### Voice Commands (Natural Language)
- "Ok PIE, let's record" → Start recording
- "Ok PIE, stop recording" → Stop all recordings
- "Ok PIE, save the recording as meeting notes" → Save with custom name
- "Ok PIE, list devices" → Show available audio devices
- "Ok PIE, status" → Get current status

### CLI Commands
```bash
pie daemon              # Start background service
pie stop               # Stop daemon
pie status             # Check daemon status
pie record [OPTIONS]   # Manual recording (existing)
pie list              # List devices (existing)
pie config            # Configure settings
```

## Configuration

### Config File Location
- Linux: `~/.config/pie/config.toml`
- macOS: `~/Library/Application Support/pie/config.toml`

### Configuration Options
```toml
[wake_word]
sensitivity = 0.5           # 0.0 to 1.0
keyword_path = ""          # Custom wake word file

[ai]
provider = "anthropic"     # AI provider for routing
model = "claude-3-haiku"   # Model to use
api_key_env = "ANTHROPIC_API_KEY"

[notifications]
enabled = true
timeout = 5000            # milliseconds

[recording]
default_format = "wav"
default_duration = null   # null for continuous
output_directory = "~/Documents/pie-recordings"
```

## Error Handling

### Graceful Degradation
- If AI is unavailable: Fall back to basic commands
- If notifications fail: Log to stdout/file
- If wake word fails: Provide CLI fallback
- If audio system fails: Clear error messages

### Recovery Strategies
- Automatic reconnection to services
- Command retry with exponential backoff
- State persistence across restarts
- Clean shutdown on system signals

## Security Considerations

- API keys stored in environment variables or secure keyring
- No recording without explicit activation
- Clear indication when recording is active
- Automatic timeout for listening mode
- No network requests without user commands

## Performance Requirements

- Wake word detection latency: < 500ms
- Command processing time: < 2s
- Memory usage: < 100MB idle, < 200MB active
- CPU usage: < 5% idle, < 25% active
- Startup time: < 1s