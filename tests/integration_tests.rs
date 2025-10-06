use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::Once;

use serde_json::Value;

static COMMAND_ENV_INIT: Once = Once::new();

fn ensure_commands_env() {
    COMMAND_ENV_INIT.call_once(|| {
        let path = std::env::temp_dir().join(format!("pie_commands_{}", std::process::id()));
        let _ = fs::create_dir_all(&path);
        std::env::set_var("PIE_COMMANDS_DIR", &path);

        let keyboard_capture = path.join("keyboard_capture.log");
        let _ = fs::remove_file(&keyboard_capture);
        std::env::set_var("PIE_KEYBOARD_CAPTURE_PATH", &keyboard_capture);

        let debug_audio_dir =
            std::env::temp_dir().join(format!("pie_test_audio_{}", std::process::id()));
        let _ = fs::create_dir_all(&debug_audio_dir);
        std::env::set_var("PIE_DEBUG_AUDIO_DIR", &debug_audio_dir);

        let pipeline_path = path.join("transcribe.yml");
        if !pipeline_path.exists() {
            let yaml = r#"name: transcribe
summary: Simple audio transcription pipeline
input:
  kind: audio
  source: listen
  capture:
    mode: timed
    max_duration_secs: 3
    route: microphone
stages:
  - id: stt
    kind: stt
    uses: ai-ox:groq/whisper-large-v3
    after: [input]
output:
  kind: keyboard
  from: stt
"#;
            fs::write(&pipeline_path, yaml).expect("Failed to write transcribe pipeline");
        }
    });
}

#[test]
fn test_pie_list_command() {
    ensure_commands_env();
    let output = Command::new("cargo")
        .args(["run", "--", "list"])
        .output()
        .expect("Failed to execute pie list command");

    // Should not crash
    assert!(
        output.status.success() || output.status.code() == Some(1),
        "pie list should either succeed or fail gracefully, got: {:?}",
        output.status.code()
    );

    // Should produce some output
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stdout.is_empty() || !stderr.is_empty(),
        "pie list should produce output"
    );
}

#[test]
fn test_pie_record_help() {
    ensure_commands_env();
    let output = Command::new("cargo")
        .args(["run", "--", "record", "--help"])
        .output()
        .expect("Failed to execute pie record --help");

    assert!(output.status.success(), "pie record --help should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("duration"),
        "Help should mention duration option"
    );
    assert!(
        stdout.contains("output"),
        "Help should mention output option"
    );
    assert!(
        stdout.contains("mic-only"),
        "Help should mention mic-only option"
    );
    assert!(
        stdout.contains("system-only"),
        "Help should mention system-only option"
    );
}

#[test]
fn test_invalid_option_combination() {
    ensure_commands_env();
    let output = Command::new("cargo")
        .args(["run", "--", "record", "--mic-only", "--system-only"])
        .output()
        .expect("Failed to execute command");

    // Should fail with error code
    assert!(
        !output.status.success(),
        "Invalid option combination should fail"
    );

    // Should produce error message
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Cannot use both")
            || stderr.contains("mic-only") && stderr.contains("system-only"),
        "Should explain the invalid combination"
    );
}

#[test]
fn test_short_duration_recording() {
    ensure_commands_env();
    // Skip if no audio system available (CI environments)
    if !audio_system_available() {
        return;
    }

    let output_file = "test_recording.wav";

    // Clean up any existing file
    if Path::new(output_file).exists() {
        std::fs::remove_file(output_file).ok();
    }

    let output = Command::new("cargo")
        .args([
            "run",
            "--",
            "record",
            "--duration",
            "1",
            "--output",
            output_file,
        ])
        .output()
        .expect("Failed to execute recording command");

    // Should complete successfully or with known audio errors
    if output.status.success() {
        // If recording succeeded, file should exist
        assert!(
            Path::new(output_file).exists(),
            "Recording file should be created"
        );

        // File should have reasonable size (not empty)
        let metadata =
            std::fs::metadata(output_file).expect("Should be able to read file metadata");
        assert!(metadata.len() > 1000, "Recording file should not be empty");

        // Clean up
        std::fs::remove_file(output_file).ok();
    } else {
        // If failed, should be due to audio system issues, not code bugs
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("PulseAudio")
                || stderr.contains("audio")
                || stderr.contains("connection")
                || stderr.contains("device"),
            "Failure should be audio-system related, got: {}",
            stderr
        );
    }
}

#[test]
fn test_list_devices_error_when_no_sources() {
    ensure_commands_env();
    // This test verifies that `pie list` fails appropriately when no audio sources are available
    // In a real environment with no PulseAudio/PipeWire, this should fail
    // In CI environments, we might not have audio, so we check the error message

    let output = Command::new("cargo")
        .args(["run", "--", "list"])
        .output()
        .expect("Failed to execute pie list command");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    if !output.status.success() {
        // If it failed, check that it's due to no audio sources (not a crash)
        assert!(
            stderr.contains("No audio sources found")
                || stderr.contains("PulseAudio")
                || stderr.contains("audio")
                || stderr.contains("connection"),
            "Failure should be due to missing audio sources or system issues, got stderr: '{}', stdout: '{}'",
            stderr,
            stdout
        );
    } else {
        // If it succeeded, there should be some audio sources listed
        assert!(
            stdout.contains("Available audio sources:"),
            "Success should show audio sources, got stdout: '{}'",
            stdout
        );
    }
}

#[test]
fn test_record_system_only_invalid_device() {
    ensure_commands_env();
    // Test that specifying an invalid system device fails with helpful error
    let output = Command::new("cargo")
        .args([
            "run",
            "--",
            "record",
            "--system-only",
            "--system-device",
            "nonexistent_monitor_device_12345",
            "--duration",
            "1",
            "--output",
            "/tmp/test_invalid_device.wav",
        ])
        .output()
        .expect("Failed to execute record command with invalid system device");

    // Should fail
    assert!(
        !output.status.success(),
        "Recording with invalid system device should fail"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Should mention the invalid device and available options
    let invalid_device_reported = stderr.contains("nonexistent_monitor_device_12345")
        && (stderr.contains("not found") || stderr.contains("Available monitor"));
    let audio_disabled = stderr.contains("Audio support not enabled");

    assert!(
        invalid_device_reported || audio_disabled,
        "Error should mention the invalid device or indicate audio is disabled, got: {}",
        stderr
    );
}

#[test]
fn test_record_mic_only_no_microphones() {
    ensure_commands_env();
    // Test that --mic-only fails when no microphones are available
    // This might pass in environments with mics, so we check the error message
    let output = Command::new("cargo")
        .args([
            "run",
            "--",
            "record",
            "--mic-only",
            "--duration",
            "1",
            "--output",
            "/tmp/test_no_mic.wav",
        ])
        .output()
        .expect("Failed to execute record command with mic-only");

    let stderr = String::from_utf8_lossy(&output.stderr);

    if !output.status.success() {
        // If it failed, check it's due to no microphones
        assert!(
            stderr.contains("No microphone sources found")
                || stderr.contains("microphone")
                || stderr.contains("input")
                || stderr.contains("PulseAudio")
                || stderr.contains("audio"),
            "Failure should be due to missing microphones or audio system, got: {}",
            stderr
        );
    }
    // If it succeeds, that's fine - the environment has microphones
}

#[test]
fn test_record_system_only_no_monitors() {
    ensure_commands_env();
    // Test that --system-only fails when no monitor sources are available
    let output = Command::new("cargo")
        .args([
            "run",
            "--",
            "record",
            "--system-only",
            "--duration",
            "1",
            "--output",
            "/tmp/test_no_monitor.wav",
        ])
        .output()
        .expect("Failed to execute record command with system-only");

    let stderr = String::from_utf8_lossy(&output.stderr);

    if !output.status.success() {
        // If it failed, check it's due to no monitors
        assert!(
            stderr.contains("No monitor sources found")
                || stderr.contains("monitor")
                || stderr.contains("system")
                || stderr.contains("PulseAudio")
                || stderr.contains("audio"),
            "Failure should be due to missing monitor sources or audio system, got: {}",
            stderr
        );
    }
    // If it succeeds, that's fine - the environment has monitors
}

#[test]
fn test_pie_status_command() {
    ensure_commands_env();
    let output = Command::new("cargo")
        .args(["run", "--", "status"])
        .output()
        .expect("Failed to execute pie status command");

    assert!(output.status.success(), "pie status should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    let status: Value = serde_json::from_str(&stdout).expect("status output should be valid JSON");

    assert!(
        status.get("running").is_some(),
        "status should include running flag"
    );
    assert_eq!(status["audio_enabled"], Value::Bool(false));
    assert_eq!(status["wake_word_mode"], Value::String("stub".to_string()));
    assert!(
        status["recording"].is_null(),
        "recording should be null when idle"
    );
    assert!(
        status["listening"].is_object(),
        "listening status should be present"
    );
}

#[test]
fn test_pie_run_without_server() {
    ensure_commands_env();
    let output = Command::new("cargo")
        .args(["run", "--", "run", "transcribe"])
        .output()
        .expect("Failed to execute pie run command");

    assert!(
        !output.status.success(),
        "pie run should fail when daemon is not running"
    );

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Failed to connect")
            || stderr.contains("control socket")
            || stderr.contains("daemon"),
        "Error output should mention daemon connectivity, got: {}",
        stderr
    );
}

#[test]
fn test_daemon_listen_duration_option() {
    ensure_commands_env();
    let output = Command::new("cargo")
        .args(["run", "--", "daemon", "--help"])
        .output()
        .expect("Failed to execute pie daemon --help");

    assert!(output.status.success(), "pie daemon --help should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("listen-duration"),
        "Help should mention listen-duration option"
    );
}

#[test]
fn test_daemon_wake_word_options_help() {
    ensure_commands_env();
    let output = Command::new("cargo")
        .args(["run", "--", "daemon", "--help"])
        .output()
        .expect("Failed to execute pie daemon --help");

    assert!(output.status.success(), "pie daemon --help should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("wake-model"),
        "Help should mention wake-model option"
    );
    assert!(
        stdout.contains("wake-keyword"),
        "Help should mention wake-keyword option"
    );
    assert!(
        stdout.contains("wake-sensitivity"),
        "Help should mention wake-sensitivity option"
    );
}

fn audio_system_available() -> bool {
    // Try to check if pulseaudio/pipewire is running
    Command::new("pulseaudio")
        .args(["--check"])
        .output()
        .map(|output| output.status.success())
        .unwrap_or(false)
        || Command::new("pipewire")
            .args(["--version"])
            .output()
            .map(|output| output.status.success())
            .unwrap_or(false)
}

#[cfg(test)]
mod unit_tests {
    // Unit tests would go here, testing individual functions
    // For now, the main.rs file would need to be refactored to expose
    // testable functions

    // TODO: Add unit test for DaemonStatus in stub mode once we have a lib.rs
}
