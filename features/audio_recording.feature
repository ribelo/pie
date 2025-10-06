Feature: Audio Recording
  As a user of PIE (Personal Intelligence Engine)
  I want to record audio from different sources
  So that I can capture system audio, microphone input, or both

  Background:
    Given the pie audio recorder is installed
    And PulseAudio or PipeWire is running on the system

  Scenario: List available audio devices
    When I run "pie list"
    Then I should see a list of available audio sources
    And I should see monitor devices for system audio
    And I should see input devices for microphone audio

  Scenario: Record system audio only
    Given there are available monitor devices
    When I run "pie record --system-only"
    And system audio is playing
    Then it should start recording from monitor devices
    And it should not record from microphone input
    And it should create a recording.wav file

  Scenario: Record microphone audio only  
    Given there are available input devices
    When I run "pie record --mic-only"
    And I speak into the microphone
    Then it should start recording from microphone input
    And it should not record from monitor devices
    And it should create a recording.wav file

  Scenario: Record with custom output filename
    When I run "pie record --output my_recording.wav"
    Then it should create a file named "my_recording.wav"

  Scenario: Record with specific duration
    When I run "pie record --duration 5"
    Then it should record for exactly 5 seconds
    And then stop recording automatically

  Scenario: Stop recording with Ctrl+C
    Given I start recording with "pie record" 
    When I press Ctrl+C
    Then recording should stop gracefully
    And the audio file should be saved properly

  Scenario: Record with specific system device
    Given there are multiple monitor devices available
    When I run "pie record --system-only --system-device hdmi-stereo"
    Then it should record from the specified HDMI monitor device
    And it should create a recording.wav file

  Scenario: Error handling for invalid options
    When I run "pie record --mic-only --system-only"
    Then it should display an error message
    And it should not start recording
    And it should exit with non-zero status code