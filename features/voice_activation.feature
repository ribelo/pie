Feature: Voice Activation
  As a user of PIE
  I want to control the assistant with voice commands
  So that I can use it hands-free

  Background:
    Given the pie daemon is running
    And the wake word detection is active
    And the AI router is configured

  Scenario: Activate with wake word
    When I say "Ok PIE"
    Then pie should send a notification "PIE is listening..."
    And pie should start listening for commands
    And the listening timeout should be 5 seconds

  Scenario: Start recording with voice command
    Given pie is in listening mode
    When I say "let's record"
    Then pie should start audio recording
    And pie should send a notification "Recording started"
    And the recording should continue until stopped

  Scenario: Stop recording with voice command
    Given pie is recording audio
    When I say "Ok PIE, stop recording"
    Then pie should stop the current recording
    And pie should save the audio file
    And pie should send a notification "Recording saved as recording_[timestamp].wav"

  Scenario: Save recording with custom name
    Given pie is in listening mode
    When I say "save the recording as meeting notes"
    Then pie should save the recording as "meeting_notes.wav"
    And pie should send a notification "Recording saved as meeting_notes.wav"

  Scenario: List devices with voice command
    Given pie is in listening mode
    When I say "list devices"
    Then pie should send a notification with available audio devices
    And the notification should include monitor devices
    And the notification should include input devices

  Scenario: Get status with voice command
    Given pie is in listening mode
    When I say "what's your status"
    Then pie should send a notification with current status
    And the status should include daemon state
    And the status should include active recordings

  Scenario: Handle ambiguous commands
    Given pie is in listening mode
    When I say something ambiguous like "do something"
    Then pie should send a notification "I didn't understand that command"
    And pie should return to passive listening mode

  Scenario: Timeout during listening mode
    Given pie is in listening mode
    When 5 seconds pass without any command
    Then pie should send a notification "Listening timeout"
    And pie should return to passive listening mode

  Scenario: Wake word sensitivity
    Given the wake word sensitivity is set to 0.5
    When I say "Ok PIE" quietly
    Then pie should still activate
    And start listening for commands

  Scenario: Ignore similar phrases
    When I say "Okay pie chart"
    Then pie should not activate
    And should remain in passive listening mode