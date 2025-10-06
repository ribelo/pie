Feature: Daemon Service
  As a user of PIE
  I want to run pie as a background service
  So that it's always available for voice commands

  Background:
    Given pie is installed on the system
    And the configuration file exists

  Scenario: Start daemon service
    When I run "pie daemon"
    Then the pie daemon should start in the background
    And it should send a notification "PIE daemon started"
    And it should create a PID file
    And it should start listening for wake words

  Scenario: Check daemon status
    Given the pie daemon is running
    When I run "pie status"
    Then I should see "PIE daemon is running"
    And I should see the process ID
    And I should see uptime information
    And I should see memory usage

  Scenario: Stop daemon service
    Given the pie daemon is running
    When I run "pie stop"
    Then the daemon should stop gracefully
    And it should send a notification "PIE daemon stopped"
    And it should remove the PID file
    And all recordings should be saved

  Scenario: Daemon already running
    Given the pie daemon is running
    When I run "pie daemon" again
    Then it should display "PIE daemon is already running"
    And it should exit with status code 1
    And it should not start a second daemon

  Scenario: Stop when daemon not running
    Given the pie daemon is not running
    When I run "pie stop"
    Then it should display "PIE daemon is not running"
    And it should exit with status code 0

  Scenario: Daemon survives audio system restart
    Given the pie daemon is running
    When PulseAudio/PipeWire restarts
    Then the daemon should reconnect automatically
    And it should send a notification "Audio system reconnected"
    And it should continue working normally

  Scenario: Daemon handles system signals
    Given the pie daemon is running
    When the system sends SIGTERM
    Then the daemon should stop gracefully
    And it should save any active recordings
    And it should clean up resources

  Scenario: Daemon auto-restart on crash
    Given the pie daemon crashes unexpectedly
    When systemd restarts the service
    Then pie should start successfully
    And it should send a notification "PIE daemon restarted after crash"
    And it should restore previous state

  Scenario: Daemon resource limits
    Given the pie daemon is running
    When memory usage exceeds 200MB
    Then pie should log a warning
    And it should attempt to free unused memory
    And it should continue operating

  Scenario: Daemon logs
    Given the pie daemon is running
    When voice commands are processed
    Then logs should be written to ~/.local/share/pie/daemon.log
    And logs should include timestamps
    And logs should rotate when exceeding 10MB