type event =
  | Capacity_reached
  | Clipboard_only
  | Attempt_failed of { retry_exhausted : bool; audio_kept : bool }
  | Config_rejected
  | Provider_outage of { retry_exhausted : bool }
  | Capture_ignored of { held_ms : int }
  | Low_input
  | Dead_input
  | No_transcript
  | Delivery_failed
  | Correction_skipped

type decision = Notify | Log

let decide = function
  | Capacity_reached | Clipboard_only | Config_rejected -> Notify
  | Attempt_failed { retry_exhausted = true; audio_kept = true }
  | Provider_outage { retry_exhausted = true } ->
      Notify
  | Capture_ignored { held_ms } when held_ms > 3000 -> Notify
  | Attempt_failed _ | Provider_outage _ | Capture_ignored _ | Low_input
  | Dead_input | No_transcript | Delivery_failed | Correction_skipped ->
      Log
