(** The complete desktop notification allowlist. No effects or timing state. *)
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

val decide : event -> decision
(** Ignored captures notify strictly after 3000 ms held, never during capture.
    Failed attempts notify only after retry and successful audio publication. *)
