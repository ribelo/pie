type state =
  | Idle
  | Capturing of { started_at : float }
  | PostRoll of { started_at : float; release_at : float }

val idle : state
val start : state -> float -> state
val release : state -> float -> state
val is_accepting_chunks : state -> bool
val post_roll_remaining_ms : ?tail_ms:int -> state -> float -> float
val duration_ms : state -> float -> float
