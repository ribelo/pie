let post_roll_ms = 2000

type state =
  | Idle
  | Capturing of { started_at : float }
  | PostRoll of { started_at : float; release_at : float }

let idle = Idle

let start state now =
  match state with
  | Capturing _ -> state
  | PostRoll { started_at; _ } -> Capturing { started_at }
  | Idle -> Capturing { started_at = now }

let release state now =
  match state with
  | Capturing { started_at } -> PostRoll { started_at; release_at = now }
  | _ -> state

let is_accepting_chunks = function
  | Idle -> false
  | Capturing _ | PostRoll _ -> true

let post_roll_remaining_ms ?(tail_ms = post_roll_ms) state now =
  match state with
  | PostRoll { release_at; _ } ->
      max 0.0 (release_at +. float_of_int tail_ms -. now)
  | _ -> 0.0

let duration_ms state now =
  match state with
  | Idle -> 0.0
  | Capturing { started_at } | PostRoll { started_at; _ } -> now -. started_at
