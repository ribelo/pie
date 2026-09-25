type activity =
  | Starting
  | Recording
  | Stopping
  | Processing
  | Delivering
  | Finishing
  | Cancelling

type t =
  | Offline
  | Disabled
  | Ready
  | Active of activity
  | Failed
  | Busy_unknown

let activity_to_string = function
  | Starting -> "starting"
  | Recording -> "recording"
  | Stopping -> "stopping"
  | Processing -> "processing"
  | Delivering -> "delivering"
  | Finishing -> "finishing"
  | Cancelling -> "cancelling"

let to_string = function
  | Offline -> "offline"
  | Disabled -> "disabled"
  | Ready -> "ready"
  | Active activity -> activity_to_string activity
  | Failed -> "failed"
  | Busy_unknown -> "busy"

let of_string = function
  | "offline" -> Some Offline
  | "disabled" -> Some Disabled
  | "ready" -> Some Ready
  | "starting" -> Some (Active Starting)
  | "recording" -> Some (Active Recording)
  | "stopping" -> Some (Active Stopping)
  | "processing" -> Some (Active Processing)
  | "delivering" -> Some (Active Delivering)
  | "finishing" -> Some (Active Finishing)
  | "cancelling" -> Some (Active Cancelling)
  | "failed" -> Some Failed
  | "busy" -> Some Busy_unknown
  | _ -> None

(* Compatibility only: old active=true never establishes microphone capture. *)
let legacy ~active ~enabled ~mode ~last_error =
  if mode = "offline" then Offline
  else if active then Busy_unknown
  else if not enabled then Disabled
  else if Option.is_some last_error then Failed
  else Ready

let can_transition before after =
  before = after
  ||
  match (before, after) with
  | Cancelling, _ -> false
  | _, Cancelling -> true
  | Starting, (Recording | Processing | Delivering | Finishing)
  | Recording, (Stopping | Processing | Finishing)
  | Stopping, (Recording | Processing | Finishing)
  | Processing, (Delivering | Finishing)
  | Delivering, Finishing ->
      true
  | _ -> false
