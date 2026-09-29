type trigger = Press of string | Release of string
type captured_clip = { duration_ms : int; pcm_bytes : bytes }

type error =
  [ `Ptt_error of string | `Ptt_cause of [ `Ptt_error of string ] Ox.Cause.t ]

type run_error = [ error | `Pulse_error of Pulse.Client.error ]

type finish =
  | Buffered of (captured_clip -> (unit, error) result)
  | Streaming of (duration_ms:int -> (unit, error) result)

type handle = {
  offer : bytes -> (unit, error) result;
  finish : finish;
  cancel : unit -> (unit, error) result;
}

type press_result =
  | Handle of handle
  | Scoped of ((handle -> (unit, error) result) -> (unit, error) result)
  | Skip

type config = {
  recognize : Keyboard.Monitor.key_event -> trigger option;
  min_duration_ms : int;
  capture_thresholds : unit -> float * float;
  log_prefix : string -> string;
  source : string option;
  on_ready : unit -> (unit, error) result;
  on_press : string -> (press_result, error) result;
  on_release : string -> (unit, error) result;
  on_abort : unit -> (bool, error) result;
  on_dead_input : unit -> (unit, error) result;
  admission_active : unit -> bool;
}

val run :
  ?control:trigger Eio.Stream.t ->
  ?on_capture_resumed:(unit -> (unit, error) result) ->
  ?on_low_input:(unit -> (unit, error) result) ->
  sw:Eio.Switch.t ->
  mono_clock:_ Eio.Time.Mono.t ->
  logger:Ox_observability.Logger.t ->
  keyboard:Keyboard.Monitor.t ->
  audio:(unit -> (bytes option, [ `Pulse_error of Pulse.Client.error ]) result) ->
  ?queue_pcm_bytes:int ->
  ?capture_pcm_bytes:int ->
  ?queue_capacity:int ->
  config ->
  (unit, run_error) result
