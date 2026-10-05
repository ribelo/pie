type trigger =
  | Press of string
  | Press_binding of Config.Stt.binding
  | Release of string

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
  ?on_press_binding:(Config.Stt.binding -> (press_result, error) result) ->
  ?on_capture_failure:(error -> (unit, error) result) ->
  ?on_ignored_capture:(History.ignored_capture -> unit) ->
  ?on_capture_resumed:(unit -> (unit, error) result) ->
  ?release_grace_ms:(unit -> int) ->
  ?on_capture_ended:(unit -> (unit, error) result) ->
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
(** Run exclusive capture with detached processing in a supervised child scope.
    Processing survives capture handoff and pause, but cancellation or failure
    of the loop also cancels its remaining workers.

    Suspension: may suspend in input/condition waits, queue mutex acquisition,
    the monotonic deadline and capture callbacks; callback and supervision
    behavior depends on caller callbacks, [Ox.Scope], [Ox.Concurrent],
    [Ox.Timeout], [Ox.Duration] and [Eio]. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; the monotonic capture deadline is classified as a
    timeout event; callback behavior depends on caller callbacks and [Ox.Scope].
    Finalizers: the supervised child scope cancels and awaits detached attempts
    before return; capture cleanup depends on handle callbacks and [Ox.Scope].
    Locking: queue mutation uses [Eio.Mutex] with [Fun.protect] unlock;
    condition waits release/reacquire it; caller capture callbacks run outside
    the queue mutex. Cancellation safety: unsafe because progress is lost for
    in-flight capture and attempt work; this loop does not replay consumed
    inputs.

    Raises [Assert_failure] if the already-filtered [Skip] state reaches capture
    startup.

    A poisoned queue mutex can raise [Eio.Mutex.Poisoned]. *)
