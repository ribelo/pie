(** Live assistant owner, test seam, and Door entrypoint. A recording attempt
    holds admission until capture, provider, delivery and resource cleanup
    settle. *)

type ptt_mode = Transcribe | Translate
type ptt_binding = { keysym : int; shift : bool }
type config_stamp = (int * int * int * float) option list

type config_control = {
  stamp : unit -> config_stamp;
  load : int -> Config.Stt.t * config_stamp;
}

type 'audio owned_inputs = {
  keyboard : Keyboard.Monitor.t;
  audio : 'audio;
  source_name : string option;
}

type attempt_owner

val effective_bindings :
  ?ptt_transcribe_keysym:int ->
  ?ptt_translate_keysym:int ->
  Config.Stt.t ->
  ptt_binding * ptt_binding
(** Transcribe and translate bindings after CLI overrides.
    @raise Config.Stt.Config_error
      when a keysym is not positive or both bindings are equal. *)

val startup_config_json :
  caps:< Ox_caps.Cap.env ; .. > ->
  ?config_path:string ->
  config:Config.Stt.t ->
  ptt_transcribe_keysym:int ->
  ptt_translate_keysym:int ->
  min_duration_ms:int ->
  unit ->
  string
(** The effective configuration logged at startup, as pretty JSON; never
    contains secrets. *)

val default_min_duration_ms : int
val config_path : caps:< Ox_caps.Cap.env ; .. > -> string

val config_stamp :
  caps:< Ox_caps.Cap.env ; .. > -> ?config_path:string -> unit -> config_stamp

val load_snapshot :
  caps:< Ox_caps.Cap.env ; .. > ->
  fs:_ Eio.Path.t ->
  source:Ox_app.Config.source ->
  int ->
  Config.Stt.t * config_stamp

val with_cli_overrides :
  ?ptt_transcribe_keysym:int ->
  ?ptt_translate_keysym:int ->
  Config.Stt.t ->
  Config.Stt.t

val make_live_recognizer :
  load_bindings:(unit -> ptt_binding * ptt_binding) ->
  ?load_config_bindings:(unit -> Config.Stt.binding list) ->
  ?coordinator:Recording.Coordinator.t ->
  ?latch_keycode:int option ref ->
  unit ->
  Keyboard.Monitor.key_event ->
  Ptt_loop.trigger option

val create_attempt_owner :
  ?dictation:Dictation.t ->
  store:Recording.Retry_store.Active.store ->
  coordinator:Recording.Coordinator.t ->
  attempt:Recording.Coordinator.attempt ->
  ptt_mode ->
  (attempt_owner, [ `Ptt_error of string ]) result

val attempt_handle : attempt_owner -> Ptt_loop.handle -> Ptt_loop.handle
val settle_attempt : attempt_owner -> (unit, [ `Ptt_error of string ]) result

val make_clip_handle :
  owner:attempt_owner ->
  stt:(Config.Stt.t -> ptt_mode -> bytes -> (string, Ptt_loop.error) result) ->
  deliver:(Config.Stt.t -> ptt_mode -> string -> (unit, Ptt_loop.error) result) ->
  config:Config.Stt.t ->
  ptt_mode ->
  Ptt_loop.handle
(** The buffered (non-streaming) capture handle: [finish] seals the attempt's
    audio, recognizes the whole clip with [stt] and delivers it, publishing
    sealed audio or the recognized text on failure (see [on_press]). [offer] and
    [cancel] are no-ops. *)

val run_acquired_attempt :
  sw:Eio.Switch.t ->
  attempt:Recording.Coordinator.attempt ->
  on_settle:(unit -> (unit, Ptt_loop.error) result) ->
  (unit -> (unit, Ptt_loop.error) result) ->
  (unit, Ptt_loop.error) result
(** Always releases the attempt. A body failure followed by settlement failure
    returns [Ptt_cause (Suppressed {primary; finalizer})], retaining both. *)

val production_stt :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. > ->
  sw:Eio.Switch.t ->
  net:[> `Generic ] Eio.Net.ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  process_mgr:_ Eio.Process.mgr ->
  Config.Stt.t ->
  ptt_mode ->
  bytes ->
  (string, [ `Ptt_error of string ]) result

val on_press :
  caps:
    < Ox_caps.Cap.env
    ; Ox_caps.Cap.fs_read
    ; Ox_caps.Cap.fs_write
    ; Ox_caps.Cap.time
    ; Ox_caps.Cap.stderr
    ; .. > ->
  sw:Eio.Switch.t ->
  net:[> `Generic ] Eio.Net.ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  coordinator:Recording.Coordinator.t ->
  config:Config.Stt.t ->
  ?store:Recording.Retry_store.Active.store ->
  ?retry_dir:string ->
  ?pipeline:Dictation.dependencies ->
  ?binding:Config.Stt.binding ->
  ?on_owner:(attempt_owner -> unit) ->
  stt:(Config.Stt.t -> ptt_mode -> bytes -> (string, Ptt_loop.error) result) ->
  deliver:(Config.Stt.t -> ptt_mode -> string -> (unit, Ptt_loop.error) result) ->
  string ->
  (Ptt_loop.press_result, Ptt_loop.error) result

val run_with_inputs :
  caps:
    < Ox_caps.Cap.env
    ; Ox_caps.Cap.fs_read
    ; Ox_caps.Cap.fs_write
    ; Ox_caps.Cap.time
    ; Ox_caps.Cap.stdout
    ; Ox_caps.Cap.stderr
    ; Ox_caps.Cap.network
    ; .. > ->
  sw:Eio.Switch.t ->
  net:[> `Generic ] Eio.Net.ty Eio.Resource.t ->
  fs:_ Eio.Path.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  process_mgr:_ Eio.Process.mgr ->
  mono_clock:_ Eio.Time.Mono.t ->
  inputs:
    (unit -> (bytes option, [ `Pulse_error of Pulse.Client.error ]) result)
    owned_inputs ->
  config_control:config_control ->
  stt:(Config.Stt.t -> ptt_mode -> bytes -> (string, Ptt_loop.error) result) ->
  deliver:(Config.Stt.t -> ptt_mode -> string -> (unit, Ptt_loop.error) result) ->
  socket_path:string ->
  retry_dir:string ->
  ?config_path:string ->
  ?initial_config:Config.Stt.t ->
  ?pipeline:Dictation.dependencies ->
  ?ptt_transcribe_keysym:int ->
  ?ptt_translate_keysym:int ->
  ?min_duration_ms:int ->
  ?queue_pcm_bytes:int ->
  ?capture_pcm_bytes:int ->
  ?queue_capacity:int ->
  ?audio_dir:string ->
  ?history_path:string ->
  unit ->
  (unit, Ptt_loop.run_error) result

val run :
  caps:
    < Ox_caps.Cap.env
    ; Ox_caps.Cap.fs_read
    ; Ox_caps.Cap.fs_write
    ; Ox_caps.Cap.time
    ; Ox_caps.Cap.stdout
    ; Ox_caps.Cap.stderr
    ; Ox_caps.Cap.network
    ; .. > ->
  sw:Eio.Switch.t ->
  fs:_ Eio.Path.t ->
  net:[> `Generic ] Eio.Net.ty Eio.Resource.t ->
  clock:_ Eio.Time.clock ->
  mono_clock:_ Eio.Time.Mono.t ->
  logger:Ox_observability.Logger.t ->
  process_mgr:_ Eio.Process.mgr ->
  config:Config.Stt.t ->
  source:Ox_app.Config.source ->
  ?ptt_transcribe_keysym:int ->
  ?ptt_translate_keysym:int ->
  ?min_duration_ms:int ->
  ?stdin:'a ->
  unit ->
  (unit, Ptt_loop.run_error) result
