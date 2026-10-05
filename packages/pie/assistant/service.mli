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
(** Transcribe and translate bindings after CLI overrides. Invalid keysyms or
    equal bindings raise a validation failure recognised by
    {!Config.Stt.config_error_message}. *)

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
    contains secrets.

    Suspension: never suspends in Pie, including native helper callbacks; JSON
    encoding and native configuration-path lookup depend on [Yojson.Safe],
    [Ox_caps], [Ox_app.Config], [Filename] and [Stdlib]. Cancellation: not a
    cancellation point in Pie; native and helper callback behavior depends on
    [Yojson.Safe], [Ox_caps], [Ox_app.Config], [Filename] and [Stdlib].
    Finalizers: creates no owned resource and registers no release hook.
    Locking: no lock is acquired.

    Invalid effective keybindings raise private [Config_error], recognised by
    [Config.Stt.config_error_message]. *)

val default_min_duration_ms : int

val config_path : caps:< Ox_caps.Cap.env ; .. > -> string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    native configuration-path construction implementation depends on [Ox_caps],
    [Ox_app.Config], [Filename] and [Stdlib]. Cancellation: not a cancellation
    point in Pie; native and helper callback behavior depends on [Ox_caps],
    [Ox_app.Config], [Filename] and [Stdlib]. Finalizers: creates no owned
    resource and registers no release hook. Locking: no lock is acquired. *)

val config_stamp :
  caps:< Ox_caps.Cap.env ; .. > -> ?config_path:string -> unit -> config_stamp
(** Suspension: never suspends in Pie, including native helper callbacks; the
    native path construction and file-stat lookup implementation depends on
    [Ox_caps], [Ox_app.Config], [Unix], [Filename] and [Stdlib]. Cancellation:
    not a cancellation point in Pie; native and helper callback behavior depends
    on [Ox_caps], [Ox_app.Config], [Unix], [Filename] and [Stdlib]. Finalizers:
    creates no owned resource and registers no release hook. Locking: no lock is
    acquired. *)

val load_snapshot :
  caps:< Ox_caps.Cap.env ; .. > ->
  fs:_ Eio.Path.t ->
  source:Ox_app.Config.source ->
  ?warn:(string -> unit) ->
  int ->
  Config.Stt.t * config_stamp
(** Suspension: may suspend while [Config.Stt.load] reads; warning behavior
    depends on the [warn] callback. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; configuration failure raises private [Config_error],
    recognised by [Config.Stt.config_error_message]; dependency behavior depends
    on [Ox_app.Config] and [warn]. Finalizers: read-resource cleanup is
    delegated to [Config.Stt.load], [Ox_app.Config] and [Eio.Path]; no release
    hook is registered here. Locking: no Pie mutex is held around loading or
    [warn]; dependency locking is outside Pie. Cancellation safety: unsafe
    because progress is lost if [warn] has non-repeatable effects; this
    operation returns a stable snapshot but does not commit it. *)

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
  ?target_window:History.window option ->
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
  deliver:
    (Config.Stt.t ->
    ptt_mode ->
    History.window option ->
    string ->
    (History.delivery, Ptt_loop.error) result) ->
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
(** Invokes [on_settle] once after child cleanup; the callback owns attempt
    release. A body failure followed by settlement failure returns
    [Ptt_cause (Suppressed {primary; finalizer})], retaining both.

    Suspension: may suspend while the body runs, while waiting for owned pause
    and during settlement; behavior depends on [body], [on_settle],
    [Ox.Concurrent] and [Eio]. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; an owned pause is classified as [Ok ()]; dependency
    and callback behavior depends on [Ox.Concurrent], [body] and [on_settle].
    Finalizers: [Fun.protect] invokes [on_settle] once after child cleanup,
    without adding cancellation protection; attempt release and callback cleanup
    depend on [on_settle]. Locking: no Pie mutex is held around [body] or
    [on_settle]; dependency locking is outside Pie. Cancellation safety: unsafe
    because progress is lost if body or settlement effects already occurred; an
    attempt is settled once, not replayed. *)

val report_delivery :
  notify:(string -> string -> unit) ->
  target_window:History.window option ->
  Input.Text_injection.outcome ->
  History.delivery

val production_deliver :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.stdout ; Ox_caps.Cap.stderr ; .. > ->
  clock:_ Eio.Time.clock ->
  process_mgr:_ Eio.Process.mgr ->
  ?clipboard_owner:Wayland.Clipboard.t ->
  Config.Stt.t ->
  ptt_mode ->
  History.window option ->
  string ->
  (History.delivery, Ptt_loop.error) result
(** Suspension: may suspend in clipboard ownership, focus lookup, text injection
    and notification subprocesses; native and process behavior depends on [Eio],
    [Ox_caps], [Unix] and [Stdlib]. Cancellation: propagates
    [Eio.Cancel.Cancelled] and [Eio.Exn.Multiple]; ordinary injection results
    are classified as delivery outcomes; dependency behavior depends on
    [Eio.Process]. Finalizers: process lifecycle is delegated to [Eio.Process];
    a supplied clipboard owner retains its own switch-owned helper lifetime;
    this function does not close the caller owner. Locking: clipboard offers
    serialize on the owner semaphore; no other Pie mutex is held across
    injection; dependency locking is outside Pie. Cancellation safety: unsafe
    because progress is lost if text, focus or clipboard state changed before
    cancellation; retry can duplicate injection. *)

val production_stt :
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
  process_mgr:_ Eio.Process.mgr ->
  Config.Stt.t ->
  ptt_mode ->
  bytes ->
  (string, Ptt_loop.error) result
(** Suspension: may suspend in focus lookup, protected response/archive writes,
    encoding and STT HTTP/WebSocket requests; behavior depends on [Eio],
    [Ox_http], [Ox_ai_realtime], [Ox_blocking], [Ox_caps], [Unix], [Stdlib] and
    [Ox_observability]; the indirect [Dictation.recognize] one-pass invocation
    delegates to the pipeline [one_pass] callback. Cancellation: propagates
    external [Eio.Cancel.Cancelled]; supported provider failures become PTT
    errors; dependency behavior depends on [Eio], [Ox_http], [Ox_ai_realtime]
    and [Ox_blocking]; cancellation behavior of the indirect
    [Dictation.recognize] one-pass invocation depends on the pipeline [one_pass]
    callback. Finalizers: provider/session resources retain their supplied
    switch/session ownership; subprocess cleanup is delegated to [Eio.Process],
    and native-pool cleanup to [Fs.blocking] and [Ox_blocking]. Locking: this
    dispatch wrapper acquires no Pie mutex; provider/session locking is
    delegated to [Eio], [Ox_http] and [Ox_ai_realtime]. Cancellation safety:
    unsafe because progress is lost for accepted provider requests or partial
    response archival; retry can repeat provider work. *)

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
  ?focused_window:(unit -> History.window option) ->
  stt:(Config.Stt.t -> ptt_mode -> bytes -> (string, Ptt_loop.error) result) ->
  deliver:
    (Config.Stt.t ->
    ptt_mode ->
    History.window option ->
    string ->
    (History.delivery, Ptt_loop.error) result) ->
  string ->
  (Ptt_loop.press_result, Ptt_loop.error) result
(** Suspension: may suspend in coordinator admission, recovery I/O and caller
    callbacks; [focused_window], [on_owner] and [pipeline] govern callback
    behavior, while [stt] and [deliver] are used by the returned scoped handle;
    dependency behavior depends on [Eio], [Ox.Concurrent], [Ox_http],
    [Ox_ai_realtime], [Ox_blocking], [Ox_caps], [Unix] and [Ox_observability].
    Cancellation: propagates external [Eio.Cancel.Cancelled]; callback behavior
    depends on [focused_window], [on_owner], [pipeline], [stt] and [deliver]; an
    unsupported mode name raises [Invalid_argument]. Finalizers: typed admission
    failures release an acquired coordinator attempt; successful admission
    returns a scoped handle using [run_acquired_attempt]; cleanup before that
    handoff depends on caller callbacks; dependency cleanup depends on [Eio],
    [Ox.Concurrent], [Ox_http], [Ox_ai_realtime] and [Ox_blocking]. Locking:
    coordinator mutation uses its protected mutex; caller callbacks run after
    admission, without that coordinator mutex held; transport/callback locking
    is outside Pie. Cancellation safety: unsafe because progress is lost if
    admission or caller effects have occurred before handle handoff; a new press
    does not replay or roll them back.

    Raises [Assert_failure] if the blocking-pool shutdown invariant is violated.
*)

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
  deliver:
    (Config.Stt.t ->
    ptt_mode ->
    History.window option ->
    string ->
    (History.delivery, Ptt_loop.error) result) ->
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
(** Suspension: may suspend in keyboard/audio input, RPC serving, attempt/retry
    supervision, protected persistence and caller callbacks; behavior depends on
    caller callbacks, [Ox.Concurrent], [Ox.Scope], [Eio], [Ox_http],
    [Ox_ai_realtime], [Ox_ipc], [Ox_blocking], [Ox_caps], [Unix], [Stdlib] and
    [Ox_observability]. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; owned pause and operational failures are classified
    separately; callback and dependency behavior depends on caller callbacks,
    [Eio], [Ox.Concurrent], [Ox.Scope], [Ox_http], [Ox_ai_realtime], [Ox_ipc]
    and [Ox_blocking]. Finalizers: a local service switch owns and awaits
    listener/workers before return; protected native writes drain through
    [Fs.blocking]; retained failure recovery follows attempt ownership;
    dependency and callback cleanup depends on caller callbacks, [Eio],
    [Ox.Concurrent], [Ox.Scope], [Ox_http], [Ox_ai_realtime], [Ox_ipc] and
    [Ox_blocking]. Locking: recording/queue state uses owner mutexes; protocol
    writes and protected persistence can wait; caller callback locking remains
    outside this interface guarantee. Cancellation safety: unsafe because
    progress is lost for interrupted capture, provider or delivery work;
    retained recovery does not undo external requests or injection.

    Raises [Assert_failure] if an internal blocking-pool or capture-state
    invariant is violated.

    Invalid effective keybindings raise private [Config_error], recognised by
    [Config.Stt.config_error_message].

    A poisoned queue mutex can raise [Eio.Mutex.Poisoned].

    Invalid arguments to delegated buffer, timing or URL helpers can raise
    [Invalid_argument]. A failing [Fun.protect] finalizer can propagate
    [Fun.Finally_raised].

    Native runtime-directory or persistence setup can raise [Unix.Unix_error].

    Keyboard ownership loss can fail the service scope with
    [Keyboard.Monitor.Input_lost]. *)

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
(** Suspension: may suspend in Pulse authentication/recording and
    [run_with_inputs]; behavior depends on [Eio], [Ox.Concurrent], [Ox.Scope],
    [Ox_http], [Ox_ai_realtime], [Ox_ipc], [Ox_blocking], [Ox_caps], [Unix],
    [Stdlib] and [Ox_observability]. Cancellation: propagates external
    [Eio.Cancel.Cancelled]; owned pause and operational failures are classified
    separately; dependency behavior depends on [Eio], [Ox.Concurrent],
    [Ox.Scope], [Ox_http], [Ox_ai_realtime], [Ox_ipc] and [Ox_blocking].
    Finalizers: the Pulse client/record-stream scopes surround the local service
    scope, which awaits its listener/workers before returning; native writes
    drain through [Fs.blocking]; dependency cleanup is delegated to [Eio],
    [Ox.Concurrent], [Ox.Scope], [Ox_http], [Ox_ai_realtime], [Ox_ipc] and
    [Ox_blocking]. Locking: Pulse protocol writes use their write mutex and
    recording/queue state uses owner mutexes; native persistence can wait;
    dependency-internal locking is outside Pie. Cancellation safety: unsafe
    because progress is lost for interrupted capture, provider or delivery work;
    a new service run does not replay consumed input or undo external effects.

    Raises [Assert_failure] if an internal blocking-pool or capture-state
    invariant is violated.

    Invalid effective keybindings raise private [Config_error], recognised by
    [Config.Stt.config_error_message].

    A poisoned queue mutex can raise [Eio.Mutex.Poisoned].

    A failing [Fun.protect] finalizer can propagate [Fun.Finally_raised].
    Invalid Pulse protocol encoding or decoding can raise [Failure].

    Native runtime-directory or persistence setup can raise [Unix.Unix_error].

    Keyboard ownership loss can fail the service scope with
    [Keyboard.Monitor.Input_lost].

    Invalid arguments to delegated buffer, timing or URL helpers can raise
    [Invalid_argument].

    Keyboard setup can raise [Keyboard.Monitor.Setup_timeout] when its
    acquisition budget expires, [Keyboard.Monitor.Unsupported_input] for
    unsupported bus addresses or malformed exchanges, or [End_of_file] for an
    incomplete setup exchange. *)
