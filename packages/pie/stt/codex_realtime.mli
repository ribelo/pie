type error = [ `Stt_error of string ]
type mode = Transcription

type event =
  | Created of string
  | Config_ack of string
  | Transcript_delta of string
  | Transcript_final of string
  | Server_error of { message : string; code : string option }
  | Other of string

module First_token_timer : sig
  type t

  val create : unit -> t
  val start : t -> at_ms:int -> unit
  val observe : t -> at_ms:int -> string -> int option
end

module Codex_auth : sig
  type t = { path : string; access_token : string }

  val auth_path : caps:< Ox_caps.Cap.env ; .. > -> unit -> string
  (** Suspension: never suspends in Pie, including native helper callbacks; the
      environment lookup and URL/path construction implementation depends on
      [Ox_caps], [Uri], [Filename] and [Stdlib]. Cancellation: not a
      cancellation point in Pie; native and helper callback behavior depends on
      [Ox_caps], [Uri], [Filename] and [Stdlib]. Finalizers: creates no owned
      resource and registers no release hook. Locking: no lock is acquired. *)

  val load :
    caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; .. > ->
    ?path:string ->
    unit ->
    (t, error) result
  (** Suspension: never suspends in Pie, including native helper callbacks; the
      native credential reads, time lookup and JSON/token decoding
      implementation depends on [Ox_caps], [Unix], [Yojson.Safe], [Filename] and
      [Stdlib]. Cancellation: not a cancellation point in Pie; native and helper
      callback behavior depends on [Ox_caps], [Unix], [Yojson.Safe], [Filename]
      and [Stdlib]. Finalizers: credential input channels are closed by
      [Fun.protect]; no release hook is registered. Locking: no lock is
      acquired. *)

  val decode_jwt_exp_seconds : string -> int option
  val is_expired : ?leeway_seconds:int -> now:float -> string -> bool

  val access_token :
    caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. > ->
    unit ->
    (string, error) result
  (** Suspension: never suspends in Pie, including native helper callbacks; the
      native credential reads, time lookup and JSON/token decoding
      implementation depends on [Ox_caps], [Unix], [Yojson.Safe], [Filename] and
      [Stdlib]; let-operator invocation delegates to the local non-suspending
      [Result.bind] callback composition. Cancellation: not a cancellation point
      in Pie; native and helper callback behavior depends on [Ox_caps], [Unix],
      [Yojson.Safe], [Filename] and [Stdlib]; let-operator invocation delegates
      to the local non-suspending [Result.bind] callback composition.
      Finalizers: credential input channels are closed by [Fun.protect]; no
      release hook is registered. Locking: no lock is acquired. *)
end

val oauth_response_idle_timeout : Ox.Duration.t
val websocket_establishment_timeout : Ox.Duration.t

val realtime_access_token :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. > ->
  client:'a ->
  unit ->
  (string, error) result
(** Suspension: never suspends in Pie, including native helper callbacks; the
    native credential reads, time lookup and JSON/token decoding implementation
    depends on [Ox_caps], [Unix], [Yojson.Safe], [Filename] and [Stdlib];
    let-operator invocation delegates to the local non-suspending [Result.bind]
    callback composition. Cancellation: not a cancellation point in Pie; native
    and helper callback behavior depends on [Ox_caps], [Unix], [Yojson.Safe],
    [Filename] and [Stdlib]; let-operator invocation delegates to the local
    non-suspending [Result.bind] callback composition. Finalizers: credential
    input channels are closed by [Fun.protect]; no release hook is registered.
    Locking: no lock is acquired. *)

val realtime_header_pairs :
  ?session_id:string -> string -> (string * string) list

val build_realtime_url :
  caps:< Ox_caps.Cap.env ; .. > ->
  ?base_url:string ->
  mode:mode ->
  model:string ->
  unit ->
  string
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and URL construction implementation depends on [Ox_caps],
    [Ox_http.Url], [Stdlib]. Cancellation: not a cancellation point in Pie;
    native and helper callback behavior depends on [Ox_caps], [Ox_http.Url],
    [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired.

    An invalid constructed WebSocket URL raises [Invalid_argument]. *)

val parse_event : string -> (event, error) result
val server_error_diagnostic : string option -> string
val event_should_stop : mode:mode -> event -> bool
val resample_s16le_mono : from_rate:int -> to_rate:int -> bytes -> bytes

val run_realtime_session :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  ?ca_file:string ->
  url:string ->
  access_token:string ->
  mode:mode ->
  ?session_update:Ox_json.t ->
  input_sample_rate:int ->
  bytes ->
  (string, error) result
(** Suspension: may suspend in WebSocket I/O, queue waits, completion waits and
    caller callbacks; behavior depends on [Ox_ai_realtime.Session],
    [Ox_http.Ws_client], [Ox.Concurrent], [Eio] and caller callbacks.
    Cancellation: propagates external [Eio.Cancel.Cancelled]; protocol failures
    become STT errors; dependency and callback behavior depends on
    [Ox_ai_realtime.Session], [Ox_http.Ws_client], [Ox.Concurrent] and caller
    callbacks. Finalizers: session transport cleanup is delegated to
    [Ox_ai_realtime.Session], [Ox_http.Ws_client] and the owning switch;
    streaming supervision is delegated to [Ox.Concurrent]; caller resources
    remain callback responsibility. Locking: streaming queue mutation uses a
    mutex with [Fun.protect] unlock; condition waits release/reacquire it;
    caller [use] and text callbacks run outside the queue mutex; dependency
    locking is outside Pie. Cancellation safety: unsafe because progress is lost
    for already-consumed audio and provider work; cancellation does not replay
    audio or roll back caller effects. *)

val run_realtime_stream_session :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  ?ca_file:string ->
  url:string ->
  access_token:string ->
  mode:mode ->
  ?session_update:Ox_json.t ->
  ?on_text:(string -> unit) ->
  input_sample_rate:int ->
  (unit -> (bytes option, 'audio_error) result) ->
  (string, error) result
(** Suspension: may suspend in WebSocket I/O, queue waits, completion waits and
    caller callbacks; behavior depends on [Ox_ai_realtime.Session],
    [Ox_http.Ws_client], [Ox.Concurrent], [Eio] and caller callbacks.
    Cancellation: propagates external [Eio.Cancel.Cancelled]; protocol failures
    become STT errors; dependency and callback behavior depends on
    [Ox_ai_realtime.Session], [Ox_http.Ws_client], [Ox.Concurrent] and caller
    callbacks. Finalizers: session transport cleanup is delegated to
    [Ox_ai_realtime.Session], [Ox_http.Ws_client] and the owning switch;
    streaming supervision is delegated to [Ox.Concurrent]; caller resources
    remain callback responsibility. Locking: streaming queue mutation uses a
    mutex with [Fun.protect] unlock; condition waits release/reacquire it;
    caller [use] and text callbacks run outside the queue mutex; dependency
    locking is outside Pie. Cancellation safety: unsafe because progress is lost
    for already-consumed audio and provider work; cancellation does not replay
    audio or roll back caller effects. *)

type stream_handle = {
  offer_chunk : bytes -> (unit, error) result;
  finish_stream : unit -> (string, error) result;
  cancel_stream : unit -> (unit, error) result;
}

val with_stream_session :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.time ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  url:string ->
  access_token:string ->
  mode:mode ->
  operation:string ->
  model:string ->
  ?session_update:Ox_json.t ->
  ?sample_rate:int ->
  cancel_message:string ->
  (stream_handle -> ('a, error) result) ->
  ('a, error) result
(** Suspension: may suspend in WebSocket I/O, queue waits, completion waits and
    caller callbacks; behavior depends on [Ox_ai_realtime.Session],
    [Ox_http.Ws_client], [Ox.Concurrent], [Eio] and caller callbacks.
    Cancellation: propagates external [Eio.Cancel.Cancelled]; protocol failures
    become STT errors; dependency and callback behavior depends on
    [Ox_ai_realtime.Session], [Ox_http.Ws_client], [Ox.Concurrent] and caller
    callbacks. Finalizers: session transport cleanup is delegated to
    [Ox_ai_realtime.Session], [Ox_http.Ws_client] and the owning switch;
    streaming supervision is delegated to [Ox.Concurrent]; caller resources
    remain callback responsibility. Locking: streaming queue mutation uses a
    mutex with [Fun.protect] unlock; condition waits release/reacquire it;
    caller [use] and text callbacks run outside the queue mutex; dependency
    locking is outside Pie. Cancellation safety: unsafe because progress is lost
    for already-consumed audio and provider work; cancellation does not replay
    audio or roll back caller effects.

    A poisoned streaming-queue mutex can raise [Eio.Mutex.Poisoned]. *)

val build_pass1_session_update :
  ?language:string -> ?context:string -> model:string -> unit -> Ox_json.t
(** [build_pass1_session_update ?language ?context ~model ()] is the
    transcription [session.update] for a dedicated speech-to-text model.

    [model] is sent as [audio.input.transcription.model]. [?language] is an
    ISO-639-1 code from [Stt.Language.iso639_1] and is sent as [languages], the
    field [gpt-live-transcribe] and [gpt-transcribe] accept; the singular
    [language] field is not sent. [?context] is the rendered glossary and is
    sent as [prompt]. Absent options omit those fields. [turn_detection] is
    null: these models do not support voice activity detection, and Pie commits
    the buffer on key release. *)

val with_pass1_stream :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  model:string ->
  ?language:string ->
  ?context:string ->
  ?sample_rate:int ->
  (stream_handle -> ('a, error) result) ->
  ('a, error) result
(** Streaming pass 1 on the transcription session
    ([wss .../v1/realtime?intent=transcription]).

    Offer audio on the handle while the key is held. [finish_stream] commits the
    buffer and returns the completed transcript.

    Suspension: may suspend in WebSocket I/O, queue waits, completion waits and
    caller callbacks; behavior depends on [Ox_ai_realtime.Session],
    [Ox_http.Ws_client], [Ox.Concurrent], [Eio] and caller callbacks.
    Cancellation: propagates external [Eio.Cancel.Cancelled]; protocol failures
    become STT errors; dependency and callback behavior depends on
    [Ox_ai_realtime.Session], [Ox_http.Ws_client], [Ox.Concurrent] and caller
    callbacks. Finalizers: session transport cleanup is delegated to
    [Ox_ai_realtime.Session], [Ox_http.Ws_client] and the owning switch;
    streaming supervision is delegated to [Ox.Concurrent]; caller resources
    remain callback responsibility. Locking: streaming queue mutation uses a
    mutex with [Fun.protect] unlock; condition waits release/reacquire it;
    caller [use] and text callbacks run outside the queue mutex; dependency
    locking is outside Pie. Cancellation safety: unsafe because progress is lost
    for already-consumed audio and provider work; cancellation does not replay
    audio or roll back caller effects.

    A poisoned streaming-queue mutex can raise [Eio.Mutex.Poisoned].

    An invalid constructed WebSocket URL raises [Invalid_argument]. *)

val transcribe_pass1 :
  caps:< Ox_caps.Cap.env ; Ox_caps.Cap.fs_read ; Ox_caps.Cap.time ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  model:string ->
  ?language:string ->
  ?context:string ->
  ?sample_rate:int ->
  bytes ->
  (string, error) result
(** Buffered pass 1. The sealed PCM is sent on the same transcription websocket
    after release, then committed. The Codex login is refused by
    [/v1/audio/transcriptions], so this does not use that endpoint.

    Suspension: may suspend in WebSocket I/O, queue waits, completion waits and
    caller callbacks; behavior depends on [Ox_ai_realtime.Session],
    [Ox_http.Ws_client], [Ox.Concurrent], [Eio] and caller callbacks.
    Cancellation: propagates external [Eio.Cancel.Cancelled]; protocol failures
    become STT errors; dependency and callback behavior depends on
    [Ox_ai_realtime.Session], [Ox_http.Ws_client], [Ox.Concurrent] and caller
    callbacks. Finalizers: session transport cleanup is delegated to
    [Ox_ai_realtime.Session], [Ox_http.Ws_client] and the owning switch;
    streaming supervision is delegated to [Ox.Concurrent]; caller resources
    remain callback responsibility. Locking: streaming queue mutation uses a
    mutex with [Fun.protect] unlock; condition waits release/reacquire it;
    caller [use] and text callbacks run outside the queue mutex; dependency
    locking is outside Pie. Cancellation safety: unsafe because progress is lost
    for already-consumed audio and provider work; cancellation does not replay
    audio or roll back caller effects.

    Invalid audio resampling input or a malformed WebSocket URL can raise
    [Invalid_argument]. *)
