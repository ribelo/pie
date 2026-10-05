type error = [ `Stt_error of string ]

val provider_failed : Yojson.Safe.t -> bool
(** Detect provider errors in HTTP-success envelopes, including choice-level
    errors. No provider text is extracted or included in diagnostics. *)

val direct_transcription_timeout : Ox.Duration.t

val resolve_api_key :
  caps:< Ox_caps.Cap.env ; .. > -> unit -> Ox_observability.Redact.t option
(** Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and endpoint construction implementation depends on
    [Ox_caps], [Uri] and [Stdlib], including [Ox_observability.Redact].
    Cancellation: not a cancellation point in Pie; native and helper callback
    behavior depends on [Ox_caps], [Uri] and [Stdlib], including
    [Ox_observability.Redact]. Finalizers: creates no owned resource and
    registers no release hook. Locking: no lock is acquired. *)

val endpoint : caps:< Ox_caps.Cap.env ; .. > -> string -> string option
(** [endpoint caps path] is [OPENROUTER_BASE_URL] joined with [path], or [None]
    when that variable is unset or blank. Trailing slashes on the base are
    stripped.

    Suspension: never suspends in Pie, including native helper callbacks; the
    environment lookup and endpoint construction implementation depends on
    [Ox_caps], [Uri] and [Stdlib]. Cancellation: not a cancellation point in
    Pie; native and helper callback behavior depends on [Ox_caps], [Uri] and
    [Stdlib]. Finalizers: creates no owned resource and registers no release
    hook. Locking: no lock is acquired. *)

val render_template : string -> (string * string) list -> string
(** Replace every [{{key}}] placeholder. Unknown placeholders are left as
    written. *)

val base64_encode : bytes -> string

val transcribe :
  caps:< Ox_caps.Cap.env ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  api_key:Ox_observability.Redact.t ->
  model:string ->
  ?language:string ->
  ?prompt:string ->
  ?sample_rate:int ->
  bytes ->
  (string, error) result
(** Buffered pass 1 on [POST /api/v1/audio/transcriptions].

    [?language] is an ISO-639-1 code and is sent as the [language] part.
    [?prompt] is the rendered glossary and is sent as the [prompt] part. An
    absent option omits that part. There is no streaming call.

    Suspension: may suspend in the HTTP request; request and response behavior
    depends on [Ox_http.Client], [Process] and the [on_response] callback.
    Cancellation: propagates external [Eio.Cancel.Cancelled]; supported HTTP
    failures become STT errors; dependency and callback behavior depends on
    [Ox_http.Client] and [on_response]. Finalizers: HTTP connection/body cleanup
    is delegated to [Ox_http.Client] and its owning switch; no additional Pie
    release hook is registered. Locking: no Pie mutex is held around the HTTP
    request or [on_response]; dependency locking is outside Pie. Cancellation
    safety: unsafe because progress is lost if the provider accepted the request
    or [on_response] committed effects before cancellation; retry can repeat
    those effects. *)
