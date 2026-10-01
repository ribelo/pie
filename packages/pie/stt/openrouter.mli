type error = [ `Stt_error of string ]

val provider_failed : Yojson.Safe.t -> bool
(** Detect provider errors in HTTP-success envelopes, including choice-level
    errors. No provider text is extracted or included in diagnostics. *)

val direct_transcription_timeout : Ox.Duration.t
val response_operation_timeout : Ox.Duration.t

val resolve_api_key :
  caps:< Ox_caps.Cap.env ; .. > -> unit -> Ox_observability.Redact.t option

val endpoint : caps:< Ox_caps.Cap.env ; .. > -> string -> string option
(** [endpoint caps path] is [OPENROUTER_BASE_URL] joined with [path], or [None]
    when that variable is unset or blank. Trailing slashes on the base are
    stripped. *)

val render_template : string -> (string * string) list -> string
(** Replace every [{{key}}] placeholder. Unknown placeholders are left as
    written. *)

val base64_encode : bytes -> string
val decode_structured_output : string -> (string, error) result

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
    absent option omits that part. There is no streaming call. *)

val transcribe_with_prompt :
  caps:< Ox_caps.Cap.env ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  model:string ->
  language:string ->
  prompt_template:string ->
  ?thinking_effort:string ->
  ?sample_rate:int ->
  bytes ->
  (string, error) result

val translate_with_prompt :
  caps:< Ox_caps.Cap.env ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  model:string ->
  source_language:string ->
  target_language:string ->
  prompt_template:string ->
  ?thinking_effort:string ->
  ?sample_rate:int ->
  bytes ->
  (string, error) result
