(** Pass 2 and one-pass OpenRouter Responses calls.

    One non-streaming request. The prompt template is rendered as the request
    instructions. Placeholders glossary, language, source_language and
    target_language are replaced. Pie adds no other instructions.

    The user message is one [input_text] part. When [audio] is given, a second
    [input_audio] part carries that WAV, base64-encoded, with format ["wav"].
    One pass passes {!audio_cue} as the text part and the recording as [audio].
    Pass 2 passes the transcript and no audio.

    [?provider_order] is sent as [provider.order] when the list is non-empty.
    Fallbacks stay allowed: [allow_fallbacks] is not set.

    [timeout] bounds the whole call. One pass uses
    [Config.Stt.one_pass_timeout_seconds] (default 30 s), not
    [correction_timeout_seconds]. Cancellation closes the HTTP socket. No
    finalizer is registered.

    Failure values and {!diagnostic} never contain provider text, prompts,
    transcripts or audio. *)

type provider =
  | Missing_api_key
  | Unavailable
  | Authentication
  | Invalid_response
  | Input_too_large
  | Model_mismatch
  | Invalid_request

type outcome =
  | Incomplete
  | Cancelled
  | Not_completed
  | Tool_continuation
  | Refusal
  | Unsupported

type failure =
  | Timeout
  | Provider of provider
  | Rejected of outcome
  | Blank
  | Over_budget

val default_temperature : float
(** [0.2]. Used when the caller does not pass a temperature. One pass uses this;
    it does not read [correction_temperature]. *)

val audio_cue : string
(** ["The recording follows."]. The short text part sent with a WAV. The
    rendered prompt stays in [instructions]. *)

val diagnostic : failure -> string
(** A short fixed phrase. Safe to log: it does not include provider text,
    prompts, transcripts or audio. *)

val run :
  caps:< Ox_caps.Cap.env ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  model:string ->
  prompt_template:string ->
  transcript:string ->
  ?audio:bytes ->
  ?provider_order:string list ->
  ?glossary:string ->
  ?language:string ->
  ?source_language:string ->
  ?target_language:string ->
  ?temperature:float ->
  ?reasoning_effort:string ->
  timeout:Ox.Duration.t ->
  unit ->
  (string, failure) result
(** [run] performs one Responses call.

    [?glossary], [?language], [?source_language] and [?target_language] default
    to the empty string and are substituted into [prompt_template].
    [?temperature] defaults to {!default_temperature}. [?reasoning_effort] is
    omitted from the request when absent. [?audio] is WAV bytes; a recording
    larger than the capture budget plus a WAV header is [Input_too_large].
    [?provider_order] is omitted when absent or empty. The returned text is
    trimmed. Blank text and text longer than
    {!Dictation_limits.result_utf8_bytes} are failures. *)
