(** Pass 2 text correction and translation.

    One non-streaming OpenRouter Responses call. The prompt template is rendered
    as the request instructions. Placeholders glossary, language,
    source_language and target_language are replaced; the transcript is the user
    message. Pie adds no other instructions.

    [timeout] bounds the whole call. Cancellation closes the HTTP socket. No
    finalizer is registered.

    Failure values and {!diagnostic} never contain provider text, prompts or
    transcripts. *)

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
(** [0.2]. Used when the caller does not pass a temperature. *)

val diagnostic : failure -> string
(** A short fixed phrase. Safe to log: it does not include provider text,
    prompts or transcripts. *)

val run :
  caps:< Ox_caps.Cap.env ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  model:string ->
  prompt_template:string ->
  transcript:string ->
  ?glossary:string ->
  ?language:string ->
  ?source_language:string ->
  ?target_language:string ->
  ?temperature:float ->
  ?reasoning_effort:string ->
  timeout:Ox.Duration.t ->
  unit ->
  (string, failure) result
(** [run] performs one pass-2 call.

    [?glossary], [?language], [?source_language] and [?target_language] default
    to the empty string and are substituted into [prompt_template].
    [?temperature] defaults to {!default_temperature}. [?reasoning_effort] is
    omitted from the request when absent. The returned text is trimmed. Blank
    text and text longer than {!Dictation_limits.result_utf8_bytes} are
    failures. *)
