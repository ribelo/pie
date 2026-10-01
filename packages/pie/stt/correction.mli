(** Pass 2 and one-pass OpenRouter Responses calls.

    One non-streaming request. The prompt template is rendered as the request
    instructions. Placeholders glossary, language, source_language and
    target_language are replaced. Pie adds no other instructions.

    The user message is one [input_text] part. When [audio] is given, a second
    [input_audio] part carries that audio, base64-encoded, with its format. One
    pass passes {!audio_cue} as the text part and the recording as [audio]. Pass
    2 passes the transcript and no audio.

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
  ?audio_format:string ->
  ?provider_order:string list ->
  ?on_response:(bytes -> unit) ->
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
    omitted from the request when absent. [?audio] is the recording in
    [?audio_format] (default ["wav"]; one pass sends ["mp3"] when an encoder is
    available); audio larger than the capture budget plus a WAV header is
    [Input_too_large]. The request may be as large as that audio base64-encoded
    plus 4 MiB, above Ox's 4 MiB transport default. [?provider_order] is omitted
    when absent or empty. The returned text is trimmed. Blank text and text
    longer than {!Dictation_limits.result_utf8_bytes} are failures. The request
    requires a strict JSON object with exactly one string field [text]; only
    that field is returned, as Markdown. Unframed, malformed, truncated or
    extra-field replies are rejected, never pasted verbatim. Reasoning is
    excluded when a reasoning effort is configured. [on_response] receives the
    private complete body before classification, including non-2xx bodies. The
    transport does not retry inside this single exchange. *)

type upload = Wav of bytes | Mp3 of { mp3 : bytes; wav : bytes }

type retry = {
  first_diagnostic : string;
  final_diagnostic : string;
  audio_format : string option;
  reasoning_effort : string option;
}

type report = { result : (string, failure) result; retry : retry option }

val of_result : (string, failure) result -> report

val run_with_retry :
  caps:< Ox_caps.Cap.env ; .. > ->
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Ox_observability.Logger.t ->
  model:string ->
  prompt_template:string ->
  transcript:string ->
  ?upload:upload ->
  ?provider_order:string list ->
  ?on_response:(first:bool -> bytes -> unit) ->
  ?glossary:string ->
  ?language:string ->
  ?source_language:string ->
  ?target_language:string ->
  ?temperature:float ->
  ?reasoning_effort:string ->
  timeout:Ox.Duration.t ->
  unit ->
  report
(** The shared one-pass/pass-2 attempt policy: any typed failure except
    cancellation gets exactly one retry. MP3 falls back to its WAV; WAV and text
    retain the same input. Retry reasoning is at least medium (medium, high and
    xhigh remain unchanged). Cancellation propagates, never triggers a retry.
    Each exchange has its own [timeout]. [on_response ~first:true] retains the
    failed first body before retry; [~first:false] retains the final body. Never
    log these bodies. The retry record contains only fixed, sanitized
    diagnostics, including [stt:ok] on successful recovery. *)
