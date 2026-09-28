type error = [ `Stt_error of string ]

val direct_transcription_timeout : Eta.Duration.t
val response_operation_timeout : Eta.Duration.t
val resolve_api_key : unit -> Eta_observability.Redact.t option
val base64_encode : bytes -> string
val decode_structured_output : string -> (string, error) result

val transcribe :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  api_key:Eta_observability.Redact.t ->
  model:string ->
  language:string ->
  ?sample_rate:int ->
  bytes ->
  (string, error) result

val transcribe_with_prompt :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Eta_observability.Logger.t ->
  model:string ->
  language:string ->
  prompt_template:string ->
  ?thinking_effort:string ->
  ?sample_rate:int ->
  bytes ->
  (string, error) result

val translate_with_prompt :
  sw:Eio.Switch.t ->
  net:_ Eio.Net.t ->
  clock:_ Eio.Time.clock ->
  logger:Eta_observability.Logger.t ->
  model:string ->
  source_language:string ->
  target_language:string ->
  prompt_template:string ->
  ?thinking_effort:string ->
  ?sample_rate:int ->
  bytes ->
  (string, error) result
