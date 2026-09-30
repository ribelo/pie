(* Never render provider-owned text. Refusal text, tool arguments and
   Invalid_response.got can contain credentials, prompts or transcripts.
   Diagnostics are fixed phrases. See the comment in stt/openrouter.ml.

   Ox_ai_openrouter.Responses encodes only audio requests. The text request
   below is local until Ox grows one; see docs/missing-ox-api.md. *)

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

let default_temperature = 0.2
let audio_cue = "The recording follows."

let diagnostic = function
  | Timeout -> "timeout"
  | Provider Missing_api_key -> "missing api key"
  | Provider Unavailable -> "unavailable"
  | Provider Authentication -> "authentication"
  | Provider Invalid_response -> "invalid response"
  | Provider Input_too_large -> "input too large"
  | Provider Model_mismatch -> "model mismatch"
  | Provider Invalid_request -> "invalid request"
  | Rejected Incomplete -> "incomplete"
  | Rejected Cancelled -> "cancelled"
  | Rejected Not_completed -> "missing finish markers or non-completed status"
  | Rejected Tool_continuation -> "tool continuation"
  | Rejected Refusal -> "refusal"
  | Rejected Unsupported -> "unsupported output"
  | Blank -> "blank"
  | Over_budget -> "over budget"

let failure_of_ai : Ox_ai.Error.t -> failure = function
  | `Timeout -> Timeout
  | `Unavailable _ -> Provider Unavailable
  | `Authentication _ -> Provider Authentication
  | `Invalid_response _ -> Provider Invalid_response
  | `Input_too_large _ -> Provider Input_too_large
  | `Model_mismatch _ -> Provider Model_mismatch

let outcome_of_rejection = function
  | `Not_completed "incomplete" -> Incomplete
  | `Not_completed "cancelled" -> Cancelled
  | `Not_completed _ -> Not_completed
  | `Tool_continuation _ -> Tool_continuation
  | `Refusal _ -> Refusal
  | `Other _ -> Unsupported

let render ~glossary ~language ~source_language ~target_language template =
  Openrouter.render_template template
    [
      ("glossary", glossary);
      ("language", language);
      ("source_language", source_language);
      ("target_language", target_language);
    ]

let responses_endpoint ~caps =
  match Openrouter.endpoint ~caps "/api/v1/responses" with
  | Some url -> url
  | None -> Ox_ai_openrouter.Responses.default_endpoint

(* Framing headroom so a result just over the UTF-8 budget is decoded and
   classified as [Over_budget]. A larger body is refused by the transport as
   an invalid response, which is still a pass-2 failure. *)
let max_response_bytes = Dictation_limits.result_utf8_bytes + 65_536
let obj = Ox_json.object_
let s value = Some (Ox_json.string value)

(* A request with [reasoning.effort] gets a [reasoning] output item next to the
   message (seen from Gemini through OpenRouter on 2026-09-30). It carries no
   deliverable text and is not a tool call or refusal, so it is dropped before
   the delivery verdict; every other kind still disqualifies the reply. *)
let without_reasoning (response : Ox_ai_openrouter.Responses.response) =
  {
    response with
    output =
      List.filter
        (function
          | Ox_ai_openrouter.Responses.Other "reasoning" -> false | _ -> true)
        response.output;
  }

let audio_part wav =
  obj
    [
      ("type", s "input_audio");
      ( "input_audio",
        Some
          (obj
             [ ("data", s (Openrouter.base64_encode wav)); ("format", s "wav") ])
      );
    ]

let provider_field = function
  | None | Some [] -> None
  | Some names ->
      Some
        (obj
           [ ("order", Some (Ox_json.array (List.map Ox_json.string names))) ])

let encode ~model ~instructions ~input_text ?audio ?provider_order ~temperature
    ~reasoning_effort () =
  let text = obj [ ("type", s "input_text"); ("text", s input_text) ] in
  let parts =
    match audio with None -> [ text ] | Some wav -> [ text; audio_part wav ]
  in
  let message =
    obj [ ("role", s "user"); ("content", Some (Ox_json.array parts)) ]
  in
  obj
    [
      ("model", s model);
      ("instructions", s instructions);
      ("input", Some (Ox_json.array [ message ]));
      ("stream", Some (Ox_json.bool false));
      ("temperature", Ox_json.float temperature);
      ( "reasoning",
        Option.map (fun effort -> obj [ ("effort", s effort) ]) reasoning_effort
      );
      ("provider", provider_field provider_order);
    ]

(* A WAV is the capture plus a 44-byte header. Anything larger cannot be a
   recording this process admitted. *)
let max_audio_bytes = Dictation_limits.capture_pcm_bytes + 44

let parse_body bytes =
  match Ox_json.parse (Bytes.to_string bytes) with
  | Ok json -> Ok json
  | Error (`Malformed _) -> Error (Provider Invalid_response)
  | Error (`Too_large _) -> Error (Provider Input_too_large)

let accept_text raw =
  let text = String.trim raw in
  if text = "" then Error Blank
  else if String.length text > Dictation_limits.result_utf8_bytes then
    Error Over_budget
  else Ok text

let run ~caps ~sw ~net ~clock ~logger ~model ~prompt_template ~transcript ?audio
    ?provider_order ?(glossary = "") ?(language = "") ?(source_language = "")
    ?(target_language = "") ?(temperature = default_temperature)
    ?reasoning_effort ~timeout () =
  if String.trim model = "" || not (Float.is_finite temperature) then
    Error (Provider Invalid_request)
  else if
    Option.fold ~none:false
      ~some:(fun wav -> Bytes.length wav > max_audio_bytes)
      audio
  then Error (Provider Input_too_large)
  else
    match Openrouter.resolve_api_key ~caps () with
    | None -> Error (Provider Missing_api_key)
    | Some api_key -> (
        let instructions =
          render ~glossary ~language ~source_language ~target_language
            prompt_template
        in
        let body =
          Ox_ai.Transport.bytes_of_json
            (encode ~model ~instructions ~input_text:transcript ?audio
               ?provider_order ~temperature ~reasoning_effort ())
        in
        let http = Process.make_http ~sw ~net ~clock in
        match
          Ox_ai.Transport.post_json ~http ~clock ~logger ~timeout
            ~max_response_bytes ~endpoint:(responses_endpoint ~caps)
            ~authorization:(Ox_ai.Bearer.header ~api_key)
            ~body ~deadline:timeout ()
        with
        | Error error -> Error (failure_of_ai error)
        | Ok response -> (
            match parse_body response.body with
            | Error _ as error -> error
            | Ok json -> (
                match Ox_ai_openrouter.Responses.decode json with
                | Error error -> Error (failure_of_ai error)
                | Ok decoded -> (
                    match
                      Ox_ai_openrouter.Responses.deliverable
                        (without_reasoning decoded)
                    with
                    | Error rejection ->
                        Error (Rejected (outcome_of_rejection rejection))
                    | Ok raw -> accept_text raw))))
