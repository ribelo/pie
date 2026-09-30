(* Never render provider-owned text. Refusal text, tool arguments and
   Invalid_response.got can contain credentials, prompts or transcripts.
   Diagnostics are fixed phrases. See the comment in stt/openrouter.ml.

   Requests are encoded by Ox_ai_openrouter.Responses (text-only or audio,
   WAV or MP3); only OpenRouter provider routing is added locally. *)

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

module Responses = Ox_ai_openrouter.Responses

(* OpenRouter provider routing ("provider": {"order": [...]}) is not part of
   Ox's Responses request yet, so it is added to the JSON Ox encodes; see
   docs/missing-ox-api.md. Everything else (framing, audio format, reasoning
   items in the delivery verdict) is Ox's. *)
let with_provider_order json = function
  | None | Some [] -> json
  | Some names -> (
      match json with
      | `Assoc fields ->
          `Assoc
            (fields
            @ [
                ( "provider",
                  `Assoc
                    [ ("order", `List (List.map (fun n -> `String n) names)) ]
                );
              ])
      | other -> other)

let audio_format_of_string = function
  | "wav" -> Some Responses.Wav
  | "mp3" -> Some Responses.Mp3
  | _ -> None

let encode ~model ~instructions ~input_text ?audio ?(audio_format = "wav")
    ?provider_order ~temperature ~reasoning_effort () =
  let json =
    match audio with
    | None ->
        Ok
          (Responses.encode_text
             {
               model;
               instructions;
               input = input_text;
               temperature = Some temperature;
               text_format = None;
               reasoning_effort;
             })
    | Some audio -> (
        match audio_format_of_string audio_format with
        | None -> Error (Provider Invalid_request)
        | Some format ->
            Ok
              (Responses.encode ~audio_format:format ~instructions
                 {
                   model;
                   prompt = input_text;
                   wav = audio;
                   temperature = Some temperature;
                   text_format = None;
                   reasoning_effort;
                 }))
  in
  Result.map (fun json -> with_provider_order json provider_order) json

(* A WAV is the capture plus a 44-byte header. Anything larger cannot be a
   recording this process admitted. *)
let max_audio_bytes = Dictation_limits.capture_pcm_bytes + 44

(* Ox's transport default is 4 MiB, which a WAV of about 90 seconds already
   exceeds (2026-09-30: a 4-minute dictation failed locally with "input too
   large"). Allow the largest admitted audio, base64-encoded, plus the prompt
   and glossary. *)
let max_request_bytes = (max_audio_bytes / 3 * 4) + (4 * 1024 * 1024)

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
    ?audio_format ?provider_order ?(glossary = "") ?(language = "")
    ?(source_language = "") ?(target_language = "")
    ?(temperature = default_temperature) ?reasoning_effort ~timeout () =
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
        match
          encode ~model ~instructions ~input_text:transcript ?audio
            ?audio_format ?provider_order ~temperature ~reasoning_effort ()
        with
        | Error _ as error -> error
        | Ok json -> (
            let body = Ox_ai.Transport.bytes_of_json json in
            let http = Process.make_http ~sw ~net ~clock in
            match
              Ox_ai.Transport.post_json ~http ~clock ~logger ~timeout
                ~max_request_bytes ~max_response_bytes
                ~endpoint:(responses_endpoint ~caps)
                ~authorization:(Ox_ai.Bearer.header ~api_key)
                ~body ~deadline:timeout ()
            with
            | Error error -> Error (failure_of_ai error)
            | Ok response -> (
                match parse_body response.body with
                | Error _ as error -> error
                | Ok json -> (
                    match Responses.decode json with
                    | Error error -> Error (failure_of_ai error)
                    | Ok decoded -> (
                        match Responses.deliverable decoded with
                        | Error rejection ->
                            Error (Rejected (outcome_of_rejection rejection))
                        | Ok raw -> accept_text raw)))))
