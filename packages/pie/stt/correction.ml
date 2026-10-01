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
(* The message contains a JSON string inside the response JSON. A control
   character can require six bytes in the inner string and double escaping
   outside it. The user-text budget is checked after decoding both layers. *)
let max_response_bytes = (12 * Dictation_limits.result_utf8_bytes) + 65_536

module Responses = Ox_ai_openrouter.Responses

(* Protocol framing, not writing instructions: arbitrary free-form model
   output (including its analysis) must never be pasted into the destination.
   The user's prompt controls the Markdown inside the text field. *)
let response_schema = "dictation/text-v1"

let final_text_format ~output_language =
  Responses.Json_schema
    {
      name = "dictation";
      strict = Some true;
      schema =
        `Assoc
          [
            ("type", `String "object");
            ( "properties",
              `Assoc
                [
                  ( "text",
                    `Assoc
                      [
                        ("type", `String "string");
                        ( "description",
                          `String
                            (Printf.sprintf
                               "The final message to type, in %s, exactly as \
                                the instructions require. Nothing else."
                               output_language) );
                      ] );
                ] );
            ("required", `List [ `String "text" ]);
            ("additionalProperties", `Bool false);
          ];
    }

let exclude_reasoning json =
  match json with
  | `Assoc fields ->
      `Assoc
        (List.map
           (function
             | "reasoning", `Assoc reasoning ->
                 ("reasoning", `Assoc (reasoning @ [ ("exclude", `Bool true) ]))
             | field -> field)
           fields)
  | other -> other

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
    ?provider_order ~output_language ~temperature ~reasoning_effort () =
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
               text_format = Some (final_text_format ~output_language);
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
                   text_format = Some (final_text_format ~output_language);
                   reasoning_effort;
                 }))
  in
  Result.map
    (fun json -> with_provider_order (exclude_reasoning json) provider_order)
    json

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

(* Yojson accepts JSON comments. Outside the two quoted strings of our
   envelope, allow only object punctuation and JSON whitespace. Comments or
   other prose must be rejected, not silently skipped by the JSON parser. *)
let envelope_tokens raw =
  let rec scan i quoted escaped =
    if i = String.length raw then not quoted
    else
      let c = raw.[i] in
      if quoted then
        if escaped then
          match c with
          | '"' | '\\' | '/' | 'b' | 'f' | 'n' | 'r' | 't' | 'u' ->
              scan (i + 1) true false
          | _ -> false
        else if c = '"' then scan (i + 1) false false
        else if c = '\\' then scan (i + 1) true true
        else if Char.code c < 32 then false
        else scan (i + 1) true false
      else
        match c with
        | '"' -> scan (i + 1) true false
        | '{' | '}' | ':' | ',' | ' ' | '\t' | '\r' | '\n' ->
            scan (i + 1) false false
        | _ -> false
  in
  scan 0 false false

let accept_text raw =
  if not (envelope_tokens raw) then Error (Provider Invalid_response)
  else
    match Ox_json.parse raw with
    | Ok (`Assoc [ ("text", `String value) ]) ->
        let text = String.trim value in
        if text = "" then Error Blank
        else if String.length text > Dictation_limits.result_utf8_bytes then
          Error Over_budget
        else Ok text
    | Ok _ | Error (`Malformed _) -> Error (Provider Invalid_response)
    | Error (`Too_large _) -> Error Over_budget

(* A single HTTP exchange: Ox_ai.Transport.post_json has its own retry policy
   and discards non-2xx bodies. Neither is suitable for attempt evidence. *)
let run ~caps ~sw ~net ~clock ~logger:_ ~model ~prompt_template ~transcript
    ?audio ?audio_format ?provider_order ?(on_response = fun _ -> ())
    ?(glossary = "") ?(language = "") ?(source_language = "")
    ?(target_language = "") ?(temperature = default_temperature)
    ?reasoning_effort ?output_language ~timeout () =
  let output_language = Option.value output_language ~default:language in
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
            ?audio_format ?provider_order ~output_language ~temperature
            ~reasoning_effort ()
        with
        | Error _ as error -> error
        | Ok json -> (
            let body = Ox_ai.Transport.bytes_of_json json in
            if Bytes.length body > max_request_bytes then
              Error (Provider Input_too_large)
            else
              let http = Process.make_http ~sw ~net ~clock in
              match
                Ox_http.Client.request http ~method_:"POST"
                  ~url:(responses_endpoint ~caps)
                  ~headers:
                    (("Content-Type", "application/json")
                    :: Ox_ai.Bearer.header ~api_key)
                  ~body ~deadline:timeout ~max_body_bytes:max_response_bytes ()
              with
              | Error (`Timeout | `Idle_timeout) -> Error Timeout
              | Error `Body_too_large -> Error (Provider Invalid_response)
              | Error (`Closed | `Connect _ | `Tls _ | `Protocol _) ->
                  Error (Provider Unavailable)
              | Ok response -> (
                  on_response response.body;
                  if response.status = 401 then Error (Provider Authentication)
                  else if response.status = 413 then
                    Error (Provider Input_too_large)
                  else if response.status < 200 || response.status >= 300 then
                    Error (Provider Unavailable)
                  else
                    match parse_body response.body with
                    | Error _ as error -> error
                    | Ok json when Openrouter.provider_failed json ->
                        Error (Provider Unavailable)
                    | Ok json -> (
                        match Responses.decode json with
                        | Error error -> Error (failure_of_ai error)
                        | Ok decoded -> (
                            match Responses.deliverable decoded with
                            | Error rejection ->
                                Error
                                  (Rejected (outcome_of_rejection rejection))
                            | Ok raw -> accept_text raw)))))

type upload = Wav of bytes | Mp3 of { mp3 : bytes; wav : bytes }

type retry = {
  first_diagnostic : string;
  final_diagnostic : string;
  audio_format : string option;
  reasoning_effort : string option;
}

type report = { result : (string, failure) result; retry : retry option }

let of_result result = { result; retry = None }

let run_with_retry ~caps ~sw ~net ~clock ~logger ~model ~prompt_template
    ~transcript ?upload ?provider_order ?(on_response = fun ~first:_ _ -> ())
    ?glossary ?language ?source_language ?target_language ?temperature
    ?reasoning_effort ?output_language ~timeout () =
  let retry_effort =
    match reasoning_effort with
    | Some ("medium" | "high" | "xhigh") -> reasoning_effort
    | _ -> Some "medium"
  in
  let call ~retry =
    let reasoning_effort = if retry then retry_effort else reasoning_effort in
    let audio, audio_format =
      match upload with
      | None -> (None, None)
      | Some (Wav wav) -> (Some wav, Some "wav")
      | Some (Mp3 { mp3; wav }) ->
          if retry then (Some wav, Some "wav") else (Some mp3, Some "mp3")
    in
    let response = ref None in
    let result =
      run ~caps ~sw ~net ~clock ~logger ~model ~prompt_template ~transcript
        ?audio ?audio_format ?provider_order
        ~on_response:(fun body -> response := Some body)
        ?glossary ?language ?source_language ?target_language ?temperature
        ?reasoning_effort ?output_language ~timeout ()
    in
    (result, !response)
  in
  match call ~retry:false with
  | ((Ok _ | Error (Rejected Cancelled)) as result), response ->
      Option.iter (on_response ~first:false) response;
      of_result result
  | Error failure, response ->
      (* Store the first failed body before starting the only retry, so a
         cancelled retry cannot lose evidence of the completed first call. *)
      Option.iter (on_response ~first:true) response;
      let result, response = call ~retry:true in
      Option.iter (on_response ~first:false) response;
      let final_diagnostic =
        match result with
        | Ok _ -> "stt:ok"
        | Error failure -> diagnostic failure
      in
      {
        result;
        retry =
          Some
            {
              first_diagnostic = diagnostic failure;
              final_diagnostic;
              audio_format = Option.map (fun _ -> "wav") upload;
              reasoning_effort = retry_effort;
            };
      }
