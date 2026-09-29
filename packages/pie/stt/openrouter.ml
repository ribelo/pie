type error = [ `Stt_error of string ]

module Responses = Ox_ai_openrouter.Responses
module Openrouter_audio = Ox_ai_openrouter.Audio
module Redact = Ox_observability.Redact

let direct_transcription_timeout = Ox.Duration.seconds 60
let response_operation_timeout = Ox.Duration.seconds 120

let resolve_api_key ~caps () =
  match Ox_caps.Cap_sys.getenv_opt ~caps "OPENROUTER_API_KEY" with
  | Some value when String.trim value <> "" ->
      Some (Redact.make (String.trim value))
  | _ -> None

let endpoint ~caps path =
  match Ox_caps.Cap_sys.getenv_opt ~caps "OPENROUTER_BASE_URL" with
  | Some base when String.trim base <> "" ->
      let base = String.trim base in
      let rec trim n =
        if n > 0 && base.[n - 1] = '/' then trim (n - 1) else n
      in
      Some (String.sub base 0 (trim (String.length base)) ^ path)
  | _ -> None

(* Never render provider-owned text: Invalid_response.got and refusal text can
   contain credentials, prompts or transcripts. *)
let ai_error_message : Ox_ai.Error.t -> string = function
  | `Unavailable _ -> "unavailable"
  | `Timeout -> "timeout"
  | `Authentication _ -> "authentication"
  | `Invalid_response _ -> "invalid response"
  | `Input_too_large _ -> "input too large"
  | `Model_mismatch _ -> "model mismatch"

let stt_error_of_ai error =
  `Stt_error ("OpenRouter request failed: " ^ ai_error_message error)

let base64_table =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode bytes =
  let len = Bytes.length bytes in
  let out = Buffer.create ((len + 2) / 3 * 4) in
  let enc n = base64_table.[n land 0x3f] in
  let rec loop i =
    if i >= len then ()
    else
      let b0 = Bytes.get_uint8 bytes i in
      let b1 = if i + 1 < len then Bytes.get_uint8 bytes (i + 1) else 0 in
      let b2 = if i + 2 < len then Bytes.get_uint8 bytes (i + 2) else 0 in
      Buffer.add_char out (enc (b0 lsr 2));
      Buffer.add_char out (enc (((b0 land 0x03) lsl 4) lor (b1 lsr 4)));
      Buffer.add_char out
        (if i + 1 < len then enc (((b1 land 0x0f) lsl 2) lor (b2 lsr 6))
         else '=');
      Buffer.add_char out (if i + 2 < len then enc b2 else '=');
      loop (i + 3)
  in
  loop 0;
  Buffer.contents out

let render_template template variables =
  List.fold_left
    (fun acc (key, value) ->
      let needle = "{{" ^ key ^ "}}" in
      let needle_len = String.length needle in
      let buf = Buffer.create (String.length acc) in
      let rec loop start =
        match String.index_from_opt acc start '{' with
        | None -> Buffer.add_substring buf acc start (String.length acc - start)
        | Some i
          when i + needle_len <= String.length acc
               && String.sub acc i needle_len = needle ->
            Buffer.add_substring buf acc start (i - start);
            Buffer.add_string buf value;
            loop (i + needle_len)
        | Some i ->
            Buffer.add_substring buf acc start (i - start + 1);
            loop (i + 1)
      in
      loop 0;
      Buffer.contents buf)
    template variables

let wav_bytes ~sample_rate pcm_bytes =
  let header =
    Audio.Pcm.build_pcm_wav_header ~data_length:(Bytes.length pcm_bytes)
      ~sample_rate
  in
  let wav = Bytes.create (Bytes.length header + Bytes.length pcm_bytes) in
  Bytes.blit header 0 wav 0 (Bytes.length header);
  Bytes.blit pcm_bytes 0 wav (Bytes.length header) (Bytes.length pcm_bytes);
  wav

let output_text_field = "text"

let structured_output task =
  let schema =
    `Assoc
      [
        ("type", `String "object");
        ( "properties",
          `Assoc [ (output_text_field, `Assoc [ ("type", `String "string") ]) ]
        );
        ("required", `List [ `String output_text_field ]);
        ("additionalProperties", `Bool false);
      ]
  in
  Responses.Json_schema
    { name = task ^ "_response"; schema; strict = Some true }

let decode_structured_output raw =
  if String.length raw > Dictation_limits.result_utf8_bytes then
    Error (`Stt_error "OpenRouter result budget exceeded")
  else
    try
      match Yojson.Safe.from_string (String.trim raw) with
      | `Assoc fields -> (
          match List.assoc_opt output_text_field fields with
          | Some (`String text) when String.trim text <> "" ->
              let text = String.trim text in
              if String.length text > Dictation_limits.result_utf8_bytes then
                Error (`Stt_error "OpenRouter result budget exceeded")
              else Ok text
          | _ ->
              Error (`Stt_error "OpenRouter structured response missing text"))
      | _ ->
          Error (`Stt_error "OpenRouter structured response must be an object")
    with Yojson.Json_error _ ->
      Error (`Stt_error "OpenRouter structured response was not valid JSON")

let outcome_rejection = function
  | `Not_completed "incomplete" -> "incomplete"
  | `Not_completed "cancelled" -> "cancelled"
  | `Not_completed _ -> "missing finish markers or non-completed status"
  | `Tool_continuation _ -> "tool continuation"
  | `Refusal _ -> "refusal"
  | `Other _ -> "unsupported output"

let run_audio_responses ~caps ~sw ~net ~clock ~logger ~model ~prompt
    ~sample_rate ~task ?thinking_effort pcm_bytes =
  if Bytes.length pcm_bytes > Dictation_limits.capture_pcm_bytes then
    Error (`Stt_error "OpenRouter captured audio budget exceeded")
  else
    match resolve_api_key ~caps () with
    | None ->
        Error (`Stt_error "Missing OpenRouter API key. Set OPENROUTER_API_KEY.")
    | Some api_key -> (
        let http = Process.make_http ~sw ~net ~clock in
        let request : Responses.request =
          {
            model;
            prompt;
            wav = wav_bytes ~sample_rate pcm_bytes;
            temperature = Some 0.0;
            text_format = Some (structured_output task);
            reasoning_effort = thinking_effort;
          }
        in
        match
          Responses.create ~http ~clock ~logger ~api_key
            ?endpoint:(endpoint ~caps "/api/v1/responses")
            ~deadline:response_operation_timeout request
        with
        | Error error -> Error (stt_error_of_ai error)
        | Ok response -> (
            match Responses.deliverable response with
            | Error rejection ->
                Error
                  (`Stt_error
                     ("OpenRouter outcome rejected: "
                     ^ outcome_rejection rejection))
            | Ok raw -> decode_structured_output raw))

(* A 64 MiB admitted PCM capture has a 44-byte WAV header and bounded multipart
   framing. The extra 8 KiB covers the model/language and transport field names. *)
let max_audio_request_bytes = Dictation_limits.capture_pcm_bytes + 44 + 8192

let transcribe ~caps ~sw ~net ~clock ~api_key ~model ~language
    ?(sample_rate = 16000) pcm_bytes =
  if Bytes.length pcm_bytes > Dictation_limits.capture_pcm_bytes then
    Error (`Stt_error "OpenRouter captured audio budget exceeded")
  else
    let http = Process.make_http ~sw ~net ~clock in
    let request : Openrouter_audio.request =
      {
        model;
        filename = "pie.wav";
        wav = wav_bytes ~sample_rate pcm_bytes;
        language = Some language;
        prompt = None;
        temperature = Some 0.0;
      }
    in
    match
      Openrouter_audio.create ~http ~api_key
        ?endpoint:(endpoint ~caps "/api/v1/audio/transcriptions")
        ~max_request_bytes:max_audio_request_bytes
        ~deadline:direct_transcription_timeout request
    with
    | Error error -> Error (stt_error_of_ai error)
    | Ok text ->
        if String.length text > Dictation_limits.result_utf8_bytes then
          Error (`Stt_error "OpenRouter result budget exceeded")
        else Ok text

let transcribe_with_prompt ~caps ~sw ~net ~clock ~logger ~model ~language
    ~prompt_template ?thinking_effort ?(sample_rate = 16000) pcm_bytes =
  let prompt = render_template prompt_template [ ("language", language) ] in
  run_audio_responses ~caps ~sw ~net ~clock ~logger ~model ~prompt ~sample_rate
    ~task:"transcription" ?thinking_effort pcm_bytes

let translate_with_prompt ~caps ~sw ~net ~clock ~logger ~model ~source_language
    ~target_language ~prompt_template ?thinking_effort ?(sample_rate = 16000)
    pcm_bytes =
  let prompt =
    render_template prompt_template
      [
        ("source_language", source_language);
        ("target_language", target_language);
      ]
  in
  run_audio_responses ~caps ~sw ~net ~clock ~logger ~model ~prompt ~sample_rate
    ~task:"translation" ?thinking_effort pcm_bytes
