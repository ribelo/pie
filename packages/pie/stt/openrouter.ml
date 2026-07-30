type error = [`Stt_error of string]

module A = Eta_ai
module Http = Eta_http
module Http_eio = Eta_http_eio
module Openrouter = Eta_ai_openrouter

open Eta.Syntax

let direct_transcription_timeout = Eta.Duration.seconds 60
let response_operation_timeout = Eta.Duration.seconds 120

let with_direct_transcription_timeout effect =
  effect
  |> Eta.Effect.timeout_as
       direct_transcription_timeout
       ~on_timeout:(`Stt_error "OpenRouter transcription timed out after 60 seconds")

let with_response_operation_timeout effect =
  effect
  |> Eta.Effect.timeout_as
       response_operation_timeout
       ~on_timeout:(`Stt_error "OpenRouter response operation timed out after 120 seconds")

let resolve_api_key () =
  match Sys.getenv_opt "OPENROUTER_API_KEY" with
  | Some value when String.trim value <> "" -> Some (String.trim value)
  | _ -> None

let provider () =
  match Sys.getenv_opt "OPENROUTER_BASE_URL" with
  | Some value when String.trim value <> "" ->
    Openrouter.provider ~base_url:(String.trim value) ()
  | _ -> Openrouter.provider ()

let responses_provider () =
  match Sys.getenv_opt "OPENROUTER_BASE_URL" with
  | Some value when String.trim value <> "" ->
    Openrouter.responses_provider ~base_url:(String.trim value) ()
  | _ -> Openrouter.responses_provider ()

let http_client ~sw ~net =
  Http_eio.Client.make_h1_direct ~sw ~net ()

let ai_error_message = function
  | A.Eta_http_error error -> Http.Error.to_string error
  | Provider_error { provider; status; code; message; _ } ->
    let status =
      match status with
      | None -> ""
      | Some status -> Printf.sprintf " HTTP %d" status
    in
    let code =
      match code with
      | None -> ""
      | Some code -> " (" ^ code ^ ")"
    in
    Printf.sprintf "%s provider error%s%s: %s" provider status code message
  | Decode_error { provider; message; _ } ->
    Printf.sprintf "%s decode error: %s" provider message
  | Invalid_request { provider; message } ->
    Printf.sprintf "%s invalid request: %s" provider message
  | Invalid_tool { name; message } ->
    Printf.sprintf "invalid tool %s: %s" name message
  | Unsupported { provider; feature } ->
    Printf.sprintf "%s unsupported feature: %s" provider feature

let stt_error_of_ai error =
  `Stt_error ("OpenRouter request failed: " ^ ai_error_message error)

let base64_table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let base64_encode bytes =
  let len = Bytes.length bytes in
  let out = Buffer.create (((len + 2) / 3) * 4) in
  let enc n = base64_table.[n land 0x3f] in
  let rec loop i =
    if i >= len then ()
    else
      let b0 = Bytes.get_uint8 bytes i in
      let b1 = if i + 1 < len then Bytes.get_uint8 bytes (i + 1) else 0 in
      let b2 = if i + 2 < len then Bytes.get_uint8 bytes (i + 2) else 0 in
      Buffer.add_char out (enc (b0 lsr 2));
      Buffer.add_char out (enc (((b0 land 0x03) lsl 4) lor (b1 lsr 4)));
      Buffer.add_char out (if i + 1 < len then enc (((b1 land 0x0f) lsl 2) lor (b2 lsr 6)) else '=');
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
         | Some i when i + needle_len <= String.length acc && String.sub acc i needle_len = needle ->
           Buffer.add_substring buf acc start (i - start);
           Buffer.add_string buf value;
           loop (i + needle_len)
         | Some i ->
           Buffer.add_substring buf acc start (i - start + 1);
           loop (i + 1)
       in
       loop 0;
       Buffer.contents buf)
    template
    variables

let wav_bytes ~sample_rate pcm_bytes =
  let header = Audio.Pcm.build_pcm_wav_header ~data_length:(Bytes.length pcm_bytes) ~sample_rate in
  let wav = Bytes.create (Bytes.length header + Bytes.length pcm_bytes) in
  Bytes.blit header 0 wav 0 (Bytes.length header);
  Bytes.blit pcm_bytes 0 wav (Bytes.length header) (Bytes.length pcm_bytes);
  wav

let output_text_field = "text"

let structured_output task =
  let schema =
    `Assoc [
      ("type", `String "object");
      ("properties", `Assoc [(output_text_field, `Assoc [("type", `String "string")])]);
      ("required", `List [`String output_text_field]);
      ("additionalProperties", `Bool false);
    ]
  in
  {
    A.Responses.format =
      A.Responses.Json_schema
        {
          name = task ^ "_response";
          schema;
          strict = Some true;
        };
  }

let reasoning_for_effort = function
  | None -> Eta.Effect.pure None
  | Some effort ->
    (match Openrouter.reasoning ~effort () with
     | Ok reasoning -> Eta.Effect.pure (Some reasoning)
     | Error error -> Eta.Effect.fail (stt_error_of_ai error))

let extract_message_content = function
  | A.Assistant { content; _ }
  | A.User content
  | A.Tool { content; _ } ->
    let parts =
      List.filter_map
        (function
          | A.Text text
          | A.Json text -> Some text
          | A.Image _
          | A.Audio _
          | A.Video _ -> None)
        content
    in
    String.concat "" parts
  | A.System text -> text

let decode_structured_output raw =
  let trimmed = String.trim raw in
  try
    match Yojson.Safe.from_string trimmed with
    | `Assoc fields ->
      (match List.assoc_opt output_text_field fields with
       | Some (`String text) when String.trim text <> "" -> Ok (String.trim text)
       | _ -> Error (`Stt_error "OpenRouter structured response missing text"))
    | _ -> Error (`Stt_error "OpenRouter structured response must be an object")
  with _ ->
    Error (`Stt_error "OpenRouter structured response was not valid JSON")

let debug_response_content content raw =
  match Sys.getenv_opt "CAMELPIE_OPENROUTER_DEBUG" with
  | Some value when String.trim value <> "" ->
    let truncate value =
      if String.length value <= 1000 then value
      else String.sub value 0 1000 ^ "...<truncated>"
    in
    Printf.eprintf "openrouter responses content: %s\n%!" (truncate content);
    Option.iter
      (fun raw -> Printf.eprintf "openrouter responses raw: %s\n%!" (truncate raw))
      raw
  | _ -> ()

let responses_request ~model ~prompt ~sample_rate ~task pcm_bytes : A.tool A.Responses.request =
  {
    model;
    input =
      A.Responses.Messages
        [
          A.User
            [
              A.Text prompt;
              A.Audio
                {
                  data = A.Bytes (wav_bytes ~sample_rate pcm_bytes);
                  format = A.Wav;
                  transcript = None;
                };
            ];
        ];
    instructions = None;
    previous_response_id = None;
    store = None;
    include_ = [];
    tools = [];
    tool_choice = None;
    parallel_tool_calls = None;
    max_turns = None;
    max_output_tokens = None;
    temperature = Some 0.0;
    top_p = None;
    top_k = None;
    min_p = None;
    text = Some (structured_output task);
    reasoning = None;
    reasoning_effort = None;
    service_tier = None;
    user = None;
    prompt_cache_key = None;
    replay_items = [];
    stream = false;
  }

let run_audio_responses ~sw ~net ~model ~prompt ~sample_rate ~task ?thinking_effort pcm_bytes =
  (match resolve_api_key () with
   | None ->
     Eta.Effect.fail
       (`Stt_error "Missing OpenRouter API key. Set OPENROUTER_API_KEY.")
   | Some api_key ->
     let client = http_client ~sw ~net in
     let* reasoning = reasoning_for_effort thinking_effort in
     let request = responses_request ~model ~prompt ~sample_rate ~task pcm_bytes in
     let* raw =
       Openrouter.responses
         ~provider:(responses_provider ())
         ?reasoning
         client
         ~api_key:(A.api_key api_key)
         request
       |> Eta.Effect.map_error stt_error_of_ai
       |> Eta.Effect.map (fun response ->
         let content = extract_message_content response.A.message in
         debug_response_content content response.raw;
         content)
     in
     Eta.Effect.from_result (decode_structured_output raw))
  |> with_response_operation_timeout

let transcribe ~sw ~net ~api_key ~model ~language ?(sample_rate = 16000) pcm_bytes =
  (let client = http_client ~sw ~net in
   let request : A.Transcription.request =
     {
       model;
       file =
         {
           filename = "pie.wav";
           content_type = "audio/wav";
           data = wav_bytes ~sample_rate pcm_bytes;
         };
       language = Some language;
       prompt = None;
       response_format = None;
       temperature = Some 0.0;
       extra_fields = [];
     }
   in
   let* response =
     Openrouter.transcription
       ~provider:(provider ())
       client
       ~api_key:(A.api_key api_key)
       request
     |> Eta.Effect.map_error stt_error_of_ai
   in
   match response.A.Transcription.text with
   | Some text when String.trim text <> "" -> Eta.Effect.pure (String.trim text)
   | _ -> Eta.Effect.fail (`Stt_error "OpenRouter transcription response was empty"))
  |> with_direct_transcription_timeout

let transcribe_with_prompt
    ~sw
    ~net
    ~model
    ~language
    ~prompt_template
    ?thinking_effort
    ?(sample_rate = 16000)
    pcm_bytes =
  let prompt = render_template prompt_template [("language", language)] in
  run_audio_responses
    ~sw
    ~net
    ~model
    ~prompt
    ~sample_rate
    ~task:"transcription"
    ?thinking_effort
    pcm_bytes

let translate_with_prompt
    ~sw
    ~net
    ~model
    ~source_language
    ~target_language
    ~prompt_template
    ?thinking_effort
    ?(sample_rate = 16000)
    pcm_bytes =
  let prompt =
    render_template prompt_template [
      ("source_language", source_language);
      ("target_language", target_language);
    ]
  in
  run_audio_responses
    ~sw
    ~net
    ~model
    ~prompt
    ~sample_rate
    ~task:"translation"
    ?thinking_effort
    pcm_bytes
