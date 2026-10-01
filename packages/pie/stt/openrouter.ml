type error = [ `Stt_error of string ]

module Openrouter_audio = Ox_ai_openrouter.Audio
module Redact = Ox_observability.Redact

let provider_failed json =
  let fields = function `Assoc fields -> fields | _ -> [] in
  let has_error fields =
    match List.assoc_opt "error" fields with
    | None | Some `Null -> false
    | Some _ -> true
  in
  let fields = fields json in
  has_error fields
  || List.assoc_opt "status" fields = Some (`String "failed")
  ||
  match List.assoc_opt "choices" fields with
  | Some (`List choices) ->
      List.exists
        (fun choice ->
          let fields = match choice with `Assoc f -> f | _ -> [] in
          has_error fields
          || List.assoc_opt "finish_reason" fields = Some (`String "error"))
        choices
  | _ -> false

let direct_transcription_timeout = Ox.Duration.seconds 60

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

(* A 64 MiB admitted PCM capture has a 44-byte WAV header and bounded multipart
   framing. The extra 8 KiB covers the model, language and transport field
   names. The prompt length is added so a glossary is not refused locally. *)
let audio_request_bound ~language ~prompt =
  let extra =
    (match language with None -> 0 | Some text -> String.length text)
    + match prompt with None -> 0 | Some text -> String.length text
  in
  Dictation_limits.capture_pcm_bytes + 44 + 8192 + extra

let transcribe ~caps ~sw ~net ~clock ~api_key ~model ?language ?prompt
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
        language;
        prompt;
        temperature = Some 0.0;
      }
    in
    match
      Openrouter_audio.create ~http ~api_key
        ?endpoint:(endpoint ~caps "/api/v1/audio/transcriptions")
        ~max_request_bytes:(audio_request_bound ~language ~prompt)
        ~deadline:direct_transcription_timeout request
    with
    | Error error -> Error (stt_error_of_ai error)
    | Ok text ->
        if String.length text > Dictation_limits.result_utf8_bytes then
          Error (`Stt_error "OpenRouter result budget exceeded")
        else Ok text
