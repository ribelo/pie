type error = [`Stt_error of string]

open Eta.Syntax

let api_url = "https://openrouter.ai/api/v1/audio/transcriptions"

let transcribe_file_impl ~process_mgr ~api_key ~model ~language file_path =
  let args = [
    "-s"; "-X"; "POST"; api_url;
    "-H"; "Authorization: Bearer " ^ api_key;
    "-F"; "file=@" ^ file_path;
    "-F"; "model=" ^ model;
    "-F"; "language=" ^ language;
    "-F"; "response_format=json"
  ] in
  let* raw = Sh.run_capture ~process_mgr ~prog:"curl" ~args in
  Eta.Effect.sync (fun () ->
    let json = Yojson.Safe.from_string raw in
    match json with
    | `Assoc fields ->
      (match List.assoc_opt "text" fields with
       | Some (`String text) -> Ok text
       | _ ->
         (match List.assoc_opt "error" fields with
          | Some (`Assoc err_fields) ->
            let msg = match List.assoc_opt "message" err_fields with
              | Some (`String m) -> m
              | _ -> "unknown OpenRouter error"
            in
            Error (`Stt_error msg)
          | _ -> Error (`Stt_error "unexpected response format")))
    | _ -> Error (`Stt_error "unexpected response format"))
  |> Eta.Effect.bind Eta.Effect.from_result

let transcribe ~process_mgr ~api_key ~model ~language pcm_bytes =
  let* tmp_wav =
    Eta.Effect.sync (fun () ->
      let tmp = Filename.temp_file "pie_stt" ".wav" in
      let header = Audio.Pcm.build_pcm_wav_header ~data_length:(Bytes.length pcm_bytes) ~sample_rate:16000 in
      let oc = open_out_bin tmp in
      output_bytes oc header;
      output_bytes oc pcm_bytes;
      close_out oc;
      tmp)
  in
  transcribe_file_impl ~process_mgr ~api_key ~model ~language tmp_wav
  |> Eta.Effect.finally
       (Eta.Effect.sync (fun () -> try Sys.remove tmp_wav with _ -> ()))

let chat_api_url () =
  let base =
    match Sys.getenv_opt "ERG_OPENROUTER_BASE_URL", Sys.getenv_opt "OPENROUTER_BASE_URL" with
    | Some v, _ when String.trim v <> "" -> String.trim v
    | _, Some v when String.trim v <> "" -> String.trim v
    | _ -> "https://openrouter.ai/api/v1"
  in
  let base =
    if String.length base > 0 && base.[String.length base - 1] = '/' then
      String.sub base 0 (String.length base - 1)
    else
      base
  in
  base ^ "/chat/completions"

let resolve_api_key () =
  match Sys.getenv_opt "ERG_OPENROUTER_API_KEY", Sys.getenv_opt "OPENROUTER_API_KEY" with
  | Some v, _ when String.trim v <> "" -> Some (String.trim v)
  | _, Some v when String.trim v <> "" -> Some (String.trim v)
  | _ -> None

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

let extract_message_content = function
  | `String s -> Some s
  | `List items ->
    let parts =
      List.filter_map
        (function
          | `Assoc fields ->
            (match List.assoc_opt "text" fields with
             | Some (`String s) -> Some s
             | _ -> None)
          | _ -> None)
        items
    in
    if parts = [] then None else Some (String.concat "" parts)
  | _ -> None

let decode_structured_field field raw =
  let trimmed = String.trim raw in
  try
    match Yojson.Safe.from_string trimmed with
    | `Assoc fields ->
      (match List.assoc_opt field fields with
       | Some (`String s) when String.trim s <> "" -> Ok (String.trim s)
       | _ -> Error (`Stt_error ("OpenRouter response missing " ^ field)))
    | _ -> Error (`Stt_error "OpenRouter structured response must be an object")
  with _ ->
    if trimmed <> "" then Ok trimmed else Error (`Stt_error "OpenRouter response was empty")

let run_audio_chat ~process_mgr ~model ~prompt ~sample_rate ~field pcm_bytes =
  match resolve_api_key () with
  | None -> Eta.Effect.fail (`Stt_error "Missing OpenRouter API key. Set ERG_OPENROUTER_API_KEY or OPENROUTER_API_KEY.")
  | Some api_key ->
    let wav_data = wav_bytes ~sample_rate pcm_bytes |> base64_encode in
    let schema =
      `Assoc [
        ("type", `String "object");
        ("properties", `Assoc [(field, `Assoc [("type", `String "string")])]);
        ("required", `List [`String field]);
        ("additionalProperties", `Bool false);
      ]
    in
    let body =
      `Assoc [
        ("model", `String model);
        ("temperature", `Int 0);
        ("messages", `List [
          `Assoc [
            ("role", `String "user");
            ("content", `List [
              `Assoc [("type", `String "text"); ("text", `String prompt)];
              `Assoc [
                ("type", `String "input_audio");
                ("input_audio", `Assoc [
                  ("data", `String wav_data);
                  ("format", `String "wav");
                ]);
              ];
            ]);
          ];
        ]);
        ("response_format", `Assoc [
          ("type", `String "json_schema");
          ("json_schema", `Assoc [
            ("name", `String (field ^ "_response"));
            ("strict", `Bool true);
            ("schema", schema);
          ]);
        ]);
        ("stream", `Bool false);
      ]
    in
    let args = [
      "-sS"; "-f"; "-X"; "POST"; chat_api_url ();
      "-H"; "Authorization: Bearer " ^ api_key;
      "-H"; "Content-Type: application/json";
      "-d"; Yojson.Safe.to_string body;
    ] in
    let* raw = Sh.run_capture ~process_mgr ~prog:"curl" ~args in
    Eta.Effect.sync (fun () ->
      try
        match Yojson.Safe.from_string raw with
        | `Assoc fields ->
          (match List.assoc_opt "choices" fields with
           | Some (`List (`Assoc choice_fields :: _)) ->
             (match List.assoc_opt "message" choice_fields with
              | Some (`Assoc message_fields) ->
                (match List.assoc_opt "content" message_fields with
                 | Some content ->
                   (match extract_message_content content with
                    | Some content -> decode_structured_field field content
                    | None -> Error (`Stt_error "OpenRouter response did not contain message content"))
                 | None -> Error (`Stt_error "OpenRouter response missing message content"))
              | _ -> Error (`Stt_error "OpenRouter response missing message"))
           | _ ->
             (match List.assoc_opt "error" fields with
              | Some (`Assoc err_fields) ->
                let msg =
                  match List.assoc_opt "message" err_fields with
                  | Some (`String s) -> s
                  | _ -> "unknown OpenRouter error"
                in
                Error (`Stt_error msg)
              | _ -> Error (`Stt_error "OpenRouter response missing choices")))
        | _ -> Error (`Stt_error "OpenRouter response must be an object")
      with exn -> Error (`Stt_error ("OpenRouter response parse failed: " ^ Printexc.to_string exn)))
    |> Eta.Effect.bind Eta.Effect.from_result

let transcribe_with_prompt ~process_mgr ~model ~language ~prompt_template ?(sample_rate = 16000) pcm_bytes =
  let prompt = render_template prompt_template [("language", language)] in
  run_audio_chat ~process_mgr ~model ~prompt ~sample_rate ~field:"transcription" pcm_bytes

let translate_with_prompt ~process_mgr ~model ~source_language ~target_language ~prompt_template ?(sample_rate = 16000) pcm_bytes =
  let prompt =
    render_template prompt_template [
      ("source_language", source_language);
      ("target_language", target_language);
    ]
  in
  run_audio_chat ~process_mgr ~model ~prompt ~sample_rate ~field:"translation" pcm_bytes
