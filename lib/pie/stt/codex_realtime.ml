type error = [`Stt_error of string]

module Http = Eta_http
module Http_eio = Eta_http_eio
module Stream = Eta_stream

open Eta.Syntax

type mode =
  | Transcription
  | Translation
  | Conversation

type event =
  | Session of string
  | Transcript_delta of string
  | Transcript_done of string
  | Response_done
  | Server_error of { message : string; code : string option }
  | Other of string

let realtime_sample_rate = 24_000
let default_base_url = "wss://api.openai.com"
let realtime_version = "0.0.0"
let default_conversation_model = "gpt-realtime-2.1"
let oauth_response_idle_timeout = Http.Request.Response_idle_timeout.of_ms 30_000
let websocket_establishment_timeout = Eta.Duration.seconds 10

let is_conversation_model model =
  model = "gpt-realtime-2" || model = default_conversation_model

let transcription_delta_types = [
  "conversation.item.input_audio_transcription.delta";
  "conversation.input_transcript.delta";
  "conversation.output_transcript.delta";
  "session.input_transcript.delta";
  "session.output_transcript.delta";
  "response.output_text.delta";
  "response.output_audio_transcript.delta";
]

let transcript_done_types = [
  "conversation.item.input_audio_transcription.completed";
  "response.output_audio_transcript.done";
  "response.output_text.done";
  "conversation.output_transcript.done";
]

let session_event_types = [
  "session.created";
  "session.updated";
  "transcription_session.created";
  "transcription_session.updated";
]

let member name fields = List.assoc_opt name fields

let string_member name fields =
  match member name fields with
  | Some (`String value) -> Some value
  | _ -> None

let strip_trailing_slashes s =
  let rec last_non_slash i =
    if i < 0 then -1
    else if s.[i] = '/' then last_non_slash (i - 1)
    else i
  in
  let last = last_non_slash (String.length s - 1) in
  if last < 0 then "" else String.sub s 0 (last + 1)

let resolve_base_url () =
  match Sys.getenv_opt "CODEX_REALTIME_BASE_URL_OVERRIDE" with
  | Some value when String.trim value <> "" -> strip_trailing_slashes (String.trim value)
  | _ -> default_base_url

let path_for_mode = function
  | Translation -> "/v1/realtime/translations"
  | Transcription
  | Conversation -> "/v1/realtime"

let http_scheme_stand_in url =
  if String.starts_with ~prefix:"wss://" url then
    ("wss://", "https://" ^ String.sub url 6 (String.length url - 6))
  else if String.starts_with ~prefix:"ws://" url then
    ("ws://", "http://" ^ String.sub url 5 (String.length url - 5))
  else
    ("", url)

let restore_scheme prefix http_url =
  match prefix with
  | "wss://" -> "wss://" ^ String.sub http_url 8 (String.length http_url - 8)
  | "ws://" -> "ws://" ^ String.sub http_url 7 (String.length http_url - 7)
  | _ -> http_url

let build_realtime_url ?(base_url = resolve_base_url ()) ~mode ~model () =
  let base_url = strip_trailing_slashes base_url in
  let path =
    if is_conversation_model model then "/v1/realtime" else path_for_mode mode
  in
  let scheme, http_base = http_scheme_stand_in base_url in
  match Http.Core.Url.parse (http_base ^ path) with
  | Error parse_error ->
    invalid_arg (Http.Core.Url.parse_error_to_string parse_error)
  | Ok url ->
    let url =
      if is_conversation_model model then Http.Core.Url.with_query_param url "model" model
      else
        match mode with
        | Transcription -> Http.Core.Url.with_query_param url "intent" "transcription"
        | Translation | Conversation -> Http.Core.Url.with_query_param url "model" model
    in
    restore_scheme scheme (Http.Core.Url.to_string url)

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

let build_audio_append ?(mode = Transcription) pcm_bytes =
  let frame_type =
    match mode with
    | Translation -> "session.input_audio_buffer.append"
    | Transcription
    | Conversation -> "input_audio_buffer.append"
  in
  `Assoc [
    ("type", `String frame_type);
    ("audio", `String (Openrouter.base64_encode pcm_bytes));
  ]

let build_audio_commit () =
  `Assoc [("type", `String "input_audio_buffer.commit")]

let build_transcription_session_update ?prompt ~model () =
  let transcription_fields =
    [("model", `String model)]
    @ (match prompt with Some p -> [("prompt", `String p)] | None -> [])
  in
  `Assoc [
    ("type", `String "session.update");
    ("session", `Assoc [
      ("type", `String "transcription");
      ("audio", `Assoc [
        ("input", `Assoc [
          ("format", `Assoc [("type", `String "audio/pcm"); ("rate", `Int realtime_sample_rate)]);
          ("transcription", `Assoc transcription_fields);
        ]);
      ]);
    ]);
  ]

let build_translation_session_update ?prompt ?target_language () =
  let instructions =
    match prompt with
    | Some p -> Some p
    | None ->
      Option.map
        (fun lang -> Printf.sprintf "Translate the spoken audio into %s." lang)
        target_language
  in
  let session_fields =
    (match instructions with
     | Some i -> [("instructions", `String i)]
     | None -> [])
    @ (match target_language with
       | None -> []
       | Some language ->
         [("audio", `Assoc [("output", `Assoc [("language", `String language)])])])
  in
  `Assoc [("type", `String "session.update"); ("session", `Assoc session_fields)]

let build_conversation_session_update ~model ~prompt =
  `Assoc [
    ("type", `String "session.update");
    ("session", `Assoc [
      ("type", `String "realtime");
      ("model", `String model);
      ("output_modalities", `List [`String "text"]);
      ("instructions", `String prompt);
      ("audio", `Assoc [
        ("input", `Assoc [
          ("format", `Assoc [("type", `String "audio/pcm"); ("rate", `Int realtime_sample_rate)]);
          ("turn_detection", `Null);
        ]);
      ]);
    ]);
  ]

let build_conversation_response_create ~prompt =
  `Assoc [
    ("type", `String "response.create");
    ("response", `Assoc [
      ("output_modalities", `List [`String "text"]);
      ("instructions", `String prompt);
    ]);
  ]

let parse_event payload =
  try
    match Yojson.Safe.from_string payload with
    | `Assoc fields ->
      (match string_member "type" fields with
       | None -> Error (`Stt_error "Codex realtime event missing type")
       | Some "error" ->
         let message, code =
           match member "error" fields with
           | Some (`Assoc error_fields) ->
             let message =
               match string_member "message" error_fields, string_member "message" fields with
               | Some value, _ -> value
               | None, Some value -> value
               | None, None -> "Codex realtime server returned an unspecified error"
             in
             (message, string_member "code" error_fields)
           | _ ->
             (match string_member "message" fields with
              | Some value -> (value, None)
              | None -> ("Codex realtime server returned an unspecified error", None))
         in
         Ok (Server_error { message; code })
       | Some event_type when List.mem event_type transcription_delta_types ->
         (match string_member "delta" fields with
          | Some delta -> Ok (Transcript_delta delta)
          | None -> Error (`Stt_error "Codex realtime delta event missing delta"))
       | Some event_type when List.mem event_type transcript_done_types ->
         (match string_member "transcript" fields, string_member "text" fields with
          | Some text, _
          | None, Some text -> Ok (Transcript_done text)
          | None, None -> Error (`Stt_error "Codex realtime done event missing transcript text"))
       | Some "response.done" -> Ok Response_done
       | Some event_type when List.mem event_type session_event_types -> Ok (Session event_type)
       | Some event_type -> Ok (Other event_type))
    | _ -> Error (`Stt_error "Codex realtime event must be a JSON object")
  with Yojson.Json_error msg ->
    Error (`Stt_error ("Codex realtime event is not valid JSON: " ^ msg))

let get_int16_le bytes offset =
  let value = Bytes.get_uint16_le bytes offset in
  if value land 0x8000 = 0 then value else value - 0x10000

let set_int16_le bytes offset value =
  Bytes.set_int16_le bytes offset (max (-32768) (min 32767 value))

let resample_s16le_mono ~from_rate ~to_rate pcm_bytes =
  if from_rate <= 0 || to_rate <= 0 then
    invalid_arg "resample_s16le_mono: sample rates must be positive";
  if from_rate = to_rate then pcm_bytes
  else
    let input_samples = Bytes.length pcm_bytes / 2 in
    if input_samples = 0 then Bytes.empty
    else
      let output_samples = max 1 ((input_samples * to_rate) / from_rate) in
      let out = Bytes.create (output_samples * 2) in
      for i = 0 to output_samples - 1 do
        let source_num = i * from_rate in
        let source_idx = min (input_samples - 1) (source_num / to_rate) in
        let next_idx = min (input_samples - 1) (source_idx + 1) in
        let rem = source_num mod to_rate in
        let a = get_int16_le pcm_bytes (source_idx * 2) in
        let b = get_int16_le pcm_bytes (next_idx * 2) in
        let sample = ((a * (to_rate - rem)) + (b * rem)) / to_rate in
        set_int16_le out (i * 2) sample
      done;
      out

let stt_error message = Eta.Effect.fail (`Stt_error message)

let http_error_message context error =
  context
  ^ ": "
  ^ Http.Error.category_to_string (Http.Error.category error)
  ^ " "
  ^ Http.Error.to_string error

let token_refresh_failure error =
  match Http.Error.status error with
  | Some (401 | 403) ->
    `Stt_error
      (Printf.sprintf
         "Codex token refresh failed with HTTP %d. Run 'codex login' to re-authenticate. details=%s"
         (Option.value (Http.Error.status error) ~default:0)
         (Option.value (Http.Error.buffered_body error) ~default:""
          |> fun raw -> String.sub raw 0 (min 200 (String.length raw))))
  | Some status ->
    let raw = Option.value (Http.Error.buffered_body error) ~default:"" in
    `Stt_error
      (Printf.sprintf
         "Codex token refresh failed with HTTP %d. Retry later; the Codex auth service may be temporarily unavailable. details=%s"
         status
         (String.sub raw 0 (min 200 (String.length raw))))
  | None -> `Stt_error (http_error_message "Codex token refresh" error)

let ws_error_message = function
  | `Connect message -> "connect: " ^ message
  | `Upgrade_failed failure ->
    Printf.sprintf "upgrade failed with HTTP %d" failure.Http_eio.Ws.Client.status
  | `Closed (code, reason) -> Printf.sprintf "closed (%d): %s" code reason
  | `Protocol message -> "protocol: " ^ message
  | `Timeout -> "timeout"

let headers pairs = Http.Core.Header.unsafe_of_list pairs

let make_realtime_session_id () =
  Printf.sprintf
    "pie-%Lx-%06x"
    (Int64.of_float (Unix.gettimeofday () *. 1000.))
    (Random.bits () land 0xffffff)

let realtime_header_pairs ?session_id access_token =
  let pairs = [
    ("Authorization", "Bearer " ^ access_token);
    ("originator", "codex_cli_rs");
    ("User-Agent", "codex_cli_rs/0.0.0 (pie)");
    ("version", realtime_version);
  ] in
  match session_id with
  | Some session_id when String.trim session_id <> "" ->
    ("x-session-id", session_id) :: pairs
  | _ -> pairs

let realtime_headers ?session_id access_token =
  headers (realtime_header_pairs ?session_id access_token)

let realtime_rest_client ~sw ~net = Process.http_client ~sw ~net

let read_fetched_body body =
  Http.Body.Stream.read_all body
  |> Eta.Effect.bind_error (fun error ->
    stt_error (http_error_message "failed to read Codex HTTP response body" error))
  |> Eta.Effect.map Bytes.to_string

module Codex_auth = struct
  type tokens = {
    access_token : string;
    refresh_token : string option;
    id_token : string option;
  }

  type t = {
    path : string;
    json : Yojson.Safe.t;
    tokens : tokens;
  }

  let refresh_token_url = "https://auth.openai.com/oauth/token"
  let client_id = "app_EMoamEEZ73f0CkXaXp7hrann"
  let refresh_scope = "openid profile email"

  let codex_home () =
    match Sys.getenv_opt "CODEX_HOME" with
    | Some value when String.trim value <> "" -> String.trim value
    | _ ->
      match Sys.getenv_opt "HOME" with
      | Some home when String.trim home <> "" -> Filename.concat home ".codex"
      | _ -> ".codex"

  let auth_path () = Filename.concat (codex_home ()) "auth.json"

  let read_file path =
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
         let len = in_channel_length ic in
         really_input_string ic len)

  let load ?(path = auth_path ()) () =
    Eta.Effect.sync (fun () ->
        try
          let raw = read_file path in
          match Yojson.Safe.from_string raw with
          | `Assoc fields as json ->
            (match List.assoc_opt "tokens" fields with
             | Some (`Assoc token_fields) ->
               (match string_member "access_token" token_fields with
                | Some access_token when String.trim access_token <> "" ->
                  Ok {
                    path;
                    json;
                    tokens = {
                      access_token;
                      refresh_token = string_member "refresh_token" token_fields;
                      id_token = string_member "id_token" token_fields;
                    };
                  }
                | _ ->
                  Error (`Stt_error
                           ("Codex auth.json at " ^ path
                            ^ " is malformed or missing tokens.access_token. Run 'codex login' to recreate it.")))
             | _ ->
               Error (`Stt_error
                        ("Codex auth.json at " ^ path
                         ^ " is malformed or missing tokens. Run 'codex login' to recreate it.")))
          | _ -> Error (`Stt_error ("Codex auth.json at " ^ path ^ " must be a JSON object."))
        with
        | Sys_error message ->
          Error (`Stt_error
                   ("Codex auth.json not found/readable at " ^ path
                    ^ ": " ^ message ^ ". Run 'codex login' or set CODEX_HOME."))
        | Yojson.Json_error message ->
          Error (`Stt_error ("Codex auth.json at " ^ path ^ " is not valid JSON: " ^ message)))
    |> Eta.Effect.flatten_result

  let base64_value = function
    | 'A' .. 'Z' as c -> Some (Char.code c - Char.code 'A')
    | 'a' .. 'z' as c -> Some (26 + Char.code c - Char.code 'a')
    | '0' .. '9' as c -> Some (52 + Char.code c - Char.code '0')
    | '+' -> Some 62
    | '/' -> Some 63
    | '=' -> Some 0
    | _ -> None

  let base64url_decode input =
    let normalized =
      input
      |> String.map (function '-' -> '+' | '_' -> '/' | c -> c)
    in
    let pad = (4 - (String.length normalized mod 4)) mod 4 in
    let padded = normalized ^ String.make pad '=' in
    let out = Buffer.create ((String.length padded / 4) * 3) in
    let rec loop i =
      if i >= String.length padded then Ok (Buffer.contents out)
      else
        match
          base64_value padded.[i],
          base64_value padded.[i + 1],
          base64_value padded.[i + 2],
          base64_value padded.[i + 3]
        with
        | Some a, Some b, Some c, Some d ->
          let n = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
          Buffer.add_char out (Char.chr ((n lsr 16) land 0xff));
          if padded.[i + 2] <> '=' then Buffer.add_char out (Char.chr ((n lsr 8) land 0xff));
          if padded.[i + 3] <> '=' then Buffer.add_char out (Char.chr (n land 0xff));
          loop (i + 4)
        | _ -> Error "invalid base64url"
    in
    if String.length padded mod 4 <> 0 then Error "invalid base64url length" else loop 0

  let decode_jwt_exp_seconds token =
    match String.split_on_char '.' token with
    | _header :: payload :: _ ->
      (match base64url_decode payload with
       | Error _ -> None
       | Ok json ->
         (try
            match Yojson.Safe.from_string json with
            | `Assoc fields ->
              (match List.assoc_opt "exp" fields with
               | Some (`Int exp) -> Some exp
               | Some (`Intlit exp) -> int_of_string_opt exp
               | _ -> None)
            | _ -> None
          with _ -> None))
    | _ -> None

  let is_expired ?(leeway_seconds = 60) ~now token =
    match decode_jwt_exp_seconds token with
    | None -> false
    | Some exp -> (now /. 1000.) +. float_of_int leeway_seconds >= float_of_int exp

  let update_assoc key value fields =
    let rec loop acc = function
      | [] -> List.rev ((key, value) :: acc)
      | (k, _) :: rest when k = key -> List.rev_append acc ((key, value) :: rest)
      | field :: rest -> loop (field :: acc) rest
    in
    loop [] fields

  let persisted_json auth refreshed =
    match auth.json with
    | `Assoc fields ->
      let token_fields =
        match List.assoc_opt "tokens" fields with
        | Some (`Assoc token_fields) -> token_fields
        | _ -> []
      in
      let token_fields =
        update_assoc "access_token" (`String refreshed.access_token) token_fields
      in
      let token_fields =
        match refreshed.refresh_token with
        | None -> token_fields
        | Some value -> update_assoc "refresh_token" (`String value) token_fields
      in
      let token_fields =
        match refreshed.id_token with
        | None -> token_fields
        | Some value -> update_assoc "id_token" (`String value) token_fields
      in
      `Assoc (update_assoc "tokens" (`Assoc token_fields) fields)
    | json -> json

  let write auth refreshed =
    Eta.Effect.sync (fun () ->
        try
          let oc =
            open_out_gen
              [Open_wronly; Open_creat; Open_trunc; Open_binary]
              0o600
              auth.path
          in
          Fun.protect
            ~finally:(fun () -> close_out_noerr oc)
            (fun () ->
               output_string oc (Yojson.Safe.pretty_to_string (persisted_json auth refreshed));
               output_char oc '\n';
               Ok ())
        with Sys_error message ->
          Error (`Stt_error ("Failed to persist refreshed Codex auth at " ^ auth.path ^ ": " ^ message)))
    |> Eta.Effect.flatten_result

  let refresh_endpoint () =
    match Sys.getenv_opt "CODEX_REFRESH_TOKEN_URL_OVERRIDE" with
    | Some value when String.trim value <> "" -> String.trim value
    | _ -> refresh_token_url

  let refresh ~client auth refresh_token =
    let body =
      `Assoc [
        ("client_id", `String client_id);
        ("grant_type", `String "refresh_token");
        ("refresh_token", `String refresh_token);
        ("scope", `String refresh_scope);
      ]
    in
    let url = refresh_endpoint () in
    let url =
      match Http.Core.Url.parse url with
      | Error parse_error ->
        invalid_arg (Http.Core.Url.parse_error_to_string parse_error)
      | Ok parsed -> Http.Core.Url.to_string parsed
    in
    let request =
      Http.Request.make
        ~headers:(headers [("Content-Type", "application/json"); ("Accept", "application/json")])
        ~body:(Http.Request.Fixed [Bytes.of_string (Yojson.Safe.to_string body)])
        ~response_idle_timeout:oauth_response_idle_timeout
        "POST"
        url
    in
    let* raw =
      Http.Client.fetch client request
      |> Eta.Effect.bind_error (fun error -> Eta.Effect.fail (token_refresh_failure error))
      |> Eta.Effect.bind (fun (_response, body) -> read_fetched_body body)
    in
      let* tokens =
        Eta.Effect.sync (fun () ->
            try
              match Yojson.Safe.from_string raw with
              | `Assoc fields ->
                (match string_member "access_token" fields with
                 | Some access_token when String.trim access_token <> "" ->
                   Ok {
                     access_token;
                     refresh_token = string_member "refresh_token" fields;
                     id_token = string_member "id_token" fields;
                   }
                 | _ -> Error (`Stt_error "Codex token refresh response missing access_token"))
              | _ -> Error (`Stt_error "Codex token refresh response must be a JSON object")
            with Yojson.Json_error message ->
              Error (`Stt_error ("Codex token refresh response was not valid JSON: " ^ message)))
        |> Eta.Effect.flatten_result
      in
      let* () = write auth tokens in
      Eta.Effect.pure tokens.access_token

  let access_token ~client () =
    let* auth = load () in
    let now = Unix.gettimeofday () *. 1000. in
    if not (is_expired ~now auth.tokens.access_token) then
      Eta.Effect.pure auth.tokens.access_token
    else
      match auth.tokens.refresh_token with
      | Some refresh_token when String.trim refresh_token <> "" ->
        refresh ~client auth refresh_token
      | _ ->
        stt_error
          ("Codex access token at " ^ auth.path
           ^ " is expired and no refresh_token is stored. Run 'codex login'.")
end

let realtime_access_token ~client () =
  match Sys.getenv_opt "OPENAI_API_KEY" with
  | Some value when String.trim value <> "" ->
    Eta.Effect.pure (String.trim value)
  | _ -> Codex_auth.access_token ~client ()

type session_result = {
  final : string option;
  deltas : string list;
  error : string option;
  session_seen : bool;
}

let empty_session_result = { final = None; deltas = []; error = None; session_seen = false }

let close_ws_ignore ws =
  Http_eio.Ws.Client.close ws |> Eta.Effect.ignore_errors

let with_websocket_establishment_timeout effect =
  effect
  |> Eta.Effect.timeout_as
       websocket_establishment_timeout
       ~on_timeout:(`Stt_error "Codex realtime WebSocket establishment timed out after 10 seconds")

let connect_realtime_ws ~headers ~sw ~net url =
  Http_eio.Ws.Client.connect ~headers ~sw ~net url
  |> Eta.Effect.bind_error (fun error ->
    stt_error ("Codex realtime WebSocket connect failed: " ^ ws_error_message error))
  |> with_websocket_establishment_timeout

let debug_realtime_payload direction payload =
  match Sys.getenv_opt "CAMELPIE_CODEX_REALTIME_DEBUG" with
  | Some value when String.trim value <> "" ->
    let payload =
      if String.length payload <= 2_000 then
        payload
      else
        String.sub payload 0 2_000
        ^ Printf.sprintf "... <truncated %d bytes>" (String.length payload)
    in
    Printf.eprintf "codex realtime %s: %s\n%!" direction payload
  | _ -> ()

let event_should_stop ~mode = function
  | Server_error _
  | Response_done -> true
  | Transcript_done _ ->
    (match mode with
     | Conversation -> false
     | Transcription
     | Translation -> true)
  | Session _
  | Transcript_delta _
  | Other _ -> false

let receive_should_stop ~mode = function
  | `Binary _ -> Eta.Effect.pure true
  | `Text payload ->
    match parse_event payload with
    | Error _ -> Eta.Effect.pure true
    | Ok event -> Eta.Effect.pure (event_should_stop ~mode event)

let receive_result ~mode ws =
  let step state = function
    | `Binary _ ->
      Eta.Effect.pure { state with error = Some "Codex realtime sent a binary WebSocket message" }
    | `Text payload ->
      debug_realtime_payload "recv" payload;
      match parse_event payload with
      | Error (`Stt_error message) ->
        Eta.Effect.pure { state with error = Some ("Failed to parse Codex realtime event: " ^ message) }
      | Ok (Server_error { message; code }) ->
        let detail =
          match code with
          | None -> message
          | Some code -> message ^ " (" ^ code ^ ")"
        in
        Eta.Effect.pure { state with error = Some ("Codex realtime server error: " ^ detail) }
      | Ok (Transcript_delta delta) ->
        Eta.Effect.pure { state with deltas = delta :: state.deltas }
      | Ok (Transcript_done transcript) ->
        Eta.Effect.pure { state with final = Some transcript }
      | Ok Response_done ->
        Eta.Effect.pure state
      | Ok (Session _ | Other _) ->
        Eta.Effect.pure state
  in
  Stream.run
    (Http_eio.Ws.Client.incoming ws |> Stream.Stream.take_until_effect (receive_should_stop ~mode))
    (Stream.Sink.fold_effect step empty_session_result)
  |> Eta.Effect.bind_error (fun error ->
    stt_error ("Codex realtime WebSocket receive failed: " ^ ws_error_message error))

let initial_receive_should_stop = function
  | `Binary _ -> Eta.Effect.pure true
  | `Text payload ->
    match parse_event payload with
    | Error _ -> Eta.Effect.pure true
    | Ok (Server_error _ | Session _) -> Eta.Effect.pure true
    | Ok (Transcript_delta _ | Transcript_done _ | Response_done | Other _) ->
      Eta.Effect.pure false

let await_session_ready ws =
  let step state = function
    | `Binary _ ->
      Eta.Effect.pure { state with error = Some "Codex realtime sent a binary WebSocket message" }
    | `Text payload ->
      debug_realtime_payload "recv" payload;
      match parse_event payload with
      | Error (`Stt_error message) ->
        Eta.Effect.pure { state with error = Some ("Failed to parse Codex realtime event: " ^ message) }
      | Ok (Server_error { message; code }) ->
        let detail =
          match code with
          | None -> message
          | Some code -> message ^ " (" ^ code ^ ")"
        in
        Eta.Effect.pure { state with error = Some ("Codex realtime server error: " ^ detail) }
      | Ok (Session _) -> Eta.Effect.pure { state with session_seen = true }
      | Ok (Transcript_delta _ | Transcript_done _ | Response_done | Other _) ->
        Eta.Effect.pure state
  in
  let receive =
    Stream.run
      (Http_eio.Ws.Client.incoming ws |> Stream.Stream.take_until_effect initial_receive_should_stop)
      (Stream.Sink.fold_effect step empty_session_result)
    |> Eta.Effect.bind_error (fun error ->
      stt_error ("Codex realtime WebSocket receive failed: " ^ ws_error_message error))
  in
  let* result = receive in
  match result.error with
  | Some message -> stt_error message
  | None when result.session_seen -> Eta.Effect.unit
  | None -> stt_error "Codex realtime WebSocket closed before session.updated"

let send_text ?(ignore_closing = fun () -> false) ws payload =
  debug_realtime_payload "send" payload;
  Http_eio.Ws.Client.send_text ws payload
  |> Eta.Effect.bind_error (fun error ->
    match error with
    | `Closed _ when ignore_closing () ->
      Eta.Effect.unit
    | _ ->
      stt_error ("Codex realtime WebSocket send failed: " ^ ws_error_message error))

let send_json ?ignore_closing ws json =
  send_text ?ignore_closing ws (Yojson.Safe.to_string json)

let send_audio_chunks ?ignore_closing ws ~mode pcm_bytes =
  let len = Bytes.length pcm_bytes in
  let rec loop offset =
    if offset >= len then Eta.Effect.unit
    else
      let chunk_len = min 32_000 (len - offset) in
      let chunk = Bytes.sub pcm_bytes offset chunk_len in
      let* () = send_json ?ignore_closing ws (build_audio_append ~mode chunk) in
      loop (offset + chunk_len)
  in
  loop 0

let receive_result_marking_done ~mode ws receive_done =
  let mark_done =
    Eta.Effect.sync (fun () -> Atomic.set receive_done true)
  in
  let cleanup =
    let* () = mark_done in
    close_ws_ignore ws
  in
  receive_result ~mode ws |> Eta.Effect.finally cleanup

let finish_result result =
  match result.error with
  | Some message -> stt_error message
  | None ->
    match result.final with
    | Some text -> Eta.Effect.pure text
    | None ->
      let text = result.deltas |> List.rev |> String.concat "" |> String.trim in
      Eta.Effect.pure text

let run_realtime_session
    ~sw
    ~net
    ~url
    ~access_token
    ~mode
    ?session_update
    ?response_create
    ?(translation_output_drain_ms = 2_000)
    ~input_sample_rate
    pcm_bytes =
  let pcm_bytes = resample_s16le_mono ~from_rate:input_sample_rate ~to_rate:realtime_sample_rate pcm_bytes in
  let session_id = make_realtime_session_id () in
  let* ws =
    connect_realtime_ws
      ~headers:(realtime_headers ~session_id access_token)
      ~sw
      ~net
      url
  in
  let receive_done = Atomic.make false in
  let ignore_closing () = Atomic.get receive_done in
  let* () =
    match session_update with
    | None -> Eta.Effect.unit
    | Some json ->
      let* () = send_json ~ignore_closing:(fun () -> true) ws json in
      await_session_ready ws
  in
  let send =
    let* () = send_audio_chunks ~ignore_closing ws ~mode pcm_bytes in
    match mode with
    | Translation ->
      Eta.Effect.delay
        (Eta.Duration.ms translation_output_drain_ms)
        (close_ws_ignore ws)
    | Transcription
    | Conversation ->
      let* () = send_json ~ignore_closing ws (build_audio_commit ()) in
      (match response_create with
       | None -> Eta.Effect.unit
       | Some json -> send_json ~ignore_closing ws json)
  in
  (* stttime-i0cx: an established realtime stream has no provider deadline. *)
  let receive = receive_result_marking_done ~mode ws receive_done in
  let* result, () = Eta.Effect.par receive send in
  let* () = close_ws_ignore ws in
  finish_result result

let run_realtime_stream_session
    ~sw
    ~net
    ~url
    ~access_token
    ~mode
    ?session_update
    ?response_create
    ?(translation_output_drain_ms = 2_000)
    ~input_sample_rate
    audio =
  let session_id = make_realtime_session_id () in
  let* ws =
    connect_realtime_ws
      ~headers:(realtime_headers ~session_id access_token)
      ~sw
      ~net
      url
  in
  let receive_done = Atomic.make false in
  let ignore_closing () = Atomic.get receive_done in
  let* () =
    match session_update with
    | None -> Eta.Effect.unit
    | Some json ->
      let* () = send_json ~ignore_closing:(fun () -> true) ws json in
      await_session_ready ws
  in
  let send_chunk () chunk =
    let chunk =
      resample_s16le_mono
        ~from_rate:input_sample_rate
        ~to_rate:realtime_sample_rate
        chunk
    in
    let* () = send_audio_chunks ~ignore_closing ws ~mode chunk in
    Eta.Effect.pure ()
  in
  let send =
    let* () = Stream.run audio (Stream.Sink.fold_effect send_chunk ()) in
    match mode with
    | Translation ->
      Eta.Effect.delay
        (Eta.Duration.ms translation_output_drain_ms)
        (close_ws_ignore ws)
    | Transcription
    | Conversation ->
      let* () = send_json ~ignore_closing ws (build_audio_commit ()) in
      (match response_create with
       | None -> Eta.Effect.unit
       | Some json -> send_json ~ignore_closing ws json)
  in
  let receive = receive_result_marking_done ~mode ws receive_done in
  let* result, () = Eta.Effect.par receive send in
  let* () = close_ws_ignore ws in
  finish_result result

module Async_completion = struct
  type ('a, 'err) phase =
    | Pending
    | Waiting of (('a, 'err) Eta.Exit.t -> unit)
    | Resolved of ('a, 'err) Eta.Exit.t
    | Done

  type ('a, 'err) t = {
    mutex : Mutex.t;
    mutable phase : ('a, 'err) phase;
    canceler : (unit, 'err) Eta.Effect.t;
  }

  let create ~canceler () =
    { mutex = Mutex.create (); phase = Pending; canceler }

  let with_lock completion f =
    Mutex.lock completion.mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock completion.mutex) f

  let resolve completion exit =
    let accepted, resume =
      with_lock completion @@ fun () ->
      match completion.phase with
      | Pending ->
        completion.phase <- Resolved exit;
        true, None
      | Waiting resume ->
        completion.phase <- Done;
        true, Some resume
      | Resolved _
      | Done -> false, None
    in
    Option.iter (fun resume -> resume exit) resume;
    accepted

  let await completion =
    Eta.Effect.async ~register:(fun resume ->
      let resolved =
        with_lock completion @@ fun () ->
        match completion.phase with
        | Pending ->
          completion.phase <- Waiting resume;
          None
        | Resolved exit ->
          completion.phase <- Done;
          Some exit
        | Waiting _
        | Done -> invalid_arg "Codex async completion may only be awaited once"
      in
      Option.iter resume resolved;
      Some
        (let* () =
           Eta.Effect.sync (fun () ->
             with_lock completion @@ fun () ->
             completion.phase <- Done)
         in
         completion.canceler))
end

type stream_handle = {
  offer_chunk : bytes -> (unit, error) Eta.Effect.t;
  finish_stream : unit -> (string, error) Eta.Effect.t;
  cancel_stream : (unit, error) Eta.Effect.t;
}

let with_stream_session
    ~sw
    ~net
    ~url
    ~access_token
    ~mode
    ?session_update
    ?response_create
    ?(sample_rate = 16000)
    ~cancel_message
    use =
  let queue = Eta.Queue.unbounded () in
  let cancel_stream =
    Eta.Queue.close_with_error_effect queue (`Stt_error cancel_message)
  in
  let completion = Async_completion.create ~canceler:cancel_stream () in
  let publish exit =
    Eta.Effect.sync (fun () -> ignore (Async_completion.resolve completion exit))
  in
  let audio = Stream.Stream.from_queue queue in
  let session =
    let* exit =
      run_realtime_stream_session
        ~sw
        ~net
        ~url
        ~access_token
        ~mode
        ?session_update
        ?response_create
        ~input_sample_rate:sample_rate
        audio
      |> Eta.Effect.to_exit
    in
    publish exit
  in
  let handle =
    {
      offer_chunk =
        (fun chunk ->
           Eta.Queue.send queue chunk
           |> Eta.Effect.ignore_errors);
      finish_stream =
        (fun () ->
           let* () = Eta.Queue.close_effect queue in
           Async_completion.await completion);
      cancel_stream;
    }
  in
  Eta.Effect.with_background ~name:"codex.realtime.stream" session (fun () -> use handle)

let with_transcription_stream
    ~sw
    ~net
    ~model
    ~language
    ~prompt_template
    ?(sample_rate = 16000)
    use =
  let client = realtime_rest_client ~sw ~net in
  let* access_token = realtime_access_token ~client () in
  let prompt = render_template prompt_template [ ("language", language) ] in
  if is_conversation_model model then
    with_stream_session
      ~sw
      ~net
      ~url:(build_realtime_url ~mode:Conversation ~model ())
      ~access_token
      ~mode:Conversation
      ~session_update:(build_conversation_session_update ~model ~prompt)
      ~response_create:(build_conversation_response_create ~prompt)
      ~sample_rate
      ~cancel_message:"Codex realtime transcription cancelled"
      use
  else
    with_stream_session
      ~sw
      ~net
      ~url:(build_realtime_url ~mode:Transcription ~model ())
      ~access_token
      ~mode:Transcription
      ~session_update:(build_transcription_session_update ~model ~prompt ())
      ~sample_rate
      ~cancel_message:"Codex realtime transcription cancelled"
      use

let with_translation_stream
    ~sw
    ~net
    ~model
    ~source_language
    ~target_language
    ~prompt_template
    ?(sample_rate = 16000)
    use =
  let client = realtime_rest_client ~sw ~net in
  let* access_token = realtime_access_token ~client () in
  let prompt =
    render_template prompt_template [
      ("source_language", source_language);
      ("target_language", target_language);
    ]
  in
  if is_conversation_model model then
    with_stream_session
      ~sw
      ~net
      ~url:(build_realtime_url ~mode:Conversation ~model ())
      ~access_token
      ~mode:Conversation
      ~session_update:(build_conversation_session_update ~model ~prompt)
      ~response_create:(build_conversation_response_create ~prompt)
      ~sample_rate
      ~cancel_message:"Codex realtime translation cancelled"
      use
  else
    with_stream_session
      ~sw
      ~net
      ~url:(build_realtime_url ~mode:Translation ~model ())
      ~access_token
      ~mode:Translation
      ~session_update:(build_translation_session_update ~prompt ~target_language ())
      ~sample_rate
      ~cancel_message:"Codex realtime translation cancelled"
      use

let transcribe_with_prompt ~sw ~net ~model ~language ~prompt_template ?(sample_rate = 16000) pcm_bytes =
  let client = realtime_rest_client ~sw ~net in
  let* access_token = realtime_access_token ~client () in
  let prompt = render_template prompt_template [ ("language", language) ] in
  if is_conversation_model model then
    run_realtime_session
      ~sw
      ~net
      ~url:(build_realtime_url ~mode:Conversation ~model ())
      ~access_token
      ~mode:Conversation
      ~session_update:(build_conversation_session_update ~model ~prompt)
      ~response_create:(build_conversation_response_create ~prompt)
      ~input_sample_rate:sample_rate
      pcm_bytes
  else
    run_realtime_session
      ~sw
      ~net
      ~url:(build_realtime_url ~mode:Transcription ~model ())
      ~access_token
      ~mode:Transcription
      ~session_update:(build_transcription_session_update ~model ~prompt ())
      ~input_sample_rate:sample_rate
      pcm_bytes

let translate_with_prompt
    ~sw
    ~net
    ~model
    ~source_language
    ~target_language
    ~prompt_template
    ?(sample_rate = 16000)
    pcm_bytes =
  let client = realtime_rest_client ~sw ~net in
  let* access_token = realtime_access_token ~client () in
  let prompt =
    render_template prompt_template [
      ("source_language", source_language);
      ("target_language", target_language);
    ]
  in
  if is_conversation_model model then
    run_realtime_session
      ~sw
      ~net
      ~url:(build_realtime_url ~mode:Conversation ~model ())
      ~access_token
      ~mode:Conversation
      ~session_update:(build_conversation_session_update ~model ~prompt)
      ~response_create:(build_conversation_response_create ~prompt)
      ~input_sample_rate:sample_rate
      pcm_bytes
  else
    run_realtime_session
      ~sw
      ~net
      ~url:(build_realtime_url ~mode:Translation ~model ())
      ~access_token
      ~mode:Translation
      ~session_update:(build_translation_session_update ~prompt ~target_language ())
      ~input_sample_rate:sample_rate
      pcm_bytes
