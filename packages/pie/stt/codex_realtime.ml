type error = [ `Stt_error of string ]

module Http = Eta_http
module Http_eio = Eta_http_eio
module Stream = Eta_stream
open Eta.Syntax

type mode = Transcription | Translation | Conversation

module First_token_timer = struct
  type t = { started_at_ms : int Atomic.t; emitted : bool Atomic.t }

  let create () =
    { started_at_ms = Atomic.make (-1); emitted = Atomic.make false }

  let start t ~at_ms = Atomic.set t.started_at_ms at_ms

  let observe t ~at_ms text =
    let started_at_ms = Atomic.get t.started_at_ms in
    if
      started_at_ms < 0
      || String.trim text = ""
      || not (Atomic.compare_and_set t.emitted false true)
    then None
    else Some (max 0 (at_ms - started_at_ms))
end

let string_of_mode = function
  | Transcription -> "transcription"
  | Translation -> "translation"
  | Conversation -> "conversation"

let log_first_token ~timer ~operation ~model ~mode text =
  let* at_ms = Eta.Effect.now_ms in
  match First_token_timer.observe timer ~at_ms text with
  | None -> Eta.Effect.unit
  | Some ttft_ms ->
      Eta_observability.log_info "stt first token"
        ~attrs:
          [
            ("stt.provider", "codex-realtime");
            ("stt.operation", operation);
            ("stt.model", model);
            ("stt.stream_mode", string_of_mode mode);
            ("stt.time_to_first_token_ms", string_of_int ttft_ms);
          ]

type terminal_status = Completed | Failed | Cancelled | Incomplete | Unknown

type event =
  | Created of string
  | Config_ack of string
  | Transcript_delta of string
  | Transcript_done of string
  | Transcript_final of string
  | Response_terminal of terminal_status
  | Server_error of { message : string; code : string option }
  | Other of string

let realtime_sample_rate = 24_000
let default_base_url = "wss://api.openai.com"
let realtime_version = "0.0.0"
let default_conversation_model = "gpt-realtime-2.1"

let oauth_response_idle_timeout =
  Http.Request.Response_idle_timeout.of_ms 30_000

let websocket_establishment_timeout = Eta.Duration.seconds 10

let is_conversation_model model =
  model = "gpt-realtime-2" || model = default_conversation_model

let transcription_delta_types =
  [
    "conversation.item.input_audio_transcription.delta";
    "conversation.input_transcript.delta";
    "conversation.output_transcript.delta";
    "session.input_transcript.delta";
    "session.output_transcript.delta";
    "response.output_text.delta";
    "response.output_audio_transcript.delta";
  ]

let transcript_done_types =
  [
    "response.output_audio_transcript.done";
    "response.output_text.done";
    "conversation.output_transcript.done";
  ]

let transcript_final_types =
  [ "conversation.item.input_audio_transcription.completed" ]

let created_event_types = [ "session.created"; "transcription_session.created" ]
let config_ack_event_types = [ "session.updated" ]
let member name fields = List.assoc_opt name fields

let string_member name fields =
  match member name fields with Some (`String value) -> Some value | _ -> None

let strip_trailing_slashes s =
  let rec last_non_slash i =
    if i < 0 then -1 else if s.[i] = '/' then last_non_slash (i - 1) else i
  in
  let last = last_non_slash (String.length s - 1) in
  if last < 0 then "" else String.sub s 0 (last + 1)

let resolve_base_url () =
  match Sys.getenv_opt "CODEX_REALTIME_BASE_URL_OVERRIDE" with
  | Some value when String.trim value <> "" ->
      strip_trailing_slashes (String.trim value)
  | _ -> default_base_url

let path_for_mode = function
  | Translation -> "/v1/realtime/translations"
  | Transcription | Conversation -> "/v1/realtime"

let http_scheme_stand_in url =
  if String.starts_with ~prefix:"wss://" url then
    ("wss://", "https://" ^ String.sub url 6 (String.length url - 6))
  else if String.starts_with ~prefix:"ws://" url then
    ("ws://", "http://" ^ String.sub url 5 (String.length url - 5))
  else ("", url)

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
        if is_conversation_model model then
          Http.Core.Url.with_query_param url "model" model
        else
          match mode with
          | Transcription ->
              Http.Core.Url.with_query_param url "intent" "transcription"
          | Translation | Conversation ->
              Http.Core.Url.with_query_param url "model" model
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

let build_audio_append ?(mode = Transcription) pcm_bytes =
  let frame_type =
    match mode with
    | Translation -> "session.input_audio_buffer.append"
    | Transcription | Conversation -> "input_audio_buffer.append"
  in
  `Assoc
    [
      ("type", `String frame_type);
      ("audio", `String (Openrouter.base64_encode pcm_bytes));
    ]

let build_audio_commit () =
  `Assoc [ ("type", `String "input_audio_buffer.commit") ]

let build_transcription_session_update ?prompt ~model () =
  let transcription_fields =
    [ ("model", `String model) ]
    @ match prompt with Some p -> [ ("prompt", `String p) ] | None -> []
  in
  `Assoc
    [
      ("type", `String "session.update");
      ( "session",
        `Assoc
          [
            ("type", `String "transcription");
            ( "audio",
              `Assoc
                [
                  ( "input",
                    `Assoc
                      [
                        ( "format",
                          `Assoc
                            [
                              ("type", `String "audio/pcm");
                              ("rate", `Int realtime_sample_rate);
                            ] );
                        ("transcription", `Assoc transcription_fields);
                      ] );
                ] );
          ] );
    ]

let build_translation_session_update ?prompt ?target_language () =
  let instructions =
    match prompt with
    | Some p -> Some p
    | None ->
        Option.map
          (fun lang ->
            Printf.sprintf "Translate the spoken audio into %s." lang)
          target_language
  in
  let session_fields =
    (match instructions with
      | Some i -> [ ("instructions", `String i) ]
      | None -> [])
    @
    match target_language with
    | None -> []
    | Some language ->
        [
          ( "audio",
            `Assoc [ ("output", `Assoc [ ("language", `String language) ]) ] );
        ]
  in
  `Assoc
    [ ("type", `String "session.update"); ("session", `Assoc session_fields) ]

let build_conversation_session_update ~model ~prompt =
  `Assoc
    [
      ("type", `String "session.update");
      ( "session",
        `Assoc
          [
            ("type", `String "realtime");
            ("model", `String model);
            ("output_modalities", `List [ `String "text" ]);
            ("instructions", `String prompt);
            ( "audio",
              `Assoc
                [
                  ( "input",
                    `Assoc
                      [
                        ( "format",
                          `Assoc
                            [
                              ("type", `String "audio/pcm");
                              ("rate", `Int realtime_sample_rate);
                            ] );
                        ("turn_detection", `Null);
                      ] );
                ] );
          ] );
    ]

let build_conversation_response_create ~prompt =
  `Assoc
    [
      ("type", `String "response.create");
      ( "response",
        `Assoc
          [
            ("output_modalities", `List [ `String "text" ]);
            ("instructions", `String prompt);
          ] );
    ]

let parse_event payload =
  try
    match Yojson.Safe.from_string payload with
    | `Assoc fields -> (
        match string_member "type" fields with
        | None -> Error (`Stt_error "Codex realtime event missing type")
        | Some "error" ->
            let message, code =
              match member "error" fields with
              | Some (`Assoc error_fields) ->
                  let message =
                    match
                      ( string_member "message" error_fields,
                        string_member "message" fields )
                    with
                    | Some value, _ -> value
                    | None, Some value -> value
                    | None, None ->
                        "Codex realtime server returned an unspecified error"
                  in
                  (message, string_member "code" error_fields)
              | _ -> (
                  match string_member "message" fields with
                  | Some value -> (value, None)
                  | None ->
                      ( "Codex realtime server returned an unspecified error",
                        None ))
            in
            Ok (Server_error { message; code })
        | Some event_type when List.mem event_type transcription_delta_types
          -> (
            match string_member "delta" fields with
            | Some delta -> Ok (Transcript_delta delta)
            | None ->
                Error (`Stt_error "Codex realtime delta event missing delta"))
        | Some event_type when List.mem event_type transcript_final_types -> (
            match
              (string_member "transcript" fields, string_member "text" fields)
            with
            | Some text, _ | None, Some text -> Ok (Transcript_final text)
            | None, None ->
                Error
                  (`Stt_error
                     "Codex realtime transcription completed event missing text")
            )
        | Some event_type when List.mem event_type transcript_done_types -> (
            match
              (string_member "transcript" fields, string_member "text" fields)
            with
            | Some text, _ | None, Some text -> Ok (Transcript_done text)
            | None, None ->
                Error
                  (`Stt_error
                     "Codex realtime done event missing transcript text"))
        | Some "response.done" ->
            let status =
              match member "response" fields with
              | Some (`Assoc response_fields) ->
                  string_member "status" response_fields
              | Some _ -> None
              | None -> string_member "status" fields
            in
            let terminal =
              match status with
              | Some "completed" -> Completed
              | Some "failed" -> Failed
              | Some "cancelled" -> Cancelled
              | Some "incomplete" -> Incomplete
              | Some _ | None -> Unknown
            in
            Ok (Response_terminal terminal)
        | Some event_type when List.mem event_type created_event_types ->
            Ok (Created event_type)
        | Some event_type when List.mem event_type config_ack_event_types ->
            Ok (Config_ack event_type)
        | Some event_type -> Ok (Other event_type))
    | _ -> Error (`Stt_error "Codex realtime event must be a JSON object")
  with Yojson.Json_error _ ->
    Error (`Stt_error "Codex realtime event is not valid JSON")

let resample_pcm ~from_rate ~to_rate pcm_bytes =
  match Audio.Resampler.create ~from_rate ~to_rate with
  | Error (`Resample_error msg) -> Error msg
  | Ok resampler -> (
      match Audio.Resampler.push resampler pcm_bytes with
      | Error (`Resample_error msg) -> Error msg
      | Ok first -> (
          match Audio.Resampler.finish resampler with
          | Error (`Resample_error msg) -> Error msg
          | Ok last when Bytes.length last = 0 -> Ok first
          | Ok last -> Ok (Bytes.cat first last)))

let resample_s16le_mono ~from_rate ~to_rate pcm_bytes =
  match resample_pcm ~from_rate ~to_rate pcm_bytes with
  | Error msg -> invalid_arg ("resample_s16le_mono: " ^ msg)
  | Ok bytes -> bytes

let stt_error message = Eta.Effect.fail (`Stt_error message)

let ws_error_message = function
  | `Connect _ -> "connect failed"
  | `Upgrade_failed failure ->
      Printf.sprintf "upgrade failed with HTTP %d"
        failure.Http_eio.Ws.Client.status
  | `Closed (code, _) -> Printf.sprintf "closed (%d)" code
  | `Protocol _ -> "protocol error"
  | `Timeout -> "timeout"

let headers pairs = Http.Core.Header.unsafe_of_list pairs

let make_realtime_session_id () =
  Printf.sprintf "pie-%Lx-%06x"
    (Int64.of_float (Unix.gettimeofday () *. 1000.))
    (Random.bits () land 0xffffff)

let realtime_header_pairs ?session_id access_token =
  let pairs =
    [
      ("Authorization", "Bearer " ^ access_token);
      ("originator", "codex_cli_rs");
      ("User-Agent", "codex_cli_rs/0.0.0 (pie)");
      ("version", realtime_version);
    ]
  in
  match session_id with
  | Some session_id when String.trim session_id <> "" ->
      ("x-session-id", session_id) :: pairs
  | _ -> pairs

let realtime_headers ?session_id access_token =
  headers (realtime_header_pairs ?session_id access_token)

let realtime_rest_client ~sw ~net = Process.http_client ~sw ~net

module Codex_auth = struct
  type t = { path : string; access_token : string }

  let codex_home () =
    match Sys.getenv_opt "CODEX_HOME" with
    | Some value when String.trim value <> "" -> String.trim value
    | _ -> (
        match Sys.getenv_opt "HOME" with
        | Some home when String.trim home <> "" -> Filename.concat home ".codex"
        | _ -> ".codex")

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
          | `Assoc fields -> (
              match List.assoc_opt "tokens" fields with
              | Some (`Assoc token_fields) -> (
                  match string_member "access_token" token_fields with
                  | Some access_token when String.trim access_token <> "" ->
                      Ok { path; access_token }
                  | _ ->
                      Error
                        (`Stt_error
                           ("Codex auth.json at " ^ path
                          ^ " is malformed or missing tokens.access_token. Run \
                             'codex login' to recreate it.")))
              | _ ->
                  Error
                    (`Stt_error
                       ("Codex auth.json at " ^ path
                      ^ " is malformed or missing tokens. Run 'codex login' to \
                         recreate it.")))
          | _ ->
              Error
                (`Stt_error
                   ("Codex auth.json at " ^ path ^ " must be a JSON object."))
        with
        | Sys_error _ ->
            Error
              (`Stt_error
                 ("Codex auth.json not found/readable at " ^ path
                ^ ". Run 'codex login' or set CODEX_HOME."))
        | Yojson.Json_error _ ->
            Error
              (`Stt_error
                 ("Codex auth.json at " ^ path
                ^ " is not valid JSON. Run 'codex login' to recreate it.")))
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
      input |> String.map (function '-' -> '+' | '_' -> '/' | c -> c)
    in
    let pad = (4 - (String.length normalized mod 4)) mod 4 in
    let padded = normalized ^ String.make pad '=' in
    let out = Buffer.create (String.length padded / 4 * 3) in
    let rec loop i =
      if i >= String.length padded then Ok (Buffer.contents out)
      else
        match
          ( base64_value padded.[i],
            base64_value padded.[i + 1],
            base64_value padded.[i + 2],
            base64_value padded.[i + 3] )
        with
        | Some a, Some b, Some c, Some d ->
            let n = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
            Buffer.add_char out (Char.chr ((n lsr 16) land 0xff));
            if padded.[i + 2] <> '=' then
              Buffer.add_char out (Char.chr ((n lsr 8) land 0xff));
            if padded.[i + 3] <> '=' then
              Buffer.add_char out (Char.chr (n land 0xff));
            loop (i + 4)
        | _ -> Error "invalid base64url"
    in
    if String.length padded mod 4 <> 0 then Error "invalid base64url length"
    else loop 0

  let decode_jwt_exp_seconds token =
    match String.split_on_char '.' token with
    | _header :: payload :: _ -> (
        match base64url_decode payload with
        | Error _ -> None
        | Ok json -> (
            try
              match Yojson.Safe.from_string json with
              | `Assoc fields -> (
                  match List.assoc_opt "exp" fields with
                  | Some (`Int exp) -> Some exp
                  | Some (`Intlit exp) -> int_of_string_opt exp
                  | _ -> None)
              | _ -> None
            with _ -> None))
    | _ -> None

  let is_expired ?(leeway_seconds = 60) ~now token =
    match decode_jwt_exp_seconds token with
    | None -> false
    | Some exp ->
        (now /. 1000.) +. float_of_int leeway_seconds >= float_of_int exp

  let access_token () =
    let* auth = load () in
    let now = Unix.gettimeofday () *. 1000. in
    if not (is_expired ~now auth.access_token) then
      Eta.Effect.pure auth.access_token
    else
      stt_error
        ("Codex access token at " ^ auth.path
       ^ " is expired. Run 'codex login' so the Codex credential owner can \
          refresh it.")
end

let realtime_access_token ~client:_ () =
  match Sys.getenv_opt "OPENAI_API_KEY" with
  | Some value when String.trim value <> "" ->
      Eta.Effect.pure (String.trim value)
  | _ -> Codex_auth.access_token ()

type session_result = {
  final : string option;
  deltas : string list;
  error : string option;
  config_ack : bool;
  terminal : terminal_status option;
  output_bytes : int;
}

let empty_session_result =
  {
    final = None;
    deltas = [];
    error = None;
    config_ack = false;
    terminal = None;
    output_bytes = 0;
  }

let result_budget_error = "Codex realtime result budget exceeded"

let admit_output ~replace state text =
  let n = String.length text in
  let used = if replace then 0 else state.output_bytes in
  if used + n > Dictation_limits.result_utf8_bytes then
    Error result_budget_error
  else Ok n

let close_ws ws = Http_eio.Ws.Client.close ws

let with_websocket_establishment_timeout computation =
  computation
  |> Eta.Effect.timeout_as websocket_establishment_timeout
       ~on_timeout:
         (`Stt_error
            "Codex realtime WebSocket establishment timed out after 10 seconds")

let connect_realtime_ws ~headers ~sw ~net url =
  (* Preserve OpenSSL's SSL_CERT_FILE trust override across Eta transports. *)
  Http_eio.Ws.Client.connect ?ca_file:(Sys.getenv_opt "SSL_CERT_FILE") ~headers
    ~sw ~net url
  |> Eta.Effect.bind_error (fun error ->
      stt_error
        ("Codex realtime WebSocket connect failed: " ^ ws_error_message error))
  |> with_websocket_establishment_timeout

(* PIE002: every realtime operation owns exactly one connection. Acquisition,
   readiness, sending, receiving, cancellation and final settlement all run
   inside that owner. The settlement is registered immediately after the
   handshake, not after readiness, so a pre-ready server error, malformed
   readiness, stalled-readiness cancellation, send failure and mid-stream
   cancellation settle the same way and preserve the original failure.

   The installed Eta transport runs its reader as runtime-owned daemon work
   (Eta.Spi.daemon on the runtime outer switch) and Eio's reference-counted
   descriptor defers Unix.close while that reader is blocked. A peer that stays
   open therefore retains the reader descriptor for the runtime's lifetime; see
   docs/issues/realtime-session-ownership/01-retire-session-workers-and-connections.md.
   Keeping settlement in one operation owner is what lets that migration become
   a scope change instead of a second transport. *)
type realtime_connection = {
  ws : Http_eio.Ws.Client.t;
  receive_done : bool Atomic.t;
}

let mark_receive_done connection =
  Eta.Effect.sync (fun () -> Atomic.set connection.receive_done true)

(* Settle the operation's connection: no further receive is expected and the
   transport is closed once. The close does not wait for the provider to answer
   or to close its side. *)
let settle_realtime_connection connection =
  let* () = mark_receive_done connection in
  close_ws connection.ws
  |> Eta.Effect.map_error (fun error ->
      `Stt_error
        ("Codex realtime WebSocket close failed: " ^ ws_error_message error))

let acquire_realtime_connection ~sw ~net ~url ~access_token ~session_id =
  let* ws =
    connect_realtime_ws
      ~headers:(realtime_headers ~session_id access_token)
      ~sw ~net url
  in
  Eta.Effect.pure { ws; receive_done = Atomic.make false }

(* One owner for the whole operation: [use] runs with the acquired connection
   and the settlement runs for success, typed failure, defect and cancellation. *)
let with_realtime_connection ~sw ~net ~url ~access_token ~session_id use =
  Eta.Effect.with_resource
    ~acquire:
      (acquire_realtime_connection ~sw ~net ~url ~access_token ~session_id)
    ~release:settle_realtime_connection use

let server_error_diagnostic _code = "Codex realtime server error"

type decoded_frame = Decoded of event | Binary | Parse_error of string

let decode_frame = function
  | `Binary _ -> Binary
  | `Text payload -> (
      match parse_event payload with
      | Ok event -> Decoded event
      | Error (`Stt_error message) -> Parse_error message)

let decoded_incoming ws =
  Http_eio.Ws.Client.incoming ws |> Stream.Stream.map decode_frame

let event_should_stop ~mode = function
  | Server_error _ -> true
  | Transcript_final _ -> (
      match mode with
      | Transcription -> true
      | Conversation | Translation -> false)
  | Response_terminal _ -> (
      match mode with
      | Conversation -> true
      | Transcription | Translation -> false)
  | Created _ | Config_ack _ | Transcript_delta _ | Transcript_done _ | Other _
    ->
      false

let receive_should_stop ~mode = function
  | Binary | Parse_error _ -> Eta.Effect.pure true
  | Decoded event -> Eta.Effect.pure (event_should_stop ~mode event)

let receive_result ?(on_text = fun _ -> Eta.Effect.unit) ~mode ws =
  let step state = function
    | Binary ->
        Eta.Effect.pure
          {
            state with
            error = Some "Codex realtime sent a binary WebSocket message";
          }
    | Parse_error message ->
        Eta.Effect.pure
          {
            state with
            error = Some ("Failed to parse Codex realtime event: " ^ message);
          }
    | Decoded (Server_error { code; _ }) ->
        Eta.Effect.pure
          { state with error = Some (server_error_diagnostic code) }
    | Decoded (Transcript_delta delta) when String.length delta = 0 ->
        Eta.Effect.pure state
    | Decoded (Transcript_done transcript) when String.length transcript = 0 ->
        Eta.Effect.pure state
    | Decoded (Transcript_delta delta) -> (
        match admit_output ~replace:false state delta with
        | Error message -> Eta.Effect.pure { state with error = Some message }
        | Ok n ->
            let+ () = on_text delta in
            {
              state with
              deltas = delta :: state.deltas;
              output_bytes = state.output_bytes + n;
            })
    | Decoded (Transcript_done transcript) -> (
        match admit_output ~replace:true state transcript with
        | Error message -> Eta.Effect.pure { state with error = Some message }
        | Ok n ->
            let+ () = on_text transcript in
            {
              state with
              final = Some transcript;
              deltas = [];
              output_bytes = n;
            })
    | Decoded (Transcript_final transcript) -> (
        match admit_output ~replace:true state transcript with
        | Error message -> Eta.Effect.pure { state with error = Some message }
        | Ok n -> (
            let+ () = on_text transcript in
            let state =
              {
                state with
                final = Some transcript;
                deltas = [];
                output_bytes = n;
              }
            in
            match mode with
            | Transcription -> { state with terminal = Some Completed }
            | Conversation | Translation -> state))
    | Decoded (Response_terminal status) -> (
        match mode with
        | Conversation -> Eta.Effect.pure { state with terminal = Some status }
        | Transcription | Translation -> Eta.Effect.pure state)
    | Decoded (Created _ | Config_ack _ | Other _) -> Eta.Effect.pure state
  in
  Stream.run
    (decoded_incoming ws
    |> Stream.Stream.take_until_effect (receive_should_stop ~mode))
    (Stream.Sink.fold_effect step empty_session_result)
  |> Eta.Effect.map_error (fun error ->
      `Stt_error
        ("Codex realtime WebSocket receive failed: " ^ ws_error_message error))
  |> Eta.Effect.bind (fun result ->
      match result.error with
      | Some message -> stt_error message
      | None -> Eta.Effect.pure result)

let readiness_should_stop = function
  | Binary | Parse_error _ -> Eta.Effect.pure true
  | Decoded (Config_ack _ | Server_error _) -> Eta.Effect.pure true
  | Decoded _ -> Eta.Effect.pure false

let await_session_ready ws =
  let step state = function
    | Binary ->
        Eta.Effect.pure
          {
            state with
            error = Some "Codex realtime sent a binary WebSocket message";
          }
    | Parse_error message ->
        Eta.Effect.pure
          {
            state with
            error = Some ("Failed to parse Codex realtime event: " ^ message);
          }
    | Decoded (Server_error { code; _ }) ->
        Eta.Effect.pure
          { state with error = Some (server_error_diagnostic code) }
    | Decoded (Config_ack _) -> Eta.Effect.pure { state with config_ack = true }
    | Decoded
        ( Created _ | Transcript_delta _ | Transcript_done _
        | Transcript_final _ | Response_terminal _ | Other _ ) ->
        Eta.Effect.pure state
  in
  let receive =
    Stream.run
      (decoded_incoming ws
      |> Stream.Stream.take_until_effect readiness_should_stop)
      (Stream.Sink.fold_effect step empty_session_result)
    |> Eta.Effect.map_error (fun error ->
        `Stt_error
          ("Codex realtime WebSocket receive failed: " ^ ws_error_message error))
  in
  let* result = receive in
  match result.error with
  | Some message -> stt_error message
  | None when result.config_ack -> Eta.Effect.unit
  | None ->
      stt_error
        "Codex realtime WebSocket closed before configuration acknowledgement"

let send_text ?(ignore_closing = fun () -> false) ws payload =
  Http_eio.Ws.Client.send_text ws payload
  |> Eta.Effect.bind_error (fun error ->
      match error with
      | `Closed _ when ignore_closing () -> Eta.Effect.unit
      | _ ->
          stt_error
            ("Codex realtime WebSocket send failed: " ^ ws_error_message error))

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

(* The receive side records that no further incoming message is expected, so a
   send that races the settling connection is tolerated instead of reported as
   a new failure. Settlement itself belongs to the operation owner. *)
let receive_result_marking_done ?on_text ~mode connection =
  receive_result ?on_text ~mode connection.ws
  |> Eta.Effect.finally (mark_receive_done connection)

let collected_text result =
  match result.final with
  | Some text -> text
  | None -> result.deltas |> List.rev |> String.concat "" |> String.trim

let finish_result ~mode result =
  match result.error with
  | Some message -> stt_error message
  | None -> (
      match (mode, result.terminal) with
      | Transcription, Some Completed -> Eta.Effect.pure (collected_text result)
      | Conversation, Some Completed -> Eta.Effect.pure (collected_text result)
      | Conversation, Some Failed -> stt_error "Codex realtime response failed"
      | Conversation, Some Cancelled ->
          stt_error "Codex realtime response cancelled"
      | Translation, _ -> stt_error "Codex realtime response incomplete"
      | _, (Some Incomplete | Some Unknown | Some Failed | Some Cancelled | None)
        ->
          stt_error "Codex realtime response incomplete")

let run_realtime_session ~sw ~net ~url ~access_token ~mode ?session_update
    ?response_create ?(translation_output_drain_ms = 2_000) ~input_sample_rate
    pcm_bytes =
  if Bytes.length pcm_bytes > Dictation_limits.capture_pcm_bytes then
    stt_error "Codex realtime captured audio budget exceeded"
  else if input_sample_rate <= 0 then
    stt_error "Codex realtime input sample rate must be positive"
  else
    match
      resample_pcm ~from_rate:input_sample_rate ~to_rate:realtime_sample_rate
        pcm_bytes
    with
    | Error msg -> stt_error ("Codex realtime " ^ msg)
    | Ok pcm_bytes ->
        let session_id = make_realtime_session_id () in
        with_realtime_connection ~sw ~net ~url ~access_token ~session_id
        @@ fun connection ->
        let ws = connection.ws in
        let ignore_closing () = Atomic.get connection.receive_done in
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
              (* Translation ends its own receive side after the drain window; the
           operation owner still settles the connection on every exit. *)
              Eta.Effect.delay
                (Eta.Duration.ms translation_output_drain_ms)
                (settle_realtime_connection connection)
          | Transcription | Conversation -> (
              let* () = send_json ~ignore_closing ws (build_audio_commit ()) in
              match response_create with
              | None -> Eta.Effect.unit
              | Some json -> send_json ~ignore_closing ws json)
        in
        (* stttime-i0cx: an established realtime stream has no provider deadline. *)
        let receive = receive_result_marking_done ~mode connection in
        let* result, () = Eta.Effect.par receive send in
        finish_result ~mode result

let run_realtime_stream_session ~sw ~net ~url ~access_token ~mode
    ?session_update ?response_create ?on_text
    ?(translation_output_drain_ms = 2_000) ~input_sample_rate audio =
  match
    Audio.Resampler.create ~from_rate:input_sample_rate
      ~to_rate:realtime_sample_rate
  with
  | Error (`Resample_error msg) -> stt_error ("Codex realtime " ^ msg)
  | Ok resampler ->
      let session_id = make_realtime_session_id () in
      with_realtime_connection ~sw ~net ~url ~access_token ~session_id
      @@ fun connection ->
      let ws = connection.ws in
      let ignore_closing () = Atomic.get connection.receive_done in
      let* () =
        match session_update with
        | None -> Eta.Effect.unit
        | Some json ->
            let* () = send_json ~ignore_closing:(fun () -> true) ws json in
            await_session_ready ws
      in
      let send =
        let captured = ref 0 in
        let send_resampled pcm =
          if Bytes.length pcm = 0 then Eta.Effect.unit
          else send_audio_chunks ~ignore_closing ws ~mode pcm
        in
        let send_chunk () chunk =
          let n = Bytes.length chunk in
          if !captured + n > Dictation_limits.capture_pcm_bytes then
            stt_error "Codex realtime captured audio budget exceeded"
          else (
            captured := !captured + n;
            match Audio.Resampler.push resampler chunk with
            | Error (`Resample_error msg) -> stt_error ("Codex realtime " ^ msg)
            | Ok pcm -> send_resampled pcm)
        in
        let* () = Stream.run audio (Stream.Sink.fold_effect send_chunk ()) in
        let* () =
          match Audio.Resampler.finish resampler with
          | Error (`Resample_error msg) -> stt_error ("Codex realtime " ^ msg)
          | Ok tail -> send_resampled tail
        in
        match mode with
        | Translation ->
            (* Translation ends its own receive side after the drain window; the
               operation owner still settles the connection on every exit. *)
            Eta.Effect.delay
              (Eta.Duration.ms translation_output_drain_ms)
              (settle_realtime_connection connection)
        | Transcription | Conversation -> (
            let* () = send_json ~ignore_closing ws (build_audio_commit ()) in
            match response_create with
            | None -> Eta.Effect.unit
            | Some json -> send_json ~ignore_closing ws json)
      in
      let receive = receive_result_marking_done ?on_text ~mode connection in
      let* result, () = Eta.Effect.par receive send in
      finish_result ~mode result

type stream_handle = {
  offer_chunk : bytes -> (unit, error) Eta.Effect.t;
  finish_stream : unit -> (string, error) Eta.Effect.t;
  cancel_stream : (unit, error) Eta.Effect.t;
}

type stream_admission = Accepting | Failed of error | Closed

type stream_budget = {
  mutex : Mutex.t;
  mutable queued : int;
  mutable captured : int;
  mutable items : int;
  mutable admission : stream_admission;
}

let with_budget_lock budget f =
  Mutex.lock budget.mutex;
  Fun.protect ~finally:(fun () -> Mutex.unlock budget.mutex) f

let replay_exit exit =
  Eta.Effect.async ~register:(fun resume ->
      resume exit;
      None)

let with_stream_session ~sw ~net ~url ~access_token ~mode ~operation ~model
    ?session_update ?response_create ?(sample_rate = 16000) ~cancel_message use
    =
  Eta.Effect.defer (fun () ->
      let queue : (string, error) Eta.Queue.t = Eta.Queue.unbounded () in
      let completion : (string, error) Eta.Promise.t = Eta.Promise.create () in
      let first_token_timer = First_token_timer.create () in
      let budget =
        {
          mutex = Mutex.create ();
          queued = 0;
          captured = 0;
          items = 0;
          admission = Accepting;
        }
      in
      let on_text =
        log_first_token ~timer:first_token_timer ~operation ~model ~mode
      in
      let record_admission next =
        with_budget_lock budget @@ fun () ->
        match budget.admission with
        | Accepting -> budget.admission <- next
        | Failed _ | Closed -> ()
      in
      let publish exit =
        Eta.Effect.defer (fun () ->
            let* () =
              Eta.Effect.sync (fun () ->
                  match exit with
                  | Eta.Exit.Ok _ ->
                      record_admission Closed;
                      Eta.Queue.close_now queue
                  | Eta.Exit.Error cause ->
                      let error =
                        match cause with
                        | Eta.Cause.Fail e -> e
                        | _ -> `Stt_error "Codex realtime stream failed"
                      in
                      record_admission (Failed error);
                      Eta.Queue.close_with_error_now queue error)
            in
            let* _accepted = Eta.Promise.resolve completion exit in
            Eta.Effect.unit)
      in
      let audio =
        Stream.Stream.from_queue queue
        |> Stream.Stream.tap (fun snapshot ->
            Eta.Effect.sync (fun () ->
                with_budget_lock budget @@ fun () ->
                let n = String.length snapshot in
                if budget.items < 1 || budget.queued < n then
                  failwith "Codex realtime queue accounting underflow"
                else (
                  budget.queued <- budget.queued - n;
                  budget.items <- budget.items - 1)))
        |> Stream.Stream.map Bytes.of_string
      in
      let session =
        run_realtime_stream_session ~sw ~net ~url ~access_token ~mode
          ?session_update ?response_create ~on_text
          ~input_sample_rate:sample_rate audio
        |> Eta.Effect.to_exit
        |> Eta.Effect.bind (fun exit ->
            replay_exit exit
            |> Eta.Effect.map (fun _ -> ())
            |> Eta.Effect.finally (publish exit))
      in
      let offer_chunk chunk =
        Eta.Effect.sync (fun () ->
            with_budget_lock budget @@ fun () ->
            match budget.admission with
            | Failed error -> Error error
            | Closed -> Error (`Stt_error "Codex realtime stream closed")
            | Accepting -> (
                let n = Bytes.length chunk in
                if n = 0 then Ok ()
                else if budget.items >= Dictation_limits.queue_items then
                  Error
                    (`Stt_error "Codex realtime queued audio budget exceeded")
                else if budget.queued + n > Dictation_limits.queue_pcm_bytes
                then
                  Error
                    (`Stt_error "Codex realtime queued audio budget exceeded")
                else if budget.captured + n > Dictation_limits.capture_pcm_bytes
                then
                  Error
                    (`Stt_error "Codex realtime captured audio budget exceeded")
                else
                  let snapshot = Bytes.to_string chunk in
                  match Eta.Queue.try_offer_now queue snapshot with
                  | `Sent ->
                      budget.queued <- budget.queued + n;
                      budget.captured <- budget.captured + n;
                      budget.items <- budget.items + 1;
                      Ok ()
                  | `Closed -> Error (`Stt_error "Codex realtime stream closed")
                  | `Closed_with_error error -> Error error
                  | `Full | `Dropped ->
                      Error
                        (`Stt_error
                           "Codex realtime queued audio budget exceeded")))
        |> Eta.Effect.flatten_result
      in
      let cancel_stream =
        let error = `Stt_error cancel_message in
        let* () =
          Eta.Effect.sync (fun () ->
              record_admission (Failed error);
              Eta.Queue.close_with_error_now queue error)
        in
        let* _accepted =
          Eta.Promise.resolve completion (Eta.Exit.Error (Eta.Cause.Fail error))
        in
        Eta.Effect.unit
      in
      let handle =
        {
          offer_chunk;
          finish_stream =
            (fun () ->
              let* started_at_ms = Eta.Effect.now_ms in
              let* () =
                Eta.Effect.sync (fun () ->
                    First_token_timer.start first_token_timer
                      ~at_ms:started_at_ms)
              in
              record_admission Closed;
              let* () = Eta.Queue.close queue in
              Eta.Promise.await completion);
          cancel_stream;
        }
      in
      Eta.Effect.with_background ~name:"codex.realtime.stream" session
        (fun () -> use handle))

let with_transcription_stream ~sw ~net ~model ~language ~prompt_template
    ?(sample_rate = 16000) use =
  let client = realtime_rest_client ~sw ~net in
  let* access_token = realtime_access_token ~client () in
  let prompt = render_template prompt_template [ ("language", language) ] in
  if is_conversation_model model then
    with_stream_session ~sw ~net
      ~url:(build_realtime_url ~mode:Conversation ~model ())
      ~access_token ~mode:Conversation ~operation:"transcribe" ~model
      ~session_update:(build_conversation_session_update ~model ~prompt)
      ~response_create:(build_conversation_response_create ~prompt)
      ~sample_rate ~cancel_message:"Codex realtime transcription cancelled" use
  else
    with_stream_session ~sw ~net
      ~url:(build_realtime_url ~mode:Transcription ~model ())
      ~access_token ~mode:Transcription ~operation:"transcribe" ~model
      ~session_update:(build_transcription_session_update ~model ~prompt ())
      ~sample_rate ~cancel_message:"Codex realtime transcription cancelled" use

let with_translation_stream ~sw ~net ~model ~source_language ~target_language
    ~prompt_template ?(sample_rate = 16000) use =
  let client = realtime_rest_client ~sw ~net in
  let* access_token = realtime_access_token ~client () in
  let prompt =
    render_template prompt_template
      [
        ("source_language", source_language);
        ("target_language", target_language);
      ]
  in
  if is_conversation_model model then
    with_stream_session ~sw ~net
      ~url:(build_realtime_url ~mode:Conversation ~model ())
      ~access_token ~mode:Conversation ~operation:"translate" ~model
      ~session_update:(build_conversation_session_update ~model ~prompt)
      ~response_create:(build_conversation_response_create ~prompt)
      ~sample_rate ~cancel_message:"Codex realtime translation cancelled" use
  else
    with_stream_session ~sw ~net
      ~url:(build_realtime_url ~mode:Translation ~model ())
      ~access_token ~mode:Translation ~operation:"translate" ~model
      ~session_update:
        (build_translation_session_update ~prompt ~target_language ())
      ~sample_rate ~cancel_message:"Codex realtime translation cancelled" use

let transcribe_with_prompt ~sw ~net ~model ~language ~prompt_template
    ?(sample_rate = 16000) pcm_bytes =
  let client = realtime_rest_client ~sw ~net in
  let* access_token = realtime_access_token ~client () in
  let prompt = render_template prompt_template [ ("language", language) ] in
  if is_conversation_model model then
    run_realtime_session ~sw ~net
      ~url:(build_realtime_url ~mode:Conversation ~model ())
      ~access_token ~mode:Conversation
      ~session_update:(build_conversation_session_update ~model ~prompt)
      ~response_create:(build_conversation_response_create ~prompt)
      ~input_sample_rate:sample_rate pcm_bytes
  else
    run_realtime_session ~sw ~net
      ~url:(build_realtime_url ~mode:Transcription ~model ())
      ~access_token ~mode:Transcription
      ~session_update:(build_transcription_session_update ~model ~prompt ())
      ~input_sample_rate:sample_rate pcm_bytes

let translate_with_prompt ~sw ~net ~model ~source_language ~target_language
    ~prompt_template ?(sample_rate = 16000) pcm_bytes =
  let client = realtime_rest_client ~sw ~net in
  let* access_token = realtime_access_token ~client () in
  let prompt =
    render_template prompt_template
      [
        ("source_language", source_language);
        ("target_language", target_language);
      ]
  in
  if is_conversation_model model then
    run_realtime_session ~sw ~net
      ~url:(build_realtime_url ~mode:Conversation ~model ())
      ~access_token ~mode:Conversation
      ~session_update:(build_conversation_session_update ~model ~prompt)
      ~response_create:(build_conversation_response_create ~prompt)
      ~input_sample_rate:sample_rate pcm_bytes
  else
    run_realtime_session ~sw ~net
      ~url:(build_realtime_url ~mode:Translation ~model ())
      ~access_token ~mode:Translation
      ~session_update:
        (build_translation_session_update ~prompt ~target_language ())
      ~input_sample_rate:sample_rate pcm_bytes
