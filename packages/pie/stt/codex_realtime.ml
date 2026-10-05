type error = [ `Stt_error of string ]

module Session = Ox_ai_realtime.Session
module Ws = Ox_http.Ws_client
module Logger = Ox_observability.Logger

let ( let* ) = Result.bind

type mode = Transcription

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

let string_of_mode = function Transcription -> "transcription"

let log_first_token ~clock ~logger ~timer ~operation ~model ~mode text =
  let at_ms = int_of_float (Eio.Time.now clock *. 1000.) in
  match First_token_timer.observe timer ~at_ms text with
  | None -> ()
  | Some ttft_ms ->
      Logger.info logger "stt first token"
        ~fields:
          [
            ("stt.provider", Logger.String "codex-realtime");
            ("stt.operation", Logger.String operation);
            ("stt.model", Logger.String model);
            ("stt.stream_mode", Logger.String (string_of_mode mode));
            ("stt.time_to_first_token_ms", Logger.Int ttft_ms);
          ]

type event =
  | Created of string
  | Config_ack of string
  | Transcript_delta of string
  | Transcript_final of string
  | Server_error of { message : string; code : string option }
  | Other of string

let realtime_sample_rate = 24_000
let default_base_url = "wss://api.openai.com"
let realtime_version = "0.0.0"
let oauth_response_idle_timeout = Ox.Duration.seconds 30
let websocket_establishment_timeout = Ox.Duration.seconds 10

let transcription_delta_types =
  [ "conversation.item.input_audio_transcription.delta" ]

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

let resolve_base_url ~caps () =
  match Ox_caps.Cap_sys.getenv_opt ~caps "CODEX_REALTIME_BASE_URL_OVERRIDE" with
  | Some value when String.trim value <> "" ->
      strip_trailing_slashes (String.trim value)
  | _ -> default_base_url

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

let build_realtime_url ~caps ?base_url ~mode:_ ~model:_ () =
  let base_url =
    strip_trailing_slashes
      (Option.value base_url ~default:(resolve_base_url ~caps ()))
  in
  let path = "/v1/realtime" in
  let scheme, http_base = http_scheme_stand_in base_url in
  match Ox_http.Url.parse (http_base ^ path) with
  | Error parse_error ->
      invalid_arg (Ox_http.Url.parse_error_to_string parse_error)
  | Ok url ->
      let url = Ox_http.Url.with_query_param url "intent" "transcription" in
      restore_scheme scheme (Ox_http.Url.to_string url)

let build_audio_append ~mode:_ pcm_bytes =
  `Assoc
    [
      ("type", `String "input_audio_buffer.append");
      ("audio", `String (Openrouter.base64_encode pcm_bytes));
    ]

let build_audio_commit () =
  `Assoc [ ("type", `String "input_audio_buffer.commit") ]

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

let stt_error message = Error (`Stt_error message)

let make_realtime_session_id ~caps () =
  Printf.sprintf "pie-%Lx-%06x"
    (Int64.of_float (Ox_caps.Cap_unix.gettimeofday ~caps () *. 1000.))
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

let realtime_rest_client ~sw ~net ~clock = Process.make_http ~sw ~net ~clock

module Codex_auth = struct
  type t = { path : string; access_token : string }

  let codex_home ~caps () =
    match Ox_caps.Cap_sys.getenv_opt ~caps "CODEX_HOME" with
    | Some value when String.trim value <> "" -> String.trim value
    | _ -> (
        match Ox_caps.Cap_sys.getenv_opt ~caps "HOME" with
        | Some home when String.trim home <> "" -> Filename.concat home ".codex"
        | _ -> ".codex")

  let auth_path ~caps () = Filename.concat (codex_home ~caps ()) "auth.json"

  let read_file ~caps path =
    let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        really_input_string ic len)

  let load ~caps ?path () =
    let path = Option.value path ~default:(auth_path ~caps ()) in
    try
      let raw = read_file ~caps path in
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
            ^ " is not valid JSON. Run 'codex login' to recreate it."))

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
            with
            | Eio.Cancel.Cancelled _ as exn -> raise exn
            | _ -> None))
    | _ -> None

  let is_expired ?(leeway_seconds = 60) ~now token =
    match decode_jwt_exp_seconds token with
    | None -> false
    | Some exp ->
        (now /. 1000.) +. float_of_int leeway_seconds >= float_of_int exp

  let access_token ~caps () =
    let* auth = load ~caps () in
    let now = Ox_caps.Cap_unix.gettimeofday ~caps () *. 1000. in
    if not (is_expired ~now auth.access_token) then Ok auth.access_token
    else
      Error
        (`Stt_error
           ("Codex access token at " ^ auth.path
          ^ " is expired. Run 'codex login' so the Codex credential owner can \
             refresh it."))
end

let realtime_access_token ~caps ~client:_ () =
  match Ox_caps.Cap_sys.getenv_opt ~caps "OPENAI_API_KEY" with
  | Some value when String.trim value <> "" -> Ok (String.trim value)
  | _ -> Codex_auth.access_token ~caps ()

let websocket_message = function
  | `Url _ -> "invalid URL"
  | `Connect _ -> "connect failed"
  | `Tls _ -> "TLS failed"
  | `Upgrade_failed (status, _) ->
      Printf.sprintf "upgrade failed with HTTP %d" status
  | `Closed (code, _) -> Printf.sprintf "closed (%d)" code
  | `Protocol _ -> "protocol error"
  | `Message_too_large -> "message too large"
  | `Timeout -> "timeout"

let session_error = function
  | #Ws.error as error -> websocket_message error
  | `Invalid_event _ -> "invalid event"

let send_json session json =
  Session.send session json
  |> Result.map_error (fun error ->
      `Stt_error ("Codex realtime " ^ websocket_message error))

let server_error_diagnostic _code = "Codex realtime server error"

let event_should_stop ~mode:_ = function
  | Server_error _ | Transcript_final _ -> true
  | Created _ | Config_ack _ | Transcript_delta _ | Other _ -> false

type session_result = {
  final : string option;
  deltas : string list;
  output_bytes : int;
}

let empty_session_result = { final = None; deltas = []; output_bytes = 0 }

let receive_event session =
  match Session.receive session with
  | Error error -> stt_error ("Codex realtime " ^ session_error error)
  | Ok event -> parse_event (Ox_json.to_string event.raw)

let await_session_ready session =
  let rec loop () =
    let* event =
      match receive_event session with
      | Error (`Stt_error "Codex realtime closed (1000)") ->
          stt_error "Codex realtime session acknowledgement missing"
      | result -> result
    in
    match event with
    | Config_ack _ -> Ok ()
    | Server_error { code; _ } -> stt_error (server_error_diagnostic code)
    | _ -> loop ()
  in
  loop ()

let receive_result ?on_text ~mode:_ session =
  let rec loop state =
    let* event =
      match receive_event session with
      | Error (`Stt_error "Codex realtime closed (1000)") ->
          stt_error "Codex realtime response incomplete"
      | result -> result
    in
    let admit ~replace text =
      let n = String.length text in
      let used = if replace then 0 else state.output_bytes in
      if used + n > Dictation_limits.result_utf8_bytes then
        stt_error "Codex realtime result budget exceeded"
      else (
        Option.iter (fun callback -> callback text) on_text;
        Ok
          (if replace then { final = Some text; deltas = []; output_bytes = n }
           else
             {
               state with
               deltas = text :: state.deltas;
               output_bytes = used + n;
             }))
    in
    let* state, done_ =
      match event with
      | Server_error { code; _ } -> stt_error (server_error_diagnostic code)
      | Transcript_delta text when text <> "" ->
          let* state = admit ~replace:false text in
          Ok (state, false)
      | Transcript_final text ->
          let* state = admit ~replace:true text in
          Ok (state, true)
      | Created _ | Config_ack _ | Transcript_delta _ | Other _ ->
          Ok (state, false)
    in
    if done_ then Ok state else loop state
  in
  loop empty_session_result

let collected_text result =
  match result.final with
  | Some text -> text
  | None -> result.deltas |> List.rev |> String.concat "" |> String.trim

let finish_result ~mode:_ result = Ok (collected_text result)

let send_audio_chunks session ~mode pcm =
  let rec send offset =
    if offset >= Bytes.length pcm then Ok ()
    else
      let n = min 32_000 (Bytes.length pcm - offset) in
      let* () =
        send_json session (build_audio_append ~mode (Bytes.sub pcm offset n))
      in
      send (offset + n)
  in
  send 0

let send_audio_stream session ~mode ~input_sample_rate audio =
  match
    Audio.Resampler.create ~from_rate:input_sample_rate
      ~to_rate:realtime_sample_rate
  with
  | Error (`Resample_error message) -> stt_error ("Codex realtime " ^ message)
  | Ok resampler ->
      let captured = ref 0 in
      let rec send () =
        let* chunk =
          audio ()
          |> Result.map_error (fun _ ->
              `Stt_error "Codex realtime audio source failed")
        in
        match chunk with
        | None ->
            let* tail =
              Audio.Resampler.finish resampler
              |> Result.map_error (fun (`Resample_error message) ->
                  `Stt_error ("Codex realtime " ^ message))
            in
            send_audio_chunks session ~mode tail
        | Some chunk ->
            if
              Bytes.length chunk
              > Dictation_limits.capture_pcm_bytes - !captured
            then stt_error "Codex realtime captured audio budget exceeded"
            else (
              captured := !captured + Bytes.length chunk;
              let* pcm =
                Audio.Resampler.push resampler chunk
                |> Result.map_error (fun (`Resample_error message) ->
                    `Stt_error ("Codex realtime " ^ message))
              in
              let* () = send_audio_chunks session ~mode pcm in
              send ())
      in
      send ()

let connect ~caps ~sw ~net ~clock ?ca_file ~url ~access_token () =
  let ca_file =
    match ca_file with
    | Some _ -> ca_file
    | None -> Ox_caps.Cap_sys.getenv_opt ~caps "SSL_CERT_FILE"
  in
  let session_id = make_realtime_session_id ~caps () in
  let headers =
    realtime_header_pairs ~session_id access_token
    |> List.filter (fun (key, _) -> key <> "Authorization")
  in
  Session.connect ~sw ~net ~clock ?ca_file
    ~api_key:(Ox_observability.Redact.make access_token)
    ~headers ~establishment_timeout:(Ox.Duration.seconds 10) url
  |> Result.map_error (fun error ->
      `Stt_error ("Codex realtime " ^ websocket_message error))

let run_realtime_session ~caps ~sw ~net ~clock ?ca_file ~url ~access_token ~mode
    ?session_update ~input_sample_rate pcm_bytes =
  if Bytes.length pcm_bytes > Dictation_limits.capture_pcm_bytes then
    stt_error "Codex realtime captured audio budget exceeded"
  else if input_sample_rate <= 0 then
    stt_error "Codex realtime input sample rate must be positive"
  else
    let* pcm =
      resample_pcm ~from_rate:input_sample_rate ~to_rate:realtime_sample_rate
        pcm_bytes
      |> Result.map_error (fun message ->
          `Stt_error ("Codex realtime " ^ message))
    in
    Eio.Switch.run (fun sw ->
        let* session =
          connect ~caps ~sw ~net ~clock ?ca_file ~url ~access_token ()
        in
        let* () =
          match session_update with
          | None -> Ok ()
          | Some json ->
              let* () = send_json session json in
              await_session_ready session
        in
        let send () =
          let* () = send_audio_chunks session ~mode pcm in
          send_json session (build_audio_commit ())
        in
        let receive () = receive_result ~mode session in
        let* result, () = Ox.Concurrent.par ~sw receive send in
        finish_result ~mode result)

let make_pull_queue () =
  ( Queue.create (),
    Eio.Mutex.create (),
    Eio.Condition.create (),
    ref None,
    ref 0 )

let queue_pull (queue, mutex, changed, terminal, queued_bytes) () =
  Eio.Mutex.lock mutex;
  Fun.protect
    ~finally:(fun () -> Eio.Mutex.unlock mutex)
    (fun () ->
      let rec next () =
        match Queue.take_opt queue with
        | Some chunk ->
            queued_bytes := !queued_bytes - Bytes.length chunk;
            Ok (Some chunk)
        | None -> (
            match !terminal with
            | Some state -> state
            | None ->
                Eio.Condition.await changed mutex;
                next ())
      in
      next ())

let queue_offer (queue, mutex, changed, terminal, queued_bytes) chunk =
  Eio.Mutex.lock mutex;
  Fun.protect
    ~finally:(fun () -> Eio.Mutex.unlock mutex)
    (fun () ->
      if !terminal <> None then stt_error "Codex realtime stream closed"
      else if
        Bytes.length chunk > Dictation_limits.queue_pcm_bytes - !queued_bytes
      then stt_error "Codex realtime audio queue budget exceeded"
      else (
        Queue.add chunk queue;
        queued_bytes := !queued_bytes + Bytes.length chunk;
        Eio.Condition.broadcast changed;
        Ok ()))

let queue_close (queue, mutex, changed, terminal, _) outcome =
  Eio.Mutex.lock mutex;
  Fun.protect
    ~finally:(fun () -> Eio.Mutex.unlock mutex)
    (fun () ->
      terminal := Some outcome;
      Eio.Condition.broadcast changed)

let run_realtime_stream_session ~caps ~sw ~net ~clock ?ca_file ~url
    ~access_token ~mode ?session_update ?on_text ~input_sample_rate audio =
  if input_sample_rate <= 0 then
    stt_error "Codex realtime input sample rate must be positive"
  else
    Eio.Switch.run (fun sw ->
        let* session =
          connect ~caps ~sw ~net ~clock ?ca_file ~url ~access_token ()
        in
        let* () =
          match session_update with
          | None -> Ok ()
          | Some json ->
              let* () = send_json session json in
              await_session_ready session
        in
        let send () =
          let* () = send_audio_stream session ~mode ~input_sample_rate audio in
          send_json session (build_audio_commit ())
        in
        let receive () = receive_result ?on_text ~mode session in
        let* result, () = Ox.Concurrent.par ~sw receive send in
        finish_result ~mode result)

type stream_handle = {
  offer_chunk : bytes -> (unit, error) result;
  finish_stream : unit -> (string, error) result;
  cancel_stream : unit -> (unit, error) result;
}

let with_stream_session ~caps ~sw ~net ~clock ~logger ~url ~access_token ~mode
    ~operation ~model ?session_update ?(sample_rate = 16000) ~cancel_message use
    =
  let queue = make_pull_queue () in
  let completion, complete = Eio.Promise.create () in
  let captured = ref 0 in
  let timer = First_token_timer.create () in
  let on_text = log_first_token ~clock ~logger ~timer ~operation ~model ~mode in
  let offer_chunk chunk =
    let queue_items, mutex, changed, terminal, queued_bytes = queue in
    Eio.Mutex.lock mutex;
    let result =
      Fun.protect
        ~finally:(fun () -> Eio.Mutex.unlock mutex)
        (fun () ->
          if !terminal <> None then stt_error "Codex realtime stream closed"
          else if
            Bytes.length chunk > Dictation_limits.capture_pcm_bytes - !captured
          then stt_error "Codex realtime captured audio budget exceeded"
          else if Queue.length queue_items >= Dictation_limits.queue_items then
            stt_error "Codex realtime queued audio budget exceeded"
          else
            let result =
              if Bytes.length chunk = 0 then Ok ()
              else if Queue.length queue_items >= Dictation_limits.queue_items
              then stt_error "Codex realtime queued audio budget exceeded"
              else if
                Bytes.length chunk
                > Dictation_limits.queue_pcm_bytes - !queued_bytes
              then stt_error "Codex realtime queued audio budget exceeded"
              else (
                Queue.add (Bytes.copy chunk) queue_items;
                queued_bytes := !queued_bytes + Bytes.length chunk;
                Eio.Condition.broadcast changed;
                Ok ())
            in
            match result with
            | Error _ as error -> error
            | Ok () ->
                captured := !captured + Bytes.length chunk;
                Ok ())
    in
    result
  in
  let pull () = queue_pull queue () in
  let finish_stream () =
    First_token_timer.start timer
      ~at_ms:(int_of_float (Eio.Time.now clock *. 1000.));
    queue_close queue (Ok None);
    Eio.Promise.await completion
  in
  let cancel_stream () =
    let error = `Stt_error cancel_message in
    queue_close queue (Error error);
    Ok ()
  in
  let background () =
    let result =
      run_realtime_stream_session ~caps ~sw ~net ~clock ~url ~access_token ~mode
        ?session_update ~on_text ~input_sample_rate:sample_rate pull
    in
    Eio.Promise.resolve complete result;
    queue_close queue
      (match result with Ok _ -> Ok None | Error error -> Error error);
    Ok ()
  in
  Ox.Concurrent.with_background ~sw background (fun () ->
      use { offer_chunk; finish_stream; cancel_stream })

let build_pass1_session_update ?language ?context ~model () =
  let transcription_fields =
    [ ("model", `String model) ]
    @ (match context with
      | None -> []
      | Some text -> [ ("prompt", `String text) ])
    @
    match language with
    | None -> []
    | Some code -> [ ("languages", `List [ `String code ]) ]
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
                        ("turn_detection", `Null);
                      ] );
                ] );
          ] );
    ]

let with_pass1_stream ~caps ~sw ~net ~clock ~logger ~model ?language ?context
    ?(sample_rate = 16000) use =
  let* access_token = realtime_access_token ~caps ~client:() () in
  with_stream_session ~caps ~sw ~net ~clock ~logger
    ~url:(build_realtime_url ~caps ~mode:Transcription ~model ())
    ~access_token ~mode:Transcription ~operation:"transcribe" ~model
    ~session_update:(build_pass1_session_update ?language ?context ~model ())
    ~sample_rate ~cancel_message:"Codex realtime transcription cancelled" use

let transcribe_pass1 ~caps ~sw ~net ~clock ~model ?language ?context
    ?(sample_rate = 16000) pcm_bytes =
  let* access_token = realtime_access_token ~caps ~client:() () in
  run_realtime_session ~caps ~sw ~net ~clock
    ~url:(build_realtime_url ~caps ~mode:Transcription ~model ())
    ~access_token ~mode:Transcription
    ~session_update:(build_pass1_session_update ?language ?context ~model ())
    ~input_sample_rate:sample_rate pcm_bytes
