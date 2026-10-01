let ( let* ) = Result.bind

type mode = Transcribe | Translate

let string_of_mode = function
  | Transcribe -> "transcribe"
  | Translate -> "translate"

let mode_of_string = function
  | "transcribe" -> Some Transcribe
  | "translate" -> Some Translate
  | _ -> None

type outcome =
  | Delivered
  | Delivered_raw_after_correction_failure
  | Correction_failed
  | Delivery_failed
  | Failed

let string_of_outcome = function
  | Delivered -> "delivered"
  | Delivered_raw_after_correction_failure ->
      "delivered-raw-after-correction-failure"
  | Correction_failed -> "correction-failed"
  | Delivery_failed -> "delivery-failed"
  | Failed -> "failed"

let outcome_of_string = function
  | "delivered" -> Some Delivered
  | "delivered-raw-after-correction-failure" ->
      Some Delivered_raw_after_correction_failure
  | "correction-failed" -> Some Correction_failed
  | "delivery-failed" -> Some Delivery_failed
  | "failed" -> Some Failed
  | _ -> None

type window = { app_id : string option; title : string option }

type transcribed = {
  id : string;
  time : float;
  mode : mode;
  binding : string;
  window : window option;
  glossary_paths : string list;
  pass1_model : string;
  pass1_duration_ms : int;
  raw_text : string;
}

type retry = {
  first_diagnostic : string;
  final_diagnostic : string;
  audio_format : string option;
  reasoning_effort : string option;
}

type finished = {
  id : string;
  time : float;
  final_text : string option;
  pass2_model : string option;
  pass2_duration_ms : int option;
  outcome : outcome;
  diagnostic : string option;
  audio_path : string option;
  response_path : string option;
  first_response_path : string option;
  retry : retry option;
}

type entry = {
  id : string;
  time : float;
  mode : mode option;
  binding : string option;
  window : window option;
  glossary_paths : string list;
  pass1_model : string option;
  pass1_duration_ms : int option;
  raw_text : string option;
  final_text : string option;
  pass2_model : string option;
  pass2_duration_ms : int option;
  outcome : outcome option;
  diagnostic : string option;
  finished_at : float option;
  audio_path : string option;
  response_path : string option;
  first_response_path : string option;
  retry : retry option;
}

type audio_recorded = { id : string; time : float; path : string }

type event =
  | Transcribed of transcribed
  | Finished of finished
  | Audio of audio_recorded

let file_name = "history.jsonl"
let path ~caps = Filename.concat (Paths.data_dir ~caps) file_name
let err message = Error (`History message)

let describe_binding ~keysym ~shift ~pass2 =
  Printf.sprintf "keysym=%d%s%s" keysym
    (if shift then "+shift" else "")
    (if pass2 then " pass2" else "")

let id_counter = Atomic.make 0

let fresh_id ~caps () =
  let n = Atomic.fetch_and_add id_counter 1 in
  let micros =
    Int64.of_float (Ox_caps.Cap_unix.gettimeofday ~caps () *. 1_000_000.)
  in
  Printf.sprintf "%d-%d-%Ld" (Unix.getpid ()) n micros

let validate_id id =
  let n = String.length id in
  if n = 0 then err "attempt id is empty"
  else if n > 128 then err "attempt id is too long"
  else if String.equal id "last" then err "attempt id 'last' is reserved"
  else
    let rec loop i =
      if i = n then Ok ()
      else
        let c = id.[i] in
        if c <= ' ' || c = '\x7f' then
          err "attempt id contains a control character"
        else loop (i + 1)
    in
    loop 0

let validate_time time =
  if Float.is_finite time && time >= 0. then Ok ()
  else err "history time is not a finite non-negative timestamp"

let validate_duration name duration =
  if duration >= 0 then Ok () else err (name ^ " duration is negative")

(* Howard Hinnant's days-from-civil. Returns days since the Unix epoch. *)
let days_from_civil year month day =
  let year = if month <= 2 then year - 1 else year in
  let era = (if year >= 0 then year else year - 399) / 400 in
  let year_of_era = year - (era * 400) in
  let day_of_year =
    (((153 * (month + if month > 2 then -3 else 9)) + 2) / 5) + day - 1
  in
  let day_of_era =
    (year_of_era * 365) + (year_of_era / 4) - (year_of_era / 100) + day_of_year
  in
  (era * 146097) + day_of_era - 719468

let iso_of_unix time =
  let millis = Float.round (time *. 1000.) in
  if (not (Float.is_finite millis)) || millis < 0. || millis > float Int.max_int
  then "invalid-time"
  else
    let millis = int_of_float millis in
    let seconds = millis / 1000 in
    let fraction = millis mod 1000 in
    let tm = Unix.gmtime (float seconds) in
    Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ" (tm.tm_year + 1900)
      (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec fraction

let unix_of_iso text =
  let len = String.length text in
  if len <> 20 && len <> 24 then None
  else
    try
      let year = int_of_string (String.sub text 0 4) in
      let month = int_of_string (String.sub text 5 2) in
      let day = int_of_string (String.sub text 8 2) in
      let hour = int_of_string (String.sub text 11 2) in
      let minute = int_of_string (String.sub text 14 2) in
      let second = int_of_string (String.sub text 17 2) in
      let millis, terminated =
        if len = 20 && text.[19] = 'Z' then (0, true)
        else if len = 24 && text.[19] = '.' && text.[23] = 'Z' then
          (int_of_string (String.sub text 20 3), true)
        else (0, false)
      in
      if
        (not terminated)
        || text.[4] <> '-'
        || text.[7] <> '-'
        || text.[10] <> 'T'
        || text.[13] <> ':'
        || text.[16] <> ':'
        || year < 1970 || month < 1 || month > 12 || day < 1 || day > 31
        || hour < 0 || hour > 23 || minute < 0 || minute > 59 || second < 0
        || second > 59 || millis < 0 || millis > 999
      then None
      else
        let days = days_from_civil year month day in
        Some
          ((float days *. 86400.)
          +. (float hour *. 3600.)
          +. (float minute *. 60.)
          +. float second
          +. (float millis /. 1000.))
    with Failure _ -> None

let json_string_opt = function None -> `Null | Some text -> `String text
let json_int_opt = function None -> `Null | Some n -> `Int n

let json_window = function
  | None -> `Null
  | Some window ->
      `Assoc
        [
          ("appId", json_string_opt window.app_id);
          ("title", json_string_opt window.title);
        ]

let json_retry = function
  | None -> `Null
  | Some retry ->
      `Assoc
        [
          ("firstDiagnostic", `String retry.first_diagnostic);
          ("finalDiagnostic", `String retry.final_diagnostic);
          ("audioFormat", json_string_opt retry.audio_format);
          ("reasoningEffort", json_string_opt retry.reasoning_effort);
        ]

let json_of_entry (entry : entry) =
  `Assoc
    [
      ("id", `String entry.id);
      ("time", `String (iso_of_unix entry.time));
      ( "mode",
        match entry.mode with
        | None -> `Null
        | Some mode -> `String (string_of_mode mode) );
      ("binding", json_string_opt entry.binding);
      ("window", json_window entry.window);
      ( "glossaryPaths",
        `List (List.map (fun path -> `String path) entry.glossary_paths) );
      ("pass1Model", json_string_opt entry.pass1_model);
      ("pass1DurationMs", json_int_opt entry.pass1_duration_ms);
      ("rawText", json_string_opt entry.raw_text);
      ("finalText", json_string_opt entry.final_text);
      ("pass2Model", json_string_opt entry.pass2_model);
      ("pass2DurationMs", json_int_opt entry.pass2_duration_ms);
      ( "outcome",
        match entry.outcome with
        | None -> `Null
        | Some outcome -> `String (string_of_outcome outcome) );
      ("diagnostic", json_string_opt entry.diagnostic);
      ( "finishedAt",
        match entry.finished_at with
        | None -> `Null
        | Some time -> `String (iso_of_unix time) );
      ("audioPath", json_string_opt entry.audio_path);
      ("responsePath", json_string_opt entry.response_path);
      ("firstResponsePath", json_string_opt entry.first_response_path);
      ("retry", json_retry entry.retry);
    ]

let json_of_entries entries = `List (List.map json_of_entry entries)

let event_json = function
  | Transcribed event ->
      let fields =
        [
          ("v", `Int 1);
          ("event", `String "transcribed");
          ("id", `String event.id);
          ("time", `String (iso_of_unix event.time));
          ("mode", `String (string_of_mode event.mode));
          ("binding", `String event.binding);
        ]
      in
      let fields =
        match event.window with
        | None -> fields
        | Some window ->
            let window_fields =
              (match window.app_id with
                | None -> []
                | Some app_id -> [ ("appId", `String app_id) ])
              @
              match window.title with
              | None -> []
              | Some title -> [ ("title", `String title) ]
            in
            if window_fields = [] then fields
            else fields @ [ ("window", `Assoc window_fields) ]
      in
      `Assoc
        (fields
        @ [
            ( "glossaryPaths",
              `List (List.map (fun path -> `String path) event.glossary_paths)
            );
            ("pass1Model", `String event.pass1_model);
            ("pass1DurationMs", `Int event.pass1_duration_ms);
            ("rawText", `String event.raw_text);
          ])
  | Finished event ->
      let add key value fields =
        match value with
        | None -> fields
        | Some json -> fields @ [ (key, json) ]
      in
      let fields =
        [
          ("v", `Int 1);
          ("event", `String "finished");
          ("id", `String event.id);
          ("time", `String (iso_of_unix event.time));
        ]
      in
      let fields =
        add "finalText"
          (Option.map (fun text -> `String text) event.final_text)
          fields
      in
      let fields =
        add "pass2Model"
          (Option.map (fun model -> `String model) event.pass2_model)
          fields
      in
      let fields =
        add "pass2DurationMs"
          (Option.map (fun duration -> `Int duration) event.pass2_duration_ms)
          fields
      in
      let fields =
        fields @ [ ("outcome", `String (string_of_outcome event.outcome)) ]
      in
      let fields =
        add "diagnostic"
          (Option.map (fun text -> `String text) event.diagnostic)
          fields
      in
      let fields =
        add "responsePath"
          (Option.map (fun path -> `String path) event.response_path)
          fields
      in
      let fields =
        add "firstResponsePath"
          (Option.map (fun path -> `String path) event.first_response_path)
          fields
      in
      let fields =
        match event.retry with
        | None -> fields
        | Some _ -> fields @ [ ("retry", json_retry event.retry) ]
      in
      `Assoc
        (add "audioPath"
           (Option.map (fun path -> `String path) event.audio_path)
           fields)
  | Audio event ->
      `Assoc
        [
          ("v", `Int 1);
          ("event", `String "audio");
          ("id", `String event.id);
          ("time", `String (iso_of_unix event.time));
          ("audioPath", `String event.path);
        ]

let encode_line event =
  let line = Yojson.Safe.to_string (event_json event) in
  if String.exists (fun c -> c = '\n' || c = '\r') line then
    err "history event did not encode as one line"
  else Ok (line ^ "\n")

let assoc_fields = function
  | `Assoc fields -> Ok fields
  | _ -> Error "not an object"

let json_int = function
  | `Int n -> Some n
  | `Intlit text -> int_of_string_opt text
  | _ -> None

let require_string fields key =
  match List.assoc_opt key fields with
  | Some (`String text) -> Ok text
  | Some _ -> Error (key ^ " is not a string")
  | None -> Error ("missing " ^ key)

let optional_string fields key =
  match List.assoc_opt key fields with
  | None | Some `Null -> Ok None
  | Some (`String text) -> Ok (Some text)
  | Some _ -> Error (key ^ " is not a string")

let require_int fields key =
  match List.assoc_opt key fields with
  | Some value -> (
      match json_int value with
      | Some n -> Ok n
      | None -> Error (key ^ " is not an integer"))
  | None -> Error ("missing " ^ key)

let optional_int fields key =
  match List.assoc_opt key fields with
  | None | Some `Null -> Ok None
  | Some value -> (
      match json_int value with
      | Some n -> Ok (Some n)
      | None -> Error (key ^ " is not an integer"))

let string_list fields key =
  match List.assoc_opt key fields with
  | Some (`List values) ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | `String text :: rest -> loop (text :: acc) rest
        | _ -> Error (key ^ " is not a list of strings")
      in
      loop [] values
  | Some _ -> Error (key ^ " is not a list of strings")
  | None -> Error ("missing " ^ key)

let retry_of fields =
  match List.assoc_opt "retry" fields with
  | None | Some `Null -> Ok None
  | Some (`Assoc fields) ->
      let* first_diagnostic = require_string fields "firstDiagnostic" in
      let* final_diagnostic = require_string fields "finalDiagnostic" in
      let* audio_format = optional_string fields "audioFormat" in
      let* reasoning_effort = optional_string fields "reasoningEffort" in
      Ok
        (Some
           {
             first_diagnostic;
             final_diagnostic;
             audio_format;
             reasoning_effort;
           })
  | Some _ -> Error "retry is not an object"

let window_of fields =
  match List.assoc_opt "window" fields with
  | None | Some `Null -> Ok None
  | Some (`Assoc window_fields) ->
      let* app_id = optional_string window_fields "appId" in
      let* title = optional_string window_fields "title" in
      if app_id = None && title = None then Ok None
      else Ok (Some { app_id; title })
  | Some _ -> Error "window is not an object"

let time_of fields =
  let* text = require_string fields "time" in
  match unix_of_iso text with
  | Some time -> Ok time
  | None -> Error "time is not UTC ISO-8601"

let event_of_json json =
  let* fields = assoc_fields json in
  let* version = require_int fields "v" in
  if version <> 1 then Error "unsupported history version"
  else
    let* kind = require_string fields "event" in
    let* id = require_string fields "id" in
    let* time = time_of fields in
    match kind with
    | "transcribed" ->
        let* mode_text = require_string fields "mode" in
        let* mode =
          match mode_of_string mode_text with
          | Some mode -> Ok mode
          | None -> Error "mode is not transcribe or translate"
        in
        let* binding = require_string fields "binding" in
        let* window = window_of fields in
        let* glossary_paths = string_list fields "glossaryPaths" in
        let* pass1_model = require_string fields "pass1Model" in
        let* pass1_duration_ms = require_int fields "pass1DurationMs" in
        let* raw_text = require_string fields "rawText" in
        if pass1_duration_ms < 0 then Error "pass1DurationMs is negative"
        else
          Ok
            (Transcribed
               {
                 id;
                 time;
                 mode;
                 binding;
                 window;
                 glossary_paths;
                 pass1_model;
                 pass1_duration_ms;
                 raw_text;
               })
    | "finished" ->
        let* final_text = optional_string fields "finalText" in
        let* pass2_model = optional_string fields "pass2Model" in
        let* pass2_duration_ms = optional_int fields "pass2DurationMs" in
        let* outcome_text = require_string fields "outcome" in
        let* outcome =
          match outcome_of_string outcome_text with
          | Some outcome -> Ok outcome
          | None -> Error "unknown outcome"
        in
        let* diagnostic = optional_string fields "diagnostic" in
        let* audio_path = optional_string fields "audioPath" in
        let* response_path = optional_string fields "responsePath" in
        let* first_response_path = optional_string fields "firstResponsePath" in
        let* retry = retry_of fields in
        if Option.fold ~none:false ~some:(fun n -> n < 0) pass2_duration_ms then
          Error "pass2DurationMs is negative"
        else
          Ok
            (Finished
               {
                 id;
                 time;
                 final_text;
                 pass2_model;
                 pass2_duration_ms;
                 outcome;
                 diagnostic;
                 audio_path;
                 response_path;
                 first_response_path;
                 retry;
               })
    | "audio" ->
        let* path = require_string fields "audioPath" in
        if path = "" then Error "audioPath is empty"
        else Ok (Audio { id; time; path })
    | _ -> Error "unknown history event"

let event_of_line line =
  if line = "" then None
  else
    match Yojson.Safe.from_string line with
    | exception Yojson.Json_error _ -> None
    | json -> (
        match event_of_json json with Ok event -> Some event | Error _ -> None)

let lines_of content =
  if content = "" then []
  else
    let length = String.length content in
    let body =
      if content.[length - 1] = '\n' then String.sub content 0 (length - 1)
      else content
    in
    if body = "" then [] else String.split_on_char '\n' body

type pending = {
  transcribed : transcribed option;
  finished : finished option;
  audio_path : string option;
  sort_time : float;
  sort_index : int;
}

let entry_of id pending =
  let transcribed = pending.transcribed in
  let finished = pending.finished in
  let time =
    match transcribed with
    | Some (event : transcribed) -> event.time
    | None -> (
        match finished with
        | Some (event : finished) -> event.time
        | None -> pending.sort_time)
  in
  {
    id;
    time;
    mode = Option.map (fun (event : transcribed) -> event.mode) transcribed;
    binding =
      Option.map (fun (event : transcribed) -> event.binding) transcribed;
    window = Option.bind transcribed (fun (event : transcribed) -> event.window);
    glossary_paths =
      (match transcribed with
      | Some (event : transcribed) -> event.glossary_paths
      | None -> []);
    pass1_model =
      Option.map (fun (event : transcribed) -> event.pass1_model) transcribed;
    pass1_duration_ms =
      Option.map
        (fun (event : transcribed) -> event.pass1_duration_ms)
        transcribed;
    raw_text =
      Option.map (fun (event : transcribed) -> event.raw_text) transcribed;
    final_text =
      Option.bind finished (fun (event : finished) -> event.final_text);
    pass2_model =
      Option.bind finished (fun (event : finished) -> event.pass2_model);
    pass2_duration_ms =
      Option.bind finished (fun (event : finished) -> event.pass2_duration_ms);
    outcome = Option.map (fun (event : finished) -> event.outcome) finished;
    diagnostic =
      Option.bind finished (fun (event : finished) -> event.diagnostic);
    finished_at = Option.map (fun (event : finished) -> event.time) finished;
    audio_path = pending.audio_path;
    response_path =
      Option.bind finished (fun (event : finished) -> event.response_path);
    first_response_path =
      Option.bind finished (fun (event : finished) -> event.first_response_path);
    retry = Option.bind finished (fun (event : finished) -> event.retry);
  }

let merge events =
  let table = Hashtbl.create 16 in
  let order = ref [] in
  List.iteri
    (fun index event ->
      let id, time =
        match event with
        | Transcribed event -> (event.id, event.time)
        | Finished event -> (event.id, event.time)
        | Audio event -> (event.id, event.time)
      in
      let audio_path_of = function
        | Audio event -> Some event.path
        | Finished event -> event.audio_path
        | Transcribed _ -> None
      in
      match Hashtbl.find_opt table id with
      | None ->
          order := id :: !order;
          Hashtbl.add table id
            {
              transcribed =
                (match event with
                | Transcribed event -> Some event
                | Finished _ | Audio _ -> None);
              finished =
                (match event with
                | Finished event -> Some event
                | Transcribed _ | Audio _ -> None);
              audio_path = audio_path_of event;
              sort_time = time;
              sort_index = index;
            }
      | Some previous ->
          let audio_path =
            match audio_path_of event with
            | Some path -> Some path
            | None -> previous.audio_path
          in
          let next =
            match event with
            | Transcribed event ->
                { previous with transcribed = Some event; audio_path }
            | Finished event ->
                { previous with finished = Some event; audio_path }
            | Audio _ -> { previous with audio_path }
          in
          let sort_time, sort_index =
            if
              time < previous.sort_time
              || (time = previous.sort_time && index < previous.sort_index)
            then (time, index)
            else (previous.sort_time, previous.sort_index)
          in
          Hashtbl.replace table id { next with sort_time; sort_index })
    events;
  List.filter_map
    (fun id ->
      match Hashtbl.find_opt table id with
      | None -> None
      | Some pending -> Some (id, pending))
    (List.rev !order)
  |> List.sort (fun (_, left) (_, right) ->
      let by_time = Float.compare right.sort_time left.sort_time in
      if by_time <> 0 then by_time
      else Int.compare right.sort_index left.sort_index)
  |> List.map (fun (id, pending) -> entry_of id pending)

let read_contents ~caps path =
  match Unix.lstat path with
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> Ok None
  | exception Unix.Unix_error (error, _, _) ->
      err ("history: " ^ Unix.error_message error)
  | { st_kind = Unix.S_LNK; _ } -> err "history file is a symbolic link"
  | { st_kind = Unix.S_REG; st_uid; _ } when st_uid <> Unix.geteuid () ->
      err "history file is not owned by this user"
  | { st_kind = Unix.S_REG; _ } -> (
      try
        let channel = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
        Fun.protect
          ~finally:(fun () -> In_channel.close_noerr channel)
          (fun () -> Ok (Some (In_channel.input_all channel)))
      with
      | Unix.Unix_error (error, _, _) ->
          err ("history: " ^ Unix.error_message error)
      | Sys_error message -> err ("history: " ^ message))
  | _ -> err "history file is not a regular file"

let read ~caps ~path =
  let* contents = read_contents ~caps path in
  match contents with
  | None -> Ok []
  | Some contents ->
      Ok (merge (List.filter_map event_of_line (lines_of contents)))

(* Create only missing ancestors at 0700. Existing directories are left
   untouched, including a historical 0755 parent. *)
let rec ensure_private_missing ~caps path =
  if path = "" || path = Filename.dirname path then ()
  else
    match Unix.lstat path with
    | { st_kind = Unix.S_DIR; _ } -> ()
    | _ -> raise (Unix.Unix_error (Unix.EEXIST, "mkdir", path))
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
        ensure_private_missing ~caps (Filename.dirname path);
        try Ox_caps.Cap_unix.mkdir ~caps path 0o700
        with Unix.Unix_error (Unix.EEXIST, _, _) -> (
          match Unix.lstat path with
          | { st_kind = Unix.S_DIR; _ } -> ()
          | _ -> raise (Unix.Unix_error (Unix.EEXIST, "mkdir", path))))

let private_reg stats =
  stats.Unix.st_kind = Unix.S_REG
  && stats.st_uid = Unix.geteuid ()
  && stats.st_perm land 0o777 = 0o600

let close_fd fd = try Unix.close fd with Unix.Unix_error _ -> ()

let unlink_quiet ~caps path =
  try Ox_caps.Cap_unix.unlink ~caps path with Unix.Unix_error _ -> ()

let needs_repair fd =
  let size = (Unix.fstat fd).Unix.st_size in
  if size <= 0 then false
  else (
    ignore (Unix.lseek fd (size - 1) Unix.SEEK_SET);
    let byte = Bytes.create 1 in
    match Unix.read fd byte 0 1 with
    | 1 -> Bytes.get byte 0 <> '\n'
    | _ -> true
    | exception Unix.Unix_error _ -> true)

let write_line fd line =
  let payload = if needs_repair fd then "\n" ^ line else line in
  let bytes = Bytes.of_string payload in
  let length = Bytes.length bytes in
  let wrote = Unix.single_write fd bytes 0 length in
  if wrote <> length then err "short write to history"
  else (
    Unix.fsync fd;
    Ok ())

let finish fd result =
  let close_error =
    match Unix.close fd with
    | () -> None
    | exception Unix.Unix_error (error, _, _) -> Some (Unix.error_message error)
  in
  match (result, close_error) with
  | Ok (), None -> Ok ()
  | (Error _ as error), _ -> error
  | Ok (), Some message -> err ("history: " ^ message)

let open_existing ~caps path stats =
  if stats.Unix.st_kind = Unix.S_LNK then err "history file is a symbolic link"
  else if not (private_reg stats) then
    err "history file is not a private regular file"
  else
    try
      let fd =
        Ox_caps.Cap_unix.openfile ~caps path
          [ Unix.O_RDWR; Unix.O_APPEND; Unix.O_CLOEXEC ]
          0
      in
      let current = Unix.fstat fd in
      if
        stats.st_dev <> current.st_dev
        || stats.st_ino <> current.st_ino
        || not (private_reg current)
      then (
        close_fd fd;
        err "history file is not a private regular file")
      else Ok fd
    with
    | Unix.Unix_error (error, _, _) ->
        err ("history: " ^ Unix.error_message error)
    | Sys_error message -> err ("history: " ^ message)

let create_excl ~caps path =
  try
    let fd =
      Ox_caps.Cap_unix.openfile ~caps path
        [
          Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL; Unix.O_APPEND; Unix.O_CLOEXEC;
        ]
        0o600
    in
    try
      Unix.fchmod fd 0o600;
      let stats = Unix.fstat fd in
      if not (private_reg stats) then (
        close_fd fd;
        unlink_quiet ~caps path;
        `Error (err "could not create a private history file"))
      else `Created fd
    with
    | Unix.Unix_error (error, _, _) ->
        close_fd fd;
        unlink_quiet ~caps path;
        `Error (err ("history: " ^ Unix.error_message error))
    | Sys_error message ->
        close_fd fd;
        unlink_quiet ~caps path;
        `Error (err ("history: " ^ message))
  with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> `Exists
  | Unix.Unix_error (error, _, _) ->
      `Error (err ("history: " ^ Unix.error_message error))
  | Sys_error message -> `Error (err ("history: " ^ message))

let rec open_append ~caps ~attempts path =
  if attempts <= 0 then err "could not open history file"
  else
    match Unix.lstat path with
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> (
        match create_excl ~caps path with
        | `Created fd -> Ok fd
        | `Exists -> open_append ~caps ~attempts:(attempts - 1) path
        | `Error error -> error)
    | exception Unix.Unix_error (error, _, _) ->
        err ("history: " ^ Unix.error_message error)
    | stats -> open_existing ~caps path stats

let append_line ~caps ~path line =
  let prepared =
    try
      ensure_private_missing ~caps (Filename.dirname path);
      Ok ()
    with
    | Unix.Unix_error (error, _, _) ->
        err ("could not create history directory: " ^ Unix.error_message error)
    | Sys_error message -> err ("could not create history directory: " ^ message)
  in
  let* () = prepared in
  let* fd = open_append ~caps ~attempts:3 path in
  try
    let result = write_line fd line in
    finish fd result
  with
  | Unix.Unix_error (error, _, _) ->
      close_fd fd;
      err ("history: " ^ Unix.error_message error)
  | Sys_error message ->
      close_fd fd;
      err ("history: " ^ message)

let append_event ~caps ~path event =
  let* line = encode_line event in
  append_line ~caps ~path line

let append_transcribed ~caps ~path (event : transcribed) =
  let* () = validate_id event.id in
  let* () = validate_time event.time in
  let* () = validate_duration "pass-1" event.pass1_duration_ms in
  append_event ~caps ~path (Transcribed event)

let append_finished ~caps ~path (event : finished) =
  let* () = validate_id event.id in
  let* () = validate_time event.time in
  let* () =
    match event.pass2_duration_ms with
    | None -> Ok ()
    | Some duration -> validate_duration "pass-2" duration
  in
  append_event ~caps ~path (Finished event)

let append_audio ~caps ~path (event : audio_recorded) =
  let* () = validate_id event.id in
  let* () = validate_time event.time in
  if event.path = "" then err "audio path is empty"
  else append_event ~caps ~path (Audio event)

let preview_limit = 80

let scalar_end text index =
  let code = Char.code text.[index] in
  let width =
    if code < 0x80 then 1
    else if code < 0xE0 then 2
    else if code < 0xF0 then 3
    else 4
  in
  let next = index + width in
  if next > String.length text then index + 1 else next

let rec prefix_bytes text max_scalars index count =
  if index >= String.length text || count = max_scalars then index
  else prefix_bytes text max_scalars (scalar_end text index) (count + 1)

let first_line text =
  let text =
    match String.index_opt text '\n' with
    | None -> text
    | Some index -> String.sub text 0 index
  in
  let length = String.length text in
  if length > 0 && text.[length - 1] = '\r' then String.sub text 0 (length - 1)
  else text

let truncate_preview text =
  let text = first_line text in
  let full = prefix_bytes text preview_limit 0 0 in
  if full >= String.length text then text
  else
    let cut = prefix_bytes text (preview_limit - 3) 0 0 in
    String.sub text 0 cut ^ "..."

let format_local time =
  match Unix.localtime time with
  | tm ->
      Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" (tm.tm_year + 1900)
        (tm.tm_mon + 1) tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec
  | exception Unix.Unix_error _ -> iso_of_unix time

let format_listing_line entry =
  let mode =
    match entry.mode with None -> "-" | Some mode -> string_of_mode mode
  in
  let outcome =
    match entry.outcome with
    | None -> "-"
    | Some outcome -> string_of_outcome outcome
  in
  let preview =
    match entry.final_text with
    | Some text -> truncate_preview text
    | None -> (
        match entry.raw_text with
        | Some text -> truncate_preview text
        | None -> "")
  in
  let head =
    Printf.sprintf "%s  %s  %s" (format_local entry.time) mode outcome
  in
  if preview = "" then head else head ^ "  " ^ preview
