type recording_snapshot = {
  phase : Recording.Phase.t;
  active : bool;
  mode : string;
  enabled : bool;
  started_at : string option;
  updated_at : string option;
  transcript_path : string option;
  last_error : string option;
  build_commit : string option;
  build_time : string option;
}

type request = Status | Pause | Resume | Toggle | Action of string

type action_result = {
  action : string;
  accepted : bool;
  message : string option;
  snapshot : recording_snapshot;
}

type response =
  | Status_resp of recording_snapshot
  | Pause_resp
  | Resume_resp
  | Toggle_resp of bool
  | Action_resp of action_result
  | Error_resp of string

type protocol_error = string

let not_object = "not-object"
let duplicate_field = "duplicate-field"
let missing_field = "missing-field"
let wrong_type = "wrong-type"
let unexpected_field = "unexpected-field"
let unknown_tag = "unknown-tag"
let malformed_error = "malformed-error"
let action_mismatch = "action-mismatch"
let protocol_error_label label = label
let ( let* ) = Result.bind

let no_duplicate_keys fields =
  let rec loop seen = function
    | [] -> Ok ()
    | (key, _) :: rest ->
        if List.exists (String.equal key) seen then Error duplicate_field
        else loop (key :: seen) rest
  in
  loop [] fields

let assoc json =
  match json with
  | `Assoc fields ->
      let* () = no_duplicate_keys fields in
      Ok fields
  | _ -> Error not_object

let required_bool key fields =
  match List.assoc_opt key fields with
  | None -> Error missing_field
  | Some (`Bool value) -> Ok value
  | Some _ -> Error wrong_type

let required_string key fields =
  match List.assoc_opt key fields with
  | None -> Error missing_field
  | Some (`String value) -> Ok value
  | Some _ -> Error wrong_type

let optional_string key fields =
  match List.assoc_opt key fields with
  | None | Some `Null -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some _ -> Error wrong_type

let reject_unknown allowed fields =
  let rec loop = function
    | [] -> Ok ()
    | (key, _) :: rest ->
        if List.exists (String.equal key) allowed then loop rest
        else Error unexpected_field
  in
  loop fields

let snapshot_keys =
  [
    "phase";
    "active";
    "mode";
    "enabled";
    "startedAt";
    "updatedAt";
    "transcriptPath";
    "lastError";
    "buildCommit";
    "buildTime";
  ]

let snapshot_of_json json =
  let* fields = assoc json in
  let* () = reject_unknown snapshot_keys fields in
  let* active = required_bool "active" fields in
  let* mode = required_string "mode" fields in
  let* enabled = required_bool "enabled" fields in
  let* started_at = optional_string "startedAt" fields in
  let* updated_at = optional_string "updatedAt" fields in
  let* transcript_path = optional_string "transcriptPath" fields in
  let* last_error = optional_string "lastError" fields in
  let* build_commit = optional_string "buildCommit" fields in
  let* build_time = optional_string "buildTime" fields in
  let* phase =
    match List.assoc_opt "phase" fields with
    | None -> Ok (Recording.Phase.legacy ~active ~enabled ~mode ~last_error)
    | Some (`String value) -> (
        match Recording.Phase.of_string value with
        | Some phase -> Ok phase
        | None -> Error unknown_tag)
    | Some _ -> Error wrong_type
  in
  Ok
    {
      phase;
      active;
      mode;
      enabled;
      started_at;
      updated_at;
      transcript_path;
      last_error;
      build_commit;
      build_time;
    }

let json_of_snapshot snap =
  `Assoc
    [
      ("phase", `String (Recording.Phase.to_string snap.phase));
      ("active", `Bool snap.active);
      ("mode", `String snap.mode);
      ("enabled", `Bool snap.enabled);
      ( "startedAt",
        match snap.started_at with Some s -> `String s | None -> `Null );
      ( "updatedAt",
        match snap.updated_at with Some s -> `String s | None -> `Null );
      ( "transcriptPath",
        match snap.transcript_path with Some s -> `String s | None -> `Null );
      ( "lastError",
        match snap.last_error with Some s -> `String s | None -> `Null );
      ( "buildCommit",
        match snap.build_commit with Some s -> `String s | None -> `Null );
      ( "buildTime",
        match snap.build_time with Some s -> `String s | None -> `Null );
    ]

let json_of_action_result result =
  `Assoc
    [
      ("action", `String result.action);
      ("accepted", `Bool result.accepted);
      ( "message",
        match result.message with Some s -> `String s | None -> `Null );
      ("snapshot", json_of_snapshot result.snapshot);
    ]

let tag_of_request = function
  | Status -> "Status"
  | Pause -> "Pause"
  | Resume -> "Resume"
  | Toggle -> "Toggle"
  | Action _ -> "Action"

let request_of_tag = function
  | "Status" -> Ok Status
  | "Pause" -> Ok Pause
  | "Resume" -> Ok Resume
  | "Toggle" -> Ok Toggle
  | "Action" -> Ok (Action "")
  | tag -> Error ("unknown method: " ^ tag)

let request_to_json = function
  | Action action ->
      `Assoc [ ("method", `String "Action"); ("action", `String action) ]
  | req -> `Assoc [ ("method", `String (tag_of_request req)) ]

let request_of_json = function
  | `Assoc fields -> (
      match List.assoc_opt "method" fields with
      | Some (`String "Action") -> (
          match List.assoc_opt "action" fields with
          | Some (`String action) when String.trim action <> "" ->
              Ok (Action action)
          | _ -> Error "missing action")
      | Some (`String method_) -> request_of_tag method_
      | _ -> Error "missing method")
  | _ -> Error "request must be an object"

let response_to_json = function
  | Status_resp snap -> json_of_snapshot snap
  | Pause_resp | Resume_resp -> `Assoc []
  | Toggle_resp value -> `Assoc [ ("value", `Bool value) ]
  | Action_resp result -> json_of_action_result result
  | Error_resp message -> `Assoc [ ("error", `String message) ]

let decode_pause_resume json =
  let* fields = assoc json in
  match fields with [] -> Ok () | _ -> Error unexpected_field

let decode_toggle json =
  let* fields = assoc json in
  let* () = reject_unknown [ "value" ] fields in
  required_bool "value" fields

let decode_action json =
  let* fields = assoc json in
  let* () =
    reject_unknown [ "action"; "accepted"; "message"; "snapshot" ] fields
  in
  let* action = required_string "action" fields in
  let* accepted = required_bool "accepted" fields in
  let* message = optional_string "message" fields in
  let* snapshot_json =
    match List.assoc_opt "snapshot" fields with
    | None -> Error missing_field
    | Some json -> Ok json
  in
  let* snapshot = snapshot_of_json snapshot_json in
  Ok { action; accepted; message; snapshot }

let decode_for_request req json =
  match req with
  | Status ->
      let* snap = snapshot_of_json json in
      Ok (Status_resp snap)
  | Pause ->
      let* () = decode_pause_resume json in
      Ok Pause_resp
  | Resume ->
      let* () = decode_pause_resume json in
      Ok Resume_resp
  | Toggle ->
      let* value = decode_toggle json in
      Ok (Toggle_resp value)
  | Action expected ->
      let* result = decode_action json in
      if String.equal result.action expected then Ok (Action_resp result)
      else Error action_mismatch

let response_of_json req json =
  match json with
  | `Assoc fields -> (
      let* () = no_duplicate_keys fields in
      match List.assoc_opt "error" fields with
      | Some (`String message) -> Ok (Error_resp message)
      | Some _ -> Error malformed_error
      | None -> decode_for_request req json)
  | _ -> Error not_object

let decode_line line =
  match Yojson.Safe.from_string (String.trim line) with
  | json -> request_of_json json
  | exception Yojson.Json_error _ -> Error "malformed-request"

let encode_response response = Yojson.Safe.to_string (response_to_json response)

let snapshot_of_status_response = function
  | Status_resp snap -> Ok snap
  | _ -> Error wrong_type
