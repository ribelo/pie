type recording_snapshot = {
  active : bool;
  mode : string;
  enabled : bool;
  started_at : string option;
  updated_at : string option;
  transcript_path : string option;
  last_error : string option;
}

type request =
  | Status
  | Pause
  | Resume
  | Toggle
  | Meeting_start
  | Meeting_stop
  | Meeting_toggle

type start_result =
  | Started of string
  | Busy of string
  | Disabled

type meeting_start_result = {
  result : start_result;
  snapshot : recording_snapshot;
}

type response =
  | Status_resp of recording_snapshot
  | Pause_resp
  | Resume_resp
  | Toggle_resp of bool
  | Meeting_start_resp of meeting_start_result
  | Meeting_stop_resp of recording_snapshot
  | Meeting_toggle_resp of recording_snapshot

let tag_of_request = function
  | Status -> "Status"
  | Pause -> "Pause"
  | Resume -> "Resume"
  | Toggle -> "Toggle"
  | Meeting_start -> "MeetingStart"
  | Meeting_stop -> "MeetingStop"
  | Meeting_toggle -> "MeetingToggle"

let request_of_tag = function
  | "Status" -> Ok Status
  | "Pause" -> Ok Pause
  | "Resume" -> Ok Resume
  | "Toggle" -> Ok Toggle
  | "MeetingStart" -> Ok Meeting_start
  | "MeetingStop" -> Ok Meeting_stop
  | "MeetingToggle" -> Ok Meeting_toggle
  | tag -> Error ("unknown method: " ^ tag)

let request_to_json = function
  | req -> `Assoc [("method", `String (tag_of_request req))]

let request_of_json = function
  | `Assoc fields ->
    (match List.assoc_opt "method" fields with
     | Some (`String method_) -> request_of_tag method_
     | _ -> Error "missing method")
  | _ -> Error "request must be an object"

let snapshot_of_json = function
  | `Assoc fields ->
    let find_bool key =
      match List.assoc_opt key fields with
      | Some (`Bool b) -> b
      | _ -> false
    in
    let find_string key =
      match List.assoc_opt key fields with
      | Some (`String s) -> Some s
      | _ -> None
    in
    {
      active = find_bool "active";
      mode = Option.value (find_string "mode") ~default:"idle";
      enabled = find_bool "enabled";
      started_at = find_string "startedAt";
      updated_at = find_string "updatedAt";
      transcript_path = find_string "transcriptPath";
      last_error =
        (match find_string "lastError" with
         | Some _ as v -> v
         | None -> find_string "error");
    }
  | _ ->
    {
      active = false;
      mode = "idle";
      enabled = false;
      started_at = None;
      updated_at = None;
      transcript_path = None;
      last_error = None;
    }

let json_of_snapshot snap =
  `Assoc [
    ("active", `Bool snap.active);
    ("mode", `String snap.mode);
    ("enabled", `Bool snap.enabled);
    ("startedAt", (match snap.started_at with Some s -> `String s | None -> `Null));
    ("updatedAt", (match snap.updated_at with Some s -> `String s | None -> `Null));
    ("transcriptPath", (match snap.transcript_path with Some s -> `String s | None -> `Null));
    ("lastError", (match snap.last_error with Some s -> `String s | None -> `Null));
  ]

let start_result_of_json = function
  | `Assoc fields ->
    (match List.assoc_opt "_tag" fields with
     | Some (`String "Started") ->
       let mode =
         match List.assoc_opt "mode" fields with
         | Some (`String s) -> s
         | _ -> "idle"
       in
       Started mode
     | Some (`String "Busy") ->
       let mode =
         match List.assoc_opt "activeMode" fields with
         | Some (`String s) -> s
         | _ -> "idle"
       in
       Busy mode
     | Some (`String "Disabled") -> Disabled
     | _ -> Disabled)
  | `String "Started" -> Started "meeting-transcribe"
  | `String "Busy" -> Busy "meeting-transcribe"
  | _ -> Disabled

let json_of_start_result = function
  | Started mode -> `Assoc [("_tag", `String "Started"); ("mode", `String mode)]
  | Busy mode -> `Assoc [("_tag", `String "Busy"); ("activeMode", `String mode)]
  | Disabled -> `Assoc [("_tag", `String "Disabled")]

let response_to_json = function
  | Status_resp snap -> json_of_snapshot snap
  | Pause_resp -> `Assoc []
  | Resume_resp -> `Assoc []
  | Toggle_resp b -> `Assoc [("value", `Bool b)]
  | Meeting_start_resp r ->
    `Assoc [
      ("result", json_of_start_result r.result);
      ("snapshot", json_of_snapshot r.snapshot);
    ]
  | Meeting_stop_resp snap -> json_of_snapshot snap
  | Meeting_toggle_resp snap -> json_of_snapshot snap

let response_of_json req json =
  match req with
  | Status -> Status_resp (snapshot_of_json json)
  | Pause -> Pause_resp
  | Resume -> Resume_resp
  | Toggle ->
    (match json with
     | `Assoc fields ->
       (match List.assoc_opt "value" fields with
        | Some (`Bool b) -> Toggle_resp b
        | _ -> Toggle_resp false)
     | _ -> Toggle_resp false)
  | Meeting_start ->
    (match json with
     | `Assoc fields ->
       let result =
         match List.assoc_opt "result" fields with
         | Some j -> start_result_of_json j
         | _ -> Disabled
       in
       let snapshot =
         match List.assoc_opt "snapshot" fields with
         | Some j -> snapshot_of_json j
         | None -> snapshot_of_json (`Assoc [])
       in
       Meeting_start_resp { result; snapshot }
     | _ -> Meeting_start_resp { result = Disabled; snapshot = snapshot_of_json (`Assoc []) })
  | Meeting_stop -> Meeting_stop_resp (snapshot_of_json json)
  | Meeting_toggle -> Meeting_toggle_resp (snapshot_of_json json)
