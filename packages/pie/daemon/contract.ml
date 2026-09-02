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
  | Action of string
  | Meeting_start
  | Meeting_stop
  | Meeting_toggle

type action_result = {
  action : string;
  accepted : bool;
  message : string option;
  snapshot : recording_snapshot;
}

type start_result =
  | Started of string
  | Busy of string
  | Disabled
  | Error of string

type meeting_start_result = {
  result : start_result;
  snapshot : recording_snapshot;
}

type meeting_stop_result = { stopped : bool; snapshot : recording_snapshot }

type response =
  | Status_resp of recording_snapshot
  | Pause_resp
  | Resume_resp
  | Toggle_resp of bool
  | Action_resp of action_result
  | Meeting_start_resp of meeting_start_result
  | Meeting_stop_resp of meeting_stop_result
  | Meeting_toggle_resp of recording_snapshot
  | Error_resp of string

let tag_of_request = function
  | Status -> "Status"
  | Pause -> "Pause"
  | Resume -> "Resume"
  | Toggle -> "Toggle"
  | Action _ -> "Action"
  | Meeting_start -> "MeetingStart"
  | Meeting_stop -> "MeetingStop"
  | Meeting_toggle -> "MeetingToggle"

let request_of_tag = function
  | "Status" -> Ok Status
  | "Pause" -> Ok Pause
  | "Resume" -> Ok Resume
  | "Toggle" -> Ok Toggle
  | "Action" -> Ok (Action "")
  | "MeetingStart" -> Ok Meeting_start
  | "MeetingStop" -> Ok Meeting_stop
  | "MeetingToggle" -> Ok Meeting_toggle
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

let snapshot_of_json = function
  | `Assoc fields ->
      let find_bool key =
        match List.assoc_opt key fields with Some (`Bool b) -> b | _ -> false
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
  `Assoc
    [
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

let start_result_of_json = function
  | `Assoc fields -> (
      match List.assoc_opt "_tag" fields with
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
      | Some (`String "Error") -> (
          match List.assoc_opt "message" fields with
          | Some (`String s) -> Error s
          | _ -> Error "unknown error")
      | _ -> Disabled)
  | `String "Started" -> Started "meeting-transcribe"
  | `String "Busy" -> Busy "meeting-transcribe"
  | _ -> Disabled

let json_of_start_result = function
  | Started mode ->
      `Assoc [ ("_tag", `String "Started"); ("mode", `String mode) ]
  | Busy mode ->
      `Assoc [ ("_tag", `String "Busy"); ("activeMode", `String mode) ]
  | Disabled -> `Assoc [ ("_tag", `String "Disabled") ]
  | Error message ->
      `Assoc [ ("_tag", `String "Error"); ("message", `String message) ]

let response_to_json = function
  | Status_resp snap -> json_of_snapshot snap
  | Pause_resp -> `Assoc []
  | Resume_resp -> `Assoc []
  | Toggle_resp b -> `Assoc [ ("value", `Bool b) ]
  | Action_resp result -> json_of_action_result result
  | Meeting_start_resp r ->
      `Assoc
        [
          ("result", json_of_start_result r.result);
          ("snapshot", json_of_snapshot r.snapshot);
        ]
  | Meeting_stop_resp r -> (
      match json_of_snapshot r.snapshot with
      | `Assoc fields -> `Assoc (fields @ [ ("stopped", `Bool r.stopped) ])
      | other -> other)
  | Meeting_toggle_resp snap -> (
      (* Mirror Meeting_stop's "stopped" indicator: expose whether meeting
       transcription is active after the toggle so callers can tell the
       resulting state programmatically. *)
      match json_of_snapshot snap with
      | `Assoc fields ->
          `Assoc
            (fields
            @ [
                ( "toggled",
                  `Bool (snap.active && snap.mode = "meeting-transcribe") );
              ])
      | other -> other)
  | Error_resp message -> `Assoc [ ("error", `String message) ]

let error_message_of_json = function
  | `Assoc fields -> (
      match List.assoc_opt "error" fields with
      | Some (`String s) -> Some s
      | Some other -> Some (Yojson.Safe.to_string other)
      | None -> None)
  | _ -> None

let response_of_json req json =
  match (error_message_of_json json, req) with
  | Some message, Meeting_start ->
      (* Meeting_start carries a snapshot alongside its result, so its error is
       represented inside the start_result rather than as a bare Error_resp. *)
      Meeting_start_resp
        { result = Error message; snapshot = snapshot_of_json json }
  | Some message, _ -> Error_resp message
  | None, _ -> (
      match req with
      | Status -> Status_resp (snapshot_of_json json)
      | Pause -> Pause_resp
      | Resume -> Resume_resp
      | Toggle -> (
          match json with
          | `Assoc fields -> (
              match List.assoc_opt "value" fields with
              | Some (`Bool b) -> Toggle_resp b
              | _ -> Toggle_resp false)
          | _ -> Toggle_resp false)
      | Action action ->
          let action_result =
            match json with
            | `Assoc fields ->
                let find_string key =
                  match List.assoc_opt key fields with
                  | Some (`String s) -> Some s
                  | _ -> None
                in
                let accepted =
                  match List.assoc_opt "accepted" fields with
                  | Some (`Bool b) -> b
                  | _ -> false
                in
                let snapshot =
                  match List.assoc_opt "snapshot" fields with
                  | Some json -> snapshot_of_json json
                  | None -> snapshot_of_json (`Assoc [])
                in
                {
                  action = Option.value (find_string "action") ~default:action;
                  accepted;
                  message = find_string "message";
                  snapshot;
                }
            | _ ->
                {
                  action;
                  accepted = false;
                  message = Some "invalid action response";
                  snapshot = snapshot_of_json (`Assoc []);
                }
          in
          Action_resp action_result
      | Meeting_start -> (
          match json with
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
          | _ ->
              Meeting_start_resp
                { result = Disabled; snapshot = snapshot_of_json (`Assoc []) })
      | Meeting_stop ->
          let stopped =
            match json with
            | `Assoc fields -> (
                match List.assoc_opt "stopped" fields with
                | Some (`Bool b) -> b
                | _ -> false)
            | _ -> false
          in
          Meeting_stop_resp { stopped; snapshot = snapshot_of_json json }
      | Meeting_toggle -> Meeting_toggle_resp (snapshot_of_json json))
