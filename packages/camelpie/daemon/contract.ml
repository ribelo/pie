type recording_snapshot = {
  active : bool;
  mode : string option;
  enabled : bool;
  error : string option;
}

type request =
  | Status
  | Pause
  | Resume
  | Toggle
  | Meeting_start
  | Meeting_stop
  | Meeting_toggle

type meeting_start_result = {
  result : string;
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

let request_to_json = function
  | Status -> `Assoc [("method", `String "Status")]
  | Pause -> `Assoc [("method", `String "Pause")]
  | Resume -> `Assoc [("method", `String "Resume")]
  | Toggle -> `Assoc [("method", `String "Toggle")]
  | Meeting_start -> `Assoc [("method", `String "MeetingStart")]
  | Meeting_stop -> `Assoc [("method", `String "MeetingStop")]
  | Meeting_toggle -> `Assoc [("method", `String "MeetingToggle")]

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
      mode = find_string "mode";
      enabled = find_bool "enabled";
      error = find_string "error";
    }
  | _ -> { active = false; mode = None; enabled = false; error = None }

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
         | Some (`String s) -> s
         | _ -> ""
       in
       let snapshot =
         match List.assoc_opt "snapshot" fields with
         | Some j -> snapshot_of_json j
         | None -> { active = false; mode = None; enabled = false; error = None }
       in
       Meeting_start_resp { result; snapshot }
     | _ -> Meeting_start_resp { result = ""; snapshot = { active = false; mode = None; enabled = false; error = None } })
  | Meeting_stop -> Meeting_stop_resp (snapshot_of_json json)
  | Meeting_toggle -> Meeting_toggle_resp (snapshot_of_json json)
