type recording_snapshot = {
  phase : Recording.Phase.t;
  active : bool;
  processing : int;
  mode : string;
  enabled : bool;
  started_at : string option;
  updated_at : string option;
  transcript_path : string option;
  last_error : string option;
  build_commit : string option;
  build_time : string option;
  reasoning_effort : string option;
  response_schema : string option;
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

val not_object : protocol_error
val duplicate_field : protocol_error
val missing_field : protocol_error
val wrong_type : protocol_error
val unexpected_field : protocol_error
val unknown_tag : protocol_error
val malformed_error : protocol_error
val action_mismatch : protocol_error
val protocol_error_label : protocol_error -> string
val tag_of_request : request -> string
val request_of_tag : string -> (request, string) result
val request_to_json : request -> Yojson.Safe.t
val request_of_json : Yojson.Safe.t -> (request, string) result

val snapshot_of_json :
  Yojson.Safe.t -> (recording_snapshot, protocol_error) result

val json_of_snapshot : recording_snapshot -> Yojson.Safe.t
val json_of_action_result : action_result -> Yojson.Safe.t
val response_to_json : response -> Yojson.Safe.t

val response_of_json :
  request -> Yojson.Safe.t -> (response, protocol_error) result

val decode_line : string -> (request, string) result
val encode_response : response -> string

val snapshot_of_status_response :
  response -> (recording_snapshot, protocol_error) result
