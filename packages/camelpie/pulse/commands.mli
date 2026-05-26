type command_packet = {
  tag : int;
  bytes : bytes;
}

type parsed_command_envelope = {
  packet_type : int;
  tag : int;
  body : bytes;
}

val parse_command_envelope : bytes -> parsed_command_envelope
val parse_error_code : bytes -> int option

val build_auth_command :
  ?cookie:bytes -> ?protocol_version:int -> unit -> (command_packet, string) result

val parse_auth_response : bytes -> int

val build_set_client_name_command :
  ?client_name_or_props:[ `String of string | `Props of (string * string) list ] -> unit -> command_packet

val parse_set_client_name_response : bytes -> int

val build_get_server_info_command : unit -> command_packet
val parse_server_info_response : bytes -> Defs.server_info

val build_get_source_list_command : unit -> command_packet
val parse_source_list_response : bytes -> Defs.source_info list

val build_create_record_stream_command :
  ?options:Defs.record_stream_options -> unit -> command_packet

val parse_create_record_stream_response : bytes -> Defs.record_stream_info

val build_delete_record_stream_command : int -> command_packet

val parse_protocol_compatibility : int -> (unit, string) result