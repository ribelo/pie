type command_packet = {
  tag : int;
  bytes : bytes;
}

type parsed_command_envelope = {
  packet_type : int;
  tag : int;
  body : bytes;
}

let request_tag = ref 0xfffffffe

let next_request_tag () =
  request_tag := (!request_tag + 1) mod 0xfffffffe;
  !request_tag

let command_packet command build =
  let tag = next_request_tag () in
  let writer = Protocol.Tag_struct_writer.create () in
  Protocol.Tag_struct_writer.add_u32 writer command;
  Protocol.Tag_struct_writer.add_u32 writer tag;
  build writer;
  {
    tag;
    bytes = Protocol.frame_packet (Protocol.Tag_struct_writer.finalize writer) ~channel:Defs.pa_no_index;
  }

let parse_command_envelope (payload : bytes) : parsed_command_envelope =
  let reader = Protocol.Tag_struct_reader.create payload in
  let packet_type = Protocol.Tag_struct_reader.get_u32 reader in
  let tag = Protocol.Tag_struct_reader.get_u32 reader in
  {
    packet_type;
    tag;
    body = Protocol.Tag_struct_reader.remaining_bytes reader;
  }

let parse_error_code (payload : bytes) : int option =
  let reader = Protocol.Tag_struct_reader.create payload in
  if not (Protocol.Tag_struct_reader.has_remaining reader) then None
  else Some (Protocol.Tag_struct_reader.get_u32 reader)

let build_auth_command ?(cookie : bytes option) ?(protocol_version = Defs.pa_native_protocol_version) () : (command_packet, string) result =
  let auth_cookie =
    match cookie with
    | Some c -> c
    | None -> Bytes.make Defs.pa_native_cookie_length '\x00'
  in
  if Bytes.length auth_cookie <> Defs.pa_native_cookie_length then
    Error (Printf.sprintf "invalid pulse cookie length: %d" (Bytes.length auth_cookie))
  else
    Ok (command_packet Defs.Pa_command.auth (fun writer ->
      Protocol.Tag_struct_writer.add_u32 writer protocol_version;
      Protocol.Tag_struct_writer.add_arbitrary writer auth_cookie
    ))

let parse_auth_response (payload : bytes) : int =
  let reader = Protocol.Tag_struct_reader.create payload in
  Protocol.Tag_struct_reader.get_u32 reader land 0xffff

let build_set_client_name_command ?(client_name_or_props = (`String "pie" : [ `String of string | `Props of (string * string) list ])) () : command_packet =
  let props =
    match client_name_or_props with
    | `String name -> [("application.name", name)]
    | `Props p -> p
  in
  command_packet Defs.Pa_command.set_client_name (fun writer ->
    Protocol.Tag_struct_writer.add_props writer props
  )

let parse_set_client_name_response (payload : bytes) : int =
  let reader = Protocol.Tag_struct_reader.create payload in
  Protocol.Tag_struct_reader.get_u32 reader

let build_get_server_info_command () : command_packet =
  command_packet Defs.Pa_command.get_server_info (fun _writer -> ())

let parse_server_info_response (payload : bytes) : Defs.server_info =
  let reader = Protocol.Tag_struct_reader.create payload in
  {
    name = Protocol.Tag_struct_reader.get_string reader;
    version = Protocol.Tag_struct_reader.get_string reader;
    username = Protocol.Tag_struct_reader.get_string reader;
    hostname = Protocol.Tag_struct_reader.get_string reader;
    sample_spec = Protocol.Tag_struct_reader.get_sample_spec reader;
    default_sink = Protocol.Tag_struct_reader.get_string reader;
    default_source = Protocol.Tag_struct_reader.get_string reader;
    cookie = Int32.of_int (Protocol.Tag_struct_reader.get_u32 reader);
    default_channel_map = Protocol.Tag_struct_reader.get_channel_map reader;
  }

let build_get_source_list_command () : command_packet =
  command_packet Defs.Pa_command.get_source_info_list (fun _writer -> ())

let parse_source_info (reader : Protocol.Tag_struct_reader.t) : Defs.source_info =
  let index = Protocol.Tag_struct_reader.get_u32 reader in
  let name = Protocol.Tag_struct_reader.get_string reader in
  let description = Protocol.Tag_struct_reader.get_string reader in
  let sample_spec = Protocol.Tag_struct_reader.get_sample_spec reader in
  let channel_map = Protocol.Tag_struct_reader.get_channel_map reader in
  ignore (Protocol.Tag_struct_reader.get_u32 reader);
  ignore (Protocol.Tag_struct_reader.get_cvolume reader);
  ignore (Protocol.Tag_struct_reader.get_bool reader);
  let monitor_index = Protocol.Tag_struct_reader.get_u32 reader in
  let monitor_name = Protocol.Tag_struct_reader.get_string reader in
  let latency_usec = Protocol.Tag_struct_reader.get_usec reader in
  let driver = Protocol.Tag_struct_reader.get_string reader in
  let flags = Protocol.Tag_struct_reader.get_u32 reader in
  ignore (Protocol.Tag_struct_reader.get_props reader);
  ignore (Protocol.Tag_struct_reader.get_usec reader);
  ignore (Protocol.Tag_struct_reader.get_volume reader);
  ignore (Protocol.Tag_struct_reader.get_u32 reader);
  ignore (Protocol.Tag_struct_reader.get_u32 reader);
  ignore (Protocol.Tag_struct_reader.get_u32 reader);
  let ports = Protocol.Tag_struct_reader.get_u32 reader in
  for _i = 1 to ports do
    ignore (Protocol.Tag_struct_reader.get_string reader);
    ignore (Protocol.Tag_struct_reader.get_string reader);
    ignore (Protocol.Tag_struct_reader.get_u32 reader);
    ignore (Protocol.Tag_struct_reader.get_u32 reader);
  done;
  ignore (Protocol.Tag_struct_reader.get_string reader);
  let formats = Protocol.Tag_struct_reader.get_u8 reader in
  for _i = 1 to formats do
    ignore (Protocol.Tag_struct_reader.get_format_info reader);
  done;
  {
    index;
    name;
    description;
    sample_spec;
    channel_map;
    monitor_index;
    monitor_name;
    latency_usec;
    driver;
    flags;
  }

let parse_source_list_response (payload : bytes) : Defs.source_info list =
  let reader = Protocol.Tag_struct_reader.create payload in
  let rec read_sources acc =
    if Protocol.Tag_struct_reader.has_remaining reader then
      read_sources (parse_source_info reader :: acc)
    else
      List.rev acc
  in
  read_sources []

let build_create_record_stream_command ?(options : Defs.record_stream_options option) () : command_packet =
  let config =
    match options with
    | Some o -> o
    | None -> Defs.default_record_stream_options ()
  in
  command_packet Defs.Pa_command.create_record_stream (fun writer ->
    Protocol.Tag_struct_writer.add_sample_spec writer config.sample_spec;
    Protocol.Tag_struct_writer.add_channel_map writer config.channel_map;
    Protocol.Tag_struct_writer.add_u32 writer config.source_index;
    Protocol.Tag_struct_writer.add_string writer config.source_name;
    Protocol.Tag_struct_writer.add_u32 writer config.maximum_length;
    Protocol.Tag_struct_writer.add_bool writer config.corked;
    Protocol.Tag_struct_writer.add_u32 writer config.fragment_size;
    Protocol.Tag_struct_writer.add_bool writer config.no_remap;
    Protocol.Tag_struct_writer.add_bool writer config.no_remix;
    Protocol.Tag_struct_writer.add_bool writer config.fix_format;
    Protocol.Tag_struct_writer.add_bool writer config.fix_rate;
    Protocol.Tag_struct_writer.add_bool writer config.fix_channels;
    Protocol.Tag_struct_writer.add_bool writer config.no_move;
    Protocol.Tag_struct_writer.add_bool writer config.variable_rate;
    Protocol.Tag_struct_writer.add_bool writer config.peak_detect;
    Protocol.Tag_struct_writer.add_bool writer config.adjust_latency;
    Protocol.Tag_struct_writer.add_props writer config.properties;
    Protocol.Tag_struct_writer.add_u32 writer config.direct_on_input_index;
    Protocol.Tag_struct_writer.add_bool writer config.early_requests;
    Protocol.Tag_struct_writer.add_bool writer config.dont_inhibit_auto_suspend;
    Protocol.Tag_struct_writer.add_bool writer config.fail_on_suspend;
    Protocol.Tag_struct_writer.add_u8 writer 0;
    Protocol.Tag_struct_writer.add_cvolume writer config.volume;
    Protocol.Tag_struct_writer.add_bool writer config.muted;
    Protocol.Tag_struct_writer.add_bool writer config.volume_set;
    Protocol.Tag_struct_writer.add_bool writer config.muted_set;
    Protocol.Tag_struct_writer.add_bool writer config.relative_volume;
    Protocol.Tag_struct_writer.add_bool writer config.passthrough;
  )

let parse_create_record_stream_response (payload : bytes) : Defs.record_stream_info =
  let reader = Protocol.Tag_struct_reader.create payload in
  let stream_index = Protocol.Tag_struct_reader.get_u32 reader in
  let source_output_index = Protocol.Tag_struct_reader.get_u32 reader in
  let maximum_length = Protocol.Tag_struct_reader.get_u32 reader in
  let fragment_size = Protocol.Tag_struct_reader.get_u32 reader in
  let sample_spec = Protocol.Tag_struct_reader.get_sample_spec reader in
  let channel_map = Protocol.Tag_struct_reader.get_channel_map reader in
  let source_index = Protocol.Tag_struct_reader.get_u32 reader in
  let source_name = Protocol.Tag_struct_reader.get_string reader in
  let source_suspended = Protocol.Tag_struct_reader.get_bool reader in
  let configured_source_latency_usec = Protocol.Tag_struct_reader.get_usec reader in
  ignore (Protocol.Tag_struct_reader.get_format_info reader);
  {
    stream_index;
    source_output_index;
    maximum_length;
    fragment_size;
    sample_spec;
    channel_map;
    source_index;
    source_name;
    source_suspended;
    configured_source_latency_usec;
  }

let build_delete_record_stream_command (stream_index : int) : command_packet =
  command_packet Defs.Pa_command.delete_record_stream (fun writer ->
    Protocol.Tag_struct_writer.add_u32 writer stream_index
  )

let parse_protocol_compatibility (server_protocol_version : int) : (unit, string) result =
  let client_protocol_version = Defs.pa_native_protocol_version in
  if server_protocol_version < client_protocol_version then
    Error (Printf.sprintf "PulseAudio server protocol %d is older than required %d"
      server_protocol_version client_protocol_version)
  else
    Ok ()