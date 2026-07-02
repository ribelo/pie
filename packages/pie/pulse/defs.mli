val pa_max_channels : int
val pa_default_source : string
val pa_no_value : int
val pa_no_index : int
val pa_no_tag : int
val pa_native_cookie_length : int
val pa_native_protocol_version : int
val pa_volume_norm : int
val pa_default_socket_path : string
val pa_default_cookie_path : string

type pa_stream_descriptor = {
  length : int;
  channel : int;
  offset_hi : int;
  offset_lo : int;
  flags : int;
}

val pa_stream_descriptor_size : int

module Pa_tag : sig
  val invalid : int
  val string : int
  val string_null : int
  val u32 : int
  val u8 : int
  val u64 : int
  val s64 : int
  val sample_spec : int
  val arbitrary : int
  val boolean_true : int
  val boolean_false : int
  val timeval : int
  val usec : int
  val channel_map : int
  val cvolume : int
  val proplist : int
  val volume : int
  val format_info : int
end

module Pa_command : sig
  val error : int
  val timeout : int
  val reply : int
  val create_playback_stream : int
  val delete_playback_stream : int
  val create_record_stream : int
  val delete_record_stream : int
  val exit : int
  val auth : int
  val set_client_name : int
  val lookup_sink : int
  val lookup_source : int
  val drain_playback_stream : int
  val stat : int
  val get_playback_latency : int
  val create_upload_stream : int
  val delete_upload_stream : int
  val finish_upload_stream : int
  val play_sample : int
  val remove_sample : int
  val get_server_info : int
  val get_sink_info : int
  val get_sink_info_list : int
  val get_source_info : int
  val get_source_info_list : int
end

module Pa_sample_format : sig
  val u8 : int
  val alaw : int
  val ulaw : int
  val s16le : int
  val s16be : int
  val float32le : int
  val float32be : int
  val s32le : int
  val s32be : int
  val s24le : int
  val s24be : int
  val s24_32le : int
  val s24_32be : int
end

type sample_spec = {
  format : int;
  channels : int;
  rate : int;
}

type channel_map = int list

type source_info = {
  index : int;
  name : string option;
  description : string option;
  sample_spec : sample_spec;
  channel_map : channel_map;
  monitor_index : int;
  monitor_name : string option;
  latency_usec : int64;
  driver : string option;
  flags : int;
}

type server_info = {
  name : string option;
  version : string option;
  username : string option;
  hostname : string option;
  sample_spec : sample_spec;
  default_sink : string option;
  default_source : string option;
  cookie : int32;
  default_channel_map : channel_map;
}

type record_stream_options = {
  sample_spec : sample_spec;
  channel_map : channel_map;
  source_index : int;
  source_name : string option;
  stream_name : string option;
  maximum_length : int;
  fragment_size : int;
  corked : bool;
  no_remap : bool;
  no_remix : bool;
  fix_format : bool;
  fix_rate : bool;
  fix_channels : bool;
  no_move : bool;
  variable_rate : bool;
  peak_detect : bool;
  adjust_latency : bool;
  direct_on_input_index : int;
  early_requests : bool;
  dont_inhibit_auto_suspend : bool;
  fail_on_suspend : bool;
  volume : int list;
  muted : bool;
  volume_set : bool;
  muted_set : bool;
  relative_volume : bool;
  passthrough : bool;
  properties : (string * string) list;
}

type record_stream_info = {
  stream_index : int;
  source_output_index : int;
  maximum_length : int;
  fragment_size : int;
  sample_spec : sample_spec;
  channel_map : channel_map;
  source_index : int;
  source_name : string option;
  source_suspended : bool;
  configured_source_latency_usec : int64;
}

val default_record_stream_options : ?overrides:record_stream_options -> unit -> record_stream_options

type pcm_record_options = {
  sample_spec : sample_spec;
  fragment_size : int;
  source_name : string option;
}

val make_pcm_record_options : ?channels:int -> rate:int -> fragment_size:int -> source_name:string option -> unit -> pcm_record_options