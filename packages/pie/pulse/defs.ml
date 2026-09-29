let pa_max_channels = 32
let pa_default_source = "@DEFAULT_SOURCE@"
let pa_no_value = 0xffffffff
let pa_no_index = pa_no_value
let pa_no_tag = pa_no_value
let pa_native_cookie_length = 256
let pa_native_protocol_version = 32
let pa_volume_norm = 0x10000

let pa_default_socket_path ~caps =
  match Ox_caps.Cap_sys.getenv_opt ~caps "XDG_RUNTIME_DIR" with
  | Some dir -> Filename.concat dir "pulse/native"
  | None -> "/run/user/1000/pulse/native"

let pa_default_cookie_path ~caps =
  match Ox_caps.Cap_sys.getenv_opt ~caps "HOME" with
  | Some home -> Filename.concat home ".config/pulse/cookie"
  | None -> "/tmp/.config/pulse/cookie"

type pa_stream_descriptor = {
  length : int;
  channel : int;
  offset_hi : int;
  offset_lo : int;
  flags : int;
}

let pa_stream_descriptor_size = 20

module Pa_tag = struct
  let invalid = 0
  let string = 0x74
  let string_null = 0x4e
  let u32 = 0x4c
  let u8 = 0x42
  let u64 = 0x52
  let s64 = 0x72
  let sample_spec = 0x61
  let arbitrary = 0x78
  let boolean_true = 0x31
  let boolean_false = 0x30
  let timeval = 0x54
  let usec = 0x55
  let channel_map = 0x6d
  let cvolume = 0x76
  let proplist = 0x50
  let volume = 0x56
  let format_info = 0x66
end

module Pa_command = struct
  let error = 0
  let timeout = 1
  let reply = 2
  let create_playback_stream = 3
  let delete_playback_stream = 4
  let create_record_stream = 5
  let delete_record_stream = 6
  let exit = 7
  let auth = 8
  let set_client_name = 9
  let lookup_sink = 10
  let lookup_source = 11
  let drain_playback_stream = 12
  let stat = 13
  let get_playback_latency = 14
  let create_upload_stream = 15
  let delete_upload_stream = 16
  let finish_upload_stream = 17
  let play_sample = 18
  let remove_sample = 19
  let get_server_info = 20
  let get_sink_info = 21
  let get_sink_info_list = 22
  let get_source_info = 23
  let get_source_info_list = 24
end

module Pa_sample_format = struct
  let u8 = 0
  let alaw = 1
  let ulaw = 2
  let s16le = 3
  let s16be = 4
  let float32le = 5
  let float32be = 6
  let s32le = 7
  let s32be = 8
  let s24le = 9
  let s24be = 10
  let s24_32le = 11
  let s24_32be = 12
end

type sample_spec = { format : int; channels : int; rate : int }
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

let default_record_stream_options ?(overrides : record_stream_options option) ()
    : record_stream_options =
  let sample_spec =
    match overrides with
    | Some o -> o.sample_spec
    | None -> { format = Pa_sample_format.s16le; channels = 1; rate = 16000 }
  in
  let channel_map =
    let default_map = List.init sample_spec.channels (fun i -> i + 1) in
    match overrides with
    | Some o when List.length o.channel_map = sample_spec.channels ->
        o.channel_map
    | _ ->
        (* PulseAudio requires exactly one channel-map entry per channel. Ignore
         an override map whose length does not match and derive a default. *)
        default_map
  in
  let default_volume =
    List.init sample_spec.channels (fun _ -> pa_volume_norm)
  in
  {
    sample_spec;
    channel_map;
    source_index =
      (match overrides with Some o -> o.source_index | None -> pa_no_index);
    source_name =
      (match overrides with Some o -> o.source_name | None -> None);
    stream_name =
      (match overrides with Some o -> o.stream_name | None -> None);
    maximum_length =
      (match overrides with Some o -> o.maximum_length | None -> pa_no_value);
    fragment_size =
      (match overrides with Some o -> o.fragment_size | None -> pa_no_value);
    corked = (match overrides with Some o -> o.corked | None -> false);
    no_remap = (match overrides with Some o -> o.no_remap | None -> false);
    no_remix = (match overrides with Some o -> o.no_remix | None -> false);
    fix_format = (match overrides with Some o -> o.fix_format | None -> false);
    fix_rate = (match overrides with Some o -> o.fix_rate | None -> false);
    fix_channels =
      (match overrides with Some o -> o.fix_channels | None -> false);
    no_move = (match overrides with Some o -> o.no_move | None -> false);
    variable_rate =
      (match overrides with Some o -> o.variable_rate | None -> false);
    peak_detect =
      (match overrides with Some o -> o.peak_detect | None -> false);
    adjust_latency =
      (match overrides with Some o -> o.adjust_latency | None -> false);
    direct_on_input_index =
      (match overrides with
      | Some o -> o.direct_on_input_index
      | None -> pa_no_value);
    early_requests =
      (match overrides with Some o -> o.early_requests | None -> false);
    dont_inhibit_auto_suspend =
      (match overrides with
      | Some o -> o.dont_inhibit_auto_suspend
      | None -> false);
    fail_on_suspend =
      (match overrides with Some o -> o.fail_on_suspend | None -> false);
    volume =
      (match overrides with Some o -> o.volume | None -> default_volume);
    muted = (match overrides with Some o -> o.muted | None -> false);
    volume_set = (match overrides with Some o -> o.volume_set | None -> false);
    muted_set = (match overrides with Some o -> o.muted_set | None -> false);
    relative_volume =
      (match overrides with Some o -> o.relative_volume | None -> false);
    passthrough =
      (match overrides with Some o -> o.passthrough | None -> false);
    properties = (match overrides with Some o -> o.properties | None -> []);
  }

type pcm_record_options = {
  sample_spec : sample_spec;
  fragment_size : int;
  source_name : string option;
}

let make_pcm_record_options ?(channels = 1) ~(rate : int) ~(fragment_size : int)
    ~(source_name : string option) () : pcm_record_options =
  {
    sample_spec = { format = Pa_sample_format.s16le; channels; rate };
    fragment_size;
    source_name;
  }
