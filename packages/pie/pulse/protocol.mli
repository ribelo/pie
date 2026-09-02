type packet_header = {
  length : int;
  channel : int;
  offset_hi : int;
  offset_lo : int;
  flags : int;
}

val encode_packet_header : packet_header -> bytes
val decode_packet_header : bytes -> packet_header
val frame_packet : ?channel:int -> ?flags:int -> bytes -> bytes

type framed_packet = { header : packet_header; payload : bytes }

val split_framed_packet : bytes -> framed_packet

module Tag_struct_writer : sig
  type t

  val create : ?initial_capacity:int -> unit -> t
  val add_u8 : t -> int -> unit
  val add_u32 : t -> int -> unit
  val add_u64 : t -> int64 -> unit
  val add_usec : t -> int64 -> unit
  val add_string : t -> string option -> unit
  val add_bool : t -> bool -> unit
  val add_arbitrary : t -> bytes -> unit
  val add_sample_spec : t -> Defs.sample_spec -> unit
  val add_channel_map : t -> Defs.channel_map -> unit
  val add_props : t -> (string * string) list -> unit
  val add_cvolume : t -> int list -> unit
  val add_volume : t -> int -> unit
  val add_format_info : t -> int -> (string * string) list -> unit
  val finalize : t -> bytes
end

module Tag_struct_reader : sig
  type t

  val create : bytes -> t
  val get_u8 : t -> int
  val get_u32 : t -> int
  val get_u64 : t -> int64
  val get_s64 : t -> int64
  val get_usec : t -> int64
  val get_string : t -> string option
  val get_arbitrary : t -> bytes
  val get_bool : t -> bool
  val get_sample_spec : t -> Defs.sample_spec
  val get_channel_map : t -> Defs.channel_map
  val get_props : t -> (string * string) list
  val get_cvolume : t -> int list
  val get_volume : t -> int
  val get_format_info : t -> int * (string * string) list
  val has_remaining : t -> bool
  val remaining_bytes : t -> bytes
end
