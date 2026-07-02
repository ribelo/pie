type packet_header = {
  length : int;
  channel : int;
  offset_hi : int;
  offset_lo : int;
  flags : int;
}

let int_of_uint32_be buf offset =
  let value = Int32.to_int (Bytes.get_int32_be buf offset) in
  if value < 0 then value + 0x100000000 else value

let encode_packet_header { length; channel; offset_hi; offset_lo; flags } =
  let buf = Bytes.create 20 in
  Bytes.set_int32_be buf 0 (Int32.of_int length);
  Bytes.set_int32_be buf 4 (Int32.of_int channel);
  Bytes.set_int32_be buf 8 (Int32.of_int offset_hi);
  Bytes.set_int32_be buf 12 (Int32.of_int offset_lo);
  Bytes.set_int32_be buf 16 (Int32.of_int flags);
  buf

let decode_packet_header (buf : bytes) : packet_header =
  if Bytes.length buf <> 20 then
    failwith "invalid packet header size";
  {
    length = int_of_uint32_be buf 0;
    channel = int_of_uint32_be buf 4;
    offset_hi = int_of_uint32_be buf 8;
    offset_lo = int_of_uint32_be buf 12;
    flags = int_of_uint32_be buf 16;
  }

let frame_packet ?(channel = Defs.pa_no_index) ?(flags = 0) (payload : bytes) : bytes =
  let header = encode_packet_header { length = Bytes.length payload; channel; offset_hi = 0; offset_lo = 0; flags } in
  let result = Bytes.create (20 + Bytes.length payload) in
  Bytes.blit header 0 result 0 20;
  Bytes.blit payload 0 result 20 (Bytes.length payload);
  result

type framed_packet = {
  header : packet_header;
  payload : bytes;
}

let split_framed_packet (packet : bytes) : framed_packet =
  if Bytes.length packet < 20 then
    failwith "packet shorter than descriptor";
  let header = decode_packet_header (Bytes.sub packet 0 20) in
  let payload = Bytes.sub packet 20 (Bytes.length packet - 20) in
  if Bytes.length payload <> header.length then
    failwith (Printf.sprintf "packet payload mismatch: expected %d, got %d" header.length (Bytes.length payload));
  { header; payload }

module Tag_struct_writer = struct
  type t = {
    mutable buf : bytes;
    mutable offset : int;
  }

  let create ?(initial_capacity = 64) () =
    { buf = Bytes.create initial_capacity; offset = 0 }

  let ensure t extra_bytes =
    let required = t.offset + extra_bytes in
    if required > Bytes.length t.buf then begin
      let rec next_size n =
        if n >= required then n else next_size (n * 2)
      in
      let new_buf = Bytes.create (next_size (Bytes.length t.buf)) in
      Bytes.blit t.buf 0 new_buf 0 t.offset;
      t.buf <- new_buf
    end

  let write_tag t tag =
    ensure t 1;
    Bytes.set_uint8 t.buf t.offset tag;
    t.offset <- t.offset + 1

  let add_u8 t value =
    write_tag t Defs.Pa_tag.u8;
    ensure t 1;
    Bytes.set_uint8 t.buf t.offset (value land 0xff);
    t.offset <- t.offset + 1

  let add_u32 t value =
    write_tag t Defs.Pa_tag.u32;
    ensure t 4;
    Bytes.set_int32_be t.buf t.offset (Int32.of_int (value land 0xffffffff));
    t.offset <- t.offset + 4

  let add_u64 t value =
    write_tag t Defs.Pa_tag.u64;
    ensure t 8;
    Bytes.set_int64_be t.buf t.offset value;
    t.offset <- t.offset + 8

  let add_usec t value =
    write_tag t Defs.Pa_tag.usec;
    ensure t 8;
    Bytes.set_int64_be t.buf t.offset value;
    t.offset <- t.offset + 8

  let add_string t value =
    match value with
    | None ->
      write_tag t Defs.Pa_tag.string_null
    | Some s ->
      write_tag t Defs.Pa_tag.string;
      ensure t (String.length s + 1);
      String.iteri (fun i c -> Bytes.set t.buf (t.offset + i) c) s;
      t.offset <- t.offset + String.length s;
      Bytes.set_uint8 t.buf t.offset 0;
      t.offset <- t.offset + 1

  let add_bool t value =
    write_tag t (if value then Defs.Pa_tag.boolean_true else Defs.Pa_tag.boolean_false)

  let add_arbitrary t (value : bytes) =
    write_tag t Defs.Pa_tag.arbitrary;
    ensure t (4 + Bytes.length value);
    Bytes.set_int32_be t.buf t.offset (Int32.of_int (Bytes.length value));
    t.offset <- t.offset + 4;
    if Bytes.length value > 0 then begin
      Bytes.blit value 0 t.buf t.offset (Bytes.length value);
      t.offset <- t.offset + Bytes.length value
    end

  let add_sample_spec t (spec : Defs.sample_spec) =
    write_tag t Defs.Pa_tag.sample_spec;
    if spec.channels < 0 || spec.channels >= Defs.pa_max_channels then
      failwith (Printf.sprintf "invalid channel count: %d" spec.channels);
    ensure t 6;
    Bytes.set_uint8 t.buf t.offset spec.format;
    t.offset <- t.offset + 1;
    Bytes.set_uint8 t.buf t.offset spec.channels;
    t.offset <- t.offset + 1;
    Bytes.set_int32_be t.buf t.offset (Int32.of_int spec.rate);
    t.offset <- t.offset + 4

  let add_channel_map t (map : Defs.channel_map) =
    if List.length map > 255 then
      failwith (Printf.sprintf "channel map too long: %d" (List.length map));
    write_tag t Defs.Pa_tag.channel_map;
    ensure t (1 + List.length map);
    Bytes.set_uint8 t.buf t.offset (List.length map);
    t.offset <- t.offset + 1;
    List.iter (fun channel ->
      Bytes.set_uint8 t.buf t.offset (channel land 0xff);
      t.offset <- t.offset + 1
    ) map

  let add_props t (props : (string * string) list) =
    write_tag t Defs.Pa_tag.proplist;
    List.iter (fun (key, value) ->
      add_string t (Some key);
      let encoded = Bytes.of_string value in
      let with_term = Bytes.create (Bytes.length encoded + 1) in
      Bytes.blit encoded 0 with_term 0 (Bytes.length encoded);
      Bytes.set_uint8 with_term (Bytes.length encoded) 0;
      add_u32 t (Bytes.length with_term);
      add_arbitrary t with_term
    ) props;
    add_string t None

  let add_cvolume t (volumes : int list) =
    if List.length volumes > 255 then
      failwith (Printf.sprintf "too many channels in cvolume: %d" (List.length volumes));
    write_tag t Defs.Pa_tag.cvolume;
    ensure t (1 + List.length volumes * 4);
    Bytes.set_uint8 t.buf t.offset (List.length volumes);
    t.offset <- t.offset + 1;
    List.iter (fun volume ->
      Bytes.set_int32_be t.buf t.offset (Int32.of_int (volume land 0xffffffff));
      t.offset <- t.offset + 4
    ) volumes

  let add_volume t value =
    write_tag t Defs.Pa_tag.volume;
    ensure t 4;
    Bytes.set_int32_be t.buf t.offset (Int32.of_int (value land 0xffffffff));
    t.offset <- t.offset + 4

  let add_format_info t encoding (props : (string * string) list) =
    write_tag t Defs.Pa_tag.format_info;
    add_u8 t encoding;
    add_props t props

  let finalize t =
    Bytes.sub t.buf 0 t.offset
end

module Tag_struct_reader = struct
  type t = {
    buf : bytes;
    mutable offset : int;
  }

  let create (buf : bytes) = { buf; offset = 0 }

  let assert_readable t bytes =
    if t.offset + bytes > Bytes.length t.buf then
      failwith (Printf.sprintf "packet too short: need %d more bytes" bytes)

  let assert_tag t expected =
    assert_readable t 1;
    let tag = Bytes.get_uint8 t.buf t.offset in
    if tag <> expected then
      failwith (Printf.sprintf "invalid tag: expected %d, got %d" expected tag);
    t.offset <- t.offset + 1

  let get_u8 t =
    assert_tag t Defs.Pa_tag.u8;
    assert_readable t 1;
    let value = Bytes.get_uint8 t.buf t.offset in
    t.offset <- t.offset + 1;
    value

  let get_u32 t =
    assert_tag t Defs.Pa_tag.u32;
    assert_readable t 4;
    let value = Int32.to_int (Bytes.get_int32_be t.buf t.offset) in
    t.offset <- t.offset + 4;
    if value < 0 then value + 0x100000000 else value

  let get_u64 t =
    assert_tag t Defs.Pa_tag.u64;
    assert_readable t 8;
    let value = Bytes.get_int64_be t.buf t.offset in
    t.offset <- t.offset + 8;
    value

  let get_s64 t =
    assert_tag t Defs.Pa_tag.s64;
    assert_readable t 8;
    let value = Bytes.get_int64_be t.buf t.offset in
    t.offset <- t.offset + 8;
    value

  let get_usec t =
    assert_tag t Defs.Pa_tag.usec;
    assert_readable t 8;
    let value = Bytes.get_int64_be t.buf t.offset in
    t.offset <- t.offset + 8;
    value

  let get_string t : string option =
    assert_readable t 1;
    let tag = Bytes.get_uint8 t.buf t.offset in
    if tag = Defs.Pa_tag.string_null then begin
      t.offset <- t.offset + 1;
      None
    end else if tag <> Defs.Pa_tag.string then
      failwith (Printf.sprintf "invalid tag: expected %d or %d, got %d" Defs.Pa_tag.string Defs.Pa_tag.string_null tag)
    else begin
      t.offset <- t.offset + 1;
      let start = t.offset in
      while t.offset < Bytes.length t.buf && Bytes.get_uint8 t.buf t.offset <> 0 do
        t.offset <- t.offset + 1
      done;
      if t.offset >= Bytes.length t.buf then
        failwith "unterminated string";
      let value = Bytes.sub_string t.buf start (t.offset - start) in
      t.offset <- t.offset + 1;
      Some value
    end

  let get_arbitrary t : bytes =
    assert_tag t Defs.Pa_tag.arbitrary;
    assert_readable t 4;
    let length = Int32.to_int (Bytes.get_int32_be t.buf t.offset) in
    t.offset <- t.offset + 4;
    assert_readable t length;
    let value = Bytes.sub t.buf t.offset length in
    t.offset <- t.offset + length;
    value

  let get_bool t : bool =
    assert_readable t 1;
    let tag = Bytes.get_uint8 t.buf t.offset in
    if tag = Defs.Pa_tag.boolean_true then begin
      t.offset <- t.offset + 1;
      true
    end else if tag = Defs.Pa_tag.boolean_false then begin
      t.offset <- t.offset + 1;
      false
    end else
      failwith (Printf.sprintf "invalid bool tag: %d" tag)

  let is_sample_format value =
    value >= 0 && value <= 12

  let get_sample_spec t : Defs.sample_spec =
    assert_tag t Defs.Pa_tag.sample_spec;
    assert_readable t 6;
    let format = Bytes.get_uint8 t.buf t.offset in
    if not (is_sample_format format) then
      failwith (Printf.sprintf "invalid sample format: %d" format);
    t.offset <- t.offset + 1;
    let channels = Bytes.get_uint8 t.buf t.offset in
    t.offset <- t.offset + 1;
    let rate = Int32.to_int (Bytes.get_int32_be t.buf t.offset) in
    t.offset <- t.offset + 4;
    { format; channels; rate }

  let get_channel_map t : Defs.channel_map =
    assert_tag t Defs.Pa_tag.channel_map;
    assert_readable t 1;
    let channels = Bytes.get_uint8 t.buf t.offset in
    t.offset <- t.offset + 1;
    assert_readable t channels;
    let rec read n acc =
      if n = 0 then List.rev acc
      else begin
        let ch = Bytes.get_uint8 t.buf t.offset in
        t.offset <- t.offset + 1;
        read (n - 1) (ch :: acc)
      end
    in
    read channels []

  let get_props t : (string * string) list =
    assert_tag t Defs.Pa_tag.proplist;
    let rec read_props acc =
      match get_string t with
      | None -> List.rev acc
      | Some key ->
        ignore (get_u32 t);
        let value_bytes = get_arbitrary t in
        let without_term =
          if Bytes.length value_bytes > 0 && Bytes.get_uint8 value_bytes (Bytes.length value_bytes - 1) = 0 then
            Bytes.sub value_bytes 0 (Bytes.length value_bytes - 1)
          else
            value_bytes
        in
        let value = Bytes.to_string without_term in
        read_props ((key, value) :: acc)
    in
    read_props []

  let get_cvolume t : int list =
    assert_tag t Defs.Pa_tag.cvolume;
    assert_readable t 1;
    let channels = Bytes.get_uint8 t.buf t.offset in
    t.offset <- t.offset + 1;
    assert_readable t (channels * 4);
    let rec read n acc =
      if n = 0 then List.rev acc
      else begin
        let vol = Int32.to_int (Bytes.get_int32_be t.buf t.offset) in
        let vol = if vol < 0 then vol + 0x100000000 else vol in
        t.offset <- t.offset + 4;
        read (n - 1) (vol :: acc)
      end
    in
    read channels []

  let get_volume t : int =
    assert_tag t Defs.Pa_tag.volume;
    assert_readable t 4;
    let value = Int32.to_int (Bytes.get_int32_be t.buf t.offset) in
    t.offset <- t.offset + 4;
    if value < 0 then value + 0x100000000 else value

  let get_format_info t : int * (string * string) list =
    assert_tag t Defs.Pa_tag.format_info;
    let encoding = get_u8 t in
    let props = get_props t in
    (encoding, props)

  let has_remaining t = t.offset < Bytes.length t.buf

  let remaining_bytes t = Bytes.sub t.buf t.offset (Bytes.length t.buf - t.offset)
end
