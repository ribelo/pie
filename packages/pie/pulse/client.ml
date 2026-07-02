type error = {
  message : string;
  code : int option;
}

exception Client_error of error

let error message ?code () = { message; code }

let fail message ?code () = raise (Client_error { message; code })

let debug_enabled =
  match Sys.getenv_opt "CAMELPIE_PULSE_DEBUG" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let debug fmt =
  Printf.ksprintf
    (fun msg -> if debug_enabled then Printf.eprintf "%s%!" msg)
    fmt

type t =
  | T : {
      socket : 'a Eio.Flow.two_way;
      sw : Eio.Switch.t;
      mutable pending : (int, (bytes, error) result Eio.Promise.u) Hashtbl.t;
      record_queues : (int, bytes Eio.Stream.t) Hashtbl.t;
      mutable remainder : bytes;
      mutable next_tag : int;
      mutable protocol_version : int;
      request_timeout_ms : int;
      clock : 'b Eio.Time.clock;
    } -> t

let load_cookie () =
  try
    let ic = open_in_bin Defs.pa_default_cookie_path in
    let len = in_channel_length ic in
    let buf = really_input_string ic len in
    close_in ic;
    Some (Bytes.of_string buf)
  with
  | Sys_error msg ->
    debug "[pulse] cookie load failed: %s (tried %s)\n" msg Defs.pa_default_cookie_path;
    None

let process_incoming (T t) (chunk : bytes) : unit =
  let buffer = Bytes.create (Bytes.length t.remainder + Bytes.length chunk) in
  Bytes.blit t.remainder 0 buffer 0 (Bytes.length t.remainder);
  Bytes.blit chunk 0 buffer (Bytes.length t.remainder) (Bytes.length chunk);
  let offset = ref 0 in
  let continue = ref true in
  while !continue do
    if Bytes.length buffer - !offset < Defs.pa_stream_descriptor_size then
      continue := false
    else begin
      let header_bytes = Bytes.sub buffer !offset Defs.pa_stream_descriptor_size in
      let header = Protocol.decode_packet_header header_bytes in
      let max_packet_length = 256 * 1024 in
      if header.length > max_packet_length then
        fail (Printf.sprintf "packet length %d exceeds max" header.length) ();
      let packet_length = Defs.pa_stream_descriptor_size + header.length in
      if Bytes.length buffer - !offset < packet_length then
        continue := false
      else begin
        let payload = Bytes.sub buffer (!offset + Defs.pa_stream_descriptor_size) header.length in
        if header.channel = Defs.pa_no_index then begin
          match Commands.parse_command_envelope payload with
          | None ->
            debug "[pulse] skipping unparseable command packet (%d bytes)\n"
              (Bytes.length payload)
          | Some envelope ->
            (match Hashtbl.find_opt t.pending envelope.tag with
             | Some resolver ->
               Hashtbl.remove t.pending envelope.tag;
               if envelope.packet_type = Defs.Pa_command.reply then
                 Eio.Promise.resolve_ok resolver envelope.body
               else if envelope.packet_type = Defs.Pa_command.error then
                 let code = Commands.parse_error_code envelope.body in
                 Eio.Promise.resolve_error resolver (error "PulseAudio command failed" ?code ())
               else
                 Eio.Promise.resolve_error resolver (error (Printf.sprintf "unsupported response type: %d" envelope.packet_type) ())
             | None -> ())
        end else begin
          match Hashtbl.find_opt t.record_queues header.channel with
          | Some queue -> Eio.Stream.add queue payload
          | None -> ()
        end;
        offset := !offset + packet_length
      end
    end
  done;
  t.remainder <-
    if !offset = 0 then buffer
    else Bytes.sub buffer !offset (Bytes.length buffer - !offset)

let read_loop (T t) =
  debug "[pulse] read loop started\n";
  let buf = Cstruct.create 8192 in
  try
    while true do
      let n = Eio.Flow.single_read t.socket buf in
      debug "[pulse] read %d bytes\n" n;
      let chunk = Cstruct.to_bytes (Cstruct.sub buf 0 n) in
      process_incoming (T t) chunk
    done;
    `Stop_daemon
  with
  | End_of_file -> debug "[pulse] read loop: EOF\n"; `Stop_daemon
  | Client_error _ -> `Stop_daemon
  | Eio.Cancel.Cancelled _ -> debug "[pulse] read loop: cancelled\n"; `Stop_daemon
  | exn ->
    debug "[pulse] read loop error: %s\n" (Printexc.to_string exn);
    let err = error (Printf.sprintf "read loop error: %s" (Printexc.to_string exn)) () in
    (* B1: iteration is safe here because Eio fibers cannot preempt
       mid-statement, and await_reply only mutates pending on the
       success/timeout paths (not the error fan-out path). *)
    Hashtbl.iter (fun _ resolver -> Eio.Promise.resolve_error resolver err) t.pending;
    `Stop_daemon

let await_reply (T t) (packet : Commands.command_packet) : (bytes, error) result =
  let promise, resolver = Eio.Promise.create () in
  Hashtbl.add t.pending packet.tag resolver;
  let write_buf = Cstruct.of_bytes packet.bytes in
  debug "[pulse] sending packet tag=%d len=%d\n" packet.tag (Bytes.length packet.bytes);
  Eio.Flow.write t.socket [write_buf];
  debug "[pulse] packet sent, waiting for reply (timeout=%.1fs)\n" (Float.of_int t.request_timeout_ms /. 1000.0);
  let timeout_s = Float.of_int t.request_timeout_ms /. 1000.0 in
  try
    let result = Eio.Time.with_timeout_exn t.clock timeout_s (fun () -> Eio.Promise.await promise) in
    debug "[pulse] reply received for tag=%d\n" packet.tag;
    result
  with
  | Eio.Time.Timeout ->
    debug "[pulse] timeout waiting for reply to tag=%d\n" packet.tag;
    Hashtbl.remove t.pending packet.tag;
    Error (error (Printf.sprintf "timed out waiting for response to tag %d" packet.tag) ())

let connect
    ?(socket_path = Defs.pa_default_socket_path)
    ?cookie
    ?(client_name = "pie")
    ?(protocol_version = Defs.pa_native_protocol_version)
    ?(request_timeout_ms = 3000)
    ~net ~clock ~sw () =
  try
    let socket = Eio.Net.connect ~sw net (`Unix socket_path) in
    let t = T {
      socket;
      sw;
      pending = Hashtbl.create 16;
      record_queues = Hashtbl.create 4;
      remainder = Bytes.empty;
      next_tag = 0;
      protocol_version = 0;
      request_timeout_ms;
      clock;
    } in
    let (T tr) = t in
    Eio.Fiber.fork_daemon ~sw (fun () -> read_loop t);
    let cookie =
      match cookie with
      | Some c -> c
      | None ->
        match load_cookie () with
        | Some c -> c
        | None -> fail "PulseAudio cookie not found" ()
    in
    let auth_packet =
      match Commands.build_auth_command ~cookie ~protocol_version () with
      | Ok pkt -> pkt
      | Error msg -> fail msg ()
    in
    let auth_payload = await_reply t auth_packet in
    match auth_payload with
    | Error e -> fail e.message ?code:e.code ()
    | Ok payload ->
      let auth_version = Commands.parse_auth_response payload in
      (match Commands.parse_protocol_compatibility auth_version with
       | Error msg -> fail msg ()
       | Ok () -> ());
      tr.protocol_version <- auth_version;
      (match
         await_reply t
           (Commands.build_set_client_name_command
              ~client_name_or_props:(`String client_name) ())
       with
       | Error e -> fail e.message ?code:e.code ()
       | Ok _ -> ());
      Ok t
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | Client_error e ->
    Error (`Pulse_error (Printf.sprintf "PulseAudio: %s" e.message))
  | exn ->
    Error (`Pulse_error (Printf.sprintf "PulseAudio connect failed: %s" (Printexc.to_string exn)))

let get_server_info_impl (T t) =
  match await_reply (T t) (Commands.build_get_server_info_command ()) with
  | Error e -> Error (`Pulse_error e.message)
  | Ok payload -> Ok (Commands.parse_server_info_response payload)

let list_sources_impl (T t) =
  match await_reply (T t) (Commands.build_get_source_list_command ()) with
  | Error e -> Error (`Pulse_error e.message)
  | Ok payload -> Ok (Commands.parse_source_list_response payload)

let with_client ~sw ~net ~clock ?socket_path ?cookie ?client_name ?protocol_version ?request_timeout_ms f =
  Eta.Effect.with_resource
    ~acquire:
      (Eta.Effect.sync (fun () ->
         connect ~net ~clock ~sw ?socket_path ?cookie ?client_name
           ?protocol_version ?request_timeout_ms ())
       |> Eta.Effect.flatten_result)
    ~release:(fun (T t) -> Eta.Effect.sync (fun () ->
      Hashtbl.iter (fun _ resolver ->
        Eio.Promise.resolve_error resolver (error "client released" ())
      ) t.pending;
      Hashtbl.clear t.pending))
    f

let get_server_info client =
  Eta.Effect.sync (fun () -> get_server_info_impl client)
  |> Eta.Effect.flatten_result

let list_sources client =
  Eta.Effect.sync (fun () -> list_sources_impl client)
  |> Eta.Effect.flatten_result

let with_record_stream (T t) ?(options : Defs.record_stream_options option) f =
  let acquire =
    Eta.Effect.sync (fun () ->
        let packet = Commands.build_create_record_stream_command ?options () in
        match await_reply (T t) packet with
        | Error e -> Error (`Pulse_error e.message)
        | Ok payload ->
          let info = Commands.parse_create_record_stream_response payload in
          let queue = Eio.Stream.create 64 in
          Hashtbl.add t.record_queues info.Defs.stream_index queue;
          Ok info)
    |> Eta.Effect.flatten_result
  in
  let release (info : Defs.record_stream_info) =
    Eta.Effect.sync (fun () ->
      Hashtbl.remove t.record_queues info.Defs.stream_index;
      let packet = Commands.build_delete_record_stream_command info.stream_index in
      try ignore (await_reply (T t) packet) with _ -> ())
  in
  Eta.Effect.with_resource ~acquire ~release (fun info ->
    let recv () =
      match Hashtbl.find_opt t.record_queues info.Defs.stream_index with
      | None -> Eta.Effect.fail `Closed
      | Some queue ->
        Eta.Effect.sync (fun () -> Eio.Stream.take queue)
        |> Eta.Effect.catch (fun _ -> Eta.Effect.fail `Closed)
    in
    f info recv)
