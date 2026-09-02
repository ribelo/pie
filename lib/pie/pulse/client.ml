type error =
  | Protocol_error of string
  | Authentication_error of string
  | Server_error of { message : string; code : int option }
  | Timeout of { operation : string; duration : Eta.Duration.t }
  | Disconnected of string
  | Malformed_packet of string

open Eta.Syntax

let default_request_timeout = Eta.Duration.seconds 3
let record_stream_capacity = 64

let error_message = function
  | Protocol_error message -> "PulseAudio protocol error: " ^ message
  | Authentication_error message -> "PulseAudio authentication failed: " ^ message
  | Server_error { message; code = None } -> "PulseAudio server error: " ^ message
  | Server_error { message; code = Some code } ->
    Printf.sprintf "PulseAudio server error %d: %s" code message
  | Timeout { operation; duration } ->
    Printf.sprintf "PulseAudio %s timed out after %s"
      operation (Eta.Duration.humanize duration)
  | Disconnected message -> "PulseAudio disconnected: " ^ message
  | Malformed_packet message -> "PulseAudio malformed packet: " ^ message

let pp_error formatter error =
  Format.pp_print_string formatter (error_message error)

let pulse_error error = `Pulse_error error

let widen_pulse_error effect =
  Eta.Effect.map_error
    (function `Pulse_error error -> `Pulse_error error)
    effect

let debug message attrs =
  Eta_observability.log_debug ~attrs message

let warn message attrs =
  Eta_observability.log_warn ~attrs message

type record_queue = (bytes, [ `Pulse_error of error ]) Eta.Queue.t

type pending = {
  accept : bytes -> (unit, [ `Pulse_error of error ]) Eta.Effect.t;
  reject : [ `Pulse_error of error ] -> (unit, [ `Pulse_error of error ]) Eta.Effect.t;
}

type t =
  | T : {
      socket : 'a Eio.Net.stream_socket;
      pending : (int, pending) Hashtbl.t;
      record_queues : (int, record_queue) Hashtbl.t;
      mutable remainder : bytes;
      mutable protocol_version : int;
      request_timeout : Eta.Duration.t;
      write_lock : Eta.Semaphore.t;
    } -> t

let load_cookie () =
  try
    let ic = open_in_bin Defs.pa_default_cookie_path in
    let len = in_channel_length ic in
    let buf = really_input_string ic len in
    close_in ic;
    Ok (Bytes.of_string buf)
  with Sys_error message ->
    Error (pulse_error (Authentication_error message))

let protocol_result context f =
  Eta.Effect.sync_exn f ~catch:(function
    | Failure message ->
      pulse_error (Protocol_error (context ^ ": " ^ message))
    | Invalid_argument message ->
      pulse_error (Malformed_packet (context ^ ": " ^ message))
    | exn -> raise exn)

let malformed context message =
  Eta.Effect.fail (pulse_error (Malformed_packet (context ^ ": " ^ message)))

let resolve_success promise value =
  Eta.Promise.resolve promise (Eta.Exit.ok value) |> Eta.Effect.discard

let resolve_failure promise failure =
  Eta.Promise.resolve promise (Eta.Exit.error (Eta.Cause.fail failure))
  |> Eta.Effect.discard

let take_pending (T t) tag =
  Eta.Effect.sync (fun () ->
    let pending = Hashtbl.find_opt t.pending tag in
    Hashtbl.remove t.pending tag;
    pending)

let process_command client envelope =
  let* pending = take_pending client envelope.Commands.tag in
  match pending with
  | None ->
    debug "PulseAudio late reply dropped"
      [("pulse.tag", string_of_int envelope.tag)]
  | Some pending when envelope.packet_type = Defs.Pa_command.reply ->
    pending.accept envelope.body
  | Some pending when envelope.packet_type = Defs.Pa_command.error ->
    (match Commands.parse_error_code envelope.body with
     | Some code ->
       pending.reject
         (pulse_error (Server_error { message = "command rejected"; code = Some code }))
     | None ->
       pending.reject
         (pulse_error
            (Protocol_error
               "PulseAudio command error payload omitted its error code")))
  | Some pending ->
    pending.reject
      (pulse_error
         (Protocol_error
            (Printf.sprintf "unsupported response type %d" envelope.packet_type)))

let process_record_chunk (T t) channel payload =
  let* queue = Eta.Effect.sync (fun () -> Hashtbl.find_opt t.record_queues channel) in
  match queue with
  | None ->
    debug "PulseAudio chunk for inactive record stream dropped"
      [("pulse.channel", string_of_int channel)]
  | Some queue -> Eta.Queue.send queue payload |> Eta.Effect.ignore_errors

let save_remainder (T t) buffer offset =
  Eta.Effect.sync (fun () ->
    t.remainder <-
      if offset >= Bytes.length buffer then Bytes.empty
      else Bytes.sub buffer offset (Bytes.length buffer - offset))

let process_incoming (T t as client) chunk =
  let buffer = Bytes.create (Bytes.length t.remainder + Bytes.length chunk) in
  Bytes.blit t.remainder 0 buffer 0 (Bytes.length t.remainder);
  Bytes.blit chunk 0 buffer (Bytes.length t.remainder) (Bytes.length chunk);
  let rec loop offset =
    if Bytes.length buffer - offset < Defs.pa_stream_descriptor_size then
      save_remainder client buffer offset
    else
      let header_bytes = Bytes.sub buffer offset Defs.pa_stream_descriptor_size in
      let* header =
        protocol_result "packet header" (fun () ->
          Protocol.decode_packet_header header_bytes)
      in
      if header.Protocol.length > 256 * 1024 then
        malformed "packet header"
          (Printf.sprintf "length %d exceeds 262144 bytes" header.length)
      else
        let packet_length = Defs.pa_stream_descriptor_size + header.length in
        if Bytes.length buffer - offset < packet_length then
          save_remainder client buffer offset
        else
          let payload =
            Bytes.sub buffer
              (offset + Defs.pa_stream_descriptor_size)
              header.length
          in
          let* () =
            if header.channel = Defs.pa_no_index then
              match Commands.parse_command_envelope payload with
              | None ->
                malformed "command envelope" "missing packet type or request tag"
              | Some envelope -> process_command client envelope
            else process_record_chunk client header.channel payload
          in
          loop (offset + packet_length)
  in
  loop 0

let detach_waiters (T t) =
  Eta.Effect.sync (fun () ->
    let pending = Hashtbl.to_seq_values t.pending |> List.of_seq in
    let queues = Hashtbl.to_seq_values t.record_queues |> List.of_seq in
    Hashtbl.clear t.pending;
    Hashtbl.clear t.record_queues;
    pending, queues)

let fail_waiters client error =
  let failure = pulse_error error in
  let* pending, queues = detach_waiters client in
  let* () =
    pending
    |> List.map (fun pending -> pending.reject failure)
    |> Eta.Effect.concat
  in
  Eta.Effect.sync (fun () ->
    List.iter (fun queue -> Eta.Queue.close_with_error queue failure) queues)

let read_chunk (T t) =
  Eta.Effect.sync_result (fun () ->
    try
      let buffer = Cstruct.create 8192 in
      let length = Eio.Flow.single_read t.socket buffer in
      if length = 0 then Error (pulse_error (Disconnected "end of stream"))
      else Ok (Some (Cstruct.to_bytes (Cstruct.sub buffer 0 length)))
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | End_of_file -> Error (pulse_error (Disconnected "end of stream"))
    | exn ->
      Error
        (pulse_error
           (Disconnected ("reader failed: " ^ Printexc.to_string exn))))

let read_loop client =
  let rec loop () =
    let* chunk = read_chunk client in
    match chunk with
    | None -> Eta.Effect.unit
    | Some chunk ->
      let* () = process_incoming client chunk in
      let* () = Eta.Effect.yield in
      loop ()
  in
  loop ()
  |> Eta.Effect.bind_error (fun (`Pulse_error error as failure) ->
    let* () = fail_waiters client error in
    Eta.Effect.fail failure)

let write_packet (T t) packet =
  Eta.Semaphore.with_permits t.write_lock 1 @@ fun () ->
  Eta.Effect.sync_result (fun () ->
    try
      Eio.Flow.write t.socket [Cstruct.of_bytes packet.Commands.bytes];
      Ok ()
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
      Error
        (pulse_error
           (Disconnected ("write failed: " ^ Printexc.to_string exn))))

let await_command (T t as client) (packet : Commands.command_packet) pending await =
  let register =
    Eta.Effect.sync (fun () ->
      Hashtbl.replace t.pending packet.Commands.tag pending)
  in
  let cleanup _exit =
    Eta.Effect.sync (fun () -> Hashtbl.remove t.pending packet.tag)
  in
  let operation =
    let* () = register in
    let* () = write_packet client packet in
    await
  in
  operation
  |> Eta.Effect.timeout_as t.request_timeout
       ~on_timeout:
         (pulse_error
            (Timeout
               {
                 operation = Printf.sprintf "request tag %d" packet.tag;
                 duration = t.request_timeout;
               }))
  |> Eta.Effect.on_exit cleanup

let await_reply client packet =
  let promise = Eta.Promise.create () in
  let pending =
    {
      accept = (fun payload -> resolve_success promise payload);
      reject = (fun failure -> resolve_failure promise failure);
    }
  in
  await_command client packet pending (Eta.Promise.await promise)

let await_record_stream (T t as client) packet =
  let promise = Eta.Promise.create () in
  let accept payload =
    let completion =
      let* info =
        protocol_result "create record stream response" (fun () ->
          Commands.parse_create_record_stream_response payload)
      in
      let queue = Eta.Queue.bounded ~capacity:record_stream_capacity () in
      let* () =
        Eta.Effect.sync (fun () ->
          Hashtbl.replace t.record_queues info.Defs.stream_index queue)
      in
      Eta.Effect.pure (info, queue)
    in
    let* completion =
      completion
      |> Eta.Effect.fold
           ~ok:(fun value -> Ok value)
           ~error:(fun failure -> Error failure)
    in
    match completion with
    | Error failure -> resolve_failure promise failure
    | Ok (info, queue) ->
      let* accepted = Eta.Promise.resolve promise (Eta.Exit.ok (info, queue)) in
      if accepted then Eta.Effect.unit
      else
        Eta.Effect.sync (fun () ->
          Hashtbl.remove t.record_queues info.Defs.stream_index;
          Eta.Queue.close queue)
  in
  let pending =
    {
      accept;
      reject = (fun failure -> resolve_failure promise failure);
    }
  in
  await_command client packet pending (Eta.Promise.await promise)

let parse_reply context parser payload =
  protocol_result context (fun () -> parser payload)

let authenticate (T t as client) ~cookie ~client_name ~protocol_version =
  let* cookie =
    match cookie with
    | Some cookie -> Eta.Effect.pure cookie
    | None -> Eta.Effect.from_result (load_cookie ())
  in
  let* auth_packet =
    Commands.build_auth_command ~cookie ~protocol_version ()
    |> Result.map_error (fun message -> pulse_error (Authentication_error message))
    |> Eta.Effect.from_result
  in
  let* auth_payload =
    await_reply client auth_packet
    |> Eta.Effect.bind_error (function
      | `Pulse_error (Server_error { message; _ }) ->
        Eta.Effect.fail (pulse_error (Authentication_error message))
      | failure -> Eta.Effect.fail failure)
  in
  let* auth_version =
    parse_reply "authentication response" Commands.parse_auth_response auth_payload
  in
  let* () =
    Commands.parse_protocol_compatibility auth_version
    |> Result.map_error (fun message -> pulse_error (Protocol_error message))
    |> Eta.Effect.from_result
  in
  let* () = Eta.Effect.sync (fun () -> t.protocol_version <- auth_version) in
  let* _ =
    await_reply client
      (Commands.build_set_client_name_command
         ~client_name_or_props:(`String client_name) ())
  in
  Eta.Effect.unit

let with_eio_switch use =
  Eta.Spi.Expert.make ~leaf_name:"pulse.client.scope" @@ fun context ->
  let contract = Eta.Spi.Expert.contract context in
  try
    Eio.Switch.run ~name:"pulse.client" @@ fun sw ->
    Eta.Spi.Expert.eval context (use sw)
  with exn ->
    match contract.cancellation_reason exn with
    | Some _ -> raise exn
    | None -> Eta.Spi.Expert.exit_of_exn context exn

let connect_socket ~sw ~net socket_path =
  Eta.Effect.sync_result (fun () ->
    try Ok (Eio.Net.connect ~sw net (`Unix socket_path)) with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | exn ->
      Error
        (pulse_error
           (Disconnected
              ("connect failed: " ^ Printexc.to_string exn))))

let with_client ~net
    ?(socket_path = Defs.pa_default_socket_path)
    ?cookie
    ?(client_name = "pie")
    ?(protocol_version = Defs.pa_native_protocol_version)
    ?(request_timeout = default_request_timeout)
    use =
  (* pulse-86o0, pulse-d36y, pulse-w447, pulse-kryi, pulse-zj05,
     pulse-brjg: this Eta effect scope owns both the socket and its reader. *)
  with_eio_switch @@ fun sw ->
  let* socket = connect_socket ~sw ~net socket_path |> widen_pulse_error in
  let client =
    T
      {
        socket;
        pending = Hashtbl.create 16;
        record_queues = Hashtbl.create 4;
        remainder = Bytes.empty;
        protocol_version = 0;
        request_timeout;
        write_lock = Eta.Semaphore.make ~permits:1;
      }
  in
  let reader = read_loop client |> widen_pulse_error in
  let workflow =
    Eta.Effect.with_background reader @@ fun () ->
    let* () =
      authenticate client ~cookie ~client_name ~protocol_version
      |> widen_pulse_error
    in
    use client
  in
  let release_client =
    let* () = fail_waiters client (Disconnected "client scope ended") in
    let* () =
      Eta.Effect.sync (fun () ->
        try Eio.Flow.close socket with _ -> ())
    in
    Eta.Effect.unit
  in
  workflow |> Eta.Effect.finally release_client

let get_server_info client =
  let* payload = await_reply client (Commands.build_get_server_info_command ()) in
  parse_reply "server info response" Commands.parse_server_info_response payload
  |> widen_pulse_error

let list_sources client =
  let* payload = await_reply client (Commands.build_get_source_list_command ()) in
  parse_reply "source list response" Commands.parse_source_list_response payload
  |> widen_pulse_error

let with_record_stream (T t as client) ?options use =
  let acquire =
    await_record_stream client
      (Commands.build_create_record_stream_command ?options ())
  in
  let release (info, queue) =
    let* () =
      Eta.Effect.sync (fun () ->
        Hashtbl.remove t.record_queues info.Defs.stream_index;
        Eta.Queue.close queue)
    in
    await_reply client
      (Commands.build_delete_record_stream_command info.stream_index)
    |> Eta.Effect.discard
    |> Eta.Effect.bind_error (fun (`Pulse_error error) ->
      warn "PulseAudio record stream cleanup failed"
        [
          ("pulse.stream_index", string_of_int info.stream_index);
          ("pulse.error", error_message error);
        ])
  in
  Eta.Effect.with_resource ~acquire:(acquire |> widen_pulse_error) ~release
  @@ fun (info, queue) ->
  use info (Eta_stream.Stream.from_queue queue)
