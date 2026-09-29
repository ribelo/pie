type error =
  | Protocol_error of string
  | Authentication_error of string
  | Server_error of { message : string; code : int option }
  | Timeout of { operation : string; duration : Ox.Duration.t }
  | Disconnected of string
  | Malformed_packet of string
  | Recording_overload of { stream_index : int; capacity : int }

let default_request_timeout = Ox.Duration.seconds 3
let record_stream_capacity = 64
let ( let* ) = Result.bind

let error_message = function
  | Protocol_error message -> "PulseAudio protocol error: " ^ message
  | Authentication_error message ->
      "PulseAudio authentication failed: " ^ message
  | Server_error { message; code = None } ->
      "PulseAudio server error: " ^ message
  | Server_error { message; code = Some code } ->
      Printf.sprintf "PulseAudio server error %d: %s" code message
  | Timeout { operation; duration } ->
      Printf.sprintf "PulseAudio %s timed out after %s" operation
        (Ox.Duration.humanize duration)
  | Disconnected message -> "PulseAudio disconnected: " ^ message
  | Malformed_packet message -> "PulseAudio malformed packet: " ^ message
  | Recording_overload { stream_index; capacity } ->
      if capacity = Dictation_limits.queue_pcm_bytes then
        Printf.sprintf
          "PulseAudio recording overloaded on stream %d (%d PCM bytes)"
          stream_index capacity
      else
        Printf.sprintf "PulseAudio recording overloaded on stream %d (%d items)"
          stream_index capacity

let pp_error formatter error =
  Format.pp_print_string formatter (error_message error)

let pulse_error error = `Pulse_error error

(* The record FIFO and its terminal state share one lock: close never needs a
   sentinel slot and protocol replies never wait for an audio consumer. *)
type record_queue = {
  queue : string Queue.t;
  mutex : Eio.Mutex.t;
  changed : Eio.Condition.t;
  mutable queued_bytes : int;
  mutable terminal : (unit, [ `Pulse_error of error ]) result option;
}

let make_record_queue () =
  {
    queue = Queue.create ();
    mutex = Eio.Mutex.create ();
    changed = Eio.Condition.create ();
    queued_bytes = 0;
    terminal = None;
  }

let locked rq f =
  Eio.Mutex.lock rq.mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock rq.mutex) f

let close_record_queue rq outcome =
  locked rq (fun () ->
      match rq.terminal with
      | Some _ -> ()
      | None ->
          rq.terminal <- Some outcome;
          Eio.Condition.broadcast rq.changed)

let offer_record_queue rq chunk =
  locked rq (fun () ->
      match rq.terminal with
      | Some _ -> `Detached
      | None ->
          let size = Bytes.length chunk in
          if size > Dictation_limits.queue_pcm_bytes - rq.queued_bytes then
            `Overload Dictation_limits.queue_pcm_bytes
          else if Queue.length rq.queue >= record_stream_capacity then
            `Overload record_stream_capacity
          else (
            Queue.add (Bytes.to_string chunk) rq.queue;
            rq.queued_bytes <- rq.queued_bytes + size;
            Eio.Condition.broadcast rq.changed;
            `Sent))

let pull_record_queue rq () =
  locked rq (fun () ->
      let rec next () =
        match Queue.take_opt rq.queue with
        | Some chunk ->
            rq.queued_bytes <- rq.queued_bytes - String.length chunk;
            Ok (Some (Bytes.of_string chunk))
        | None -> (
            match rq.terminal with
            | Some (Ok ()) -> Ok None
            | Some (Error error) -> Error error
            | None ->
                Eio.Condition.await rq.changed rq.mutex;
                next ())
      in
      next ())

type pending = {
  accept : bytes -> unit;
  reject : [ `Pulse_error of error ] -> unit;
}

type t =
  | T : {
      socket : 'a Eio.Net.stream_socket;
      pending : (int, pending) Hashtbl.t;
      record_queues : (int, record_queue) Hashtbl.t;
      mutable remainder : bytes;
      mutable protocol_version : int;
      request_timeout : Ox.Duration.t;
      clock : 'b Eio.Time.clock;
      logger : Ox_observability.Logger.t;
      write_lock : Eio.Mutex.t;
    }
      -> t

let load_cookie ~caps () =
  try
    let ic =
      Ox_caps.Cap_stdlib.open_in_bin ~caps (Defs.pa_default_cookie_path ~caps)
    in
    let buf =
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))
    in
    Ok (Bytes.of_string buf)
  with Sys_error message -> Error (pulse_error (Authentication_error message))

let protocol_result context f =
  try Ok (f ()) with
  | Failure message ->
      Error (pulse_error (Protocol_error (context ^ ": " ^ message)))
  | Invalid_argument message ->
      Error (pulse_error (Malformed_packet (context ^ ": " ^ message)))

let malformed context message =
  Error (pulse_error (Malformed_packet (context ^ ": " ^ message)))

let take_pending (T t) tag =
  let pending = Hashtbl.find_opt t.pending tag in
  Hashtbl.remove t.pending tag;
  pending

let process_command (T t as client) envelope =
  match take_pending client envelope.Commands.tag with
  | None ->
      Ox_observability.Logger.debug t.logger
        ~fields:[ ("pulse.tag", Ox_observability.Logger.Int envelope.tag) ]
        "PulseAudio late reply dropped";
      Ok ()
  | Some pending when envelope.packet_type = Defs.Pa_command.reply ->
      pending.accept envelope.body;
      Ok ()
  | Some pending when envelope.packet_type = Defs.Pa_command.error ->
      pending.reject
        (pulse_error
           (match Commands.parse_error_code envelope.body with
           | Some code ->
               Server_error { message = "command rejected"; code = Some code }
           | None ->
               Protocol_error
                 "PulseAudio command error payload omitted its error code"));
      Ok ()
  | Some pending ->
      pending.reject
        (pulse_error
           (Protocol_error
              (Printf.sprintf "unsupported response type %d"
                 envelope.packet_type)));
      Ok ()

let process_record_chunk (T t) channel payload =
  match Hashtbl.find_opt t.record_queues channel with
  | None ->
      Ox_observability.Logger.debug t.logger
        ~fields:[ ("pulse.channel", Ox_observability.Logger.Int channel) ]
        "PulseAudio chunk for inactive record stream dropped";
      Ok ()
  | Some rq ->
      (match offer_record_queue rq payload with
      | `Sent -> ()
      | `Detached -> Hashtbl.remove t.record_queues channel
      | `Overload capacity ->
          Hashtbl.remove t.record_queues channel;
          close_record_queue rq
            (Error
               (pulse_error
                  (Recording_overload { stream_index = channel; capacity }))));
      Ok ()

let process_incoming (T t as client) chunk =
  let buffer = Bytes.create (Bytes.length t.remainder + Bytes.length chunk) in
  Bytes.blit t.remainder 0 buffer 0 (Bytes.length t.remainder);
  Bytes.blit chunk 0 buffer (Bytes.length t.remainder) (Bytes.length chunk);
  let rec loop offset =
    if Bytes.length buffer - offset < Defs.pa_stream_descriptor_size then (
      t.remainder <- Bytes.sub buffer offset (Bytes.length buffer - offset);
      Ok ())
    else
      let header_bytes =
        Bytes.sub buffer offset Defs.pa_stream_descriptor_size
      in
      let* header =
        protocol_result "packet header" (fun () ->
            Protocol.decode_packet_header header_bytes)
      in
      if header.Protocol.length > Protocol.max_frame_bytes then
        Error
          (pulse_error
             (Protocol_error
                (Printf.sprintf
                   "declared packet length %d exceeds max supported frame %d \
                    bytes"
                   header.length Protocol.max_frame_bytes)))
      else
        let packet_length = Defs.pa_stream_descriptor_size + header.length in
        if Bytes.length buffer - offset < packet_length then (
          t.remainder <- Bytes.sub buffer offset (Bytes.length buffer - offset);
          Ok ())
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
                  malformed "command envelope"
                    "missing packet type or request tag"
              | Some envelope -> process_command client envelope
            else process_record_chunk client header.channel payload
          in
          loop (offset + packet_length)
  in
  loop 0

let fail_waiters (T t) error =
  let failure = pulse_error error in
  let pending = Hashtbl.to_seq_values t.pending |> List.of_seq in
  let queues = Hashtbl.to_seq_values t.record_queues |> List.of_seq in
  Hashtbl.clear t.pending;
  Hashtbl.clear t.record_queues;
  List.iter (fun p -> p.reject failure) pending;
  List.iter (fun rq -> close_record_queue rq (Error failure)) queues

let read_chunk (T t) =
  try
    let buffer = Cstruct.create 8192 in
    let length = Eio.Flow.single_read t.socket buffer in
    if length = 0 then Error (pulse_error (Disconnected "end of stream"))
    else Ok (Cstruct.to_bytes (Cstruct.sub buffer 0 length))
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | End_of_file -> Error (pulse_error (Disconnected "end of stream"))
  | exn ->
      Error
        (pulse_error
           (Disconnected ("reader failed: " ^ Printexc.to_string exn)))

let read_loop client =
  let rec loop () =
    let* chunk = read_chunk client in
    let* () = process_incoming client chunk in
    Eio.Fiber.yield ();
    loop ()
  in
  match loop () with
  | Ok () -> Ok ()
  | Error (`Pulse_error error as failure) ->
      fail_waiters client error;
      Error failure

let write_packet (T t) packet =
  Eio.Mutex.lock t.write_lock;
  Fun.protect
    ~finally:(fun () -> Eio.Mutex.unlock t.write_lock)
    (fun () ->
      try
        Eio.Flow.write t.socket [ Cstruct.of_bytes packet.Commands.bytes ];
        Ok ()
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn ->
          Error
            (pulse_error
               (Disconnected ("write failed: " ^ Printexc.to_string exn))))

let await_command (T t as client) (packet : Commands.command_packet) pending
    await =
  Hashtbl.replace t.pending packet.tag pending;
  Fun.protect
    ~finally:(fun () -> Hashtbl.remove t.pending packet.tag)
    (fun () ->
      let operation () =
        let* () = write_packet client packet in
        await ()
      in
      match
        Ox.Timeout.timeout ~clock:t.clock t.request_timeout (fun () ->
            Result.map_error (fun e -> `Operation e) (operation ()))
      with
      | Ok v -> Ok v
      | Error `Timeout ->
          Error
            (pulse_error
               (Timeout
                  {
                    operation = Printf.sprintf "request tag %d" packet.tag;
                    duration = t.request_timeout;
                  }))
      | Error (`Operation e) -> Error e)

let await_reply client packet =
  let promise, resolver = Eio.Promise.create () in
  let pending =
    {
      accept = (fun payload -> Eio.Promise.resolve resolver (Ok payload));
      reject = (fun error -> Eio.Promise.resolve resolver (Error error));
    }
  in
  await_command client packet pending (fun () -> Eio.Promise.await promise)

let await_record_stream (T t as client) packet =
  let promise, resolver = Eio.Promise.create () in
  let pending =
    {
      accept =
        (fun payload ->
          let completion =
            let* info =
              protocol_result "create record stream response" (fun () ->
                  Commands.parse_create_record_stream_response payload)
            in
            let rq = make_record_queue () in
            Hashtbl.replace t.record_queues info.Defs.stream_index rq;
            Ok (info, rq)
          in
          Eio.Promise.resolve resolver completion);
      reject = (fun error -> Eio.Promise.resolve resolver (Error error));
    }
  in
  await_command client packet pending (fun () -> Eio.Promise.await promise)

let parse_reply context parser payload =
  protocol_result context (fun () -> parser payload)

let authenticate ~caps (T t as client) ~cookie ~client_name ~protocol_version =
  let* cookie =
    match cookie with Some cookie -> Ok cookie | None -> load_cookie ~caps ()
  in
  let* auth_packet =
    Commands.build_auth_command ~cookie ~protocol_version ()
    |> Result.map_error (fun message ->
        pulse_error (Authentication_error message))
  in
  let* auth_payload =
    await_reply client auth_packet
    |> Result.map_error (function
      | `Pulse_error (Server_error { message; _ }) ->
          pulse_error (Authentication_error message)
      | failure -> failure)
  in
  let* auth_version =
    parse_reply "authentication response" Commands.parse_auth_response
      auth_payload
  in
  let* () =
    Commands.parse_protocol_compatibility auth_version
    |> Result.map_error (fun message -> pulse_error (Protocol_error message))
  in
  t.protocol_version <- auth_version;
  let* _ =
    await_reply client
      (Commands.build_set_client_name_command
         ~client_name_or_props:(`String client_name) ())
  in
  Ok ()

let connect_socket ~sw ~net socket_path =
  try Ok (Eio.Net.connect ~sw net (`Unix socket_path)) with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn ->
      Error
        (pulse_error
           (Disconnected ("connect failed: " ^ Printexc.to_string exn)))

let with_client ~caps ~net ~clock ~logger ?socket_path ?cookie
    ?(client_name = "pie") ?(protocol_version = Defs.pa_native_protocol_version)
    ?(request_timeout = default_request_timeout)
    (use : t -> ('a, ([> `Pulse_error of error ] as 'err)) result) :
    ('a, 'err) result =
  let socket_path =
    Option.value socket_path ~default:(Defs.pa_default_socket_path ~caps)
  in
  Eio.Switch.run ~name:"pulse.client" @@ fun sw ->
  match connect_socket ~sw ~net socket_path with
  | Error (`Pulse_error error) -> Error (`Pulse_error error)
  | Ok socket ->
      let client =
        T
          {
            socket;
            pending = Hashtbl.create 16;
            record_queues = Hashtbl.create 4;
            remainder = Bytes.empty;
            protocol_version = 0;
            request_timeout;
            clock;
            logger;
            write_lock = Eio.Mutex.create ();
          }
      in
      Fun.protect
        ~finally:(fun () ->
          fail_waiters client (Disconnected "client scope ended");
          Eio.Flow.close socket)
        (fun () ->
          (* The reader and its user are siblings. A reader failure cancels the user;
         a completed user cancels and awaits the blocked reader. *)
          match
            Eio.Fiber.first
              (fun () -> `Reader (read_loop client))
              (fun () ->
                `Body
                  (match
                     authenticate ~caps client ~cookie ~client_name
                       ~protocol_version
                   with
                  | Error (`Pulse_error error) -> Error (`Pulse_error error)
                  | Ok () -> use client))
          with
          | `Reader (Error (`Pulse_error error)) -> Error (`Pulse_error error)
          | `Reader (Ok ()) ->
              Error (pulse_error (Disconnected "reader stopped"))
          | `Body result -> result)

let get_server_info client =
  let* payload =
    await_reply client (Commands.build_get_server_info_command ())
  in
  parse_reply "server info response" Commands.parse_server_info_response payload

let list_sources client =
  let* payload =
    await_reply client (Commands.build_get_source_list_command ())
  in
  parse_reply "source list response" Commands.parse_source_list_response payload

let with_record_stream (T t as client) ?options
    (use :
      Defs.record_stream_info ->
      (unit -> (bytes option, [ `Pulse_error of error ]) result) ->
      ('a, ([> `Pulse_error of error ] as 'err)) result) : ('a, 'err) result =
  match
    await_record_stream client
      (Commands.build_create_record_stream_command ?options ())
  with
  | Error (`Pulse_error error) -> Error (`Pulse_error error)
  | Ok (info, rq) ->
      Fun.protect
        ~finally:(fun () ->
          Hashtbl.remove t.record_queues info.Defs.stream_index;
          close_record_queue rq (Ok ());
          (* When the reader has stopped (server closed) the body is being cancelled:
             there is no one to answer a delete, and closing the socket frees the
             stream server-side. Skip the delete then instead of raising from the
             finalizer, which would replace the typed Disconnected with a defect. *)
          match
            try
              await_reply client
                (Commands.build_delete_record_stream_command info.stream_index)
            with Eio.Cancel.Cancelled _ -> Ok Bytes.empty
          with
          | Ok _ -> ()
          | Error (`Pulse_error error) ->
              Ox_observability.Logger.warn t.logger
                ~fields:
                  [
                    ( "pulse.stream_index",
                      Ox_observability.Logger.Int info.stream_index );
                    ( "pulse.error",
                      Ox_observability.Logger.String (error_message error) );
                  ]
                "PulseAudio record stream cleanup failed")
        (fun () -> use info (pull_record_queue rq))
