open Eta.Syntax

let zero_cookie = Bytes.make Pie.Pulse.Defs.pa_native_cookie_length '\000'
let request_timeout = Eta.Duration.ms 250

let pulse_read_exact flow len =
  let buffer = Cstruct.create len in
  let rec loop off =
    if off = len then Cstruct.to_bytes buffer
    else
      let cs = Cstruct.sub buffer off (len - off) in
      match Eio.Flow.single_read flow cs with
      | 0 -> raise End_of_file
      | n -> loop (off + n)
      | exception End_of_file -> raise End_of_file
  in
  loop 0

let pulse_command_packet packet_type tag body_bytes =
  let envelope_writer = Pie.Pulse.Protocol.Tag_struct_writer.create () in
  Pie.Pulse.Protocol.Tag_struct_writer.add_u32 envelope_writer packet_type;
  Pie.Pulse.Protocol.Tag_struct_writer.add_u32 envelope_writer tag;
  let envelope =
    Pie.Pulse.Protocol.Tag_struct_writer.finalize envelope_writer
  in
  let payload =
    Bytes.create (Bytes.length envelope + Bytes.length body_bytes)
  in
  Bytes.blit envelope 0 payload 0 (Bytes.length envelope);
  Bytes.blit body_bytes 0 payload (Bytes.length envelope)
    (Bytes.length body_bytes);
  Pie.Pulse.Protocol.frame_packet payload

let pulse_read_command flow =
  let header = pulse_read_exact flow Pie.Pulse.Defs.pa_stream_descriptor_size in
  let hdr = Pie.Pulse.Protocol.decode_packet_header header in
  let payload = pulse_read_exact flow hdr.length in
  match Pie.Pulse.Commands.parse_command_envelope payload with
  | Some envelope -> envelope
  | None -> Alcotest.fail "could not parse command envelope"

let pulse_write_response flow envelope packet_type body =
  pulse_command_packet packet_type envelope.Pie.Pulse.Commands.tag body
  |> Cstruct.of_bytes
  |> fun response -> Eio.Flow.write flow [ response ]

let pulse_u32 value =
  let writer = Pie.Pulse.Protocol.Tag_struct_writer.create () in
  Pie.Pulse.Protocol.Tag_struct_writer.add_u32 writer value;
  Pie.Pulse.Protocol.Tag_struct_writer.finalize writer

let pulse_server_handshake flow =
  let auth = pulse_read_command flow in
  pulse_write_response flow auth Pie.Pulse.Defs.Pa_command.reply
    (pulse_u32 Pie.Pulse.Defs.pa_native_protocol_version);
  let set_name = pulse_read_command flow in
  pulse_write_response flow set_name Pie.Pulse.Defs.Pa_command.reply Bytes.empty

let pulse_server_info_body () =
  let writer = Pie.Pulse.Protocol.Tag_struct_writer.create () in
  Pie.Pulse.Protocol.Tag_struct_writer.add_string writer (Some "test-server");
  Pie.Pulse.Protocol.Tag_struct_writer.add_string writer (Some "1.0");
  Pie.Pulse.Protocol.Tag_struct_writer.add_string writer (Some "tester");
  Pie.Pulse.Protocol.Tag_struct_writer.add_string writer (Some "localhost");
  Pie.Pulse.Protocol.Tag_struct_writer.add_sample_spec writer
    {
      Pie.Pulse.Defs.format = Pie.Pulse.Defs.Pa_sample_format.s16le;
      channels = 1;
      rate = 16000;
    };
  Pie.Pulse.Protocol.Tag_struct_writer.add_string writer None;
  Pie.Pulse.Protocol.Tag_struct_writer.add_string writer None;
  Pie.Pulse.Protocol.Tag_struct_writer.add_u32 writer 42;
  Pie.Pulse.Protocol.Tag_struct_writer.add_channel_map writer [ 0 ];
  Pie.Pulse.Protocol.Tag_struct_writer.finalize writer

let pulse_record_stream_body stream_index =
  let writer = Pie.Pulse.Protocol.Tag_struct_writer.create () in
  Pie.Pulse.Protocol.Tag_struct_writer.add_u32 writer stream_index;
  Pie.Pulse.Protocol.Tag_struct_writer.add_u32 writer 11;
  Pie.Pulse.Protocol.Tag_struct_writer.add_u32 writer 65536;
  Pie.Pulse.Protocol.Tag_struct_writer.add_u32 writer 320;
  Pie.Pulse.Protocol.Tag_struct_writer.add_sample_spec writer
    {
      Pie.Pulse.Defs.format = Pie.Pulse.Defs.Pa_sample_format.s16le;
      channels = 1;
      rate = 16000;
    };
  Pie.Pulse.Protocol.Tag_struct_writer.add_channel_map writer [ 0 ];
  Pie.Pulse.Protocol.Tag_struct_writer.add_u32 writer 1;
  Pie.Pulse.Protocol.Tag_struct_writer.add_string writer (Some "test-source");
  Pie.Pulse.Protocol.Tag_struct_writer.add_bool writer false;
  Pie.Pulse.Protocol.Tag_struct_writer.add_usec writer 0L;
  Pie.Pulse.Protocol.Tag_struct_writer.add_format_info writer 0 [];
  Pie.Pulse.Protocol.Tag_struct_writer.finalize writer

let write_record_chunk flow ~channel ~seq =
  let payload = Bytes.make 1 (Char.chr (seq land 0xff)) in
  Pie.Pulse.Protocol.frame_packet ~channel payload |> Cstruct.of_bytes
  |> fun framed -> Eio.Flow.write flow [ framed ]

let write_record_payload flow ~channel payload =
  Pie.Pulse.Protocol.frame_packet ~channel payload |> Cstruct.of_bytes
  |> fun framed -> Eio.Flow.write flow [ framed ]

let drain_eof flow =
  let buf = Cstruct.create 256 in
  let rec loop () =
    match Eio.Flow.single_read flow buf with
    | 0 -> ()
    | _ -> loop ()
    | exception End_of_file -> ()
  in
  loop ()

let pulse_test_server ~sw ~net name handler =
  let socket_path =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "pie-pulse-live-%s-%d.sock" name (Unix.getpid ()))
  in
  (try Sys.remove socket_path with Sys_error _ -> ());
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:1 net (`Unix socket_path)
  in
  Eio.Fiber.fork ~sw (fun () ->
      Eio.Switch.run (fun conn_sw ->
          let flow, _ = Eio.Net.accept ~sw:conn_sw socket in
          handler flow));
  socket_path

let run_backpressure ~chunks ~consume () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let deleted = Atomic.make false in
  let stream_index = 7 in
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env)
      ("bp-" ^ string_of_int chunks)
      (fun flow ->
        pulse_server_handshake flow;
        let create = pulse_read_command flow in
        pulse_write_response flow create Pie.Pulse.Defs.Pa_command.reply
          (pulse_record_stream_body stream_index);
        for seq = 0 to chunks - 1 do
          write_record_chunk flow ~channel:stream_index ~seq
        done;
        let info = pulse_read_command flow in
        pulse_write_response flow info Pie.Pulse.Defs.Pa_command.reply
          (pulse_server_info_body ());
        let delete = pulse_read_command flow in
        pulse_write_response flow delete Pie.Pulse.Defs.Pa_command.reply
          Bytes.empty;
        Atomic.set deleted true;
        drain_eof flow)
  in
  let operation =
    Pie.Pulse.Client.with_client ~net:(Eio.Stdenv.net env) ~socket_path
      ~cookie:zero_cookie ~request_timeout (fun client ->
        Pie.Pulse.Client.with_record_stream client (fun _info stream ->
            let consume_side =
              if consume then
                let acc = ref [] in
                Eta_stream.run_for_each
                  (fun chunk ->
                    Eta.Effect.sync (fun () -> acc := chunk :: !acc))
                  stream
                |> Eta.Effect.to_exit
                |> Eta.Effect.map (fun exit -> (exit, List.rev !acc))
              else Eta.Effect.pure (Eta.Exit.ok (), [])
            in
            let* info = Pie.Pulse.Client.get_server_info client in
            let* consumed = consume_side in
            Eta.Effect.pure (info, consumed)))
  in
  let result = Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env) operation in
  (result, Atomic.get deleted)

let cause_has pred cause = List.exists pred (Eta.Cause.failures cause)

let test_server_info_survives chunks =
  match run_backpressure ~chunks ~consume:false () with
  | Ok _, true -> ()
  | Ok _, false -> Alcotest.fail "delete reply was not completed"
  | Error cause, _ ->
      Alcotest.failf "server-info failed after %d chunks: %a" chunks
        (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
             Pie.Pulse.Client.pp_error fmt e))
        cause

let test_63_server_info () = test_server_info_survives 63
let test_64_server_info () = test_server_info_survives 64

let test_65_overload_and_server_info () =
  match run_backpressure ~chunks:65 ~consume:true () with
  | Ok (_info, (Eta.Exit.Ok _, _)), deleted ->
      Alcotest.failf "truncated stream succeeded delete=%b" deleted
  | Ok (_info, (Eta.Exit.Error cause, _chunks)), deleted ->
      if not deleted then Alcotest.fail "delete reply was not completed";
      if
        not
          (cause_has
             (function
               | `Pulse_error
                   (Pie.Pulse.Client.Recording_overload
                      { stream_index = 7; capacity = 64 }) ->
                   true
               | _ -> false)
             cause)
      then
        Alcotest.failf "expected Recording_overload, got %a"
          (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
               Pie.Pulse.Client.pp_error fmt e))
          cause;
      if
        cause_has
          (function
            | `Pulse_error (Pie.Pulse.Client.Disconnected _) -> true
            | _ -> false)
          cause
      then Alcotest.fail "overload was classified as disconnect"
  | Error cause, _ ->
      Alcotest.failf "connection failed instead of stream overload: %a"
        (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
             Pie.Pulse.Client.pp_error fmt e))
        cause

let test_65_tail_ordered_then_error () =
  match run_backpressure ~chunks:65 ~consume:true () with
  | Ok (_info, (Eta.Exit.Error cause, chunks)), true ->
      if List.length chunks <> 64 then
        Alcotest.failf "expected 64 admitted chunks, got %d"
          (List.length chunks);
      List.iteri
        (fun i chunk ->
          if Bytes.length chunk <> 1 || Bytes.get chunk 0 <> Char.chr i then
            Alcotest.failf "chunk %d out of order" i)
        chunks;
      if
        not
          (cause_has
             (function
               | `Pulse_error (Pie.Pulse.Client.Recording_overload _) -> true
               | _ -> false)
             cause)
      then Alcotest.fail "missing overload on consumed tail"
  | Ok (_info, (Eta.Exit.Ok _, chunks)), _ ->
      Alcotest.failf "expected overload after %d chunks" (List.length chunks)
  | _ -> Alcotest.fail "unexpected tail result"

let test_cancel_closes_scope () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "cancel" (fun flow ->
        pulse_server_handshake flow;
        let create = pulse_read_command flow in
        pulse_write_response flow create Pie.Pulse.Defs.Pa_command.reply
          (pulse_record_stream_body 3);
        (try ignore (pulse_read_command flow) with End_of_file -> ());
        drain_eof flow)
  in
  let operation =
    Pie.Pulse.Client.with_client ~net:(Eio.Stdenv.net env) ~socket_path
      ~cookie:zero_cookie ~request_timeout (fun client ->
        Pie.Pulse.Client.with_record_stream client (fun _ stream ->
            let cancel =
              Eta.Effect.sync (fun () -> ())
              |> Eta.Effect.map (fun () -> `Cancel)
            in
            Eta.Effect.race
              [
                Eta_stream.run_fold (fun n _ -> n + 1) 0 stream
                |> Eta.Effect.map (fun _ -> `Body);
                cancel;
              ]))
  in
  match Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env) operation with
  | Ok `Cancel -> ()
  | Ok `Body -> Alcotest.fail "expected cancel winner"
  | Error cause ->
      Alcotest.failf "cancel failed: %a"
        (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
             Pie.Pulse.Client.pp_error fmt e))
        cause

let test_broken_connection_disconnected () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "disconnect" (fun flow ->
        pulse_server_handshake flow;
        ignore (pulse_read_command flow);
        Eio.Flow.shutdown flow `All)
  in
  let operation =
    Pie.Pulse.Client.with_client ~net:(Eio.Stdenv.net env) ~socket_path
      ~cookie:zero_cookie ~request_timeout (fun client ->
        Pie.Pulse.Client.get_server_info client)
  in
  match Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env) operation with
  | Error cause
    when cause_has
           (function
             | `Pulse_error (Pie.Pulse.Client.Disconnected _) -> true
             | _ -> false)
           cause ->
      ()
  | Error cause ->
      Alcotest.failf "expected Disconnected, got %a"
        (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
             Pie.Pulse.Client.pp_error fmt e))
        cause
  | Ok _ -> Alcotest.fail "pending command survived broken connection"

let kib n = n * 1024
let half_mib = kib 512
let queue_cap = Pie.Dictation_limits.queue_pcm_bytes

let run_payloads ~payloads ~consume ~after () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let deleted = Atomic.make false in
  let stream_index = 7 in
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "bytes" (fun flow ->
        pulse_server_handshake flow;
        let create = pulse_read_command flow in
        pulse_write_response flow create Pie.Pulse.Defs.Pa_command.reply
          (pulse_record_stream_body stream_index);
        List.iteri
          (fun seq payload ->
            let tagged =
              if Bytes.length payload = 0 then payload
              else (
                Bytes.set payload 0 (Char.chr (seq land 0xff));
                payload)
            in
            write_record_payload flow ~channel:stream_index tagged)
          payloads;
        (match after with
        | `Control ->
            let info = pulse_read_command flow in
            pulse_write_response flow info Pie.Pulse.Defs.Pa_command.reply
              (pulse_server_info_body ());
            let delete = pulse_read_command flow in
            pulse_write_response flow delete Pie.Pulse.Defs.Pa_command.reply
              Bytes.empty;
            Atomic.set deleted true
        | `None -> ());
        drain_eof flow)
  in
  let operation =
    Pie.Pulse.Client.with_client ~net:(Eio.Stdenv.net env) ~socket_path
      ~cookie:zero_cookie ~request_timeout (fun client ->
        Pie.Pulse.Client.with_record_stream client (fun _info stream ->
            let consume_side =
              if consume then
                let acc = ref [] in
                Eta_stream.run_for_each
                  (fun chunk ->
                    Eta.Effect.sync (fun () -> acc := chunk :: !acc))
                  stream
                |> Eta.Effect.to_exit
                |> Eta.Effect.map (fun exit -> (exit, List.rev !acc))
              else Eta.Effect.pure (Eta.Exit.ok (), [])
            in
            let* info = Pie.Pulse.Client.get_server_info client in
            let* consumed = consume_side in
            Eta.Effect.pure (info, consumed)))
  in
  let result = Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env) operation in
  (result, Atomic.get deleted)

let test_enormous_declared_payload () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stream_index = 7 in
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "huge" (fun flow ->
        pulse_server_handshake flow;
        let create = pulse_read_command flow in
        pulse_write_response flow create Pie.Pulse.Defs.Pa_command.reply
          (pulse_record_stream_body stream_index);
        let header =
          Pie.Pulse.Protocol.encode_packet_header
            {
              length = 64 * 1024 * 1024;
              channel = stream_index;
              offset_hi = 0;
              offset_lo = 0;
              flags = 0;
            }
        in
        Eio.Flow.write flow [ Cstruct.of_bytes header ];
        (try ignore (pulse_read_command flow) with End_of_file -> ());
        drain_eof flow)
  in
  let operation =
    Pie.Pulse.Client.with_client ~net:(Eio.Stdenv.net env) ~socket_path
      ~cookie:zero_cookie ~request_timeout (fun client ->
        Pie.Pulse.Client.with_record_stream client (fun _ stream ->
            Eta_stream.run_fold (fun n _ -> n + 1) 0 stream))
  in
  match Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env) operation with
  | Error cause
    when cause_has
           (function
             | `Pulse_error (Pie.Pulse.Client.Protocol_error message) ->
                 String.length message > 0
             | _ -> false)
           cause ->
      ()
  | Error cause ->
      Alcotest.failf "expected Protocol_error, got %a"
        (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
             Pie.Pulse.Client.pp_error fmt e))
        cause
  | Ok _ -> Alcotest.fail "enormous declared length was accepted"

let test_byte_cap_then_control () =
  let payloads = [ Bytes.make half_mib 'a'; Bytes.make half_mib 'b' ] in
  match run_payloads ~payloads ~consume:false ~after:`Control () with
  | Ok _, true -> ()
  | Ok _, false -> Alcotest.fail "delete reply was not completed under byte cap"
  | Error cause, _ ->
      Alcotest.failf "at-cap queue lost control: %a"
        (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
             Pie.Pulse.Client.pp_error fmt e))
        cause

let test_byte_cap_overload () =
  let payloads =
    [
      Bytes.make half_mib 'a';
      Bytes.make half_mib 'b';
      Bytes.make 1 'c';
    ]
  in
  match run_payloads ~payloads ~consume:true ~after:`Control () with
  | Ok (_info, (Eta.Exit.Error cause, chunks)), deleted ->
      if not deleted then Alcotest.fail "delete reply was not completed";
      let admitted = List.fold_left (fun n c -> n + Bytes.length c) 0 chunks in
      if admitted > queue_cap then
        Alcotest.failf "admitted %d bytes over cap %d" admitted queue_cap;
      if
        not
          (cause_has
             (function
               | `Pulse_error
                   (Pie.Pulse.Client.Recording_overload
                      { stream_index = 7; capacity })
                 when capacity = queue_cap ->
                   true
               | _ -> false)
             cause)
      then
        Alcotest.failf "expected byte Recording_overload, got %a"
          (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
               Pie.Pulse.Client.pp_error fmt e))
          cause
  | Ok (_info, (Eta.Exit.Ok _, chunks)), _ ->
      Alcotest.failf "expected overload after %d bytes"
        (List.fold_left (fun n c -> n + Bytes.length c) 0 chunks)
  | Error cause, _ ->
      Alcotest.failf "connection failed instead of stream overload: %a"
        (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
             Pie.Pulse.Client.pp_error fmt e))
        cause

let test_cancel_while_byte_full () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let stream_index = 3 in
  let socket_path =
    pulse_test_server ~sw ~net:(Eio.Stdenv.net env) "byte-cancel" (fun flow ->
        pulse_server_handshake flow;
        let create = pulse_read_command flow in
        pulse_write_response flow create Pie.Pulse.Defs.Pa_command.reply
          (pulse_record_stream_body stream_index);
        write_record_payload flow ~channel:stream_index
          (Bytes.make half_mib 'x');
        write_record_payload flow ~channel:stream_index
          (Bytes.make half_mib 'y');
        (try ignore (pulse_read_command flow) with End_of_file -> ());
        drain_eof flow)
  in
  let operation =
    Pie.Pulse.Client.with_client ~net:(Eio.Stdenv.net env) ~socket_path
      ~cookie:zero_cookie ~request_timeout (fun client ->
        Pie.Pulse.Client.with_record_stream client (fun _ stream ->
            let cancel =
              Eta.Effect.sync (fun () -> ())
              |> Eta.Effect.map (fun () -> `Cancel)
            in
            Eta.Effect.race
              [
                Eta_stream.run_fold (fun n _ -> n + 1) 0 stream
                |> Eta.Effect.map (fun _ -> `Body);
                cancel;
              ]))
  in
  match Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env) operation with
  | Ok `Cancel -> ()
  | Ok `Body -> Alcotest.fail "expected cancel winner"
  | Error cause ->
      Alcotest.failf "cancel failed: %a"
        (Eta.Cause.pp (fun fmt (`Pulse_error e) ->
             Pie.Pulse.Client.pp_error fmt e))
        cause

let () =
  Alcotest.run "pulse_liveness"
    [
      ( "control",
        [
          Alcotest.test_case "63 chunks then server-info" `Quick
            test_63_server_info;
          Alcotest.test_case "64 chunks then server-info" `Quick
            test_64_server_info;
          Alcotest.test_case "65 chunks overload distinct from disconnect"
            `Quick test_65_overload_and_server_info;
          Alcotest.test_case "65 chunks tail then overload" `Quick
            test_65_tail_ordered_then_error;
          Alcotest.test_case "cancellation closes scope" `Quick
            test_cancel_closes_scope;
          Alcotest.test_case "broken connection remains disconnected" `Quick
            test_broken_connection_disconnected;
          Alcotest.test_case
            "enormous declared payload rejected without body" `Quick
            test_enormous_declared_payload;
          Alcotest.test_case "variable chunks at byte cap then control lives"
            `Quick test_byte_cap_then_control;
          Alcotest.test_case "variable chunks over byte cap overload" `Quick
            test_byte_cap_overload;
          Alcotest.test_case "cancel while byte-full" `Quick
            test_cancel_while_byte_full;
        ] );
    ]
