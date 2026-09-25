open Eta.Syntax

let s16le samples =
  let bytes = Bytes.create (List.length samples * 2) in
  List.iteri (fun i sample -> Bytes.set_int16_le bytes (i * 2) sample) samples;
  bytes

let spec rate : Pie.Pulse.Defs.sample_spec =
  { format = Pie.Pulse.Defs.Pa_sample_format.s16le; channels = 1; rate }

let acquire_chunks rate chunks k =
  k
    {
      Pie.Commands.Record.format = spec rate;
      audio = Eta_stream.Stream.from_iterable chunks;
    }

let unix_io = Pie.Commands.Record.unix_io

let run ~output_path ~duration_sec ~chunks ?(rate = 16000) ?(io = unix_io) () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Pie.Commands.Record.run ~sw ~net:(Eio.Stdenv.net env)
    ~clock:(Eio.Stdenv.clock env) ~output_path ~duration_sec
    ~acquire:(acquire_chunks rate chunks)
    ~io ()
  |> fun op -> Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env) op

let leftovers output_path =
  let dir = Filename.dirname output_path in
  let base = Filename.basename output_path in
  let prefix = "." ^ base ^ ".pie-record" in
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun name -> String.starts_with ~prefix name)

let wav_byte_rate path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let buf = really_input_string ic 44 in
      Bytes.get_int32_le (Bytes.of_string buf) 28 |> Int32.to_int)

let wav_rate path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let buf = really_input_string ic 44 in
      Bytes.get_int32_le (Bytes.of_string buf) 24 |> Int32.to_int)

let wav_data path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      seek_in ic 44;
      really_input_string ic (in_channel_length ic - 44))

let cause_text cause =
  Format.asprintf "%a"
    (Eta.Cause.pp (fun fmt -> function
      | `Stt_error m -> Format.pp_print_string fmt m
      | `Pulse_error e ->
          Format.pp_print_string fmt (Pie.Pulse.Client.error_message e)))
    cause

let expect_fail pred result =
  match result with
  | Eta.Exit.Ok () -> Alcotest.fail "expected record failure"
  | Eta.Exit.Error cause ->
      if not (pred cause) then
        Alcotest.failf "unexpected failure: %s" (cause_text cause)

let contains needle cause =
  let s = cause_text cause in
  let n = String.length needle in
  let h = String.length s in
  let rec loop i = i <= h - n && (String.sub s i n = needle || loop (i + 1)) in
  loop 0

let temp_wav name =
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf "pie-diag-%s-%d.wav" name (Unix.getpid ()))

let test_partition_identical () =
  let samples = [ 1000; 2000; 3000; 4000 ] in
  let whole = s16le samples in
  let left = s16le [ 1000; 2000 ] in
  let right = s16le [ 3000; 4000 ] in
  let a = temp_wav "whole" in
  let b = temp_wav "parts" in
  (try Sys.remove a with _ -> ());
  (try Sys.remove b with _ -> ());
  (match run ~output_path:a ~duration_sec:1. ~chunks:[ whole ] () with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error c -> Alcotest.failf "whole: %s" (cause_text c));
  (match run ~output_path:b ~duration_sec:1. ~chunks:[ left; right ] () with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error c -> Alcotest.failf "parts: %s" (cause_text c));
  Alcotest.(check string) "faithful pcm" (wav_data a) (wav_data b);
  Sys.remove a;
  Sys.remove b

let test_header_negotiated_rate () =
  let path = temp_wav "rate" in
  (try Sys.remove path with _ -> ());
  (match
     run ~output_path:path ~duration_sec:1. ~rate:48000
       ~chunks:[ s16le [ 1; 2 ] ]
       ()
   with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error c -> Alcotest.failf "rate: %s" (cause_text c));
  Alcotest.(check int) "wav sample rate" 48000 (wav_rate path);
  Alcotest.(check int) "wav byte rate" (48000 * 2) (wav_byte_rate path);
  Sys.remove path

let test_duration_before_acquire duration_sec =
  let acquired = Atomic.make false in
  let path = temp_wav "dur" in
  (try Sys.remove path with _ -> ());
  let dest_existed = Sys.file_exists path in
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Pie.Commands.Record.run ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~output_path:path ~duration_sec
      ~acquire:(fun k ->
        Atomic.set acquired true;
        acquire_chunks 16000 [ s16le [ 1 ] ] k)
      ()
    |> Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
  in
  Alcotest.(check bool) "did not acquire" false (Atomic.get acquired);
  Alcotest.(check bool) "output untouched" dest_existed (Sys.file_exists path);
  expect_fail (contains "record:") result

let test_invalid_durations () =
  List.iter test_duration_before_acquire
    [ nan; infinity; neg_infinity; 0.; -1.; float_of_int max_int ]

let test_write_failure_preserves_dest () =
  let path = temp_wav "write" in
  let oc = open_out_bin path in
  output_string oc "OLD";
  close_out oc;
  let io = { unix_io with write = (fun _ _ -> failwith "write boom") } in
  let result =
    run ~output_path:path ~duration_sec:1. ~chunks:[ s16le [ 9; 8 ] ] ~io ()
  in
  expect_fail (fun _ -> true) result;
  let ic = open_in_bin path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Alcotest.(check bool) "no candidate" true (leftovers path = []);
  Sys.remove path

let test_close_failure_preserves_dest () =
  let path = temp_wav "close" in
  let oc = open_out_bin path in
  output_string oc "OLD";
  close_out oc;
  let io = { unix_io with close = (fun _ -> failwith "close boom") } in
  let result =
    run ~output_path:path ~duration_sec:1. ~chunks:[ s16le [ 3; 4 ] ] ~io ()
  in
  expect_fail (fun _ -> true) result;
  let ic = open_in_bin path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Sys.remove path

let test_read_failure_preserves_dest () =
  let path = temp_wav "read" in
  let oc = open_out_bin path in
  output_string oc "OLD";
  close_out oc;
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Pie.Commands.Record.run ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~output_path:path ~duration_sec:1.
      ~acquire:(fun k ->
        k
          {
            Pie.Commands.Record.format = spec 16000;
            audio =
              Eta_stream.Stream.fail
                (`Pulse_error (Pie.Pulse.Client.Protocol_error "read boom"));
          })
      ()
    |> Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
  in
  expect_fail (contains "read boom") result;
  let ic = open_in_bin path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Sys.remove path

let test_completed_one_wav () =
  let path = temp_wav "ok" in
  (try Sys.remove path with _ -> ());
  match
    run ~output_path:path ~duration_sec:1. ~chunks:[ s16le [ 5; 6 ] ] ()
  with
  | Eta.Exit.Ok () ->
      Alcotest.(check bool) "published" true (Sys.file_exists path);
      Alcotest.(check string)
        "candidate gone" ""
        (String.concat "," (leftovers path));
      Alcotest.(check int) "rate" 16000 (wav_rate path);
      Alcotest.(check int) "byte rate" 32000 (wav_byte_rate path);
      Sys.remove path
  | Eta.Exit.Error c -> Alcotest.failf "complete: %s" (cause_text c)

let test_riff_duration_before_acquire () = test_duration_before_acquire 200_000.

let test_collision_leaves_other_candidate () =
  let path = temp_wav "coll" in
  (try Sys.remove path with _ -> ());
  let other =
    Filename.concat (Filename.dirname path)
      (Printf.sprintf ".%s.pie-record.%d.0" (Filename.basename path)
         (Unix.getpid ()))
  in
  let oc = open_out_bin other in
  output_string oc "OTHER";
  close_out oc;
  (match
     run ~output_path:path ~duration_sec:1. ~chunks:[ s16le [ 1; 2 ] ] ()
   with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error c -> Alcotest.failf "coll: %s" (cause_text c));
  let ic = open_in_bin other in
  let kept = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "other candidate untouched" "OTHER" kept;
  Sys.remove other;
  Sys.remove path

let test_rename_fail_does_not_close_sentinel () =
  let path = temp_wav "rename" in
  let oc = open_out_bin path in
  output_string oc "OLD";
  close_out oc;
  let sentinel_path = temp_wav "sentinel" in
  let sentinel =
    Unix.openfile sentinel_path [ Unix.O_RDWR; Unix.O_CREAT ] 0o600
  in
  let closed_after = ref false in
  let io =
    {
      unix_io with
      close =
        (fun fd ->
          unix_io.close fd;
          closed_after := true);
      rename = (fun _ _ -> failwith "rename boom");
    }
  in
  let result =
    run ~output_path:path ~duration_sec:1. ~chunks:[ s16le [ 7; 8 ] ] ~io ()
  in
  expect_fail (contains "rename") result;
  (* sentinel must still be a live fd *)
  let wrote = Unix.write_substring sentinel "X" 0 1 in
  Alcotest.(check int) "sentinel still writable" 1 wrote;
  Unix.close sentinel;
  (try Sys.remove sentinel_path with _ -> ());
  let ic = open_in_bin path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Alcotest.(check bool) "no leftovers" true (leftovers path = []);
  Sys.remove path;
  ignore closed_after

let test_capture_finalizer_preserves_dest () =
  let path = temp_wav "fin" in
  let oc = open_out_bin path in
  output_string oc "OLD";
  close_out oc;
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Pie.Commands.Record.run ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~output_path:path ~duration_sec:1.
      ~acquire:(fun k ->
        let* () = acquire_chunks 16000 [ s16le [ 1; 2 ] ] k in
        Eta.Effect.fail (`Stt_error "finalizer boom"))
      ()
    |> Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
  in
  expect_fail (contains "finalizer boom") result;
  let ic = open_in_bin path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Alcotest.(check bool) "no leftovers" true (leftovers path = []);
  Sys.remove path

let test_cancel_preserves_dest () =
  let path = temp_wav "cancel" in
  let oc = open_out_bin path in
  output_string oc "OLD";
  close_out oc;
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let record =
      Pie.Commands.Record.run ~sw ~net:(Eio.Stdenv.net env)
        ~clock:(Eio.Stdenv.clock env) ~output_path:path ~duration_sec:30.
        ~acquire:(fun k ->
          k
            {
              Pie.Commands.Record.format = spec 16000;
              audio = Eta_stream.Stream.from_effect Eta.Effect.never;
            })
        ()
      |> Eta.Effect.map (fun () -> `Body)
    in
    Eta.Effect.race [ record; Eta.Effect.pure `Cancel ]
    |> Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
  in
  (match result with
  | Eta.Exit.Ok `Cancel -> ()
  | Eta.Exit.Ok `Body -> Alcotest.fail "expected cancel"
  | Eta.Exit.Error c -> Alcotest.failf "cancel: %s" (cause_text c));
  let ic = open_in_bin path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Alcotest.(check bool) "no leftovers" true (leftovers path = []);
  Sys.remove path

let test_long_stream_size () =
  let path = temp_wav "long" in
  (try Sys.remove path with _ -> ());
  let chunks = List.init 1000 (fun i -> s16le [ i land 0x7fff ]) in
  (match run ~output_path:path ~duration_sec:1. ~chunks () with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error c -> Alcotest.failf "long: %s" (cause_text c));
  Alcotest.(check int) "data bytes" 2000 (String.length (wav_data path));
  Sys.remove path

let test_flush_failure_preserves_dest () =
  let path = temp_wav "flush" in
  let oc = open_out_bin path in
  output_string oc "OLD";
  close_out oc;
  let io = { unix_io with fsync = (fun _ -> failwith "flush boom") } in
  let result =
    run ~output_path:path ~duration_sec:1. ~chunks:[ s16le [ 1; 2 ] ] ~io ()
  in
  expect_fail (contains "flush") result;
  let ic = open_in_bin path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Sys.remove path

let test_placeholder_write_failure () =
  let path = temp_wav "ph" in
  (try Sys.remove path with _ -> ());
  let acquired = Atomic.make false in
  let writes = Atomic.make 0 in
  let io =
    {
      unix_io with
      write =
        (fun _ _ ->
          Atomic.incr writes;
          failwith "placeholder boom");
    }
  in
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Pie.Commands.Record.run ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~output_path:path ~duration_sec:1.
      ~acquire:(fun k ->
        Atomic.set acquired true;
        acquire_chunks 16000 [ s16le [ 1; 2 ] ] k)
      ~io ()
    |> Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
  in
  Alcotest.(check bool) "acquired" true (Atomic.get acquired);
  Alcotest.(check bool) "no dest" false (Sys.file_exists path);
  Alcotest.(check bool) "no leftovers" true (leftovers path = []);
  expect_fail (fun _ -> true) result

let rec cause_has_finalizer = function
  | Eta.Cause.Finalizer _ | Eta.Cause.Suppressed _ -> true
  | Eta.Cause.Sequential cs | Eta.Cause.Concurrent cs ->
      List.exists cause_has_finalizer cs
  | _ -> false

let test_dual_close_unlink_failure () =
  let path = temp_wav "dual" in
  let oc = open_out_bin path in
  output_string oc "OLD";
  close_out oc;
  let unlinked = Atomic.make false in
  let io =
    {
      unix_io with
      close = (fun _ -> failwith "close boom");
      unlink =
        (fun p ->
          Atomic.set unlinked true;
          failwith "unlink boom");
    }
  in
  let result =
    run ~output_path:path ~duration_sec:1. ~chunks:[ s16le [ 1; 2 ] ] ~io ()
  in
  (match result with
  | Eta.Exit.Ok () -> Alcotest.fail "expected dual failure"
  | Eta.Exit.Error cause ->
      if not (contains "close boom" cause) then
        Alcotest.failf "missing close failure: %s" (cause_text cause);
      if not (cause_has_finalizer cause) then
        Alcotest.failf "missing unlink finalizer: %s" (cause_text cause);
      Alcotest.(check bool) "unlink attempted" true (Atomic.get unlinked));
  let ic = open_in_bin path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Sys.remove path

let test_defer_two_runs () =
  let a = temp_wav "d1" in
  let b = temp_wav "d2" in
  (try Sys.remove a with _ -> ());
  (try Sys.remove b with _ -> ());
  (match run ~output_path:a ~duration_sec:1. ~chunks:[ s16le [ 1; 2 ] ] () with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error c -> Alcotest.failf "d1: %s" (cause_text c));
  (match run ~output_path:b ~duration_sec:1. ~chunks:[ s16le [ 3; 4 ] ] () with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error c -> Alcotest.failf "d2: %s" (cause_text c));
  Alcotest.(check bool) "a published" true (Sys.file_exists a);
  Alcotest.(check bool) "b published" true (Sys.file_exists b);
  Sys.remove a;
  Sys.remove b

let test_close_once_construction_does_not_clear_claim () =
  let cand =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf ".pie-closeonce-%d" (Unix.getpid ()))
  in
  (try Sys.remove cand with _ -> ());
  let fd = unix_io.open_excl cand in
  let state : Pie.Commands.Record.candidate =
    { path = cand; fd; close_pending = true; data_length = 0; rate = 16000 }
  in
  let _constructed = Pie.Commands.Record.close_once unix_io state in
  Alcotest.(check bool) "construction leaves pending" true state.close_pending;
  let owned = ref (Some state) in
  let published = Atomic.make false in
  (match
     Eio_main.run @@ fun env ->
     Eio.Switch.run @@ fun sw ->
     Pie.Commands.Record.cleanup unix_io published owned
     |> Eta_eio.run_once ~sw ~clock:(Eio.Stdenv.clock env)
   with
  | Eta.Exit.Ok () -> ()
  | Eta.Exit.Error c -> Alcotest.failf "cleanup: %s" (cause_text c));
  Alcotest.(check bool) "cleanup closed" false state.close_pending;
  Alcotest.(check bool) "candidate unlinked" false (Sys.file_exists cand)

let () =
  Alcotest.run "diagnostic_recording"
    [
      ( "record",
        [
          Alcotest.test_case "partitioned pcm matches whole buffer" `Quick
            test_partition_identical;
          Alcotest.test_case "wav header uses negotiated rate" `Quick
            test_header_negotiated_rate;
          Alcotest.test_case "invalid duration fails before acquire" `Quick
            test_invalid_durations;
          Alcotest.test_case "write failure keeps previous output" `Quick
            test_write_failure_preserves_dest;
          Alcotest.test_case "close failure keeps previous output" `Quick
            test_close_failure_preserves_dest;
          Alcotest.test_case "read failure keeps previous output" `Quick
            test_read_failure_preserves_dest;
          Alcotest.test_case "one completed wav publication" `Quick
            test_completed_one_wav;
          Alcotest.test_case "riff overflow duration fails before acquire"
            `Quick test_riff_duration_before_acquire;
          Alcotest.test_case "unique candidate leaves colliding file" `Quick
            test_collision_leaves_other_candidate;
          Alcotest.test_case "rename failure does not close sentinel" `Quick
            test_rename_fail_does_not_close_sentinel;
          Alcotest.test_case "capture finalizer keeps previous output" `Quick
            test_capture_finalizer_preserves_dest;
          Alcotest.test_case "cancel keeps previous output" `Quick
            test_cancel_preserves_dest;
          Alcotest.test_case "long stream writes expected bytes" `Quick
            test_long_stream_size;
          Alcotest.test_case "flush failure keeps previous output" `Quick
            test_flush_failure_preserves_dest;
          Alcotest.test_case "placeholder write failure cleans candidate" `Quick
            test_placeholder_write_failure;
          Alcotest.test_case "close and unlink failures both reported" `Quick
            test_dual_close_unlink_failure;
          Alcotest.test_case "defer isolates two blueprint runs" `Quick
            test_defer_two_runs;
          Alcotest.test_case "close_once construction does not clear claim"
            `Quick test_close_once_construction_does_not_clear_claim;
        ] );
    ]
