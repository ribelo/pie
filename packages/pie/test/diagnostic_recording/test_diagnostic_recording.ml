(* ^pcaps-n1rg *)
let ( let* ) = Result.bind

let logger env =
  Ox_observability.Logger.create ~service:"pie-test"
    ~clock:(Eio.Stdenv.clock env)
    ~mono_clock:(Eio.Stdenv.mono_clock env)
    ~reporter:(Ox_observability.Logger.Reporter.records (fun _ -> ()))
    ()

let finite_pull chunks =
  let cursor = ref (List.to_seq chunks) in
  fun () ->
    match !cursor () with
    | Seq.Nil -> Ok None
    | Seq.Cons (head, tail) ->
        cursor := tail;
        Ok (Some head)

let s16le samples =
  let bytes = Bytes.create (List.length samples * 2) in
  List.iteri (fun i sample -> Bytes.set_int16_le bytes (i * 2) sample) samples;
  bytes

let spec rate : Pie.Pulse.Defs.sample_spec =
  { format = Pie.Pulse.Defs.Pa_sample_format.s16le; channels = 1; rate }

let acquire_chunks rate chunks k =
  k { Pie.Commands.Record.format = spec rate; audio = finite_pull chunks }

let unix_io ~(caps : < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >) =
  Pie.Commands.Record.unix_io ~caps

let run
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) ~output_path ~duration_sec ~chunks ?(rate = 16000)
    ?(io = unix_io ~caps) () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Pie.Commands.Record.run ~caps ~sw ~net:(Eio.Stdenv.net env)
    ~clock:(Eio.Stdenv.clock env) ~logger:(logger env) ~output_path
    ~duration_sec
    ~acquire:(acquire_chunks rate chunks)
    ~io ()

let leftovers ~(caps : < Ox_caps.Cap.fs_read ; .. >) output_path =
  let dir = Filename.dirname output_path in
  let base = Filename.basename output_path in
  let prefix = "." ^ base ^ ".pie-record" in
  Ox_caps.Cap_sys.readdir ~caps dir
  |> Array.to_list
  |> List.filter (fun name -> String.starts_with ~prefix name)

let wav_byte_rate ~(caps : < Ox_caps.Cap.fs_read ; .. >) path =
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let buf = really_input_string ic 44 in
      Bytes.get_int32_le (Bytes.of_string buf) 28 |> Int32.to_int)

let wav_rate ~(caps : < Ox_caps.Cap.fs_read ; .. >) path =
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let buf = really_input_string ic 44 in
      Bytes.get_int32_le (Bytes.of_string buf) 24 |> Int32.to_int)

let wav_data ~(caps : < Ox_caps.Cap.fs_read ; .. >) path =
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      seek_in ic 44;
      really_input_string ic (in_channel_length ic - 44))

let cause_text = function
  | `Stt_error message -> message
  | `Pulse_error error -> Pie.Pulse.Client.error_message error

let expect_fail pred result =
  match result with
  | Ok () -> Alcotest.fail "expected record failure"
  | Error cause ->
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

let test_partition_identical
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let samples = [ 1000; 2000; 3000; 4000 ] in
  let whole = s16le samples in
  let left = s16le [ 1000; 2000 ] in
  let right = s16le [ 3000; 4000 ] in
  let a = temp_wav "whole" in
  let b = temp_wav "parts" in
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps a)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps b)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  (match run ~caps ~output_path:a ~duration_sec:1. ~chunks:[ whole ] () with
  | Ok () -> ()
  | Error c -> Alcotest.failf "whole: %s" (cause_text c));
  (match
     run ~caps ~output_path:b ~duration_sec:1. ~chunks:[ left; right ] ()
   with
  | Ok () -> ()
  | Error c -> Alcotest.failf "parts: %s" (cause_text c));
  Alcotest.(check string) "faithful pcm" (wav_data ~caps a) (wav_data ~caps b);
  Ox_caps.Cap_sys.remove ~caps a;
  Ox_caps.Cap_sys.remove ~caps b

let test_header_negotiated_rate
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "rate" in
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps path)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  (match
     run ~caps ~output_path:path ~duration_sec:1. ~rate:48000
       ~chunks:[ s16le [ 1; 2 ] ]
       ()
   with
  | Ok () -> ()
  | Error c -> Alcotest.failf "rate: %s" (cause_text c));
  Alcotest.(check int) "wav sample rate" 48000 (wav_rate ~caps path);
  Alcotest.(check int) "wav byte rate" (48000 * 2) (wav_byte_rate ~caps path);
  Ox_caps.Cap_sys.remove ~caps path

let test_duration_before_acquire
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) duration_sec =
  let acquired = Atomic.make false in
  let path = temp_wav "dur" in
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps path)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  let dest_existed = Sys.file_exists path in
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Pie.Commands.Record.run ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(logger env) ~output_path:path
      ~duration_sec
      ~acquire:(fun k ->
        Atomic.set acquired true;
        acquire_chunks 16000 [ s16le [ 1 ] ] k)
      ()
  in
  Alcotest.(check bool) "did not acquire" false (Atomic.get acquired);
  Alcotest.(check bool) "output untouched" dest_existed (Sys.file_exists path);
  expect_fail (contains "record:") result

let test_invalid_durations
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  List.iter
    (test_duration_before_acquire ~caps)
    [ nan; infinity; neg_infinity; 0.; -1.; float_of_int max_int ]

let test_write_failure_preserves_dest
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "write" in
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  output_string oc "OLD";
  close_out oc;
  let io =
    { (unix_io ~caps) with write = (fun _ _ -> failwith "write boom") }
  in
  let result =
    run ~caps ~output_path:path ~duration_sec:1.
      ~chunks:[ s16le [ 9; 8 ] ]
      ~io ()
  in
  expect_fail (fun _ -> true) result;
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Alcotest.(check bool) "no candidate" true (leftovers ~caps path = []);
  Ox_caps.Cap_sys.remove ~caps path

let test_close_failure_preserves_dest
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "close" in
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  output_string oc "OLD";
  close_out oc;
  let io = { (unix_io ~caps) with close = (fun _ -> failwith "close boom") } in
  let result =
    run ~caps ~output_path:path ~duration_sec:1.
      ~chunks:[ s16le [ 3; 4 ] ]
      ~io ()
  in
  expect_fail (fun _ -> true) result;
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Ox_caps.Cap_sys.remove ~caps path

let test_read_failure_preserves_dest
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "read" in
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  output_string oc "OLD";
  close_out oc;
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Pie.Commands.Record.run ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(logger env) ~output_path:path
      ~duration_sec:1.
      ~acquire:(fun k ->
        k
          {
            Pie.Commands.Record.format = spec 16000;
            audio =
              (fun () ->
                Error
                  (`Pulse_error (Pie.Pulse.Client.Protocol_error "read boom")));
          })
      ()
  in
  expect_fail (contains "read boom") result;
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Ox_caps.Cap_sys.remove ~caps path

let test_completed_one_wav
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "ok" in
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps path)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  match
    run ~caps ~output_path:path ~duration_sec:1. ~chunks:[ s16le [ 5; 6 ] ] ()
  with
  | Ok () ->
      Alcotest.(check bool) "published" true (Sys.file_exists path);
      Alcotest.(check string)
        "candidate gone" ""
        (String.concat "," (leftovers ~caps path));
      Alcotest.(check int) "rate" 16000 (wav_rate ~caps path);
      Alcotest.(check int) "byte rate" 32000 (wav_byte_rate ~caps path);
      Ox_caps.Cap_sys.remove ~caps path
  | Error c -> Alcotest.failf "complete: %s" (cause_text c)

let test_riff_duration_before_acquire
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  test_duration_before_acquire ~caps 200_000.

let test_collision_leaves_other_candidate
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "coll" in
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps path)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  let other =
    Filename.concat (Filename.dirname path)
      (Printf.sprintf ".%s.pie-record.%d.0" (Filename.basename path)
         (Unix.getpid ()))
  in
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps other in
  output_string oc "OTHER";
  close_out oc;
  (match
     run ~caps ~output_path:path ~duration_sec:1. ~chunks:[ s16le [ 1; 2 ] ] ()
   with
  | Ok () -> ()
  | Error c -> Alcotest.failf "coll: %s" (cause_text c));
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps other in
  let kept = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "other candidate untouched" "OTHER" kept;
  Ox_caps.Cap_sys.remove ~caps other;
  Ox_caps.Cap_sys.remove ~caps path

let test_rename_fail_does_not_close_sentinel
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "rename" in
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  output_string oc "OLD";
  close_out oc;
  let sentinel_path = temp_wav "sentinel" in
  let sentinel =
    Ox_caps.Cap_unix.openfile ~caps sentinel_path
      [ Unix.O_RDWR; Unix.O_CREAT ]
      0o600
  in
  let closed_after = ref false in
  Fun.protect
    ~finally:(fun () -> Unix.close sentinel)
    (fun () ->
      let io =
        {
          (unix_io ~caps) with
          close =
            (fun fd ->
              (unix_io ~caps).close fd;
              closed_after := true);
          rename = (fun _ _ -> failwith "rename boom");
        }
      in
      let result =
        run ~caps ~output_path:path ~duration_sec:1.
          ~chunks:[ s16le [ 7; 8 ] ]
          ~io ()
      in
      expect_fail (contains "rename") result;
      (* sentinel must still be a live fd *)
      let wrote = Unix.write_substring sentinel "X" 0 1 in
      Alcotest.(check int) "sentinel still writable" 1 wrote);
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps sentinel_path)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Alcotest.(check bool) "no leftovers" true (leftovers ~caps path = []);
  Ox_caps.Cap_sys.remove ~caps path;
  ignore closed_after

let test_capture_finalizer_preserves_dest
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "fin" in
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  output_string oc "OLD";
  close_out oc;
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Pie.Commands.Record.run ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(logger env) ~output_path:path
      ~duration_sec:1.
      ~acquire:(fun k ->
        let* () = acquire_chunks 16000 [ s16le [ 1; 2 ] ] k in
        Error (`Stt_error "finalizer boom"))
      ()
  in
  expect_fail (contains "finalizer boom") result;
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Alcotest.(check bool) "no leftovers" true (leftovers ~caps path = []);
  Ox_caps.Cap_sys.remove ~caps path

let test_cancel_preserves_dest
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "cancel" in
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  output_string oc "OLD";
  close_out oc;
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let started, signal_started = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    Eio.Fiber.first
      (fun () ->
        let result =
          Pie.Commands.Record.run ~caps ~sw ~net:(Eio.Stdenv.net env)
            ~clock:(Eio.Stdenv.clock env) ~logger:(logger env) ~output_path:path
            ~duration_sec:30.
            ~acquire:(fun k ->
              k
                {
                  Pie.Commands.Record.format = spec 16000;
                  audio =
                    (fun () ->
                      Eio.Promise.resolve signal_started ();
                      Eio.Promise.await never);
                })
            ()
        in
        Result.map (fun () -> `Body) result)
      (fun () ->
        Eio.Promise.await started;
        Ok `Cancel)
  in
  (match result with
  | Ok `Cancel -> ()
  | Ok `Body -> Alcotest.fail "expected cancel"
  | Error c -> Alcotest.failf "cancel: %s" (cause_text c));
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Alcotest.(check bool) "no leftovers" true (leftovers ~caps path = []);
  Ox_caps.Cap_sys.remove ~caps path

let test_long_stream_size
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "long" in
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps path)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  let chunks = List.init 1000 (fun i -> s16le [ i land 0x7fff ]) in
  (match run ~caps ~output_path:path ~duration_sec:1. ~chunks () with
  | Ok () -> ()
  | Error c -> Alcotest.failf "long: %s" (cause_text c));
  Alcotest.(check int) "data bytes" 2000 (String.length (wav_data ~caps path));
  Ox_caps.Cap_sys.remove ~caps path

let test_flush_failure_preserves_dest
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "flush" in
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  output_string oc "OLD";
  close_out oc;
  let io = { (unix_io ~caps) with fsync = (fun _ -> failwith "flush boom") } in
  let result =
    run ~caps ~output_path:path ~duration_sec:1.
      ~chunks:[ s16le [ 1; 2 ] ]
      ~io ()
  in
  expect_fail (contains "flush") result;
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Ox_caps.Cap_sys.remove ~caps path

let test_placeholder_write_failure
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "ph" in
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps path)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  let acquired = Atomic.make false in
  let writes = Atomic.make 0 in
  let io =
    {
      (unix_io ~caps) with
      write =
        (fun _ _ ->
          Atomic.incr writes;
          failwith "placeholder boom");
    }
  in
  let result =
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    Pie.Commands.Record.run ~caps ~sw ~net:(Eio.Stdenv.net env)
      ~clock:(Eio.Stdenv.clock env) ~logger:(logger env) ~output_path:path
      ~duration_sec:1.
      ~acquire:(fun k ->
        Atomic.set acquired true;
        acquire_chunks 16000 [ s16le [ 1; 2 ] ] k)
      ~io ()
  in
  Alcotest.(check bool) "acquired" true (Atomic.get acquired);
  Alcotest.(check bool) "no dest" false (Sys.file_exists path);
  Alcotest.(check bool) "no leftovers" true (leftovers ~caps path = []);
  expect_fail (fun _ -> true) result

let test_dual_close_unlink_failure
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let path = temp_wav "dual" in
  let oc = Ox_caps.Cap_stdlib.open_out_bin ~caps path in
  output_string oc "OLD";
  close_out oc;
  let unlinked = Atomic.make false in
  let io =
    {
      (unix_io ~caps) with
      close = (fun _ -> failwith "close boom");
      unlink =
        (fun p ->
          Atomic.set unlinked true;
          failwith "unlink boom");
    }
  in
  let result =
    run ~caps ~output_path:path ~duration_sec:1.
      ~chunks:[ s16le [ 1; 2 ] ]
      ~io ()
  in
  (match result with
  | Ok () -> Alcotest.fail "expected dual failure"
  | Error cause ->
      if not (contains "close boom" cause) then
        Alcotest.failf "missing close failure: %s" (cause_text cause);
      if not (contains "unlink boom" cause) then
        Alcotest.failf "missing unlink cleanup: %s" (cause_text cause);
      Alcotest.(check bool) "unlink attempted" true (Atomic.get unlinked));
  let ic = Ox_caps.Cap_stdlib.open_in_bin ~caps path in
  let old = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Alcotest.(check string) "previous output kept" "OLD" old;
  Ox_caps.Cap_sys.remove ~caps path

let test_defer_two_runs
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) () =
  let a = temp_wav "d1" in
  let b = temp_wav "d2" in
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps a)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps b)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  (match
     run ~caps ~output_path:a ~duration_sec:1. ~chunks:[ s16le [ 1; 2 ] ] ()
   with
  | Ok () -> ()
  | Error c -> Alcotest.failf "d1: %s" (cause_text c));
  (match
     run ~caps ~output_path:b ~duration_sec:1. ~chunks:[ s16le [ 3; 4 ] ] ()
   with
  | Ok () -> ()
  | Error c -> Alcotest.failf "d2: %s" (cause_text c));
  Alcotest.(check bool) "a published" true (Sys.file_exists a);
  Alcotest.(check bool) "b published" true (Sys.file_exists b);
  Ox_caps.Cap_sys.remove ~caps a;
  Ox_caps.Cap_sys.remove ~caps b

let test_close_once_construction_does_not_clear_claim
    ~(caps : < Ox_caps.Cap.fs_read ; Ox_caps.Cap.fs_write ; .. >) () =
  let cand =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf ".pie-closeonce-%d" (Unix.getpid ()))
  in
  (* Preserve the exception as data before this test classifies its outcome. *)
  (match
     try Result.Ok (Ox_caps.Cap_sys.remove ~caps cand)
     with exn -> Result.Error exn
   with
  | Result.Ok value -> value
  | Result.Error _ -> ());
  let state =
    (unix_io ~caps).open_excl cand (fun fd ->
        ({
           path = cand;
           fd;
           close_pending = true;
           data_length = 0;
           rate = 16000;
         }
          : Pie.Commands.Record.candidate))
  in
  (match Pie.Commands.Record.close_once (unix_io ~caps) state with
  | Ok () -> ()
  | Error _ -> Alcotest.fail "close failed");
  Alcotest.(check bool) "direct close clears pending" false state.close_pending;
  let owned = ref (Some state) in
  let published = Atomic.make false in
  (match
     Eio_main.run @@ fun env ->
     Eio.Switch.run @@ fun sw ->
     Pie.Commands.Record.cleanup (unix_io ~caps) published owned
   with
  | Ok () -> ()
  | Error c -> Alcotest.failf "cleanup: %s" (cause_text c));
  Alcotest.(check bool) "cleanup closed" false state.close_pending;
  Alcotest.(check bool) "candidate unlinked" false (Sys.file_exists cand)

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stdout
       ; .. >) =
  Alcotest.run "diagnostic_recording"
    [
      ( "record",
        [
          Alcotest.test_case "partitioned pcm matches whole buffer" `Quick
            (test_partition_identical ~caps);
          Alcotest.test_case "wav header uses negotiated rate" `Quick
            (test_header_negotiated_rate ~caps);
          Alcotest.test_case "invalid duration fails before acquire" `Quick
            (test_invalid_durations ~caps);
          Alcotest.test_case "write failure keeps previous output" `Quick
            (test_write_failure_preserves_dest ~caps);
          Alcotest.test_case "close failure keeps previous output" `Quick
            (test_close_failure_preserves_dest ~caps);
          Alcotest.test_case "read failure keeps previous output" `Quick
            (test_read_failure_preserves_dest ~caps);
          Alcotest.test_case "one completed wav publication" `Quick
            (test_completed_one_wav ~caps);
          Alcotest.test_case "riff overflow duration fails before acquire"
            `Quick
            (test_riff_duration_before_acquire ~caps);
          Alcotest.test_case "unique candidate leaves colliding file" `Quick
            (test_collision_leaves_other_candidate ~caps);
          Alcotest.test_case "rename failure does not close sentinel" `Quick
            (test_rename_fail_does_not_close_sentinel ~caps);
          Alcotest.test_case "capture finalizer keeps previous output" `Quick
            (test_capture_finalizer_preserves_dest ~caps);
          Alcotest.test_case "cancel keeps previous output" `Quick
            (test_cancel_preserves_dest ~caps);
          Alcotest.test_case "long stream writes expected bytes" `Quick
            (test_long_stream_size ~caps);
          Alcotest.test_case "flush failure keeps previous output" `Quick
            (test_flush_failure_preserves_dest ~caps);
          Alcotest.test_case "placeholder write failure cleans candidate" `Quick
            (test_placeholder_write_failure ~caps);
          Alcotest.test_case "close and unlink failures both reported" `Quick
            (test_dual_close_unlink_failure ~caps);
          Alcotest.test_case "defer isolates two blueprint runs" `Quick
            (test_defer_two_runs ~caps);
          Alcotest.test_case "close_once construction does not clear claim"
            `Quick
            (test_close_once_construction_does_not_clear_claim ~caps);
        ] );
    ]

let () = main ~caps:(Ox_test.powerbox ())
