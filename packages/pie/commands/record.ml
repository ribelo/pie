let ( let* ) = Result.bind

(* Audio.Pcm.build_pcm_wav_header uses Int32.of_int bit patterns.
   On 64-bit that encodes RIFF uint32 values 0..0xFFFFFFFF. *)
let max_u32 = 0xFFFF_FFFF
let max_riff_data_length = max_u32 - 36

type run_error = [ `Pulse_error of Pulse.Client.error | `Stt_error of string ]

type capture = {
  format : Pulse.Defs.sample_spec;
  audio : unit -> (bytes option, [ `Pulse_error of Pulse.Client.error ]) result;
}

type file_io = {
  open_excl : string -> Unix.file_descr;
  write : Unix.file_descr -> bytes -> unit;
  seek_set : Unix.file_descr -> int -> unit;
  fsync : Unix.file_descr -> unit;
  close : Unix.file_descr -> unit;
  rename : string -> string -> unit;
  unlink : string -> unit;
}

type candidate = {
  path : string;
  fd : Unix.file_descr;
  mutable close_pending : bool;
  mutable data_length : int;
  rate : int;
}

let unix_io ~caps : file_io =
  {
    open_excl =
      (fun path ->
        Ox_caps.Cap_unix.openfile ~caps path
          [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ]
          0o600);
    write =
      (fun fd bytes ->
        let rec loop off =
          if off < Bytes.length bytes then
            let n = Unix.write fd bytes off (Bytes.length bytes - off) in
            if n = 0 then failwith "record write returned 0" else loop (off + n)
        in
        loop 0);
    seek_set = (fun fd off -> ignore (Unix.lseek fd off Unix.SEEK_SET));
    fsync = Unix.fsync;
    close = Unix.close;
    rename = (fun src dst -> Ox_caps.Cap_unix.rename ~caps src dst);
    unlink =
      (fun path ->
        try Ox_caps.Cap_unix.unlink ~caps path
        with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
  }

let record_error message = `Stt_error ("record: " ^ message)
let widen_pulse (`Pulse_error error) : run_error = `Pulse_error error

let validate_duration duration_sec =
  if not (Float.is_finite duration_sec) then
    Error (record_error "duration must be a finite number of seconds")
  else if duration_sec <= 0. then
    Error (record_error "duration must be positive")
  else
    let ms_f = duration_sec *. 1000. in
    if ms_f > float_of_int max_int then
      Error (record_error "duration exceeds timer representability")
    else
      let ms = int_of_float ms_f in
      if ms <= 0 then Error (record_error "duration must be positive")
      else Ok ms

let fits_u32 n = n >= 0 && n <= max_u32

let validate_spec (spec : Pulse.Defs.sample_spec) =
  if spec.format <> Pulse.Defs.Pa_sample_format.s16le then
    Error
      (record_error "negotiated Pulse format is not signed 16-bit little-endian")
  else if spec.channels <> 1 then
    Error (record_error "negotiated Pulse channel count is not mono")
  else if spec.rate <= 0 then
    Error (record_error "negotiated Pulse sample rate must be positive")
  else if not (fits_u32 spec.rate) then
    Error (record_error "sample rate exceeds WAV uint32 field")
  else if Int64.mul (Int64.of_int spec.rate) 2L > Int64.of_int max_u32 then
    Error (record_error "WAV byte-rate field exceeds uint32")
  else Ok ()

let validate_projected ~duration_sec ~rate =
  let projected = duration_sec *. float_of_int rate *. 2. in
  if
    (not (Float.is_finite projected))
    || projected > float_of_int max_riff_data_length
  then Error (record_error "duration is not representable in a RIFF WAV file")
  else Ok ()

let io_error label exn = record_error (label ^ ": " ^ Printexc.to_string exn)

let run_io label f =
  try Ok (f ()) with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn -> Error (io_error label exn)

let max_candidate_attempts = 1024

let open_unique_candidate io output_path =
  let dir = Filename.dirname output_path in
  let base = Filename.basename output_path in
  let rec loop n =
    if n >= max_candidate_attempts then
      Error (record_error "could not allocate an exclusive candidate file")
    else
      let name =
        Printf.sprintf ".%s.pie-record.%d.%d" base (Unix.getpid ()) n
      in
      let path = Filename.concat dir name in
      try Ok (path, io.open_excl path) with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (n + 1)
      | exn -> Error (io_error "open candidate" exn)
  in
  loop 0

exception Recording_io of string

let close_once io (state : candidate) =
  run_io "close candidate" (fun () ->
      if state.close_pending then (
        state.close_pending <- false;
        io.close state.fd))

let cleanup io published owned =
  let close_result =
    match !owned with Some state -> close_once io state | None -> Ok ()
  in
  let unlink_result =
    match !owned with
    | Some state when not (Atomic.get published) ->
        run_io "cleanup unlink" (fun () -> io.unlink state.path)
    | _ -> Ok ()
  in
  match (close_result, unlink_result) with
  | Ok (), Ok () -> Ok ()
  | Error error, Ok () | Ok (), Error error -> Error (error :> run_error)
  | Error (`Stt_error a), Error (`Stt_error b) ->
      Error (`Stt_error (a ^ "; " ^ b))

let claim_candidate io output_path ~rate owned =
  let* path, fd = open_unique_candidate io output_path in
  let state = { path; fd; close_pending = true; data_length = 0; rate } in
  owned := Some state;
  Ok state

let capture_bounded ~clock ~io ~state ~duration_ms capture =
  let rec read_audio () =
    match capture.audio () with
    | Error error -> Error (widen_pulse error)
    | Ok None -> Ok ()
    | Ok (Some chunk) ->
        let next = state.data_length + Bytes.length chunk in
        if next > max_riff_data_length then
          Error (record_error "captured audio exceeds RIFF WAV size")
        else
          let* () = run_io "write chunk" (fun () -> io.write state.fd chunk) in
          state.data_length <- next;
          read_audio ()
  in
  Eio.Fiber.first
    (fun () ->
      Eio.Time.sleep clock (float_of_int duration_ms /. 1000.);
      Ok ())
    read_audio

let finalize_header io (state : candidate) =
  if state.data_length land 1 <> 0 then
    Error (record_error "captured audio is not 16-bit sample aligned")
  else if state.data_length > max_riff_data_length then
    Error (record_error "captured audio exceeds RIFF WAV size")
  else
    let header =
      Audio.Pcm.build_pcm_wav_header ~data_length:state.data_length
        ~sample_rate:state.rate
    in
    let* () = run_io "seek header" (fun () -> io.seek_set state.fd 0) in
    let* () = run_io "write header" (fun () -> io.write state.fd header) in
    run_io "flush" (fun () -> io.fsync state.fd)

let default_acquire ~caps ~net ~clock ~logger k =
  Pulse.Client.with_client ~caps ~net ~clock ~logger @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun info audio ->
  k { format = info.sample_spec; audio }

let run ~caps ~sw:_ ~net ~clock ~logger ~output_path ~duration_sec
    ?(planned_rate = 16000) ?acquire ?io () =
  let io = Option.value io ~default:(unix_io ~caps) in
  let* duration_ms = validate_duration duration_sec in
  let* () = validate_projected ~duration_sec ~rate:planned_rate in
  let acquire =
    Option.value acquire ~default:(default_acquire ~caps ~net ~clock ~logger)
  in
  let owned = ref None in
  let published = Atomic.make false in
  (* Cleanup runs on success, typed failure, defect and cancellation. *)
  let cleanup_pending = ref true in
  Fun.protect
    ~finally:(fun () ->
      if !cleanup_pending then ignore (cleanup io published owned))
    (fun () ->
      let work : (unit, run_error) result =
        let* () =
          acquire (fun capture ->
              let* () = validate_spec capture.format in
              let* () =
                validate_projected ~duration_sec ~rate:capture.format.rate
              in
              let* state =
                claim_candidate io output_path ~rate:capture.format.rate owned
              in
              let* () =
                run_io "candidate header" (fun () ->
                    io.write state.fd (Bytes.make 44 '\000'))
              in
              capture_bounded ~clock ~io ~state ~duration_ms capture)
        in
        match !owned with
        | None -> Error (record_error "capture produced no candidate")
        | Some state ->
            let* () = finalize_header io state in
            let* () = close_once io state in
            let* () =
              run_io "publish" (fun () ->
                  io.rename state.path output_path;
                  Atomic.set published true)
            in
            Ox_caps.Cap_stdlib.printf ~caps "Recorded %d bytes → %s\n%!"
              state.data_length output_path;
            Ok ()
      in
      let result = work in
      let released = cleanup io published owned in
      cleanup_pending := false;
      match (result, released) with
      | Ok (), Ok () -> Ok ()
      | Error error, Ok () -> Error error
      | Ok (), Error error -> Error error
      | Error (`Stt_error a), Error (`Stt_error b) ->
          Error (`Stt_error (a ^ "; " ^ b))
      | Error (`Pulse_error error), Error (`Stt_error cleanup) ->
          Error
            (record_error (Pulse.Client.error_message error ^ "; " ^ cleanup))
      | Error error, Error _ -> Error error)
