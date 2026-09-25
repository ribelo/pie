open Eta.Syntax

(* Audio.Pcm.build_pcm_wav_header uses Int32.of_int bit patterns.
   On 64-bit that encodes RIFF uint32 values 0..0xFFFFFFFF. *)
let max_u32 = 0xFFFF_FFFF
let max_riff_data_length = max_u32 - 36

type run_error = [ `Pulse_error of Pulse.Client.error | `Stt_error of string ]

type capture = {
  format : Pulse.Defs.sample_spec;
  audio : (bytes, [ `Pulse_error of Pulse.Client.error ]) Eta_stream.Stream.t;
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

let unix_io : file_io =
  {
    open_excl =
      (fun path ->
        Unix.openfile path
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
    rename = (fun src dst -> Unix.rename src dst);
    unlink =
      (fun path ->
        try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
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
  Eta.Effect.sync (fun () ->
      try Ok (f ()) with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> Error (io_error label exn))
  |> Eta.Effect.bind Eta.Effect.from_result

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
  Eta.Effect.defer (fun () ->
      let close_eff =
        match !owned with
        | Some st -> close_once io st
        | None -> Eta.Effect.unit
      in
      let unlink_eff =
        match !owned with
        | Some st when not (Atomic.get published) ->
            run_io "cleanup unlink" (fun () -> io.unlink st.path)
        | _ -> Eta.Effect.unit
      in
      close_eff |> Eta.Effect.finally unlink_eff)

let claim_candidate io output_path ~rate owned =
  Eta.Effect.sync (fun () ->
      try
        match open_unique_candidate io output_path with
        | Error e -> Error e
        | Ok (path, fd) ->
            let state =
              { path; fd; close_pending = true; data_length = 0; rate }
            in
            owned := Some state;
            Ok state
      with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | exn -> Error (io_error "open candidate" exn))
  |> Eta.Effect.bind Eta.Effect.from_result

let capture_bounded ~io ~state ~duration_ms capture =
  let write_chunk chunk =
    Eta.Effect.sync (fun () ->
        try
          let next = state.data_length + Bytes.length chunk in
          if next > max_riff_data_length then
            raise (Recording_io "captured audio exceeds RIFF WAV size");
          io.write state.fd chunk;
          state.data_length <- next
        with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | Recording_io _ as exn -> raise exn
        | exn -> raise (Recording_io (Printexc.to_string exn)))
  in
  let timer =
    Eta.Effect.delay
      (Eta.Duration.ms duration_ms)
      (Eta.Effect.fail `Timer_elapsed)
  in
  let stream =
    Eta_stream.run_for_each write_chunk capture.audio
    |> Eta.Effect.map_error (fun err -> `Stream_error (widen_pulse err))
  in
  Eta.Effect.with_background timer (fun () -> stream)
  |> Ptt_loop.recover_single_failure (function
    | `Timer_elapsed -> Some Eta.Effect.unit
    | `Stream_error _ -> None)
  |> Eta.Effect.map_error (function
    | `Timer_elapsed -> record_error "uncatchable timer-elapsed compound"
    | `Stream_error err -> err)

let finalize_header io (state : candidate) =
  if state.data_length land 1 <> 0 then
    Eta.Effect.fail (record_error "captured audio is not 16-bit sample aligned")
  else if state.data_length > max_riff_data_length then
    Eta.Effect.fail (record_error "captured audio exceeds RIFF WAV size")
  else
    let header =
      Audio.Pcm.build_pcm_wav_header ~data_length:state.data_length
        ~sample_rate:state.rate
    in
    let* () = run_io "seek header" (fun () -> io.seek_set state.fd 0) in
    let* () = run_io "write header" (fun () -> io.write state.fd header) in
    run_io "flush" (fun () -> io.fsync state.fd)

let default_acquire ~net k =
  Pulse.Client.with_client ~net @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun info audio ->
  k { format = info.sample_spec; audio }

let run ~sw:_ ~net ~clock:_ ~output_path ~duration_sec ?(planned_rate = 16000)
    ?acquire ?(io = unix_io) () =
  match validate_duration duration_sec with
  | Error error -> Eta.Effect.fail error
  | Ok duration_ms -> (
      match validate_projected ~duration_sec ~rate:planned_rate with
      | Error error -> Eta.Effect.fail error
      | Ok () ->
          let acquire = Option.value acquire ~default:(default_acquire ~net) in
          Eta.Effect.defer (fun () ->
              let owned = ref None in
              let published = Atomic.make false in
              let work =
                let* () =
                  acquire (fun capture ->
                      let* () =
                        Eta.Effect.from_result (validate_spec capture.format)
                      in
                      let* () =
                        Eta.Effect.from_result
                          (validate_projected ~duration_sec
                             ~rate:capture.format.rate)
                      in
                      let* state =
                        claim_candidate io output_path ~rate:capture.format.rate
                          owned
                      in
                      let* () =
                        run_io "candidate header" (fun () ->
                            io.write state.fd (Bytes.make 44 '\000'))
                      in
                      capture_bounded ~io ~state ~duration_ms capture)
                in
                match !owned with
                | None ->
                    Eta.Effect.fail
                      (record_error "capture produced no candidate")
                | Some state ->
                    let* () = finalize_header io state in
                    let* () = close_once io state in
                    let* () =
                      run_io "publish" (fun () ->
                          io.rename state.path output_path;
                          Atomic.set published true)
                    in
                    Eta.Effect.sync (fun () ->
                        Printf.printf "Recorded %d bytes → %s\n%!"
                          state.data_length output_path)
              in
              work |> Eta.Effect.finally (cleanup io published owned)))
