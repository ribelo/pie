type trigger = Press of string | Release of string
type captured_clip = { duration_ms : int; pcm_bytes : bytes }
type error = [ `Ptt_error of string ]

type handle = {
  offer : bytes -> (unit, error) Eta.Effect.t;
  finish : captured_clip -> (unit, error) Eta.Effect.t;
  cancel : (unit, error) Eta.Effect.t;
}

type press_result =
  | Handle of handle
  | Scoped of
      ((handle -> (unit, error) Eta.Effect.t) -> (unit, error) Eta.Effect.t)
  | Skip

type config = {
  recognize : Keyboard.Monitor.key_event -> trigger option;
  min_duration_ms : int;
  capture_thresholds : unit -> float * float;
  log_prefix : string -> string;
  source : string option;
  on_ready : (unit, error) Eta.Effect.t;
  on_press : string -> (press_result, error) Eta.Effect.t;
  on_release : string -> (unit, error) Eta.Effect.t;
  on_abort : (bool, error) Eta.Effect.t;
  on_dead_input : (unit, error) Eta.Effect.t;
}

open Eta.Syntax

let now_ms () = int_of_float (Unix.gettimeofday () *. 1000.0)

let timestamp () =
  let now = Unix.gettimeofday () in
  let tm = Unix.localtime now in
  let millis = int_of_float ((now -. floor now) *. 1000.0) in
  Printf.sprintf "%02d:%02d:%02d.%03d" tm.Unix.tm_hour tm.tm_min tm.tm_sec
    millis

let log_info message fields =
  let attrs =
    ("pie.timestamp", timestamp ())
    :: List.map (fun (key, value) -> (key, Yojson.Safe.to_string value)) fields
  in
  Eta_observability.log_info ~attrs message

let log_error message fields =
  let attrs =
    ("pie.timestamp", timestamp ())
    :: List.map (fun (key, value) -> (key, Yojson.Safe.to_string value)) fields
  in
  Eta_observability.log_error ~attrs message

let ptt_fields ?duration_ms ?held_duration_ms ?bytes ?rms ?peak config mode =
  let base =
    [
      ("ptt.mode", `String mode);
      ("ptt.log_prefix", `String (config.log_prefix mode));
    ]
  in
  let base =
    match config.source with
    | None -> base
    | Some source -> ("ptt.source", `String source) :: base
  in
  let base =
    match duration_ms with
    | None -> base
    | Some duration_ms -> ("ptt.duration_ms", `Int duration_ms) :: base
  in
  let base =
    match held_duration_ms with
    | None -> base
    | Some held_duration_ms ->
        ("ptt.held_duration_ms", `Int held_duration_ms) :: base
  in
  let base =
    match bytes with
    | None -> base
    | Some bytes -> ("ptt.bytes", `Int bytes) :: base
  in
  let base =
    match rms with None -> base | Some rms -> ("ptt.rms", `Float rms) :: base
  in
  let base =
    match peak with
    | None -> base
    | Some peak -> ("ptt.peak", `Float peak) :: base
  in
  List.rev base

let concat_chunks chunks =
  let chunks = List.rev chunks in
  let len =
    List.fold_left (fun acc chunk -> acc + Bytes.length chunk) 0 chunks
  in
  let out = Bytes.create len in
  let offset = ref 0 in
  List.iter
    (fun chunk ->
      Bytes.blit chunk 0 out !offset (Bytes.length chunk);
      offset := !offset + Bytes.length chunk)
    chunks;
  out

let held_duration_ms started_at capture_state =
  let end_at =
    match capture_state with
    | Ptt.Capture.PostRoll { release_at; _ } -> int_of_float release_at
    | Ptt.Capture.Capturing _ | Ptt.Capture.Idle -> now_ms ()
  in
  max 0 (end_at - started_at)

let capture_stats pcm_bytes =
  (Audio.Pcm.pcm_rms pcm_bytes, Audio.Pcm.pcm_peak pcm_bytes)

let capture_is_eligible ~min_duration_ms ~min_capture_rms ~min_capture_peak
    ~held_duration_ms ~pcm_bytes ~rms ~peak =
  held_duration_ms >= min_duration_ms
  && Bytes.length pcm_bytes > 0
  && (rms >= min_capture_rms || peak >= min_capture_peak)

let run ?control ~(keyboard : Keyboard.Monitor.t) ~audio (config : config) =
  let keyboard_queue = Keyboard.Monitor.subscribe keyboard in
  let input = Eio.Stream.create 1024 in
  let push_input item = Eta.Effect.sync (fun () -> Eio.Stream.add input item) in
  let take_input = Eta.Effect.sync (fun () -> Eio.Stream.take input) in
  let rec keyboard_loop () =
    let* event = Eta.Effect.sync (fun () -> Eio.Stream.take keyboard_queue) in
    let* () = push_input (`Event event) in
    keyboard_loop ()
  in
  let rec control_loop control =
    let* trigger = Eta.Effect.sync (fun () -> Eio.Stream.take control) in
    let* () = push_input (`Trigger trigger) in
    control_loop control
  in
  let recv_loop =
    Eta_stream.run_for_each (fun chunk -> push_input (`Chunk chunk)) audio
    |> Eta.Effect.bind_error (fun (`Pulse_error error) ->
        Eta.Effect.fail (`Ptt_error (Pulse.Client.error_message error)))
  in
  let input_loop =
    let keyboard_and_audio =
      Eta.Effect.par (keyboard_loop ()) recv_loop
      |> Eta.Effect.map (fun _ -> ())
    in
    match control with
    | None -> keyboard_and_audio
    | Some control ->
        Eta.Effect.par keyboard_and_audio (control_loop control)
        |> Eta.Effect.map (fun _ -> ())
  in
  let state_loop () =
    let offer_chunks handle chunks =
      let rec loop = function
        | [] -> Eta.Effect.unit
        | chunk :: rest ->
            let* () = handle.offer chunk in
            loop rest
      in
      loop (List.rev chunks)
    in
    let finish_capture mode handle started_at capture_state offered chunks =
      let duration_ms = now_ms () - started_at in
      let held_duration_ms = held_duration_ms started_at capture_state in
      let pcm_bytes = concat_chunks chunks in
      let rms, peak = capture_stats pcm_bytes in
      let min_capture_rms, min_capture_peak = config.capture_thresholds () in
      if
        not
          (capture_is_eligible ~min_duration_ms:config.min_duration_ms
             ~min_capture_rms ~min_capture_peak ~held_duration_ms ~pcm_bytes
             ~rms ~peak)
      then
        let* () =
          log_info "PTT capture ignored"
            (ptt_fields ~duration_ms ~held_duration_ms
               ~bytes:(Bytes.length pcm_bytes) ~rms ~peak config mode)
        in
        handle.cancel
      else
        let* () =
          if offered then Eta.Effect.unit else offer_chunks handle chunks
        in
        let* () =
          log_info "PTT capture stopped"
            (ptt_fields ~duration_ms ~held_duration_ms
               ~bytes:(Bytes.length pcm_bytes) ~rms ~peak config mode)
        in
        handle.finish { duration_ms; pcm_bytes }
    in
    let run_active mode started_at handle =
      let rec loop capture_state dead_input offered chunks =
        let* abort = config.on_abort in
        if abort then handle.cancel
        else
          match capture_state with
          | Ptt.Capture.PostRoll _ ->
              let remaining =
                Ptt.Capture.post_roll_remaining_ms capture_state
                  (float_of_int (now_ms ()))
              in
              if remaining <= 0.0 then
                finish_capture mode handle started_at capture_state offered
                  chunks
              else
                let timeout =
                  Eta.Effect.delay
                    (Eta.Duration.ms (int_of_float remaining))
                    Eta.Effect.unit
                  |> Eta.Effect.map (fun () -> `Timeout)
                in
                let* next = Eta.Effect.race [ timeout; take_input ] in
                handle_active_event capture_state dead_input offered chunks next
          | Ptt.Capture.Idle | Ptt.Capture.Capturing _ ->
              let* next = take_input in
              handle_active_event capture_state dead_input offered chunks next
      and handle_active_event capture_state dead_input offered chunks = function
        | `Timeout ->
            finish_capture mode handle started_at capture_state offered chunks
        | `Chunk chunk ->
            let active = Ptt.Capture.is_accepting_chunks capture_state in
            let dead_input = Ptt.Dead_input.sync dead_input active in
            if not active then loop capture_state dead_input offered chunks
            else
              let result = Ptt.Dead_input.process_chunk dead_input chunk in
              if result.dead then
                let* () = handle.cancel in
                let* () =
                  log_info "PTT capture cancelled: no microphone input"
                    (ptt_fields config mode)
                in
                config.on_dead_input
              else
                let chunks = chunk :: chunks in
                let should_start_offering =
                  if offered || not result.has_input then false
                  else
                    let pcm_bytes = concat_chunks chunks in
                    let held_duration_ms =
                      held_duration_ms started_at capture_state
                    in
                    let rms, peak = capture_stats pcm_bytes in
                    let min_capture_rms, min_capture_peak =
                      config.capture_thresholds ()
                    in
                    capture_is_eligible ~min_duration_ms:config.min_duration_ms
                      ~min_capture_rms ~min_capture_peak ~held_duration_ms
                      ~pcm_bytes ~rms ~peak
                in
                let* () =
                  if not result.has_input then Eta.Effect.unit
                  else if offered then handle.offer chunk
                  else if should_start_offering then offer_chunks handle chunks
                  else Eta.Effect.unit
                in
                let offered = offered || should_start_offering in
                loop capture_state result.detector offered chunks
        | `Event event -> (
            match config.recognize event with
            | Some (Release release_mode) when release_mode = mode -> (
                match capture_state with
                | Ptt.Capture.Capturing _ ->
                    let next =
                      Ptt.Capture.release capture_state
                        (float_of_int (now_ms ()))
                    in
                    let* () = config.on_release mode in
                    loop next dead_input offered chunks
                | _ -> loop capture_state dead_input offered chunks)
            | Some (Press press_mode) when press_mode = mode ->
                let next =
                  Ptt.Capture.start capture_state (float_of_int (now_ms ()))
                in
                let* () =
                  match capture_state with
                  | Ptt.Capture.PostRoll _ ->
                      log_info "Post-roll cancelled, continuing capture"
                        (ptt_fields config mode)
                  | _ -> Eta.Effect.unit
                in
                loop next dead_input offered chunks
            | _ -> loop capture_state dead_input offered chunks)
        | `Trigger (Release release_mode) when release_mode = mode -> (
            match capture_state with
            | Ptt.Capture.Capturing _ ->
                let next =
                  Ptt.Capture.release capture_state (float_of_int (now_ms ()))
                in
                let* () = config.on_release mode in
                loop next dead_input offered chunks
            | _ -> loop capture_state dead_input offered chunks)
        | `Trigger (Press press_mode) when press_mode = mode ->
            let next =
              Ptt.Capture.start capture_state (float_of_int (now_ms ()))
            in
            let* () =
              match capture_state with
              | Ptt.Capture.PostRoll _ ->
                  log_info "Post-roll cancelled, continuing capture"
                    (ptt_fields config mode)
              | _ -> Eta.Effect.unit
            in
            loop next dead_input offered chunks
        | `Trigger _ -> loop capture_state dead_input offered chunks
      in
      loop
        (Ptt.Capture.start Ptt.Capture.idle (float_of_int started_at))
        (Ptt.Dead_input.initial ())
        false []
    in
    let rec idle_loop () =
      let capture_failed mode message =
        log_error "PTT capture failed"
          (("ptt.error", `String message) :: ptt_fields config mode)
      in
      let run_capture mode handle run =
        run
        |> Eta.Effect.bind_error (fun (`Ptt_error message) ->
            let* () = handle.cancel |> Eta.Effect.ignore_errors in
            let* () = capture_failed mode message in
            Eta.Effect.unit)
      in
      let* next = take_input in
      match next with
      | `Timeout -> idle_loop ()
      | `Chunk _ -> idle_loop ()
      | `Trigger (Press mode) -> (
          let* pressed = config.on_press mode in
          match pressed with
          | Skip -> idle_loop ()
          | Handle handle ->
              let started_at = now_ms () in
              let* () =
                log_info "PTT capture started" (ptt_fields config mode)
              in
              let* () =
                run_capture mode handle (run_active mode started_at handle)
              in
              idle_loop ()
          | Scoped with_handle ->
              let started_at = now_ms () in
              let* () =
                log_info "PTT capture started" (ptt_fields config mode)
              in
              let* () =
                with_handle (fun handle ->
                    run_capture mode handle (run_active mode started_at handle))
              in
              idle_loop ())
      | `Trigger (Release _) -> idle_loop ()
      | `Event event -> (
          match config.recognize event with
          | Some (Press mode) -> (
              let* pressed = config.on_press mode in
              match pressed with
              | Skip -> idle_loop ()
              | Handle handle ->
                  let started_at = now_ms () in
                  let* () =
                    log_info "PTT capture started" (ptt_fields config mode)
                  in
                  let* () =
                    run_capture mode handle (run_active mode started_at handle)
                  in
                  idle_loop ()
              | Scoped with_handle ->
                  let started_at = now_ms () in
                  let* () =
                    log_info "PTT capture started" (ptt_fields config mode)
                  in
                  let* () =
                    with_handle (fun handle ->
                        run_capture mode handle
                          (run_active mode started_at handle))
                  in
                  idle_loop ())
          | _ -> idle_loop ())
    in
    let* () = config.on_ready in
    idle_loop ()
  in
  Eta.Effect.par input_loop (state_loop ()) |> Eta.Effect.map (fun _ -> ())
