type trigger =
  | Press of string
  | Release of string

type captured_clip = {
  duration_ms : int;
  pcm_bytes : bytes;
}

type error = [ `Ptt_error of string ]

type handle = {
  offer : bytes -> (unit, error) Eta.Effect.t;
  finish : captured_clip -> (unit, error) Eta.Effect.t;
  cancel : (unit, error) Eta.Effect.t;
}

type press_result =
  | Handle of handle
  | Scoped of ((handle -> (unit, error) Eta.Effect.t) -> (unit, error) Eta.Effect.t)
  | Skip

type config = {
  recognize : Keyboard.Monitor.key_event -> trigger option;
  min_duration_ms : int;
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
  Printf.sprintf "%02d:%02d:%02d.%03d"
    tm.Unix.tm_hour tm.tm_min tm.tm_sec millis

let log_info message fields =
  let json = Yojson.Safe.to_string (`Assoc fields) in
  Printf.eprintf "[%s] INFO: %s %s\n%!" (timestamp ()) message json

let ptt_fields ?duration_ms ?bytes config mode =
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
    match bytes with
    | None -> base
    | Some bytes -> ("ptt.bytes", `Int bytes) :: base
  in
  List.rev base

let concat_chunks chunks =
  let chunks = List.rev chunks in
  let len = List.fold_left (fun acc chunk -> acc + Bytes.length chunk) 0 chunks in
  let out = Bytes.create len in
  let offset = ref 0 in
  List.iter (fun chunk ->
    Bytes.blit chunk 0 out !offset (Bytes.length chunk);
    offset := !offset + Bytes.length chunk) chunks;
  out

let run ~(keyboard : Keyboard.Monitor.t) ~(recv : unit -> (bytes, [> `Closed ]) Eta.Effect.t) (config : config) =
  let keyboard_queue = Keyboard.Monitor.subscribe keyboard in
  let input = Eio.Stream.create 1024 in
  let push_input item =
    Eta.Effect.sync (fun () -> Eio.Stream.add input item)
  in
  let take_input = Eta.Effect.sync (fun () -> Eio.Stream.take input) in
  let rec keyboard_loop () =
    let* event = Eta.Effect.sync (fun () -> Eio.Stream.take keyboard_queue) in
    let* () = push_input (`Event event) in
    keyboard_loop ()
  in
  let rec recv_loop () =
    let* chunk =
      recv ()
      |> Eta.Effect.catch (fun `Closed ->
        Eta.Effect.fail (`Ptt_error "PulseAudio record stream closed"))
    in
    let* () = push_input (`Chunk chunk) in
    recv_loop ()
  in
  let input_loop =
    Eta.Effect.par (keyboard_loop ()) (recv_loop ()) |> Eta.Effect.map (fun _ -> ())
  in
  let state_loop () =
  let finish_capture mode handle started_at chunks =
    let duration_ms = now_ms () - started_at in
    let pcm_bytes = concat_chunks chunks in
    if duration_ms < config.min_duration_ms || Bytes.length pcm_bytes = 0 then (
      log_info "PTT capture ignored"
        (ptt_fields ~duration_ms ~bytes:(Bytes.length pcm_bytes) config mode);
      handle.cancel
    ) else (
      log_info "PTT capture stopped"
        (ptt_fields ~duration_ms ~bytes:(Bytes.length pcm_bytes) config mode);
      handle.finish { duration_ms; pcm_bytes }
    )
  in
  let run_active mode started_at handle =
    let rec loop capture_state dead_input chunks =
      let* abort = config.on_abort in
      if abort then handle.cancel
      else
        match capture_state with
        | Ptt.Capture.PostRoll _ ->
          let remaining =
            Ptt.Capture.post_roll_remaining_ms capture_state (float_of_int (now_ms ()))
          in
          if remaining <= 0.0 then finish_capture mode handle started_at chunks
          else
            let timeout =
              Eta.Effect.delay (Eta.Duration.ms (int_of_float remaining)) Eta.Effect.unit
              |> Eta.Effect.map (fun () -> `Timeout)
            in
            let* next = Eta.Effect.race [
              timeout;
              take_input;
            ] in
            handle_active_event capture_state dead_input chunks next
        | Ptt.Capture.Idle
        | Ptt.Capture.Capturing _ ->
          let* next = take_input in
          handle_active_event capture_state dead_input chunks next

    and handle_active_event capture_state dead_input chunks = function
      | `Timeout -> finish_capture mode handle started_at chunks
      | `Chunk chunk ->
        let active = Ptt.Capture.is_accepting_chunks capture_state in
        let dead_input = Ptt.Dead_input.sync dead_input active in
        if not active then loop capture_state dead_input chunks
        else
          let result = Ptt.Dead_input.process_chunk dead_input chunk in
          if result.dead then
            let* () = handle.cancel in
            let* () =
              Eta.Effect.sync (fun () ->
                log_info "PTT capture cancelled: no microphone input"
                  (ptt_fields config mode))
            in
            config.on_dead_input
          else
            let chunks = chunk :: chunks in
            let* () =
              if result.has_input then handle.offer chunk else Eta.Effect.unit
            in
            loop capture_state result.detector chunks
      | `Event event ->
        (match config.recognize event with
         | Some (Release release_mode) when release_mode = mode ->
           (match capture_state with
            | Ptt.Capture.Capturing _ ->
              let next = Ptt.Capture.release capture_state (float_of_int (now_ms ())) in
              let* () = config.on_release mode in
              loop next dead_input chunks
            | _ -> loop capture_state dead_input chunks)
         | Some (Press press_mode) when press_mode = mode ->
           let next = Ptt.Capture.start capture_state (float_of_int (now_ms ())) in
           let* () =
             match capture_state with
             | Ptt.Capture.PostRoll _ ->
               Eta.Effect.sync (fun () ->
                 log_info "Post-roll cancelled, continuing capture"
                   (ptt_fields config mode))
             | _ -> Eta.Effect.unit
           in
           loop next dead_input chunks
         | _ -> loop capture_state dead_input chunks)
    in
    loop (Ptt.Capture.start Ptt.Capture.idle (float_of_int started_at)) (Ptt.Dead_input.initial ()) []
  in
  let rec idle_loop () =
    let* next = take_input in
    match next with
    | `Timeout -> idle_loop ()
    | `Chunk _ -> idle_loop ()
    | `Event event ->
      (match config.recognize event with
       | Some (Press mode) ->
         let* pressed = config.on_press mode in
         (match pressed with
          | Skip -> idle_loop ()
          | Handle handle ->
            let started_at = now_ms () in
            let* () =
              Eta.Effect.sync (fun () ->
                log_info "PTT capture started" (ptt_fields config mode))
            in
            let* () = run_active mode started_at handle in
            idle_loop ()
          | Scoped with_handle ->
            let started_at = now_ms () in
            let* () =
              Eta.Effect.sync (fun () ->
                log_info "PTT capture started" (ptt_fields config mode))
            in
            let* () = with_handle (fun handle -> run_active mode started_at handle) in
            idle_loop ())
       | _ -> idle_loop ())
  in
  let* () = config.on_ready in
  idle_loop ()
  in
  Eta.Effect.par input_loop (state_loop ()) |> Eta.Effect.map (fun _ -> ())
