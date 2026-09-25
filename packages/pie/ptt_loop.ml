type trigger = Press of string | Release of string
type captured_clip = { duration_ms : int; pcm_bytes : bytes }
type error = [ `Ptt_error of string ]
type run_error = [ error | `Pulse_error of Pulse.Client.error ]

let widen_ptt (`Ptt_error msg) : run_error = `Ptt_error msg
let widen_pulse (`Pulse_error error) : run_error = `Pulse_error error

(* catch_some recovers the first typed Fail in an otherwise-catchable
   Concurrent/Sequential tree. Recover only an exact Cause.Fail. *)
let recover_single_failure handler body =
  Eta.Effect.defer (fun () ->
      let single = ref false in
      body
      |> Eta.Effect.on_error (fun cause ->
          (single := match cause with Eta.Cause.Fail _ -> true | _ -> false);
          Eta.Effect.unit)
      |> Eta.Effect.catch_some (fun err ->
          if !single then handler err else None))

exception Queue_budget

type finish =
  | Buffered of (captured_clip -> (unit, error) Eta.Effect.t)
  | Streaming of (duration_ms:int -> (unit, error) Eta.Effect.t)

type handle = {
  offer : bytes -> (unit, error) Eta.Effect.t;
  finish : finish;
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
  admission_active : unit -> bool;
}

open Eta.Syntax

let elapsed_ms ~origin now =
  let ns = Mtime.Span.to_uint64_ns (Mtime.span origin now) in
  Int64.to_int (Int64.div ns 1_000_000L)

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
  Audio.Pcm.Accounting.copy (Bytes.length out);
  out

let held_duration_ms ~now started_at capture_state =
  let end_at =
    match capture_state with
    | Ptt.Capture.PostRoll { release_at; _ } -> int_of_float release_at
    | Ptt.Capture.Capturing _ | Ptt.Capture.Idle -> now
  in
  max 0 (end_at - started_at)

let capture_is_eligible ~min_duration_ms ~min_capture_rms ~min_capture_peak
    ~held_duration_ms ~sample_count ~rms ~peak =
  held_duration_ms >= min_duration_ms
  && sample_count > 0
  && (rms >= min_capture_rms || peak >= min_capture_peak)

type input =
  | Event of Keyboard.Monitor.key_event
  | Trigger of trigger
  | Chunk of bytes

(* PIE004: an item records whether the coordinator already owned admission when
   the item was received. A press received during an attempt is rejected at
   receipt instead of replaying as a fresh attempt after settlement; releases
   keep their meaning. Admission itself stays owned by the recording
   coordinator; this stamp only observes it. *)
type stamped_input = { operation_active : bool; input : input }

let run ?control ?(on_capture_resumed = Eta.Effect.unit)
    ?(on_low_input = Eta.Effect.unit) ~mono_clock
    ~(keyboard : Keyboard.Monitor.t) ~audio
    ?(queue_pcm_bytes = Dictation_limits.queue_pcm_bytes)
    ?(capture_pcm_bytes = Dictation_limits.capture_pcm_bytes)
    ?(queue_capacity = 1024 + 256) (config : config) =
  Eta.Effect.defer @@ fun () ->
  let origin = ref None in
  let now_ms () =
    let now = Eio.Time.Mono.now mono_clock in
    let origin_t =
      match !origin with
      | Some t -> t
      | None ->
          origin := Some now;
          now
    in
    elapsed_ms ~origin:origin_t now
  in
  let keyboard_queue = Keyboard.Monitor.subscribe keyboard in
  let input = Eio.Stream.create queue_capacity in
  let pending = Atomic.make (None : stamped_input option) in
  let queue_mutex = Eio.Mutex.create () in
  let queued_bytes = ref 0 in
  let reserve_audio n =
    if n < 0 || n > queue_pcm_bytes then false
    else
      Eio.Mutex.use_rw ~protect:true queue_mutex (fun () ->
          if !queued_bytes + n > queue_pcm_bytes then false
          else (
            queued_bytes := !queued_bytes + n;
            true))
  in
  let release_audio n =
    Eio.Mutex.use_rw ~protect:true queue_mutex (fun () ->
        queued_bytes := !queued_bytes - n)
  in
  let push_input item =
    Eta.Effect.sync (fun () ->
        Eio.Stream.add input
          { operation_active = config.admission_active (); input = item })
  in
  let take_stamped_now () =
    let stamped =
      match Atomic.exchange pending None with
      | Some stamped -> stamped
      | None -> Eio.Stream.take input
    in
    (match stamped.input with
    | Chunk chunk -> release_audio (Bytes.length chunk)
    | Event _ | Trigger _ -> ());
    stamped
  in
  let take_input = Eta.Effect.sync take_stamped_now in
  let take_tagged =
    take_input |> Eta.Effect.map (fun stamped -> `Item stamped)
  in
  let rec consume_processing_input () =
    let* step =
      Eta.Effect.sync (fun () ->
          let stamped = take_stamped_now () in
          match stamped.input with
          | Chunk _ -> `Continue
          | Trigger (Press _) when stamped.operation_active -> `Continue
          | Trigger (Release _) when stamped.operation_active -> `Continue
          | Trigger (Press _ | Release _) ->
              Atomic.set pending (Some stamped);
              `Park
          | Event event -> (
              match config.recognize event with
              | Some ((Press _ | Release _) as trigger)
                when not stamped.operation_active ->
                  Atomic.set pending
                    (Some { stamped with input = Trigger trigger });
                  `Park
              | Some (Press _) | Some (Release _) | None -> `Continue))
    in
    match step with
    | `Park -> Eta.Effect.unit
    | `Continue -> consume_processing_input ()
  in
  let finish_with_processing finish =
    Eta.Effect.with_background ~name:"ptt.processing.input"
      (consume_processing_input ()) (fun () -> finish)
  in
  let rec keyboard_loop () =
    let* event = Eta.Effect.sync (fun () -> Eio.Stream.take keyboard_queue) in
    let* () = push_input (Event event) in
    keyboard_loop ()
  in
  let rec control_loop control =
    let* trigger = Eta.Effect.sync (fun () -> Eio.Stream.take control) in
    let* () = push_input (Trigger trigger) in
    control_loop control
  in
  (* run_for_each locks the callback error row to the Pulse stream. There is
     no public Stream.map_error. Reserve/enqueue stays in a non-failing Pulse
     callback; queue overflow raises Queue_budget and is converted to
     `Ptt_error only when that Die is the primary cause, so Pulse Fail trees
     are replayed unchanged. *)
  let recv_loop =
    Eta_stream.run_for_each
      (fun chunk ->
        Eta.Effect.sync (fun () ->
            let n = Bytes.length chunk in
            if not (reserve_audio n) then raise Queue_budget;
            let added = ref false in
            Fun.protect
              ~finally:(fun () -> if not !added then release_audio n)
              (fun () ->
                Eio.Stream.add input
                  {
                    operation_active = config.admission_active ();
                    input = Chunk chunk;
                  };
                added := true)))
      audio
    |> Eta.Effect.map_error widen_pulse
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
    let finish_capture ~thresholds mode handle started_at capture_state offered
        chunks acc =
      let now = now_ms () in
      let duration_ms = max 0 (now - started_at) in
      let held_duration_ms = held_duration_ms ~now started_at capture_state in
      let rms = Audio.Pcm.s16le_rms acc in
      let peak = Audio.Pcm.s16le_peak acc in
      let min_capture_rms, min_capture_peak = thresholds in
      if acc.pending <> None then Eta.Effect.fail (`Ptt_error "pcm:framing")
      else if
        not
          (capture_is_eligible ~min_duration_ms:config.min_duration_ms
             ~min_capture_rms ~min_capture_peak ~held_duration_ms
             ~sample_count:acc.Audio.Pcm.sample_count ~rms ~peak)
      then
        let* () =
          log_info "PTT capture ignored"
            (ptt_fields ~duration_ms ~held_duration_ms
               ~bytes:(Audio.Pcm.s16le_complete_bytes acc)
               ~rms ~peak config mode)
        in
        handle.cancel
      else
        let* () =
          if offered then Eta.Effect.unit else offer_chunks handle chunks
        in
        let* () =
          log_info "PTT capture stopped"
            (ptt_fields ~duration_ms ~held_duration_ms
               ~bytes:(Audio.Pcm.s16le_complete_bytes acc)
               ~rms ~peak config mode)
        in
        let finishing =
          match handle.finish with
          | Buffered f -> f { duration_ms; pcm_bytes = concat_chunks chunks }
          | Streaming f -> f ~duration_ms
        in
        finish_with_processing finishing
    in
    let run_active ~thresholds mode started_at handle =
      let captured = ref 0 in
      let min_rms, min_peak = thresholds in
      let low_volume = ref (Ptt.Low_volume.create ~min_rms ~min_peak ()) in
      let rec loop capture_state dead_input offered chunks acc =
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
                finish_capture ~thresholds mode handle started_at capture_state
                  offered chunks acc
              else
                let timeout =
                  Eta.Effect.sync (fun () ->
                      Eio.Time.Mono.sleep mono_clock (remaining /. 1000.0))
                  |> Eta.Effect.map (fun () -> `Timeout)
                in
                let* next = Eta.Effect.race [ timeout; take_tagged ] in
                handle_active_event capture_state dead_input offered chunks acc
                  next
          | Ptt.Capture.Idle | Ptt.Capture.Capturing _ ->
              let* next = take_tagged in
              handle_active_event capture_state dead_input offered chunks acc
                next
      and handle_active_event capture_state dead_input offered chunks acc =
        function
        | `Timeout ->
            finish_capture ~thresholds mode handle started_at capture_state
              offered chunks acc
        | `Item { input = Chunk chunk; _ } ->
            let active = Ptt.Capture.is_accepting_chunks capture_state in
            let dead_input = Ptt.Dead_input.sync dead_input active in
            if not active then loop capture_state dead_input offered chunks acc
            else
              let result = Ptt.Dead_input.process_chunk dead_input chunk in
              let next_low_volume, warn_low =
                Ptt.Low_volume.process_chunk !low_volume chunk
              in
              low_volume := next_low_volume;
              if result.dead then
                let* () = handle.cancel in
                let* () =
                  log_info "PTT capture cancelled: no microphone input"
                    (ptt_fields config mode)
                in
                config.on_dead_input
              else
                let* () = if warn_low then on_low_input else Eta.Effect.unit in
                let n = Bytes.length chunk in
                if n > capture_pcm_bytes || !captured + n > capture_pcm_bytes
                then Eta.Effect.fail (`Ptt_error "pcm:capture_budget")
                else
                  let () = captured := !captured + n in
                  let acc = Audio.Pcm.s16le_update acc chunk in
                  let chunks = chunk :: chunks in
                  let should_start_offering =
                    if offered || not result.has_input then false
                    else
                      let held_duration_ms =
                        held_duration_ms ~now:(now_ms ()) started_at
                          capture_state
                      in
                      let rms = Audio.Pcm.s16le_rms acc in
                      let peak = Audio.Pcm.s16le_peak acc in
                      let min_capture_rms, min_capture_peak = thresholds in
                      capture_is_eligible
                        ~min_duration_ms:config.min_duration_ms ~min_capture_rms
                        ~min_capture_peak ~held_duration_ms
                        ~sample_count:acc.Audio.Pcm.sample_count ~rms ~peak
                  in
                  let* () =
                    if not result.has_input then Eta.Effect.unit
                    else if offered then handle.offer chunk
                    else if should_start_offering then
                      offer_chunks handle chunks
                    else Eta.Effect.unit
                  in
                  let offered = offered || should_start_offering in
                  loop capture_state result.detector offered chunks acc
        | `Item { input = Event event; _ } -> (
            match config.recognize event with
            | Some (Release release_mode) when release_mode = mode -> (
                match capture_state with
                | Ptt.Capture.Capturing _ ->
                    let next =
                      Ptt.Capture.release capture_state
                        (float_of_int (now_ms ()))
                    in
                    let* () = config.on_release mode in
                    loop next dead_input offered chunks acc
                | _ -> loop capture_state dead_input offered chunks acc)
            | Some (Press press_mode) when press_mode = mode ->
                let next =
                  Ptt.Capture.start capture_state (float_of_int (now_ms ()))
                in
                let* () =
                  match capture_state with
                  | Ptt.Capture.PostRoll _ ->
                      let* () = on_capture_resumed in
                      log_info "Post-roll cancelled, continuing capture"
                        (ptt_fields config mode)
                  | _ -> Eta.Effect.unit
                in
                loop next dead_input offered chunks acc
            | _ -> loop capture_state dead_input offered chunks acc)
        | `Item { input = Trigger (Release release_mode); _ }
          when release_mode = mode -> (
            match capture_state with
            | Ptt.Capture.Capturing _ ->
                let next =
                  Ptt.Capture.release capture_state (float_of_int (now_ms ()))
                in
                let* () = config.on_release mode in
                loop next dead_input offered chunks acc
            | _ -> loop capture_state dead_input offered chunks acc)
        | `Item { input = Trigger (Press press_mode); _ } when press_mode = mode
          ->
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
            loop next dead_input offered chunks acc
        | `Item _ -> loop capture_state dead_input offered chunks acc
      in
      loop
        (Ptt.Capture.start Ptt.Capture.idle (float_of_int started_at))
        (Ptt.Dead_input.initial ())
        false [] Audio.Pcm.s16le_empty
    in
    let rec idle_loop () =
      let capture_failed mode message =
        log_error "PTT capture failed"
          (("ptt.error", `String message) :: ptt_fields config mode)
      in
      (* Recoverable operation failure stays inside the PTT loop: the owner
         settles the attempt and the loop keeps admitting new ones. *)
      let operation_failed mode message =
        log_error "PTT operation failed"
          (("ptt.error", `String message) :: ptt_fields config mode)
      in
      let replay_exit exit =
        Eta.Effect.async ~register:(fun resume ->
            resume exit;
            None)
      in
      let recover_clean_ptt recover =
        recover_single_failure (function
          | `Ptt_error "pcm:capture_budget" | `Ptt_error "pcm:queue_budget" ->
              None
          | `Ptt_error message -> Some (recover message))
      in
      let run_capture mode handle run =
        run
        |> Eta.Effect.on_exit (function
          | Eta.Exit.Ok () -> Eta.Effect.unit
          | Eta.Exit.Error _ -> handle.cancel)
        |> recover_clean_ptt (fun message -> capture_failed mode message)
      in
      let press mode =
        Eta.Effect.sync (fun () ->
            let thresholds = config.capture_thresholds () in
            (thresholds, config.on_press mode))
        |> Eta.Effect.bind (fun (thresholds, started) ->
            started |> Eta.Effect.to_exit
            |> Eta.Effect.bind (fun (exit : (press_result, error) Eta.Exit.t) ->
                match exit with
                | Eta.Exit.Ok result -> Eta.Effect.pure (thresholds, result)
                | Eta.Exit.Error _ as failed ->
                    replay_exit failed
                    |> recover_clean_ptt (fun message ->
                        let* () = operation_failed mode message in
                        Eta.Effect.pure Skip)
                    |> Eta.Effect.map (fun result -> (thresholds, result))))
      in
      let start_handle ~thresholds mode started_at handle =
        let* () = log_info "PTT capture started" (ptt_fields config mode) in
        run_capture mode handle (run_active ~thresholds mode started_at handle)
      in
      let start_scoped ~thresholds mode started_at with_handle =
        let* () = log_info "PTT capture started" (ptt_fields config mode) in
        with_handle (fun handle ->
            run_capture mode handle
              (run_active ~thresholds mode started_at handle))
        |> recover_clean_ptt (fun message -> operation_failed mode message)
      in
      let rejected mode =
        let* () =
          log_info "PTT press ignored: operation already active"
            (ptt_fields config mode)
        in
        idle_loop ()
      in
      let begin_press mode received_during_attempt =
        if received_during_attempt then rejected mode
        else
          let* thresholds, pressed = press mode in
          match pressed with
          | Skip -> idle_loop ()
          | Handle handle ->
              let started_at = now_ms () in
              let* () = start_handle ~thresholds mode started_at handle in
              idle_loop ()
          | Scoped with_handle ->
              let started_at = now_ms () in
              let* () = start_scoped ~thresholds mode started_at with_handle in
              idle_loop ()
      in
      let* next = take_input in
      match next.input with
      | Chunk _ -> idle_loop ()
      | Trigger (Press mode) -> begin_press mode next.operation_active
      | Trigger (Release _) -> idle_loop ()
      | Event event -> (
          match config.recognize event with
          | Some (Press mode) -> begin_press mode next.operation_active
          | _ -> idle_loop ())
    in
    let* () = config.on_ready in
    idle_loop ()
  in
  let* () = Eta.Effect.sync (fun () -> ignore (now_ms ())) in
  let replay cause =
    Eta.Effect.async ~register:(fun resume ->
        resume (Eta.Exit.Error cause);
        None)
  in
  Eta.Effect.par input_loop (state_loop () |> Eta.Effect.map_error widen_ptt)
  |> Eta.Effect.map (fun _ -> ())
  |> Eta.Effect.to_exit
  |> Eta.Effect.bind (fun exit ->
      match exit with
      | Eta.Exit.Ok () -> Eta.Effect.unit
      | Eta.Exit.Error (Eta.Cause.Die { exn = Queue_budget; _ }) ->
          Eta.Effect.fail (`Ptt_error "pcm:queue_budget")
      | Eta.Exit.Error cause -> replay cause)
