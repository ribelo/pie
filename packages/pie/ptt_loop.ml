type trigger = Press of string | Release of string
type captured_clip = { duration_ms : int; pcm_bytes : bytes }

type error =
  [ `Ptt_error of string | `Ptt_cause of [ `Ptt_error of string ] Eta.Cause.t ]

type run_error = [ error | `Pulse_error of Pulse.Client.error ]

type finish =
  | Buffered of (captured_clip -> (unit, error) result)
  | Streaming of (duration_ms:int -> (unit, error) result)

type handle = {
  offer : bytes -> (unit, error) result;
  finish : finish;
  cancel : unit -> (unit, error) result;
}

type press_result =
  | Handle of handle
  | Scoped of ((handle -> (unit, error) result) -> (unit, error) result)
  | Skip

type config = {
  recognize : Keyboard.Monitor.key_event -> trigger option;
  min_duration_ms : int;
  capture_thresholds : unit -> float * float;
  log_prefix : string -> string;
  source : string option;
  on_ready : unit -> (unit, error) result;
  on_press : string -> (press_result, error) result;
  on_release : string -> (unit, error) result;
  on_abort : unit -> (bool, error) result;
  on_dead_input : unit -> (unit, error) result;
  admission_active : unit -> bool;
}

let ( let* ) = Result.bind

let elapsed_ms ~origin now =
  let ns = Mtime.Span.to_uint64_ns (Mtime.span origin now) in
  Int64.to_int (Int64.div ns 1_000_000L)

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

type stamped_input = { operation_active : bool; input : input }

(* PCM bytes and item count are admitted and released with the queue mutation
   under the same mutex. A blocked producer cannot charge an unqueued chunk. *)
type input_queue = {
  mutex : Eio.Mutex.t;
  changed : Eio.Condition.t;
  items : stamped_input Queue.t;
  mutable bytes : int;
  capacity : int;
  pcm_limit : int;
}

let queue_create ~capacity ~pcm_limit =
  {
    mutex = Eio.Mutex.create ();
    changed = Eio.Condition.create ();
    items = Queue.create ();
    bytes = 0;
    capacity;
    pcm_limit;
  }

let queue_add q item =
  let n = match item.input with Chunk b -> Bytes.length b | _ -> 0 in
  Eio.Mutex.lock q.mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock q.mutex) @@ fun () ->
  let rec add () =
    if n > q.pcm_limit - q.bytes then Error (`Ptt_error "pcm:queue_budget")
    else if Queue.length q.items >= q.capacity then (
      Eio.Condition.await q.changed q.mutex;
      add ())
    else (
      Queue.add item q.items;
      q.bytes <- q.bytes + n;
      Eio.Condition.broadcast q.changed;
      Ok ())
  in
  add ()

let queue_take q =
  Eio.Mutex.lock q.mutex;
  Fun.protect ~finally:(fun () -> Eio.Mutex.unlock q.mutex) @@ fun () ->
  let rec take () =
    match Queue.take_opt q.items with
    | None ->
        Eio.Condition.await q.changed q.mutex;
        take ()
    | Some item ->
        (match item.input with
        | Chunk b -> q.bytes <- q.bytes - Bytes.length b
        | _ -> ());
        Eio.Condition.broadcast q.changed;
        item
  in
  take ()

let ptt_fields ?duration_ms ?held_duration_ms ?bytes ?rms ?peak config mode =
  let open Eta_observability.Logger in
  let base =
    [
      ("ptt.mode", String mode);
      ("ptt.log_prefix", String (config.log_prefix mode));
    ]
  in
  let add key f value fields =
    match value with None -> fields | Some v -> (key, f v) :: fields
  in
  base
  |> add "ptt.source" (fun x -> String x) config.source
  |> add "ptt.duration_ms" (fun x -> Int x) duration_ms
  |> add "ptt.held_duration_ms" (fun x -> Int x) held_duration_ms
  |> add "ptt.bytes" (fun x -> Int x) bytes
  |> add "ptt.rms" (fun x -> Float x) rms
  |> add "ptt.peak" (fun x -> Float x) peak

let run ?control ?(on_capture_resumed = fun () -> Ok ())
    ?(on_low_input = fun () -> Ok ()) ~sw ~mono_clock ~logger
    ~(keyboard : Keyboard.Monitor.t) ~audio
    ?(queue_pcm_bytes = Dictation_limits.queue_pcm_bytes)
    ?(capture_pcm_bytes = Dictation_limits.capture_pcm_bytes)
    ?(queue_capacity = 1024 + 256) (config : config) : (unit, run_error) result
    =
  let origin = Eio.Time.Mono.now mono_clock in
  let now_ms () = elapsed_ms ~origin (Eio.Time.Mono.now mono_clock) in
  let keyboard_queue = Keyboard.Monitor.subscribe keyboard in
  let input =
    queue_create ~capacity:queue_capacity ~pcm_limit:queue_pcm_bytes
  in
  let pending = ref None in
  let stamp item =
    { operation_active = config.admission_active (); input = item }
  in
  let push_input item = queue_add input (stamp item) in
  let take_input () =
    match !pending with
    | Some item ->
        pending := None;
        item
    | None -> queue_take input
  in
  let rec consume_processing_input () =
    let stamped = take_input () in
    let park item =
      pending := Some item;
      Ok ()
    in
    match stamped.input with
    | Chunk _ -> consume_processing_input ()
    | Trigger (Press _ | Release _) when stamped.operation_active ->
        consume_processing_input ()
    | Trigger (Press _ | Release _) -> park stamped
    | Event event -> (
        match config.recognize event with
        | Some trigger when not stamped.operation_active ->
            park { stamped with input = Trigger trigger }
        | _ -> consume_processing_input ())
  in
  let finish_with_processing f =
    Eta.Concurrent.with_background ~sw consume_processing_input f
  in
  let rec keyboard_loop () =
    let* () = push_input (Event (Eio.Stream.take keyboard_queue)) in
    keyboard_loop ()
  in
  let rec control_loop control =
    let* () = push_input (Trigger (Eio.Stream.take control)) in
    control_loop control
  in
  let rec recv_loop () =
    match audio () with
    | Error (`Pulse_error e) -> Error (`Pulse_error e)
    | Ok None -> Ok ()
    | Ok (Some chunk) ->
        let* () = push_input (Chunk chunk) in
        recv_loop ()
  in
  let input_loop () =
    let keyboard_and_audio () =
      let* _ = Eta.Concurrent.par ~sw keyboard_loop recv_loop in
      Ok ()
    in
    match control with
    | None -> keyboard_and_audio ()
    | Some control ->
        let* _ =
          Eta.Concurrent.par ~sw keyboard_and_audio (fun () ->
              control_loop control)
        in
        Ok ()
  in
  let state_loop () =
    let log_info ?(fields = []) msg =
      Eta_observability.Logger.info logger ~fields msg
    in
    let log_error ?(fields = []) msg =
      Eta_observability.Logger.error logger ~fields msg
    in
    let offer_chunks handle chunks =
      let rec offer = function
        | [] -> Ok ()
        | chunk :: rest ->
            let* () = handle.offer chunk in
            offer rest
      in
      offer (List.rev chunks)
    in
    let finish_capture ~thresholds mode handle started_at capture_state offered
        chunks acc =
      let now = now_ms () in
      let duration_ms = max 0 (now - started_at) in
      let held_duration_ms = held_duration_ms ~now started_at capture_state in
      let rms = Audio.Pcm.s16le_rms acc in
      let peak = Audio.Pcm.s16le_peak acc in
      let min_capture_rms, min_capture_peak = thresholds in
      if acc.pending <> None then Error (`Ptt_error "pcm:framing")
      else if
        not
          (capture_is_eligible ~min_duration_ms:config.min_duration_ms
             ~min_capture_rms ~min_capture_peak ~held_duration_ms
             ~sample_count:acc.Audio.Pcm.sample_count ~rms ~peak)
      then (
        log_info
          ~fields:
            (ptt_fields ~duration_ms ~held_duration_ms
               ~bytes:(Audio.Pcm.s16le_complete_bytes acc)
               ~rms ~peak config mode)
          "PTT capture ignored";
        handle.cancel ())
      else
        let* () = if offered then Ok () else offer_chunks handle chunks in
        log_info
          ~fields:
            (ptt_fields ~duration_ms ~held_duration_ms
               ~bytes:(Audio.Pcm.s16le_complete_bytes acc)
               ~rms ~peak config mode)
          "PTT capture stopped";
        finish_with_processing (fun () ->
            match handle.finish with
            | Buffered f -> f { duration_ms; pcm_bytes = concat_chunks chunks }
            | Streaming f -> f ~duration_ms)
    in
    let run_active ~thresholds mode started_at handle =
      let captured = ref 0 in
      let min_rms, min_peak = thresholds in
      let low_volume = ref (Ptt.Low_volume.create ~min_rms ~min_peak ()) in
      let rec loop capture_state dead_input offered chunks acc =
        let* abort = config.on_abort () in
        if abort then handle.cancel ()
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
                let* next =
                  match
                    Eta.Concurrent.race ~sw
                      [
                        (fun () ->
                          Eio.Time.Mono.sleep mono_clock (remaining /. 1000.0);
                          Ok `Timeout);
                        (fun () -> Ok (`Item (take_input ())));
                      ]
                  with
                  | Ok x -> Ok x
                  | Error _ -> Error (`Ptt_error "ptt:postroll_race")
                in
                handle_active_event capture_state dead_input offered chunks acc
                  next
          | Ptt.Capture.Idle | Ptt.Capture.Capturing _ ->
              handle_active_event capture_state dead_input offered chunks acc
                (`Item (take_input ()))
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
              if result.dead then (
                let* () = handle.cancel () in
                log_info ~fields:(ptt_fields config mode)
                  "PTT capture cancelled: no microphone input";
                config.on_dead_input ())
              else
                let* () = if warn_low then on_low_input () else Ok () in
                let n = Bytes.length chunk in
                if n > capture_pcm_bytes - !captured then
                  Error (`Ptt_error "pcm:capture_budget")
                else (
                  captured := !captured + n;
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
                    if not result.has_input then Ok ()
                    else if offered then handle.offer chunk
                    else if should_start_offering then
                      offer_chunks handle chunks
                    else Ok ()
                  in
                  loop capture_state result.detector
                    (offered || should_start_offering)
                    chunks acc)
        | `Item { input = Event event; _ } -> (
            match config.recognize event with
            | Some trigger ->
                handle_trigger ~notify_resumed:true capture_state dead_input
                  offered chunks acc trigger
            | None -> loop capture_state dead_input offered chunks acc)
        | `Item { input = Trigger trigger; _ } ->
            handle_trigger ~notify_resumed:false capture_state dead_input
              offered chunks acc trigger
      and handle_trigger ~notify_resumed capture_state dead_input offered chunks
          acc = function
        | Release release_mode when release_mode = mode -> (
            match capture_state with
            | Ptt.Capture.Capturing _ ->
                let next =
                  Ptt.Capture.release capture_state (float_of_int (now_ms ()))
                in
                let* () = config.on_release mode in
                loop next dead_input offered chunks acc
            | _ -> loop capture_state dead_input offered chunks acc)
        | Press press_mode when press_mode = mode ->
            let next =
              Ptt.Capture.start capture_state (float_of_int (now_ms ()))
            in
            let* () =
              match capture_state with
              | Ptt.Capture.PostRoll _ ->
                  let* () =
                    if notify_resumed then on_capture_resumed () else Ok ()
                  in
                  log_info ~fields:(ptt_fields config mode)
                    "Post-roll cancelled, continuing capture";
                  Ok ()
              | _ -> Ok ()
            in
            loop next dead_input offered chunks acc
        | _ -> loop capture_state dead_input offered chunks acc
      in
      loop
        (Ptt.Capture.start Ptt.Capture.idle (float_of_int started_at))
        (Ptt.Dead_input.initial ())
        false [] Audio.Pcm.s16le_empty
    in
    let rec idle_loop () =
      let report_failure kind mode message =
        log_error
          ~fields:
            (("ptt.error", Eta_observability.Logger.String message)
            :: ptt_fields config mode)
          kind
      in
      let recover_operation mode kind = function
        | Error (`Ptt_error (("pcm:capture_budget" | "pcm:queue_budget") as m))
          ->
            Error (`Ptt_error m)
        | Error (`Ptt_error message) ->
            report_failure kind mode message;
            Ok ()
        | Error (`Ptt_cause _ as cause) -> Error cause
        | Ok () -> Ok ()
      in
      let run_capture mode handle run =
        let result = run () in
        let result =
          match result with
          | Ok () -> Ok ()
          | Error error -> (
              (* The capture's failure is primary; a failing or raising cancel is its
                 cleanup. Both are kept: a raising cancel must not drop the primary
                 (the v1 interpreter composed them; direct style composes here). *)
              let to_cause = function
                | `Ptt_error _ as e -> Eta.Cause.Fail e
                | `Ptt_cause c -> c
              in
              let with_cleanup finalizer =
                Error
                  (`Ptt_cause
                     (Eta.Cause.Suppressed
                        { primary = to_cause error; finalizer }))
              in
              match handle.cancel () with
              | Ok () -> Error error
              | Error cleanup -> with_cleanup (to_cause cleanup)
              | exception (Eio.Cancel.Cancelled _ as cancelled) ->
                  raise cancelled
              | exception exn ->
                  with_cleanup
                    (Eta.Cause.Die (exn, Printexc.get_raw_backtrace ())))
        in
        recover_operation mode "PTT capture failed" result
      in
      let begin_press mode received_during_attempt =
        if received_during_attempt then (
          log_info ~fields:(ptt_fields config mode)
            "PTT press ignored: operation already active";
          idle_loop ())
        else
          let thresholds = config.capture_thresholds () in
          match config.on_press mode with
          | Error
              (`Ptt_error (("pcm:capture_budget" | "pcm:queue_budget") as m)) ->
              Error (`Ptt_error m)
          | Error (`Ptt_error message) ->
              report_failure "PTT operation failed" mode message;
              idle_loop ()
          | Error (`Ptt_cause _ as cause) -> Error cause
          | Ok Skip -> idle_loop ()
          | Ok (Handle handle) ->
              let started_at = now_ms () in
              log_info ~fields:(ptt_fields config mode) "PTT capture started";
              let* () =
                run_capture mode handle (fun () ->
                    run_active ~thresholds mode started_at handle)
              in
              idle_loop ()
          | Ok (Scoped with_handle) ->
              let started_at = now_ms () in
              log_info ~fields:(ptt_fields config mode) "PTT capture started";
              let result =
                with_handle (fun handle ->
                    run_capture mode handle (fun () ->
                        run_active ~thresholds mode started_at handle))
              in
              let* () = recover_operation mode "PTT operation failed" result in
              idle_loop ()
      in
      match take_input () with
      | { input = Chunk _; _ } | { input = Trigger (Release _); _ } ->
          idle_loop ()
      | { input = Trigger (Press mode); operation_active } ->
          begin_press mode operation_active
      | { input = Event event; operation_active } -> (
          match config.recognize event with
          | Some (Press mode) -> begin_press mode operation_active
          | _ -> idle_loop ())
    in
    let* () = config.on_ready () in
    idle_loop ()
  in
  let input_loop () =
    Result.map_error (fun e -> (e :> run_error)) (input_loop ())
  in
  let state_loop () =
    Result.map_error (fun e -> (e :> run_error)) (state_loop ())
  in
  let* _ = Eta.Concurrent.par ~sw input_loop state_loop in
  Ok ()
