type trigger =
  | Press of string
  | Press_binding of Config.Stt.binding
  | Release of string

type captured_clip = { duration_ms : int; pcm_bytes : bytes }

type error =
  [ `Ptt_error of string | `Ptt_cause of [ `Ptt_error of string ] Ox.Cause.t ]

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
  let open Ox_observability.Logger in
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

let run ?control ?on_press_binding ?(on_capture_failure = fun _ -> Ok ())
    ?(on_ignored_capture = fun _ -> ()) ?(on_capture_resumed = fun () -> Ok ())
    ?(release_grace_ms = fun () -> 1000) ?(on_capture_ended = fun () -> Ok ())
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
  (* Only the capturing fiber reads input; the idle reader takes over once
     finishing hands ownership to a processing fiber. *)
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
      let* _ = Ox.Concurrent.par ~sw keyboard_loop recv_loop in
      Ok ()
    in
    match control with
    | None -> keyboard_and_audio ()
    | Some control ->
        let* _ =
          Ox.Concurrent.par ~sw keyboard_and_audio (fun () ->
              control_loop control)
        in
        Ok ()
  in
  let fatal, resolve_fatal = Eio.Promise.create () in
  let fatal_sent = ref false in
  let state_loop () =
    let log_info ?(fields = []) msg =
      Ox_observability.Logger.info logger ~fields msg
    in
    let log_error ?(fields = []) msg =
      Ox_observability.Logger.error logger ~fields msg
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
        on_ignored_capture
          { History.held_ms = held_duration_ms; duration_ms; rms; peak };
        handle.cancel ())
      else
        let* () = if offered then Ok () else offer_chunks handle chunks in
        log_info
          ~fields:
            (ptt_fields ~duration_ms ~held_duration_ms
               ~bytes:(Audio.Pcm.s16le_complete_bytes acc)
               ~rms ~peak config mode)
          "PTT capture stopped";
        match handle.finish with
        | Buffered f -> f { duration_ms; pcm_bytes = concat_chunks chunks }
        | Streaming f -> f ~duration_ms
    in
    let run_active ?binding ~thresholds mode started_at handle =
      let grace_ms = max 0 (release_grace_ms ()) in
      let same_binding = function
        | Press pressed_mode -> pressed_mode = mode
        | Press_binding pressed -> (
            match binding with
            | Some original ->
                pressed.Config.Stt.keysym = original.Config.Stt.keysym
                && pressed.shift = original.shift
            | None -> Config.Stt.string_of_mode pressed.mode = mode)
        | Release _ -> false
      in
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
                Ptt.Capture.post_roll_remaining_ms ~tail_ms:(max 2000 grace_ms)
                  capture_state
                  (float_of_int (now_ms ()))
              in
              if remaining <= 0.0 then
                finish_capture ~thresholds mode handle started_at capture_state
                  offered chunks acc
              else
                (* Neither branch returns an [Error]: the first to settle is the value. A
                   defect in either is raised as it is, not mapped to a typed error. *)
                let* next =
                  Ox.Concurrent.race_first ~sw
                    [
                      (fun () ->
                        Eio.Time.Mono.sleep mono_clock (remaining /. 1000.0);
                        Ok `Timeout);
                      (fun () -> Ok (`Item (take_input ())));
                    ]
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
              (* The diagnostic is latched once per hold, not audio admission. *)
              let has_input =
                Bytes.length chunk > 0 && not (Ptt.Dead_input.is_flatline chunk)
              in
              let next_low_volume, warn_low =
                Ptt.Low_volume.process_chunk !low_volume chunk
              in
              low_volume := next_low_volume;
              let* () =
                if result.warn then (
                  log_info ~fields:(ptt_fields config mode)
                    "PTT no microphone input detected; waiting for release";
                  config.on_dead_input ())
                else Ok ()
              in
              (* Flatlines settle after release, like all ignored captures. *)
              let* () = if warn_low then on_low_input () else Ok () in
              let n = Bytes.length chunk in
              if n > capture_pcm_bytes - !captured then
                Error (`Ptt_error "pcm:capture_budget")
              else (
                captured := !captured + n;
                let acc = Audio.Pcm.s16le_update acc chunk in
                let chunks = chunk :: chunks in
                let should_start_offering =
                  if offered || not has_input then false
                  else
                    let held_duration_ms =
                      held_duration_ms ~now:(now_ms ()) started_at capture_state
                    in
                    let rms = Audio.Pcm.s16le_rms acc in
                    let peak = Audio.Pcm.s16le_peak acc in
                    let min_capture_rms, min_capture_peak = thresholds in
                    capture_is_eligible ~min_duration_ms:config.min_duration_ms
                      ~min_capture_rms ~min_capture_peak ~held_duration_ms
                      ~sample_count:acc.Audio.Pcm.sample_count ~rms ~peak
                in
                let* () =
                  if not has_input then Ok ()
                  else if offered then handle.offer chunk
                  else if should_start_offering then offer_chunks handle chunks
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
          acc trigger =
        match trigger with
        | Release release_mode when release_mode = mode -> (
            match capture_state with
            | Ptt.Capture.Capturing _ ->
                let next =
                  Ptt.Capture.release capture_state (float_of_int (now_ms ()))
                in
                let* () = config.on_release mode in
                loop next dead_input offered chunks acc
            | _ -> loop capture_state dead_input offered chunks acc)
        | Press _ | Press_binding _ -> (
            match capture_state with
            | Ptt.Capture.PostRoll { release_at; _ } ->
                if
                  same_binding trigger && grace_ms > 0
                  && float_of_int (now_ms ()) -. release_at
                     < float_of_int grace_ms
                then (
                  let _ = notify_resumed in
                  let* () = on_capture_resumed () in
                  log_info ~fields:(ptt_fields config mode)
                    "Release grace cancelled, continuing capture";
                  let next =
                    Ptt.Capture.start capture_state (float_of_int (now_ms ()))
                  in
                  loop next dead_input offered chunks acc)
                else (
                  (* End the tail and admit this fresh press after handoff. *)
                  pending :=
                    Some { input = Trigger trigger; operation_active = false };
                  finish_capture ~thresholds mode handle started_at
                    capture_state offered chunks acc)
            | _ -> loop capture_state dead_input offered chunks acc)
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
            (("ptt.error", Ox_observability.Logger.String message)
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
      let run_capture ~capture_ended mode handle run =
        let result = run () in
        let result =
          match result with
          | Ok () -> Ok ()
          | Error error -> (
              (* The capture's failure is primary; a failing or raising cancel is its
                 cleanup. Both are kept: a raising cancel must not drop the primary
                 (the v1 interpreter composed them; direct style composes here). *)
              let to_cause = function
                | `Ptt_error _ as e -> Ox.Cause.Fail e
                | `Ptt_cause c -> c
              in
              let with_cleanup finalizer =
                Error
                  (`Ptt_cause
                     (Ox.Cause.Suppressed
                        { primary = to_cause error; finalizer }))
              in
              let publication =
                if !capture_ended then Ok () else on_capture_failure error
              in
              let with_publication cause =
                match publication with
                | Ok () -> cause
                | Error error ->
                    Ox.Cause.Suppressed
                      { primary = to_cause error; finalizer = cause }
              in
              match handle.cancel () with
              | Ok () -> (
                  match publication with
                  | Ok () -> Error error
                  | Error cleanup -> with_cleanup (to_cause cleanup))
              | Error cleanup ->
                  with_cleanup (with_publication (to_cause cleanup))
              | exception (Eio.Cancel.Cancelled _ as cancelled) ->
                  raise cancelled
              | exception exn ->
                  with_cleanup
                    (with_publication
                       (Ox.Cause.Die (exn, Printexc.get_raw_backtrace ()))))
        in
        recover_operation mode "PTT capture failed" result
      in
      let begin_press ?binding mode received_during_attempt =
        if received_during_attempt then (
          log_info ~fields:(ptt_fields config mode)
            "PTT press ignored: operation already active";
          idle_loop ())
        else
          let thresholds = config.capture_thresholds () in
          let pressed =
            match (binding, on_press_binding) with
            | Some binding, Some on_press -> on_press binding
            | _ -> config.on_press mode
          in
          match pressed with
          | Error
              (`Ptt_error (("pcm:capture_budget" | "pcm:queue_budget") as m)) ->
              Error (`Ptt_error m)
          | Error (`Ptt_error message) ->
              report_failure "PTT operation failed" mode message;
              idle_loop ()
          | Error (`Ptt_cause _ as cause) -> Error cause
          | Ok Skip -> idle_loop ()
          | Ok ((Handle _ | Scoped _) as operation) ->
              let started_at = now_ms () in
              let capture_done, resolve_capture = Eio.Promise.create () in
              let capture_ended = ref false in
              let handoff result =
                if not !capture_ended then (
                  capture_ended := true;
                  Eio.Promise.resolve resolve_capture result)
              in
              let use handle =
                let finish f =
                  let* () = on_capture_ended () in
                  handoff (Ok ());
                  f ()
                in
                let handle =
                  {
                    handle with
                    finish =
                      (match handle.finish with
                      | Buffered f ->
                          Buffered (fun clip -> finish (fun () -> f clip))
                      | Streaming f ->
                          Streaming
                            (fun ~duration_ms ->
                              finish (fun () -> f ~duration_ms)));
                  }
                in
                run_capture ~capture_ended mode handle (fun () ->
                    run_active ?binding ~thresholds mode started_at handle)
              in
              log_info ~fields:(ptt_fields config mode) "PTT capture started";
              Eio.Fiber.fork ~sw (fun () ->
                  let result =
                    match operation with
                    | Handle handle -> use handle
                    | Scoped with_handle -> with_handle use
                    | Skip -> assert false
                  in
                  let result =
                    recover_operation mode "PTT operation failed" result
                  in
                  if not !capture_ended then handoff result
                  else
                    match result with
                    | Error error when not !fatal_sent ->
                        fatal_sent := true;
                        Eio.Promise.resolve resolve_fatal error
                    | _ -> ());
              let* () = Eio.Promise.await capture_done in
              idle_loop ()
      in
      match take_input () with
      | { input = Chunk _; _ } | { input = Trigger (Release _); _ } ->
          idle_loop ()
      | { input = Trigger (Press_binding binding); operation_active } ->
          begin_press ~binding
            (Config.Stt.string_of_mode binding.mode)
            operation_active
      | { input = Trigger (Press mode); operation_active } ->
          begin_press mode operation_active
      | { input = Event event; operation_active } -> (
          match config.recognize event with
          | Some (Press_binding binding) ->
              begin_press ~binding
                (Config.Stt.string_of_mode binding.mode)
                operation_active
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
    Result.map_error
      (fun e -> (e :> run_error))
      (Eio.Fiber.first state_loop (fun () -> Error (Eio.Promise.await fatal)))
  in
  let* _ = Ox.Concurrent.par ~sw input_loop state_loop in
  Ok ()
