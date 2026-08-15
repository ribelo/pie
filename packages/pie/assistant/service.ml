open Eta.Syntax

(* Assistant service orchestrated via Eta_component *)

let default_min_duration_ms = 120

let pp_admission_error ppf = function
  | Eta_component.Context.Context_not_running -> Format.pp_print_string ppf "Context_not_running"
  | Duplicate_entry_id id -> Format.fprintf ppf "Duplicate_entry_id:%a" Eta_component.Entry_id.pp id
  | Entry_kind_changed id -> Format.fprintf ppf "Entry_kind_changed:%a" Eta_component.Entry_id.pp id
  | Duplicate_provider { coeffect; realm; entries } ->
    Format.fprintf ppf "Duplicate_provider:%s:%s:%s" coeffect realm (String.concat "," (List.map (fun id -> Format.asprintf "%a" Eta_component.Entry_id.pp id) entries))
  | Dependency_cycle ids ->
    Format.fprintf ppf "Dependency_cycle:%s" (String.concat "," (List.map (fun id -> Format.asprintf "%a" Eta_component.Entry_id.pp id) ids))
  | Callback_failed { callback; failure } ->
    let cb = match callback with Configuration_equivalence -> "Configuration_equivalence" | Interception_merge s -> "Interception_merge:" ^ s in
    Format.fprintf ppf "Callback_failed:%s:%a" cb Eta_component.Diagnostics.Failure.pp failure
  | Retry_not_available id -> Format.fprintf ppf "Retry_not_available:%a" Eta_component.Entry_id.pp id
  | Stale_source_revision src -> Format.fprintf ppf "Stale_source_revision:%a" Eta_component.Source_revision.pp src
  | Stale_entry_incarnation id -> Format.fprintf ppf "Stale_entry_incarnation:%a" Eta_component.Entry_id.pp id
  | Stale_target_revision id -> Format.fprintf ppf "Stale_target_revision:%a" Eta_component.Entry_id.pp id
  | Wrong_target_context id -> Format.fprintf ppf "Wrong_target_context:%a" Eta_component.Entry_id.pp id
  | Component_identity_mismatch id -> Format.fprintf ppf "Component_identity_mismatch:%a" Eta_component.Entry_id.pp id
  | Quarantined_instance id -> Format.fprintf ppf "Quarantined_instance:%a" Eta_component.Entry_id.pp id

(* Global state for components_field reporting *)
let admission_error_ref : string option ref = ref None
let last_fence_report_ref : Eta_component.Diagnostics.Fence.report option ref = ref None
let components_cache_ref : Assistant_service.Contract.components_field option ref = ref None

let phase_to_string = function
  | Eta_component.Diagnostics.Inactive -> "inactive"
  | Waiting -> "waiting"
  | Activating _ -> "activating"
  | Active _ -> "active"
  | Settling _ -> "settling"
  | Activation_failed (_, _) -> "activation-failed"
  | Recovery_failed (_, _) -> "recovery-failed"

let integrity_to_string = function
  | Eta_component.Diagnostics.Sound -> "Sound"
  | Degraded ids ->
    "Degraded:" ^ String.concat "," (List.map (fun id -> Format.asprintf "%a" Eta_component.Diagnostics.Instance_id.pp id) ids)
  | Failed failure ->
    Format.asprintf "Failed:%a" Eta_component.Diagnostics.Failure.pp failure

let build_components_field snapshot =
  let entries =
    snapshot
    |> Eta_component.Diagnostics.instances
    |> List.map (fun inst ->
      let id = Format.asprintf "%a" Eta_component.Entry_id.pp (Eta_component.Diagnostics.entry_id inst) in
      let phase = phase_to_string (Eta_component.Diagnostics.phase inst) in
      (id, phase))
  in
  let integrity = integrity_to_string (Eta_component.Diagnostics.integrity snapshot) in
  let degraded =
    snapshot
    |> Eta_component.Diagnostics.instances
    |> List.filter (fun inst ->
      match Eta_component.Diagnostics.phase inst with
      | Eta_component.Diagnostics.Active _ -> false
      | _ -> true)
    |> List.map (fun inst -> Format.asprintf "%a" Eta_component.Entry_id.pp (Eta_component.Diagnostics.entry_id inst))
  in
  let failure =
    match Eta_component.Diagnostics.integrity snapshot with
    | Failed f ->
      Some (Format.asprintf "%a" Eta_component.Diagnostics.Failure.pp f)
    | Degraded _ | Sound -> None
  in
  let fence_outcome, fence_complete =
    match !last_fence_report_ref with
    | None -> None, None
    | Some report ->
      let outcome =
        match Eta_component.Diagnostics.Fence.outcome report with
        | Quiescent -> "Quiescent"
        | Superseded -> "Superseded"
        | Rolled_back -> "Rolled_back"
        | Degraded -> "Degraded"
        | Restoration_failed -> "Restoration_failed"
        | Context_failed -> "Context_failed"
      in
      Some outcome, Some true
  in
  let admission_error = !admission_error_ref in
  ({
    entries;
    integrity;
    degraded;
    failure;
    fence_outcome;
    fence_complete;
    admission_error;
  } : Assistant_service.Contract.components_field)

let update_components_cache snapshot =
  let field = build_components_field snapshot in
  components_cache_ref := Some field;
  Assistant_service.Server.set_components_provider (fun () -> !components_cache_ref)

(* Config watcher helpers, similar to Commands.Assistant *)
let config_path = Paths.stt_config_path

let watched_paths () =
  let prompts_dir = Filename.concat (Filename.dirname config_path) "prompts" in
  [
    config_path;
    Filename.concat prompts_dir "transcription.md";
    Filename.concat prompts_dir "translation.md";
  ]

let stamp path =
  try
    let stat = Unix.stat path in
    Some (stat.Unix.st_dev, stat.Unix.st_ino, stat.Unix.st_size, stat.Unix.st_mtime)
  with Unix.Unix_error _ -> None

let config_stamp () = List.map stamp (watched_paths ())

let load_snapshot attempts =
  let before = config_stamp () in
  let config = Config.Stt.load ~config_path () in
  let after = config_stamp () in
  if before = after then config, after
  else if attempts > 0 then
    let rec retry n =
      let b = config_stamp () in
      let c = Config.Stt.load ~config_path () in
      let a = config_stamp () in
      if b = a then c, a else if n > 0 then retry (n-1) else raise (Config.Stt.Config_error "config changed repeatedly while loading")
    in
    retry (attempts-1)
  else raise (Config.Stt.Config_error "config changed repeatedly while loading")

let _effective_bindings
    ?ptt_transcribe_keysym
    ?ptt_translate_keysym
    (config : Config.Stt.t) =
  let transcribe =
    Option.value ~default:config.ptt_transcribe_keysym ptt_transcribe_keysym
  in
  let translate =
    Option.value ~default:config.ptt_translate_keysym ptt_translate_keysym
  in
  if transcribe <= 0 then
    raise (Config.Stt.Config_error "effective PTT transcribe keysym must be positive");
  if translate <= 0 then
    raise (Config.Stt.Config_error "effective PTT translate keysym must be positive");
  transcribe, translate

let run
    ~sw
    ~net
    ~clock:_clock
    ~process_mgr:_process_mgr
    ?ptt_transcribe_keysym
    ?ptt_translate_keysym
    ?(min_duration_ms = default_min_duration_ms)
    ?stdin:_stdin
    () =
  (* Load initial config with stamp handling *)
  let* (initial_config, initial_stamp) =
    Eta.Effect.sync (fun () ->
      try Ok (Eio_unix.run_in_systhread ~label:"pie initial config load" (fun () -> load_snapshot 3))
      with
      | Config.Stt.Config_error message ->
        Error (`Pulse_error (Pulse.Client.Protocol_error ("Failed to load STT config: " ^ message)))
      | exn ->
        Error (`Pulse_error (Pulse.Client.Protocol_error ("Failed to load STT config: " ^ Printexc.to_string exn))))
    |> Eta.Effect.flatten_result
  in
  let current = ref initial_config in
  let last_stamp = ref initial_stamp in
  let source_revision_counter = ref 0L in
  (* Setup RPC server outside component context to keep it after context stops? But spec says OTLP before/after, not RPC.
     For component tree, rpc-server entry provides control_events, but we start the global server here and the entry provides dummy stream.
     The server's control stream is shared via global. *)
  let coordinator = Recording.Coordinator.create () in
  let control : Ptt_loop.trigger Eio.Stream.t = Eio.Stream.create 1024 in
  let send_control trigger = Eio.Stream.add control trigger in
  let* () =
    Eta.Effect.sync (fun () ->
      Assistant_service.Server.start ~sw ~net
        ~action_handler:(fun raw_action ->
          let action = raw_action |> String.trim |> String.lowercase_ascii in
          let snap = Assistant_service.Server.snapshot_of_coordinator coordinator in
          let enqueue trigger message =
            send_control trigger;
            { Assistant_service.Contract.action = raw_action; accepted = true; message; snapshot = snap }
          in
          match action with
          | "status" -> { Assistant_service.Contract.action = raw_action; accepted = true; message = None; snapshot = snap }
          | "pause" ->
            Recording.Coordinator.set_enabled coordinator false;
            { action = raw_action; accepted = true; message = None; snapshot = snap }
          | "resume" ->
            Recording.Coordinator.set_enabled coordinator true;
            { action = raw_action; accepted = true; message = None; snapshot = snap }
          | "toggle" ->
            ignore (Recording.Coordinator.toggle_enabled coordinator);
            { action = raw_action; accepted = true; message = None; snapshot = snap }
          | "ptt-start" | "dictation-start" -> enqueue (Ptt_loop.Press "transcribe") (Some "ptt transcribe start queued")
          | "ptt-stop" | "dictation-stop" -> enqueue (Ptt_loop.Release "transcribe") (Some "ptt transcribe stop queued")
          | "ptt-toggle" | "dictation-toggle" ->
            let snap = Recording.Coordinator.snapshot coordinator in
            if snap.active && snap.mode = Recording.Coordinator.string_of_mode Recording.Coordinator.Ptt_transcribe
            then enqueue (Ptt_loop.Release "transcribe") (Some "ptt transcribe stop queued")
            else enqueue (Ptt_loop.Press "transcribe") (Some "ptt transcribe start queued")
          | "ptt-translate-start" -> enqueue (Ptt_loop.Press "translate") (Some "ptt translate start queued")
          | "ptt-translate-stop" -> enqueue (Ptt_loop.Release "translate") (Some "ptt translate stop queued")
          | "meeting-start" ->
            let result = Recording.Coordinator.try_start coordinator Recording.Coordinator.Meeting_transcribe () in
            let accepted = match result with Recording.Coordinator.Started _ -> true | _ -> false in
            { action = raw_action; accepted; message = None; snapshot = snap }
          | "meeting-stop" ->
            let stopped = Recording.Coordinator.stop coordinator Recording.Coordinator.Meeting_transcribe in
            { action = raw_action; accepted = stopped; message = None; snapshot = snap }
          | "meeting-toggle" ->
            ignore (Recording.Coordinator.toggle_meeting coordinator);
            { action = raw_action; accepted = true; message = None; snapshot = snap }
          | _ ->
            { action = raw_action; accepted = false; message = Some ("unknown action: " ^ raw_action); snapshot = snap }
        )
        coordinator;
      Observability.info "assistant_service started" ~attrs:[("config.path", config_path)]
    )
  in
  (* Initialize components provider to empty before context *)
  Assistant_service.Server.set_components_provider (fun () -> !components_cache_ref);
  let initial_tree = Tree.build ~min_duration_ms !current in
  let context_body (context : Eta_component.Context.t) (diagnostics : Eta_component.Diagnostics.t) =
    let* initial_fence =
      Eta_component.Context.reconcile context initial_tree
      |> Eta.Effect.map_error (fun err ->
        let msg = Format.asprintf "%a" pp_admission_error err in
        admission_error_ref := Some msg;
        Observability.error "initial reconcile admission failed" ~attrs:[("error", msg)];
        `Pulse_error (Pulse.Client.Protocol_error msg))
      |> Eta.Effect.fold ~ok:(fun f -> Some f) ~error:(fun _ -> None)
    in
    let* () =
      match initial_fence with
      | None -> Eta.Effect.unit
      | Some fence ->
        let* report =
          Eta_component.Diagnostics.Fence.await fence
          |> Eta.Effect.fold ~ok:(fun r -> Some r) ~error:(fun _ -> None)
        in
        (match report with
         | Some r -> last_fence_report_ref := Some r
         | None -> ());
        let* snapshot_opt =
          Eta_component.Diagnostics.snapshot diagnostics
          |> Eta.Effect.fold ~ok:(fun s -> Some s) ~error:(fun _ -> None)
        in
        (match snapshot_opt with
         | Some s -> Eta.Effect.sync (fun () -> update_components_cache s) |> Eta.Effect.ignore_errors
         | None -> Eta.Effect.unit)
    in
    (* Components updater fiber: watches diagnostics change and updates cache *)
    let components_updater =
      let rec loop () =
        let* snapshot_opt =
          Eta_component.Diagnostics.snapshot diagnostics
          |> Eta.Effect.fold ~ok:(fun s -> Some s) ~error:(fun _ -> None)
        in
        match snapshot_opt with
        | None -> Eta.Effect.unit
        | Some snap ->
          let rev = Eta_component.Diagnostics.revision snap in
          let* change_opt =
            Eta_component.Diagnostics.await_change diagnostics ~after:rev
            |> Eta.Effect.fold ~ok:(fun c -> Some c) ~error:(fun _ -> None)
          in
          match change_opt with
          | None -> Eta.Effect.unit
          | Some (Eta_component.Diagnostics.Closed final_snap) ->
            Eta.Effect.sync (fun () -> update_components_cache final_snap) |> Eta.Effect.ignore_errors
          | Some (Eta_component.Diagnostics.Changed new_snap) ->
            let* () = Eta.Effect.sync (fun () -> update_components_cache new_snap) |> Eta.Effect.ignore_errors in
            loop ()
      in
      loop ()
    in
    let watcher_router =
      let rec loop () =
        let* () = Eta.Effect.delay (Eta.Duration.ms 500) Eta.Effect.unit in
        let expected_stamp = !last_stamp in
        let* result =
          Eta.Effect.sync (fun () ->
            Eio_unix.run_in_systhread ~label:"pie config reload" (fun () ->
              let next_stamp = config_stamp () in
              if next_stamp = expected_stamp then `Unchanged
              else
                try
                  let config, loaded_stamp = load_snapshot 3 in
                  `Loaded (config, loaded_stamp)
                with exn -> `Failed (next_stamp, Printexc.to_string exn)))
        in
        let* () =
          match result with
          | `Unchanged -> Eta.Effect.unit
          | `Failed (failed_stamp, message) ->
            Eta.Effect.sync (fun () ->
              last_stamp := failed_stamp;
              Observability.error "Pie config reload failed; keeping previous config"
                ~attrs:[("config.path", config_path); ("config.error", message)])
          | `Loaded (new_config, loaded_stamp) ->
            let old_config = !current in
            let is_provider_only = Config_slices.is_only_provider_change old_config new_config in
            if is_provider_only then (
              (* Replacement path *)
              let* snapshot_opt =
                Eta_component.Diagnostics.snapshot diagnostics
                |> Eta.Effect.fold ~ok:(fun s -> Some s) ~error:(fun _ -> None)
              in
              match snapshot_opt with
              | None ->
                Eta.Effect.sync (fun () ->
                  Observability.error "replacement: failed to get snapshot" ~attrs:[])
              | Some snapshot ->
                let instances = Eta_component.Diagnostics.instances snapshot in
                let stt_instance_opt =
                  List.find_opt (fun inst ->
                    Eta_component.Entry_id.equal
                      (Eta_component.Diagnostics.entry_id inst)
                      Stt_backend.entry_id
                  ) instances
                in
                match stt_instance_opt with
                | None ->
                  Eta.Effect.sync (fun () ->
                    Observability.error "replacement: stt-backend instance not found" ~attrs:[])
                | Some stt_inst ->
                  let expected_instance = Eta_component.Diagnostics.instance_id stt_inst in
                  let expected_target =
                    match Eta_component.Diagnostics.target_revision stt_inst with
                    | Some tr -> tr
                    | None ->
                      (* If no target yet, use a dummy; batch will fail admission appropriately *)
                      Obj.magic ()
                  in
                  let stt_slice = Config_slices.stt_backend_slice new_config in
                  let entry = Stt_backend.make_entry stt_slice in
                  let target = Eta_component.Replacement.target ~entry ~expected_instance ~expected_target in
                  let candidate_result = Eta_component.Replacement.candidate ~target ~component:Stt_backend.component in
                  (match candidate_result with
                   | Error e ->
                     let msg = match e with Eta_component.Replacement.Component_identity_mismatch id ->
                       Format.asprintf "candidate identity mismatch %a" Eta_component.Entry_id.pp id
                     in
                     Eta.Effect.sync (fun () ->
                       admission_error_ref := Some msg;
                       Observability.error "replacement candidate failed" ~attrs:[("error", msg)])
                   | Ok candidate ->
                     let src = Eta_component.Source_revision.of_int64 !source_revision_counter in
                     source_revision_counter := Int64.succ !source_revision_counter;
                     let batch_result = Eta_component.Replacement.batch ~source_revision:src [candidate] in
                     (match batch_result with
                      | Error e ->
                        let msg = match e with
                          | Empty_batch -> "empty batch"
                          | Duplicate_entry id -> Format.asprintf "duplicate entry %a" Eta_component.Entry_id.pp id
                        in
                        Eta.Effect.sync (fun () ->
                          admission_error_ref := Some msg;
                          Observability.error "replacement batch failed" ~attrs:[("error", msg)])
                      | Ok batch ->
                        let* fence_opt =
                          Eta_component.Context.replace context batch
                          |> Eta.Effect.fold ~ok:(fun f -> Some f) ~error:(fun err ->
                            let msg = Format.asprintf "%a" pp_admission_error err in
                            admission_error_ref := Some msg;
                            Observability.error "replacement admission failed" ~attrs:[("error", msg)];
                            None)
                        in
                        match fence_opt with
                        | None -> Eta.Effect.unit
                        | Some fence ->
                          let* report_opt =
                            Eta_component.Diagnostics.Fence.await fence
                            |> Eta.Effect.fold ~ok:(fun r -> Some r) ~error:(fun _ -> None)
                          in
                          (match report_opt with
                           | Some report ->
                             Eta.Effect.sync (fun () ->
                               last_fence_report_ref := Some report;
                               (match Eta_component.Diagnostics.Fence.outcome report with
                                | Rolled_back ->
                                  Observability.info "stt-backend replacement rolled back" ~attrs:[]
                                | Quiescent ->
                                  current := new_config;
                                  last_stamp := loaded_stamp;
                                  admission_error_ref := None;
                                  Observability.info "Pie config reloaded via replacement" ~attrs:[("config.path", config_path)]
                                | _ ->
                                  current := new_config;
                                  last_stamp := loaded_stamp;
                                  admission_error_ref := None))
                           | None -> Eta.Effect.unit)
                    )
                )
            ) else (
              (* Reconcile path *)
              let new_tree = Tree.build ~min_duration_ms new_config in
              let* fence_opt =
                Eta_component.Context.reconcile context new_tree
                |> Eta.Effect.fold ~ok:(fun f -> Some f) ~error:(fun err ->
                  let msg = Format.asprintf "%a" pp_admission_error err in
                  admission_error_ref := Some msg;
                  Observability.error "reconcile admission failed" ~attrs:[("error", msg)];
                  None)
              in
              match fence_opt with
              | None -> Eta.Effect.unit
              | Some fence ->
                let* report_opt =
                  Eta_component.Diagnostics.Fence.await fence
                  |> Eta.Effect.fold ~ok:(fun r -> Some r) ~error:(fun _ -> None)
                in
                (match report_opt with
                 | Some report ->
                   Eta.Effect.sync (fun () ->
                     last_fence_report_ref := Some report;
                     (match Eta_component.Diagnostics.Fence.outcome report with
                      | Quiescent ->
                        current := new_config;
                        last_stamp := loaded_stamp;
                        admission_error_ref := None;
                        Observability.info "Pie config reloaded via reconcile" ~attrs:[("config.path", config_path)]
                      | _ ->
                        Observability.info "reconcile completed non-quiescent" ~attrs:[]))
                 | None -> Eta.Effect.unit)
            )
        in
        loop ()
      in
      loop ()
    in
    let supervision = Supervision.run ~context ~diagnostics in
    (* Run watcher, supervision, and components updater concurrently; any failure or cancellation will propagate *)
    Eta.Effect.par (Eta.Effect.par watcher_router supervision) components_updater
    |> Eta.Effect.ignore_errors
  in
  (* OTLP is expected to be init before Context.run and kept after; we ensure ordering by calling init here if not already, and not shutting down until after *)
  (* The caller (bin/pie) already does init/shutdown, but we also ensure context withdrawal order *)
  Eta_component.Context.run context_body
  |> Eta.Effect.map_error (fun (`Pulse_error _ as e) -> e)
  (* Context.run will withdraw consumers before providers and await owned scopes *)

