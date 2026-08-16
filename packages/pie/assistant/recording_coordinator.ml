open Eta_component
open Eta.Syntax

let family = Component.Family.create ~name:"recording-coordinator" ~module_locator:"Pie.Assistant.Recording_coordinator" ()

let component =
  match
    Component.make
      ~family
      ~config_equal:Config_slices.recording_coordinator_equal
      ~requirements:Requirement.none
      ~provisions:(Provision.one Coeffects.recording_state)
      ~pp_error:(fun ppf msg -> Format.pp_print_string ppf msg)
      ~activate:(fun _config () activation ->
        match !Env.coordinator_ref with
        | Some coordinator ->
          let* _ =
            Activation.own activation
              ~acquire:(Eta.Effect.pure ())
              ~release:(fun () -> Eta.Effect.unit)
              ~pp_release_error:(fun ppf _ -> Format.pp_print_string ppf "recording release failed")
          in
          Eta.Effect.pure coordinator
        | None ->
          let coordinator = Recording.Coordinator.create () in
          let* _ =
            Activation.own activation
              ~acquire:(Eta.Effect.pure coordinator)
              ~release:(fun _ -> Eta.Effect.unit)
              ~pp_release_error:(fun ppf _ -> Format.pp_print_string ppf "recording release failed")
          in
          Eta.Effect.pure coordinator
      )
  with
  | Ok c -> c
  | Error e ->
    let msg = match e with
      | Component.Duplicate_requirement s -> "duplicate requirement: " ^ s
      | Duplicate_provision s -> "duplicate provision: " ^ s
      | Self_dependency s -> "self dependency: " ^ s
    in
    failwith ("recording-coordinator component make failed: " ^ msg)

let entry_id =
  match Entry_id.of_string "recording-coordinator" with
  | Ok id -> id
  | Error _ -> failwith "invalid entry id recording-coordinator"

let make_entry config =
  Desired_state.Entry.make
    ~id:entry_id
    ~component
    ~config
    ~enabled:true
    ~context:Desired_state.Context_spec.empty
