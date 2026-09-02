open Eta_component
open Eta.Syntax

let family =
  Component.Family.create ~name:"keyboard-monitor"
    ~module_locator:"Pie.Assistant.Keyboard_monitor" ()

let component =
  match
    Component.make ~family ~config_equal:Config_slices.keyboard_monitor_equal
      ~requirements:Requirement.none
      ~provisions:(Provision.one Coeffects.key_events)
      ~pp_error:(fun ppf msg -> Format.pp_print_string ppf msg)
      ~activate:(fun _config () activation ->
        match try Some (Env.get_keyboard ()) with Failure _ -> None with
        | Some stream ->
            let* _ =
              Activation.own activation ~acquire:(Eta.Effect.pure ())
                ~release:(fun () -> Eta.Effect.unit)
                ~pp_release_error:(fun ppf _ ->
                  Format.pp_print_string ppf "keyboard release failed")
            in
            Eta.Effect.pure stream
        | None ->
            let t = Keyboard.Monitor.create () in
            let stream = Keyboard.Monitor.subscribe t in
            let* _ =
              Activation.own activation ~acquire:(Eta.Effect.pure t)
                ~release:(fun _ -> Eta.Effect.unit)
                ~pp_release_error:(fun ppf _ ->
                  Format.pp_print_string ppf "keyboard release failed")
            in
            Eta.Effect.pure stream)
  with
  | Ok c -> c
  | Error e ->
      let msg =
        match e with
        | Component.Duplicate_requirement s -> "duplicate requirement: " ^ s
        | Duplicate_provision s -> "duplicate provision: " ^ s
        | Self_dependency s -> "self dependency: " ^ s
      in
      failwith ("keyboard-monitor component make failed: " ^ msg)

let entry_id =
  match Entry_id.of_string "keyboard-monitor" with
  | Ok id -> id
  | Error _ -> failwith "invalid entry id keyboard-monitor"

let make_entry config =
  Desired_state.Entry.make ~id:entry_id ~component ~config ~enabled:true
    ~context:Desired_state.Context_spec.empty
