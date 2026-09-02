open Eta_component
open Eta.Syntax

let family =
  Component.Family.create ~name:"rpc-server"
    ~module_locator:"Pie.Assistant.Rpc_server" ()

let component =
  match
    Component.make ~family ~config_equal:Config_slices.rpc_server_equal
      ~requirements:(Requirement.one Coeffects.recording_state)
      ~provisions:(Provision.one Coeffects.control_events)
      ~pp_error:(fun ppf msg -> Format.pp_print_string ppf msg)
      ~activate:(fun _config coordinator activation ->
        let control_stream =
          match try Some (Env.get_control ()) with Failure _ -> None with
          | Some s -> s
          | None -> Eio.Stream.create 1024
        in
        let* _ =
          Activation.own activation
            ~acquire:(Eta.Effect.pure control_stream)
            ~release:(fun _ -> Eta.Effect.unit)
            ~pp_release_error:(fun ppf _ ->
              Format.pp_print_string ppf "rpc release failed")
        in
        ignore coordinator;
        Eta.Effect.pure control_stream)
  with
  | Ok c -> c
  | Error e ->
      let msg =
        match e with
        | Component.Duplicate_requirement s -> "duplicate requirement: " ^ s
        | Duplicate_provision s -> "duplicate provision: " ^ s
        | Self_dependency s -> "self dependency: " ^ s
      in
      failwith ("rpc-server component make failed: " ^ msg)

let entry_id =
  match Entry_id.of_string "rpc-server" with
  | Ok id -> id
  | Error _ -> failwith "invalid entry id rpc-server"

let make_entry config =
  Desired_state.Entry.make ~id:entry_id ~component ~config ~enabled:true
    ~context:Desired_state.Context_spec.empty
