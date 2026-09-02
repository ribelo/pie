open Eta_component
open Eta.Syntax

let family = Component.Family.create ~name:"stt-backend" ~module_locator:"Pie.Assistant.Stt_backend" ()

let component =
  match
    Component.make
      ~family
      ~config_equal:Config_slices.stt_backend_equal
      ~requirements:Requirement.none
      ~provisions:(Provision.one Coeffects.stt)
      ~pp_error:(fun ppf msg -> Format.pp_print_string ppf msg)
      ~activate:(fun (config : Config_slices.stt_backend_config) () _activation ->
        let factory : Coeffects.stt_factory = {
          provider = config.provider;
          transcribe = (fun _pcm_bytes ->
            Eta.Effect.pure "transcribed text");
          translate = (fun _pcm_bytes ->
            Eta.Effect.pure "translated text");
        } in
        Eta.Effect.pure factory
      )
  with
  | Ok c -> c
  | Error e ->
    let msg = match e with
      | Component.Duplicate_requirement s -> "duplicate requirement: " ^ s
      | Duplicate_provision s -> "duplicate provision: " ^ s
      | Self_dependency s -> "self dependency: " ^ s
    in
    failwith ("stt-backend component make failed: " ^ msg)

let entry_id =
  match Entry_id.of_string "stt-backend" with
  | Ok id -> id
  | Error _ -> failwith "invalid entry id stt-backend"

let make_entry config =
  Desired_state.Entry.make
    ~id:entry_id
    ~component
    ~config
    ~enabled:true
    ~context:Desired_state.Context_spec.empty
