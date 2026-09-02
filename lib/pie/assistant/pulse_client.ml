open Eta_component
open Eta.Syntax

let family = Component.Family.create ~name:"pulse-client" ~module_locator:"Pie.Assistant.Pulse_client" ()

let component =
  match
    Component.make
      ~family
      ~config_equal:Config_slices.pulse_client_equal
      ~requirements:Requirement.none
      ~provisions:(Provision.one Coeffects.audio_stream)
      ~pp_error:(fun ppf msg -> Format.pp_print_string ppf msg)
      ~activate:(fun _config () activation ->
        match (try Some (Env.get_audio ()) with Failure _ -> None) with
        | Some stream ->
          let* _ =
            Activation.own activation
              ~acquire:(Eta.Effect.pure ())
              ~release:(fun () -> Eta.Effect.unit)
              ~pp_release_error:(fun ppf _ -> Format.pp_print_string ppf "pulse release failed")
          in
          Eta.Effect.pure stream
        | None ->
          let queue = Eta.Queue.bounded ~capacity:Pulse.Client.record_stream_capacity () in
          let stream = Eta_stream.Stream.from_queue queue in
          let* _ =
            Activation.own activation
              ~acquire:(Eta.Effect.pure queue)
              ~release:(fun q ->
                Eta.Effect.sync (fun () -> Eta.Queue.close q))
              ~pp_release_error:(fun ppf _ -> Format.pp_print_string ppf "pulse release failed")
          in
          Eta.Effect.pure stream
      )
  with
  | Ok c -> c
  | Error e ->
    let msg = match e with
      | Component.Duplicate_requirement s -> "duplicate requirement: " ^ s
      | Duplicate_provision s -> "duplicate provision: " ^ s
      | Self_dependency s -> "self dependency: " ^ s
    in
    failwith ("pulse-client component make failed: " ^ msg)

let entry_id =
  match Entry_id.of_string "pulse-client" with
  | Ok id -> id
  | Error _ -> failwith "invalid entry id pulse-client"

let make_entry config =
  Desired_state.Entry.make
    ~id:entry_id
    ~component
    ~config
    ~enabled:true
    ~context:Desired_state.Context_spec.empty
