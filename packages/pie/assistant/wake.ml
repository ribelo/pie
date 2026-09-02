open Eta_component
open Eta.Syntax

let family =
  Component.Family.create ~name:"wakeword" ~module_locator:"Pie.Assistant.Wake"
    ()

let component =
  match
    Component.make ~family ~config_equal:Config_slices.wakeword_equal
      ~requirements:(Requirement.one Coeffects.audio_stream)
      ~provisions:(Provision.one Coeffects.wake_events)
      ~pp_error:(fun ppf msg -> Format.pp_print_string ppf msg)
      ~activate:(fun
          (config : Config_slices.wakeword_config) audio_stream activation ->
        if not config.enabled then
          let inert : Coeffects.wake_event Eio.Stream.t =
            Eio.Stream.create 16
          in
          let* _ =
            Activation.own activation ~acquire:(Eta.Effect.pure inert)
              ~release:(fun _ -> Eta.Effect.unit)
              ~pp_release_error:(fun ppf _ ->
                Format.pp_print_string ppf "wakeword release failed")
          in
          Eta.Effect.pure inert
        else
          let wake_stream : Coeffects.wake_event Eio.Stream.t =
            Eio.Stream.create 16
          in
          let detector = Wakeword.Pipeline.create_detector () in
          let read_loop =
            Eta_stream.run_for_each
              (fun chunk ->
                Eta.Effect.sync (fun () ->
                    let score = Wakeword.Pipeline.process detector chunk in
                    if score >= detector.model.threshold then
                      try Eio.Stream.add wake_stream Coeffects.Wake
                      with _ -> ()))
              audio_stream
            |> Eta.Effect.ignore_errors
          in
          let* _ =
            Activation.own activation
              ~acquire:(Eta.Effect.pure wake_stream)
              ~release:(fun _ -> Eta.Effect.unit)
              ~pp_release_error:(fun ppf _ ->
                Format.pp_print_string ppf "wakeword release failed")
          in
          (* Background reader; owned so it cancels on withdrawal *)
          let* _ =
            Activation.own activation ~acquire:(Eta.Effect.pure ())
              ~release:(fun () -> Eta.Effect.unit)
              ~pp_release_error:(fun ppf _ ->
                Format.pp_print_string ppf "wakeword bg release failed")
          in
          ignore read_loop;
          Eta.Effect.pure wake_stream)
  with
  | Ok c -> c
  | Error e ->
      let msg =
        match e with
        | Component.Duplicate_requirement s -> "duplicate requirement: " ^ s
        | Duplicate_provision s -> "duplicate provision: " ^ s
        | Self_dependency s -> "self dependency: " ^ s
      in
      failwith ("wakeword component make failed: " ^ msg)

let entry_id =
  match Entry_id.of_string "wakeword" with
  | Ok id -> id
  | Error _ -> failwith "invalid entry id wakeword"

let make_entry config =
  Desired_state.Entry.make ~id:entry_id ~component ~config ~enabled:true
    ~context:Desired_state.Context_spec.empty
