open Eta_component
open Eta.Syntax

let family =
  Component.Family.create ~name:"ptt-loop" ~module_locator:"Pie.Assistant.Ptt"
    ()

(* ptt-loop requires all six coeffects and provides none *)
let requirements =
  let open Requirement in
  both
    (both (one Coeffects.audio_stream) (one Coeffects.key_events))
    (both
       (both (one Coeffects.recording_state) (one Coeffects.stt))
       (both (one Coeffects.wake_events) (one Coeffects.control_events)))

let component =
  match
    Component.make ~family ~config_equal:Config_slices.ptt_loop_equal
      ~requirements ~provisions:Provision.none
      ~pp_error:(fun ppf msg -> Format.pp_print_string ppf msg)
      ~activate:(fun
          (config : Config_slices.ptt_loop_config) requirements activation ->
        (* Unpack requirements: ((audio, keys), ((recording, stt), (wake, control))) *)
        let ( (audio_stream, key_events),
              ((recording_state, stt_factory), (wake_events, control_events)) )
            =
          requirements
        in
        ignore
          ( audio_stream,
            key_events,
            recording_state,
            stt_factory,
            wake_events,
            control_events,
            config );
        (* In production, this would run Ptt_loop.run with the coeffect values.
           For now we just own a dummy and return unit, staying active. *)
        let* _ =
          Activation.own activation ~acquire:(Eta.Effect.pure ())
            ~release:(fun () -> Eta.Effect.unit)
            ~pp_release_error:(fun ppf _ ->
              Format.pp_print_string ppf "ptt-loop release failed")
        in
        Eta.Effect.pure ())
  with
  | Ok c -> c
  | Error e ->
      let msg =
        match e with
        | Component.Duplicate_requirement s -> "duplicate requirement: " ^ s
        | Duplicate_provision s -> "duplicate provision: " ^ s
        | Self_dependency s -> "self dependency: " ^ s
      in
      failwith ("ptt-loop component make failed: " ^ msg)

let entry_id =
  match Entry_id.of_string "ptt-loop" with
  | Ok id -> id
  | Error _ -> failwith "invalid entry id ptt-loop"

let make_entry config =
  Desired_state.Entry.make ~id:entry_id ~component ~config ~enabled:true
    ~context:Desired_state.Context_spec.empty
