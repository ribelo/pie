open Eta_component

(* Audio stream: bounded PCM chunk stream *)
type audio_stream =
  (bytes, [ `Pulse_error of Pulse.Client.error ]) Eta_stream.Stream.t

let audio_stream : audio_stream Coeffect.t =
  Coeffect.create ~name:"audio_stream" ~equivalent:( == ) ()

(* Key events: keyboard event stream *)
type key_events = Keyboard.Monitor.key_event Eio.Stream.t

let key_events : key_events Coeffect.t =
  Coeffect.create ~name:"key_events" ~equivalent:( == ) ()

(* Recording state *)
type recording_state = Recording.Coordinator.t

let recording_state : recording_state Coeffect.t =
  Coeffect.create ~name:"recording_state" ~equivalent:( == ) ()

(* STT factory: record of transcribe/translate session functions *)
type stt_factory = {
  transcribe : bytes -> (string, [ `Stt_error of string ]) Eta.Effect.t;
  translate : bytes -> (string, [ `Stt_error of string ]) Eta.Effect.t;
  provider : Config.Stt.provider;
}

let stt_equal a b =
  a.provider = b.provider
  && a.transcribe == b.transcribe
  && a.translate == b.translate

let stt : stt_factory Coeffect.t =
  Coeffect.create ~name:"stt" ~equivalent:stt_equal ()

(* Wake events *)
type wake_event = Wake

let wake_events : wake_event Eio.Stream.t Coeffect.t =
  Coeffect.create ~name:"wake_events" ~equivalent:( == ) ()

(* Control events: trigger stream for ptt-loop *)
type control_events = Ptt_loop.trigger Eio.Stream.t

let control_events : control_events Coeffect.t =
  Coeffect.create ~name:"control_events" ~equivalent:( == ) ()
