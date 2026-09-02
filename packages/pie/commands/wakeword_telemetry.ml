open Eta.Syntax

let run ~sw:_ ~net ~clock:_ () =
  Pulse.Client.with_client ~net @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun _info audio ->
  let detector = Wakeword.Pipeline.create_detector () in
  let* () =
    Eta.Effect.sync (fun () ->
        Printf.printf "Wakeword telemetry (synthetic scoring)\n%!";
        Printf.printf "threshold=%.2f smoothing=%.2f (Ctrl-C to stop)...\n%!"
          detector.model.threshold detector.model.smoothing)
  in
  Eta_stream.run_for_each
    (fun chunk ->
      Eta.Effect.sync (fun () ->
          let score = Wakeword.Pipeline.process detector chunk in
          let bar_len = int_of_float (score *. 80.0) in
          let bar = String.make (min bar_len 80) '#' in
          let trigger_marker =
            if detector.triggered then " *** TRIGGER ***" else ""
          in
          Printf.printf "\rScore: %.4f | [%s]%s%!" score bar trigger_marker))
    audio
