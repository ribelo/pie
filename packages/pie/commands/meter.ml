open Eta.Syntax

let run ~sw:_ ~net ~clock:_ () =
  Pulse.Client.with_client ~net @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun _info audio ->
  let* () = Eta.Effect.sync (fun () ->
    Printf.printf "Meter running (Ctrl-C to stop)...\n%!")
  in
  Eta_stream.run_for_each
    (fun chunk ->
      Eta.Effect.sync (fun () ->
      let rms = Audio.Pcm.pcm_rms chunk in
      let peak = Audio.Pcm.pcm_peak chunk in
      let bar_width = int_of_float (rms *. 80.0) in
      let bar = String.make (min bar_width 80) '#' in
      Printf.printf "\rRMS: %.4f | Peak: %.4f | [%s]%!" rms peak bar;
      if rms > 0.99 then Printf.printf " CLIP%!";
      Printf.printf "     %!"))
    audio
