let ( let* ) eff f = Eta.Effect.bind f eff

let run ~sw ~net ~clock () =
  Pulse.Client.with_client ~sw ~net ~clock @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun _info recv ->
  let* () = Eta.Effect.sync (fun () ->
    Printf.printf "Meter running (Ctrl-C to stop)...\n%!")
  in
  let rec loop () =
    let* chunk = recv () in
    let* () = Eta.Effect.sync (fun () ->
      let rms = Audio.Pcm.pcm_rms chunk in
      let peak = Audio.Pcm.pcm_peak chunk in
      let bar_width = int_of_float (rms *. 80.0) in
      let bar = String.make (min bar_width 80) '#' in
      Printf.printf "\rRMS: %.4f | Peak: %.4f | [%s]%!" rms peak bar;
      if rms > 0.99 then Printf.printf " CLIP%!";
      Printf.printf "     %!")
    in
    loop ()
  in
  loop ()
