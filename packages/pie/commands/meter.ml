let run ~sw:_ ~net ~clock ~logger () =
  Pulse.Client.with_client ~net ~clock ~logger @@ fun client ->
  Pulse.Client.with_record_stream client @@ fun _info audio ->
  Printf.printf "Meter running (Ctrl-C to stop)...\n%!";
  let rec loop () =
    match audio () with
    | Error _ as error -> error
    | Ok None -> Ok ()
    | Ok (Some chunk) ->
        let rms = Audio.Pcm.pcm_rms chunk in
        let peak = Audio.Pcm.pcm_peak chunk in
        let bar_width = int_of_float (rms *. 80.0) in
        let bar = String.make (min bar_width 80) '#' in
        Printf.printf "\rRMS: %.4f | Peak: %.4f | [%s]%!" rms peak bar;
        if rms > 0.99 then Printf.printf " CLIP%!";
        Printf.printf "     %!";
        loop ()
  in
  loop ()
