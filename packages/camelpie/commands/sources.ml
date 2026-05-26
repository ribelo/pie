let ( let* ) eff f = Eta.Effect.bind f eff

let run ~sw ~net ~clock () =
  Pulse.Client.with_client ~sw ~net ~clock @@ fun client ->
  let* sources = Pulse.Client.list_sources client in
  Eta.Effect.sync (fun () ->
    List.iter (fun (s : Pulse.Defs.source_info) ->
      let name = Option.value s.name ~default:"(no name)" in
      let desc = Option.value s.description ~default:"" in
      Printf.printf "%d: %s — %s (%d Hz, %d ch)\n%!"
        s.index name desc s.sample_spec.rate s.sample_spec.channels
    ) sources)
