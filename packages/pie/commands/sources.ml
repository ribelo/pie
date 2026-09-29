let ( let* ) = Result.bind

let run ~caps ~sw:_ ~net ~clock ~logger () =
  Pulse.Client.with_client ~caps ~net ~clock ~logger @@ fun client ->
  let* sources = Pulse.Client.list_sources client in
  List.iter
    (fun (s : Pulse.Defs.source_info) ->
      let name = Option.value s.name ~default:"(no name)" in
      let desc = Option.value s.description ~default:"" in
      Ox_caps.Cap_stdlib.printf ~caps "%d: %s — %s (%d Hz, %d ch)\n%!" s.index
        name desc s.sample_spec.rate s.sample_spec.channels)
    sources;
  Ok ()
