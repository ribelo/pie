open Eta.Syntax

let run ~sw:_ ~net ~clock:_ () =
  Pulse.Client.with_client ~net @@ fun client ->
  let* sources = Pulse.Client.list_sources client in
  Eta.Effect.sync (fun () ->
    List.iter (fun (s : Pulse.Defs.source_info) ->
      let name = Option.value s.name ~default:"(no name)" in
      let desc = Option.value s.description ~default:"" in
      Printf.printf "%d: %s — %s (%d Hz, %d ch)\n%!"
        s.index name desc s.sample_spec.rate s.sample_spec.channels
    ) sources)
