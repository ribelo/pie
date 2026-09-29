(* /bin/sh is used because NixOS has no /bin/true. "exit 0" is equivalent. *)
let () =
  Eio_main.run @@ fun env ->
  let mgr = Eio.Stdenv.process_mgr env in
  for _ = 1 to 1000 do
    Eio.Process.run mgr [ "/bin/sh"; "-c"; "exit 0" ]
  done
