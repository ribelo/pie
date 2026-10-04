(* Private read-only probe. Not a product CLI and not --no-build. *)

open Build_record_lib

let main
    ~(caps :
       < Ox_caps.Cap.env
       ; Ox_caps.Cap.exec
       ; Ox_caps.Cap.exit
       ; Ox_caps.Cap.fork
       ; Ox_caps.Cap.fs_read
       ; Ox_caps.Cap.fs_write
       ; Ox_caps.Cap.stderr
       ; Ox_caps.Cap.stdout
       ; Ox_caps.Cap.tmp
       ; .. >) =
  let root =
    if Array.length Sys.argv <> 2 then (
      Ox_caps.Cap_stdlib.prerr_endline ~caps "usage: preflight ROOT";
      Ox_caps.Cap_stdlib.exit ~caps 2)
    else Sys.argv.(1)
  in
  Ox_caps.Cap_stdlib.printf ~caps "source_fingerprint %s\n%!"
    (Provenance.source_fingerprint ~caps root);
  let cmxa = Provenance.find_ox_cmxa_env ~caps () in
  Ox_caps.Cap_stdlib.printf ~caps "visible_ox_cmxa %d\n%!" (List.length cmxa);
  List.iter
    (fun p ->
      Ox_caps.Cap_stdlib.printf ~caps "cmxa %s %s\n%!"
        (Provenance.sha256_file ~caps p)
        p)
    cmxa;
  let native = Provenance.visible_native_inputs ~caps cmxa in
  Ox_caps.Cap_stdlib.printf ~caps "visible_native_inputs %d\n%!"
    (List.length native);
  List.iter
    (fun p ->
      Ox_caps.Cap_stdlib.printf ~caps "native %s %s\n%!"
        (Provenance.sha256_file ~caps p)
        p)
    native;
  match Provenance.find_tls_stub ~caps () with
  | None -> Ox_caps.Cap_stdlib.print_endline ~caps "tls_dll none"
  | Some path ->
      Ox_caps.Cap_stdlib.printf ~caps "tls_dll %s %s\n%!"
        (Provenance.sha256_file ~caps path)
        path

let () = main ~caps:(Ox_test.powerbox ())
