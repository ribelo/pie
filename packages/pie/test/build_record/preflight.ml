(* Private read-only probe. Not a product CLI and not --no-build. *)

open Build_record_lib

let () =
  let root =
    if Array.length Sys.argv <> 2 then (
      prerr_endline "usage: preflight ROOT";
      exit 2)
    else Sys.argv.(1)
  in
  Printf.printf "source_fingerprint %s\n%!" (Provenance.source_fingerprint root);
  let cmxa = Provenance.find_eta_cmxa_env () in
  Printf.printf "visible_eta_cmxa %d\n%!" (List.length cmxa);
  List.iter
    (fun p -> Printf.printf "cmxa %s %s\n%!" (Provenance.sha256_file p) p)
    cmxa;
  let native = Provenance.visible_native_inputs cmxa in
  Printf.printf "visible_native_inputs %d\n%!" (List.length native);
  List.iter
    (fun p -> Printf.printf "native %s %s\n%!" (Provenance.sha256_file p) p)
    native;
  match Provenance.find_tls_stub () with
  | None -> print_endline "tls_dll none"
  | Some path ->
      Printf.printf "tls_dll %s %s\n%!" (Provenance.sha256_file path) path
