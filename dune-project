(lang dune 3.21)

(generate_opam_files true)

(name camelpie)

(package
 (name camelpie)
 (synopsis "Camelpie")
 (description "Camelpie OCaml library.")
 (depends
  (ocaml (= 5.2.0))
  (ocaml-variants (= 5.2.0+ox))
  (dune (>= 3.0))
  (eta (>= 0.1.0))
  eta_http
  eta_stream
  (eio (>= 1.0))
  (yojson (>= 2.0))
  (toml (>= 7.0))
  (re (>= 1.0))
  (cmdliner (>= 1.0))
  (eio_main (>= 1.0))
  (cstruct (>= 6.0.0))
  (alcotest :with-test)))
