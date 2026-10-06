(lang dune 3.21)

(generate_opam_files true)

(name pie)

(package
 (name pie)
 (synopsis "Pie")
 (description "Pie OCaml library.")
 (depends
  (ocaml
   (>= 5.4.0))
  (dune
   (>= 3.0))
  (ox
   (>= 0.1.0))
  ox_ai
  ox_ai_openrouter
  ox_ai_realtime
  ox_app
  ox_blocking
  ox_http
  ox_ipc
  ox_json
  ox_observability
  ox_otel
  (ox_test :with-test)
  (eio
   (>= 1.0))
  (yojson
   (>= 2.0))
  (toml
   (>= 7.0))
  (re
   (>= 1.0))
  (cmdliner
   (>= 1.0))
  (eio_main
   (>= 1.0))
  (cstruct
   (>= 6.0.0))
  (mtime :with-test)
  (alcotest :with-test)))
