(lang dune 3.21)

(generate_opam_files true)

(name pie)

(package
 (name pie)
 (synopsis "Pie")
 (description "Pie OCaml library.")
 (depends
  (ocaml (>= 5.2.0))
  (dune (>= 3.0))
  (eta (>= 0.1.0))
  eta_ai
  eta_ai_openrouter
  eta_eio
  eta_http
  eta_http_eio
  (eta_http_tls_openssl :with-test)
  eta_otel
  eta_stream
  (eio (>= 1.0))
  (yojson (>= 2.0))
  (toml (>= 7.0))
  (re (>= 1.0))
  (cmdliner (>= 1.0))
  (eio_main (>= 1.0))
  (cstruct (>= 6.0.0))
  (alcotest :with-test)))
