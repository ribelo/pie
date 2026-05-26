(lang dune 3.21)

(generate_opam_files true)

(name camelpie)

(package
 (name camelpie)
 (synopsis "Camelpie")
 (description "Camelpie OCaml library.")
 (depends
  (ocaml (= 5.2.0+ox))
  (dune (>= 3.0))
  (alcotest :with-test)))

