(lang dune 2.7)
(name mustache)
(using menhir 2.0)
(cram enable)

(license MIT)
(implicit_transitive_deps false)

(maintainers "Rudi Grinberg <me@rgrinerg.com>")
(authors
 "Rudi Grinberg <me@rgrinberg.com>"
 "Armaël Guéneau <armael.gueneau@ens-lyon.fr>"
 "Gabriel Scherer <gabriel.scherer@gmail.com>")

(source (github rgrinberg/ocaml-mustache))



(generate_opam_files true)

(package
 (name mustache)
 (synopsis "Mustache logic-less templates in OCaml")
 (description "
Read and write mustache templates, and render them by providing a json object.
Contains the `mustache` command line utility for driving logic-less templates.
")
 (depends
  (jsonm (>= 1.0.1))
  (ounit2 :with-test)
  (ezjsonm :with-test)
  (menhir (>= 20180703))
  (cmdliner (>= 1.0.4))
  (ocaml (>= 4.08))))
