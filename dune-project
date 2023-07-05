(lang dune 3.0)
(maintainers "Max Lang (max.lang@tum.de)")
(authors "Benjamin Bott" "Max Lang" "Nico Petzendorfer")
(source (github just-max/less-power))

(package
 (name less-power)
 (synopsis "Framework for sandboxed testing of OCaml code")
 (description
   "\| This framework consists of a number of libraries and
   "\| executables for testing untrusted OCaml code.
   "\| Designed for running programming exercises fully automatically
   "\| and at scale.
 )
 (depends
  (ocaml (= 5.0.0))
 )
)

(generate_opam_files)
(implicit_transitive_deps false)
