(lang dune 3.0)
(maintainers "Max Lang (max.lang@tum.de)")
(authors "Benjamin Bott" "Max Lang" "Nico Petzendorfer")
(source (github just-max/less-power))
(name less-power)

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
  (ocaml (= 5.4.1))
  (xmlm (and (>= 1.4) (< 1.5)))
  (fileutils (and (>= 0.6) (< 0.7)))
  (qcheck-core (and (>= 0.91) (< 0.92)))
  (ounit2 (and (>= 2.2) (< 2.3)))
  (junit (and (>= 2.0) (< 2.1)))
  (mtime (and (>= 2.1) (< 2.2)))
  (fmt (and (>= 0.9) (< 0.10)))
  (ppxlib (and (>= 0.38) (< 0.39)))
  (ocaml-compiler-libs (and (>= v0.17) (< v0.18)))
  (cmdliner (and (>= 1.3) (< 1.4)))
 )
)

(generate_opam_files)
(implicit_transitive_deps false)
