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
  (ocaml (= 5.0.0))
  (xmlm (and (>= 1.4) (< 1.5)))
  (fileutils (and (>= 0.6) (< 0.7)))
  (qcheck-core (and (>= 0.21) (< 0.22)))
  (ounit2 (and (>= 2.2) (< 2.3)))
  (mtime (and (>= 2.0) (< 2.1)))
  (fmt (and (>= 0.9) (< 0.10)))
  (ppxlib (and (>= 0.30) (< 0.31)))
  (ocaml-compiler-libs (and (>= v0.12) (< v0.13)))
  (cmdliner (and (>= 1.2) (< 1.3)))
 )
)

(generate_opam_files)
(implicit_transitive_deps false)
