# Pin the manifest digest so rebuilds do not silently pick up a new OS/compiler
# refresh under the floating debian-13-ocaml-5.4 tag. 
# Keep the semver note in sync with the dune-project (ocaml (= 5.4.1)).
FROM --platform=$BUILDPLATFORM ocaml/opam:debian-13-ocaml-5.4@sha256:a65f9f29ace57fa61b597aba076f31c30d77cfd1ac40b5cc0a321e1ddbb841df  # opam 5.4.1 (dune-project)

WORKDIR /home/opam/less-power/

# not a dependency of less-power, but tests using
# probes (partial signature checking) need it at runtime
RUN opam install -y cppo

COPY --chown=opam less-power.opam .
RUN opam install -y . --deps-only

COPY --chown=opam . .
RUN opam install -y .
