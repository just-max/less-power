FROM ocaml/opam:ubuntu-lts-ocaml-5.0

WORKDIR /home/opam/less-power/

COPY --chown=opam less-power.opam .
RUN opam install . --deps-only

COPY --chown=opam . .
RUN opam install .
