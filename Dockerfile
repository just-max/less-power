FROM ocaml/opam:ubuntu-lts-ocaml-5.0

WORKDIR /home/opam/less-power/

# not a dependency of less-power, but tests using
# probes (partial signature checking) need it at runtime
RUN opam install -y cppo

COPY --chown=opam less-power.opam .
RUN opam install -y . --deps-only

COPY --chown=opam . .
RUN opam install -y .
