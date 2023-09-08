# Less Power: Framework for sandboxed testing of OCaml code

The Less Power framework consists of a number of libraries and executables for testing untrusted OCaml code. Designed for running programming exercises fully automatically and at scale.

The LP framework was presented at the OCaml Users and Developers Workshop 2023. A high-level overview in the form of an extended abstract is available online at
https://icfp23-ocaml.hotcrp.com/doc/icfp23-ocaml-final17.pdf. Note that some features described there have yet to be contributed to this repository.

## Install

The framework is currently fixed to OCaml 5.0.0. Hence, from an OCaml 5.0.0. switch, LP can be installed using opam:

```sh
opam pin add less-power https://github.com/just-max/less-power.git#main
```

Alternatively, the provided Docker image may be used:

```sh
docker pull ghcr.io/just-max/less-power:main
```

## Deployment

For running real programming courses, programming exercises built for the Less Power framework are best run with [Artemis](https://github.com/ls1intum/Artemis). Artemis is an interactive learning platform that handles student participation and provides CI-based automated testing.

However, the framework using only standard OCaml tooling, and may be adapted for use with other platforms that offer some from of continuous integration.

## Examples

Examples to accompany this repository can be found at [just-max/less-power-examples](https://github.com/just-max/less-power-examples).
