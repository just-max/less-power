# Less Power: Framework for sandboxed testing of OCaml code

The Less Power framework consists of a number of libraries and executables for testing untrusted OCaml code. Designed for running programming exercises fully automatically and at scale.

The LP framework was presented at the OCaml Users and Developers Workshop 2023. A high-level overview in the form of an extended abstract is [available in git](https://github.com/just-max/less-power/blob/icfp23-ocaml-upload/icfp23-ocaml-final17.pdf). Note that some features described there have yet to be contributed to this repository.

## Install

The framework is currently fixed to OCaml 5.1.1. Hence, from an OCaml 5.1.1. switch, LP can be installed using opam:

```sh
opam pin add less-power https://github.com/just-max/less-power.git#main
```

Alternatively, the provided Docker image may be used:

```sh
docker pull ghcr.io/just-max/less-power:main
```

A specific tag may instead be substituted for `main`.

# Development

To make changes to the framework, first clone this repository. Then add a local pin:

```sh
opam pin add less-power ~/path/to/less-power
```

While developing, use `dune` to build:

```sh
dune build       # regular build
dune build @doc  # build docs
dune build test  # run tests
```

To use your local changes while developing a programming exercise:

```sh
dune build @install                    # prepare for install
opam upgrade --working-dir less-power  # make changes available
```

## Deployment

For running real programming courses, programming exercises built for the Less Power framework are best run with [Artemis](https://github.com/ls1intum/Artemis). Artemis is an interactive learning platform that handles student participation and provides CI-based automated testing.

However, the framework uses only standard OCaml tooling, and may be adapted for use with other platforms that offer some from of continuous integration.

## Examples

Examples to accompany this repository can be found at [just-max/less-power-examples](https://github.com/just-max/less-power-examples).
