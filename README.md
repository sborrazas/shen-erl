# shen-erl

Erlang port of the Shen programming language.

## Building

For creating the `shen-erl` executable you first need to have Erlang
20.0.2 or superior, with the `erl` binary accessible. Then just run `make`.

## Installation

TODO

## Running

For instructions on how to use the shen-erl command, run

```
SHEN_ERL_ROOTDIR=. ./bin/shen-erl --help
```

where `SHEN_ERL_ROOTDIR` is the directory where the `ebin` directory is found.

## Erlang tests

Erlang tests are run through Docker:

```
make docker-test
# also run dialyzer
make docker-dialzye
```

## Shen tests

```
make shen-tests
```
