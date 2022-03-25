# `ssponge`

## Purpose

This is a reimplementation of the `sponge` utility from the [`moreutils`](https://joeyh.name/code/moreutils/) package. It "soaks up standard input and writes to a file".

As to why and when it's necessary, see [this StackExchange answer](https://unix.stackexchange.com/questions/207919/sponge-from-moreutils-whats-the-difference-to-shell-redirect-useful-examples/207921#207921).

## Usage example

You have a configuration file and need to remove comments (lines starting with `#`) in-place:

```bash
$ grep --invert-match '^#' config | ssponge config
```

## Building

Install [Haskell stack](https://docs.haskellstack.org/en/stable/README/), then:

```bash
# build
$ stack build

# install to ~/.local/bin/
$ stack install ssponge
```
