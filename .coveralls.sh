#!/bin/sh

opam install ocveralls -y

BISECT_FILE=_build/coverage ./vg_test.native
`opam config var bin`/ocveralls --prefix _build _build/coverage*.out --send
