#!/bin/sh

set -xe

clang -c -I`ocamlc -where` `pkg-config --cflags raylib` caml_raylib.c
ocamlopt caml_raylib.o ./raylib.ml ./main.ml -o game -cclib "`pkg-config --libs raylib`"
