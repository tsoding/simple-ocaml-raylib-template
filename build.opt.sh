#!/bin/sh

set -xe

clang -I`ocamlc -where` `pkg-config --cflags raylib` -c caml_raylib.c
ocamlopt -pp 'cpp' caml_raylib.o ./raylib.ml ./game.ml ./game_plug.ml ./main.ml -o game.opt -cclib "`pkg-config --libs raylib`"
