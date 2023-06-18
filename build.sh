#!/bin/sh

set -xe

clang -shared -fPIC -I`ocamlc -where` `pkg-config --cflags raylib` -o dllcaml_raylib.so caml_raylib.c `pkg-config --libs raylib`
ocamlc -I +dynlink dynlink.cma ./raylib.ml ./game.ml ./main.ml -o game -dllib -lcaml_raylib
ocamlc -c game_plug.ml
