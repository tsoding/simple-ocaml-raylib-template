# Raylib OCaml Template Project

Intentionally simple Raylib OCaml Template. Not using [dune](https://github.com/ocaml/dune) or any other build systems. Just calling `ocamlc` and `clang` directly to demonstrate how all of this works under the hood for educational purposes.

For more info on OCaml-C Interop, read this entire 73 pages documents (it's simple, you can do that in a couple of hours): https://v2.ocaml.org/manual/intfc.html

## Dependencies

- [OCaml 5.0.0](https://ocaml.org/install)
- [Raylib](https://www.raylib.com/)
- [Clang](https://clang.llvm.org/)

*Tested only on Linux*

## Quick Start

``` console
$ ./build.sh
$ ./game
```

## Hot Reloading

Hot Reloading mechanism is based upon [Dynlink](https://v2.ocaml.org/api/Dynlink.html) module from Stdlib. You can only Hot Reload [game_plug.ml](./game_plug.ml) module. To hot reload the module press <kbd>R</kbd> during the execution.
