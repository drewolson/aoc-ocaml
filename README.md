# Advent of Code in OCaml

## Run

```
dune exec aoc -- -help
dune exec aoc -- --day 1 --part 1
dune exec aoc -- -d 1 -p 1
```

## Build

```
dune build
```

## Test

```
dune test
```

## Create Local Switch

```
opam switch create ./ ocaml-base-compiler.5.3.0 --deps-only --locked
```

## Install New Dependencies

```
opam install . --deps-only
```

## Lock New Dependencies

```
opam lock . --direct-only
```
