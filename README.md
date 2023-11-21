# Advent of Code in OCaml

## Install Dependencies

```
opam install . --deps-only
```

## Lock Dependencies

```
opam lock . --direct-only
```

## Run

```
dune exec aoc -- -help
dune exec aoc -- -day 1 -part 1
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
opam switch create ./ 5.1.0 --deps-only
```
