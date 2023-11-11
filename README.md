# Advent of Code in OCaml

## Install Dependencies

```
opam install . --deps-only --with-test
```

## Lock Dependencies

```
opam lock -d .
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
