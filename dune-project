(lang dune 3.10)

(name aoc)

(generate_opam_files true)

(source
  (github drewolson/aoc-ocaml))

(authors "Drew Olson")

(maintainers "Drew Olson")

(license MIT)

(package
  (name aoc)
  (synopsis "Advent of Code in Ocaml")
  (depends
    ocaml
    dune

    angstrom
    base
    cmdliner
    ppx_expect
    ppx_let
    re
    stdio))
