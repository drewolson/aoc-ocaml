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
    ppx_inline_test
    ppx_expect
    cmdliner))
