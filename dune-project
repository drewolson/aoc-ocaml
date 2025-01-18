(lang dune 3.14)

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
    dune
    ocaml
    ocaml-lsp-server
    ocamlformat
    odig
    odoc
    utop

    angstrom
    cmdliner
    core
    core_kernel
    pcre
    ppx_jane
    z3
    zarith))
