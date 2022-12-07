(lang dune 3.5)
(name opam-generate-big-monorepo)

(generate_opam_files true)

(package
  (name opam-generate-big-monorepo)
  (allow_empty true)
  (maintainers "Stephen Sherratt")
  (authors "Stephen Sherratt")
  (homepage "https://github.com/gridbugs/opam-generate-big-monorepo")
  (bug_reports "https://github.com/gridbugs/opam-generate-big-monorepo/issues")
  (synopsis "Generate a big project using opam monorepo")
  (depends
    (dune (>= 3.5.0))
    ocamlgraph
    opam-repository
    yojson))
