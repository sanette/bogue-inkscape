(lang dune 3.13)

(name bogue-inkscape)

(version "alpha")

(generate_opam_files true)

(source (github sanette/bogue-inkscape))

(maintainers "Vu Ngoc San <san.vu-ngoc@laposte.net>")

(authors "Vu Ngoc San <san.vu-ngoc@laposte.net>")

(license LGPL-3.0-or-later)

(documentation https://url/to/documentation)

(package
 (name bogue-inkscape)
 (synopsis "Use Inkscape to create Bogue GUI designs")
 (depends ocaml dune xmlm)
 (tags
  ("gui")))
