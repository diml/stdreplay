(lang dune 1.11)
(name stdreplay)

(generate_opam_files true)

(license BSD3)
(maintainers jeremie@dimino.org)
(authors "Jérémie Dimino")
(source (github diml/stdreplay))

(package
 (name stdreplay)
 (depends base-unix base-threads base cmdliner stdio))
