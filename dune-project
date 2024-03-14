(lang dune 3.14)

(name locksmith)

(generate_opam_files true)

(source
 (github maxrn/locksmith))

(authors "Max Große")

(maintainers "Max Große")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name locksmith)
 (synopsis "Convert between password manager formats")
 (description "Can convert between export formats of multiple password managers to make migrating easier.")
 (depends ocaml dune)
 (tags
  (password "password manager")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
