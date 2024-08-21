(lang dune 3.12)

(name ofetch)

(generate_opam_files true)

(source
 (github pterror/ofetch))

(authors "pterror")

(maintainers "pterror")

(license "ISC")

(documentation https://github.com/pterror/ofetch)

(package
 (name ofetch)
 (synopsis "A system fetch")
 (description "A system fetch")
 (depends ocaml dune ctypes ctypes-foreign)
 (tags ("system fetch")))
