(lang dune 3.4)

(name code_format_ml)

(generate_opam_files true)

(source
 (github puripuri2100/coins_special_seminar_code_format_ml))

(authors "puripuri2100")

(maintainers "puripuri2100")

(license MIT)

(package
 (name code_format_ml)
 (synopsis "code format library")
 (description "情報科学特別演習で実装しているコードフォーマットライブラリ")
 (depends ocaml dune ounit2)
 (tags
  ("code format")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
