(lang dune 2.8)
(name chip8)
; version field is optional
(version 0.0.1)

(generate_opam_files true)

(source (github sazl/ochip8))
(license "GPL-3.0-or-later")
(authors "Sami Zeinelabdin")
(maintainers "sazl@noreply.github.com")

(package
 (name chip8)
 (synopsis "OCaml Chip-8 Emulator")
 (description "A work in progress OCaml Chip-8 Emulator")
 (depends
  (alcotest :with-test)
  (dune (>= 2.8))
  (ppx_deriving (>= 5.2.1))))