(lang dune 2.0)
(using menhir 2.0)
(generate_opam_files true)

(name binsec)
(version "0.4")
(maintainers "BINSEC <binsec@saxifrage.saclay.cea.fr>")
(authors
  "Adel Djoudi"
  "Benjamin Farinier"
  "Ducousso Soline"
  "Frédéric Recoules"
  "Josselin Feist"
  "Lesly-Ann Daniel"
  "Manh-Dung Nguyen"
  "Mathilde Ollivier"
  "Matthieu Lemerre"
  "Olivier Nicole"
  "Richard Bonichon"
  "Robin David"
  "Ta Thanh Dinh"
  "Yaëlle Vinçont"
  "Guillaume Girol"
)
(license LGPL-2.1-or-later)
(homepage "https://binsec.github.io")
(source (github binsec/binsec))
(bug_reports "mailto:binsec@saxifrage.saclay.cea.fr")

(package
 (name binsec)
 (synopsis "Semantic analysis of binary executables")
 (description "
BINSEC aims at developing an open-source platform filling the gap between formal
methods over executable code and binary-level security analyses currently used
in the security industry.

The project targets the following applicative domains:

    vulnerability analyses
    malware comprehension
    code protection
    binary-level verification

BINSEC is developed at CEA List in scientfic collaboration with Verimag and LORIA.

An overview of some BINSEC features can be found in our SSPREW'17 tutorial.")
 (depends
  (ocaml (and :build (>= 4.08) (< 4.13)))
  (ocamlfind :build)
  (menhir :build)
  (ocamlgraph (>= 1.8.5))
  (zarith (>= 1.4))
  (seq (>= 0.1))
  (mmap (>= 1))
  (bitwuzla (>= 1))
  (ounit2 (and :with-test (>= 2)))
  (qcheck (and :with-test (>= 0.7))))
 (depopts
  (llvm (>= 6.0.0)))
 (tags
  ("binary code analysis"
    "symbolic execution"
    "deductive"
    "program verification"
    "formal specification"
    "automated theorem prover"
    "plugins"
    "abstract interpretation"
    "dataflow analysis"
    "linking"
    "disassembly")))
