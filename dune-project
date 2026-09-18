(lang dune 3.21)

(name rayforce-ipc)

(generate_opam_files true)

(source
 (github deepmarker/ocaml-rayforce-ipc))

(license ISC)

(authors "Vincent Bernardoff <vb@luminar.eu.org>")

(maintainers "Vincent Bernardoff <vb@luminar.eu.org>")

(documentation https://github.com/deepmarker/ocaml-rayforce-ipc)

(package
 (name rayforce-ipc)
 (synopsis "RayforceDB's IPC wire protocol, in pure OCaml")
 (description
  "An encoder, decoder and blocking client for RayforceDB's IPC protocol, written against the wire format rather than against librayforce: no C stubs, no embedded runtime, and no thread affinity. Values are built from ordinary OCaml arrays and serialized straight into an Iobuf, so a producer's columns reach the socket without passing through a foreign heap. Covers the version handshake, frames, the value types a server sends and receives, and the delta+RLE coding of a compressed frame.")
 (depends
  (ocaml
   (>= 4.14.0))
  core
  core_kernel
  core_unix
  (alcotest :with-test)))
