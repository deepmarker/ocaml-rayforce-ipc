open! Core

(** Symbols, interned locally.

    A rayforce symbol travels on the wire as a NUL-terminated string: the
    integer id a server hands out indexes its own intern table and means
    nothing to any other process. So this table is ours alone. It exists for
    the reason rayforce's does -- a column stores one small int per row, and
    the string is written once per cell at serialization time -- not because
    the peer needs the id.

    The table never shrinks: symbols are venue codes, instrument names and
    column names, a bounded set. It is not thread-safe; the intended caller
    is a single Async scheduler. *)

type t = private int [@@deriving compare, equal, hash, sexp_of]

val intern : string -> t
val to_string : t -> string

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
