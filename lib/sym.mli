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

(** The id of an already-interned symbol. Raises if there is no such id --
    ids are dense and only {!intern} hands them out. *)
val of_int_exn : int -> t

(**/**)

(* For the serializer, on a column it filled itself: the id came out of
   that column and is not in question. *)
val unsafe_of_int : int -> t
val to_string : t -> string
val to_int : t -> int

(** Bytes this symbol occupies on the wire: its name plus the NUL. Both are
    worked out at intern time, so sizing a column is a read per row and
    writing a cell is one blit. *)
val wire_len : t -> int

(** The bytes themselves, terminator included. *)
val wire : t -> string

include Comparable.S_plain with type t := t
include Hashable.S_plain with type t := t
