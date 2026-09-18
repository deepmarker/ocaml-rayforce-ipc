open! Core

(** A rayforce value, in the shape the wire format describes it.

    This is not a mirror of rayforce's in-memory [ray_t]: it carries only
    what the format carries, which is why a column is an ordinary OCaml
    array and not a reference into a foreign heap. Serializing writes the
    array straight into the frame, so the only copy between a producer's
    accumulator and the socket is the one it makes itself.

    The array constructors are the convenient way to write a value; {!Col}
    is the way to build one a row at a time without allocating.

    Columns are always written without the HAS_NULLS attribute. Rayforce
    encodes nulls as per-type sentinels in the payload, so a null column
    value is a sentinel this library does not interpret. *)

type t =
  | Null
  | Bool of bool
  | Byte of int
  | Short of int
  | Int32 of int
  | Int of int64
  | Float of float
  | Timestamp of int64 (** nanoseconds since 2000-01-01, see {!Timestamp} *)
  | Date of int (** days since 2000-01-01 *)
  | Sym of Sym.t
  | String of string
  | Bools of bool array
  | Bytes of Bigstring.t
  | Ints of int64 array
  | Floats of float array
  | Timestamps of int64 array
  | Syms of Sym.t array
  | Strings of string array
  | Col of Column.t
  (** A column built row by row, already in wire layout. What a producer
          should use: see {!Column}. *)
  | List of t array
  | Table of column array
  | Dict of t * t
  | Error of string (** a rayforce error code, at most 7 ASCII bytes *)

and column =
  { name : Sym.t
  ; data : t
  }
[@@deriving sexp_of]

exception Protocol_error of string

(** Raise {!Protocol_error}. Every malformed-frame path in this library goes
    through it. *)
val fail : ('a, unit, string, 'b) format4 -> 'a

(** Number of bytes {!fill} will write. *)
val size : t -> int

(** Write [t] at the iobuf's window, advancing past it. *)
val fill : (read_write, Iobuf.seek) Iobuf.t -> t -> unit

(** Read one value, advancing past it. Raises {!Protocol_error} on anything
    malformed or on a type this library does not decode (lambdas, builtins,
    guids and the narrow-index sym encodings a server never sends). *)
val consume : ([> read ], Iobuf.seek) Iobuf.t -> t

(** [table [ name, col; ... ]] in column order. Raises if the columns differ
    in length: a rayforce table with ragged columns is not a table. *)
val table : (string * t) list -> t

val length : t -> int
val nrows : t -> int
val ncols : t -> int
val column : t -> string -> t option
