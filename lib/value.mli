open! Core

(** A rayforce value, in the shape the wire format describes it.

    This is not a mirror of rayforce's in-memory [ray_t]: it carries only
    what the format carries. A numeric, bool or symbol vector is a {!Col}: a
    flat buffer outside the OCaml heap, so a decoded column is one blit out
    of the frame and a producer's column is one blit into it, with nothing
    boxed in between. Build one row at a time with {!Column}, or from an array
    with the literals below ({!ints}, {!syms}, ...).

    Columns are always written without the HAS_NULLS attribute. Rayforce
    encodes nulls as per-type sentinels in the payload, so a null column
    value is a sentinel this library does not interpret. *)

type t =
  | Null
  | Atom_flags of int * t
  (** Atom wire flags, including typed-null bit 0 and quoted-symbol bit 5. *)
  | Bool of bool
  | Byte of int
  | Short of int
  | Int32 of int
  | Int of int64
  | Float of float
  | Float32 of int32 (** IEEE-754 binary32 bits. *)
  | Time of int
  | Guid of string (** Exactly 16 raw bytes. *)
  | Timespan of int64 (** signed nanoseconds; see {!Timespan} *)
  | Timestamp of int64 (** nanoseconds since 2000-01-01, see {!Timestamp} *)
  | Date of int (** days since 2000-01-01 *)
  | Sym of Sym.t
  | String of string
  | Bytes of Bigstring.t
  | Guids of string array
  | Strings of string array
  | Col of Column.t
  (** Every numeric, bool and symbol vector, in wire layout: a flat buffer
      outside the OCaml heap, read with the {!Column} accessors. What a
      producer builds row by row, and what decoding returns; the literals
      below ({!ints}, {!syms}, ...) build one from an array. For the
      timespan and timestamp types, INT64_MIN entries represent null. *)
  | List of t array
  | Table of column array
  | Dict of t * t
  | Lambda of t * t * t option
  | Unary of string
  | Binary of string
  | Variadic of string
  | Error of
      { code : string (** a rayforce error code, at most 7 ASCII bytes *)
      ; message : string (** the diagnostic text, at most 255 bytes; may be empty *)
      }

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
    malformed or on a type Rayforce does not serialize (such as an index). *)
val consume : ([> read ], Iobuf.seek) Iobuf.t -> t

(** [table [ name, col; ... ]] in column order. Raises if the columns differ
    in length: a rayforce table with ragged columns is not a table. *)
val table : (string * t) list -> t

val length : t -> int
val nrows : t -> int
val ncols : t -> int
val column : t -> string -> t option

(** {2 Vector literals}

    Each builds a {!Col} of the type's own width from an array, which is how
    a vector is written when it is not accumulated row by row. [float32s]
    takes IEEE-754 bit patterns, [dates] days and [times] milliseconds. *)

val ints : int64 array -> t
val floats : float array -> t
val bools : bool array -> t
val syms : Sym.t array -> t
val timespans : int64 array -> t
val timestamps : int64 array -> t
val shorts : int array -> t
val int32s : int array -> t
val dates : int array -> t
val times : int array -> t
val float32s : int32 array -> t
