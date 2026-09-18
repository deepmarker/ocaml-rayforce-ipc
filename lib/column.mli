open! Core

(** A column being built, in the layout it goes on the wire in.

    The point is what it does *not* do. A producer accumulating rows in an
    [Int64.t Queue.t] allocates a box per row and hands the collector a
    pointer array per flush; here a row is written into a flat buffer as it
    arrives, and the whole column reaches the frame as one blit. Adding an
    [int], a [bool] or a [Sym.t] allocates nothing at all.

    The buffer is kept across flushes: {!clear} resets the row count and
    keeps the capacity, so a steady feed stops allocating entirely once its
    batches reach their usual size. *)

type t [@@deriving sexp_of]

val i64 : ?capacity:int -> unit -> t
val f64 : ?capacity:int -> unit -> t

(** Nanoseconds since 2000-01-01, see {!Timestamp}. *)
val timestamp : ?capacity:int -> unit -> t

val bool : ?capacity:int -> unit -> t

(** Symbols, held as their ids and written as their strings. *)
val sym : ?capacity:int -> unit -> t

val length : t -> int

(** Drops the rows, keeps the buffer. *)
val clear : t -> unit

val add_int : t -> int -> unit
val add_int64 : t -> Int64.t -> unit
val add_float : t -> float -> unit
val add_bool : t -> bool -> unit
val add_sym : t -> Sym.t -> unit

(** Reading back is for tests and assertions, not for the hot path. Raises
    if the index is out of range or the column holds something else. *)
val get_int : t -> int -> int

val get_int64 : t -> int -> Int64.t
val get_float : t -> int -> float
val get_bool : t -> int -> bool
val get_sym : t -> int -> Sym.t

(**/**)

(* For the serializer: the tag this column writes, its byte size on the
   wire, and the fill itself. *)
val tag : t -> int
val wire_size : t -> int
val fill : (read_write, Iobuf.seek) Iobuf.t -> t -> unit
