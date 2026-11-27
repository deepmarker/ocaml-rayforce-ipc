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

(** Signed nanoseconds; append with [add_int64]. *)
val timespan : ?capacity:int -> unit -> t

(** Nanoseconds since 2000-01-01, see {!Timestamp}. *)
val timestamp : ?capacity:int -> unit -> t

val bool : ?capacity:int -> unit -> t

(** The narrow types, held at their own width: 2 bytes for [i16], 4 for the
    rest. [date] is days and [time] milliseconds, as in {!Value}; an [f32]
    cell is its IEEE-754 bits. *)
val i16 : ?capacity:int -> unit -> t

val i32 : ?capacity:int -> unit -> t
val date : ?capacity:int -> unit -> t
val time : ?capacity:int -> unit -> t
val f32 : ?capacity:int -> unit -> t

(** Symbols, held as their ids and written as their strings. *)
val sym : ?capacity:int -> unit -> t

val length : t -> int

(** Drops the rows, keeps the buffer. *)
val clear : t -> unit

val add_int : t -> int -> unit
val add_int64 : t -> Int64.t -> unit
val add_float : t -> float -> unit
val add_bool : t -> bool -> unit
val add_int16 : t -> int -> unit

(** For [i32], [date] and [time] columns. *)
val add_int32 : t -> int -> unit

(** Appends the cell's IEEE-754 bits; exact, whatever the NaN payload. *)
val add_float32_bits : t -> Int32.t -> unit

(** Rounds to binary32. Use {!add_float32_bits} to keep a bit pattern. *)
val add_float32 : t -> float -> unit

val add_sym : t -> Sym.t -> unit

(** Reading back is for tests and assertions, not for the hot path. Raises
    if the index is out of range or the column holds something else. *)
val get_int : t -> int -> int

val get_int64 : t -> int -> Int64.t
val get_float : t -> int -> float
val get_bool : t -> int -> bool
val get_int16 : t -> int -> int
val get_int32 : t -> int -> int
val get_float32_bits : t -> int -> Int32.t
val get_float32 : t -> int -> float
val get_sym : t -> int -> Sym.t

(**/**)

(* For the serializer: the tag this column writes, its byte size on the
   wire, and the fill itself. *)
val tag : t -> int
val wire_size : t -> int
val fill : (read_write, Iobuf.seek) Iobuf.t -> t -> unit

(** Reads [len] rows of a vector of type [tag] off the wire, advancing
    [buf]. The caller has checked that [len * width] bytes remain;
    [consume_sym] reads one symbol. *)
val consume
  :  (read, Iobuf.seek) Iobuf.t
  -> tag:int
  -> len:int
  -> consume_sym:((read, Iobuf.seek) Iobuf.t -> Sym.t)
  -> t

val width_of_tag : int -> int
