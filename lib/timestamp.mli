open! Core

(** Rayforce timestamps are nanoseconds since 2000-01-01T00:00:00Z, not since
    the Unix epoch. The offset is part of the wire format: a value written
    without it lands 30 years in the future. *)

val epoch_offset_ns : int
val of_time_ns : Time_ns.t -> int64
val to_time_ns : int64 -> Time_ns.t
