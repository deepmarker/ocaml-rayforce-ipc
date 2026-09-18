open! Core

(** Rayforce timestamps are nanoseconds since 2000-01-01T00:00:00Z, not since
    the Unix epoch. The offset is part of the wire format: a value written
    without it lands 30 years in the future. *)

val epoch_offset_ns : int

(** Nanoseconds since 2000-01-01, as an [int]: the value is around 8e17
    today and an OCaml [int] holds 4.6e18, so nothing is lost and a
    producer's timestamps stay unboxed. *)
val of_time_ns : Time_ns.t -> int

val to_time_ns : int -> Time_ns.t
