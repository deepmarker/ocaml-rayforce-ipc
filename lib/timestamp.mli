open Core

(** Rayforce timestamps are nanoseconds since 2000-01-01T00:00:00Z. Core
    timestamps use Unix time. Conversion uses exact integer arithmetic. *)
val epoch_offset_ns : int

(** All representable Core times convert exactly, including values whose
    Rayforce offset does not fit an OCaml int. *)
val of_time_ns_int64 : Time_ns.t -> int64

(** Rejects INT64_MIN (null) and values outside Core's signed 63-bit range. *)
val to_time_ns_int64 : int64 -> (Time_ns.t, string) Result.t

(** Convenience int APIs. Raise instead of wrapping if the result cannot fit. *)
val of_time_ns : Time_ns.t -> int

val to_time_ns : int -> Time_ns.t
