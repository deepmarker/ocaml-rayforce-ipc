open Core

(** Rayforce timespans are signed 64-bit nanoseconds, with no epoch offset.
    Core spans have a narrower signed 63-bit range. *)
val of_span : Time_ns.Span.t -> int64

(** Rejects INT64_MIN (null) and values outside Core's representable range.
    Raw wire values retain the entire int64 range through Value.Timespan. *)
val to_span : int64 -> (Time_ns.Span.t, string) Result.t
