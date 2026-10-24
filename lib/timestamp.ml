open Core

let epoch_offset_ns = 946_684_800_000_000_000
let epoch_offset_int64 = Int64.of_int epoch_offset_ns

let of_time_ns_int64 t =
  Int64.(Int63.to_int64 (Time_ns.to_int63_ns_since_epoch t) - epoch_offset_int64)
;;

let to_time_ns_int64 ns =
  let lower = Int64.(Int63.to_int64 Int63.min_value - epoch_offset_int64) in
  let upper = Int64.(Int63.to_int64 Int63.max_value - epoch_offset_int64) in
  if Int64.equal ns Int64.min_value
  then Error "null timestamp"
  else if Int64.(ns < lower || ns > upper)
  then Error "timestamp outside Time_ns range"
  else
    Ok
      (Time_ns.of_int63_ns_since_epoch
         (Int63.of_int64_exn Int64.(ns + epoch_offset_int64)))
;;

let of_time_ns t = Int64.to_int_exn (of_time_ns_int64 t)

let to_time_ns ns =
  match to_time_ns_int64 (Int64.of_int ns) with
  | Ok time -> time
  | Error message -> invalid_arg message
;;
