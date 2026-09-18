open Core

let epoch_offset_ns = 946_684_800_000_000_000
let of_time_ns t = Int64.of_int (Time_ns.to_int_ns_since_epoch t - epoch_offset_ns)
let to_time_ns ns = Time_ns.of_int_ns_since_epoch (Int64.to_int_exn ns + epoch_offset_ns)
