open Core

let epoch_offset_ns = 946_684_800_000_000_000
let of_time_ns t = Time_ns.to_int_ns_since_epoch t - epoch_offset_ns
let to_time_ns ns = Time_ns.of_int_ns_since_epoch (ns + epoch_offset_ns)
