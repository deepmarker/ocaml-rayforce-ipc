open Core

let of_span span = Int63.to_int64 (Time_ns.Span.to_int63_ns span)

let to_span ns =
  if Int64.equal ns Int64.min_value
  then Error "null timespan"
  else (
    match Int63.of_int64 ns with
    | None -> Error "timespan outside Time_ns.Span range"
    | Some ns -> Ok (Time_ns.Span.of_int63_ns ns))
;;
