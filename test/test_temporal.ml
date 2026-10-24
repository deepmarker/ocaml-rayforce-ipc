open Core
open Rayforce_ipc

let roundtrip value =
  let buf = Iobuf.create ~len:(Value.size value) in
  Value.fill buf value;
  assert (Iobuf.length buf = 0);
  Iobuf.flip_lo buf;
  let result = Value.consume buf in
  assert (Iobuf.length buf = 0);
  assert (
    String.equal
      (Sexp.to_string (Value.sexp_of_t result))
      (Sexp.to_string (Value.sexp_of_t value)))
;;

let () =
  List.iter
    [ 0L; 1L; -1L; 86400000000000L; 9007199254740993L; Int64.min_value; Int64.max_value ]
    ~f:(fun n -> roundtrip (Value.Timespan n));
  let ns = [| 0L; -1L; 9007199254740993L; Int64.min_value; Int64.max_value |] in
  roundtrip (Value.Timespans ns);
  roundtrip (Value.Timespans [||]);
  roundtrip (Value.table [ "duration", Timespans ns ]);
  let col = Column.timespan () in
  Array.iter ns ~f:(Column.add_int64 col);
  let buf = Iobuf.create ~len:(Value.size (Value.Col col)) in
  Value.fill buf (Value.Col col);
  Iobuf.flip_lo buf;
  (match Value.consume buf with
   | Timespans values -> assert (Array.equal Int64.equal ns values)
   | _ -> assert false);
  let lower = Int63.to_int64 Int63.min_value
  and upper = Int63.to_int64 Int63.max_value in
  List.iter [ lower; upper; 0L; -1L; 9007199254740993L ] ~f:(fun n ->
    assert (Int64.equal (Timespan.of_span (Result.ok_or_failwith (Timespan.to_span n))) n));
  List.iter
    [ Int64.min_value; Int64.max_value; Int64.(lower - 1L); Int64.(upper + 1L) ]
    ~f:(fun n -> assert (Result.is_error (Timespan.to_span n)));
  let lower_time = Int64.(lower - of_int Timestamp.epoch_offset_ns)
  and upper_time = Int64.(upper - of_int Timestamp.epoch_offset_ns) in
  List.iter [ lower_time; upper_time; -1L; 0L; 1L; 9007199254740993L ] ~f:(fun n ->
    assert (
      Int64.equal
        (Timestamp.of_time_ns_int64
           (Result.ok_or_failwith (Timestamp.to_time_ns_int64 n)))
        n));
  List.iter
    [ Int64.min_value; Int64.max_value; Int64.(lower_time - 1L); Int64.(upper_time + 1L) ]
    ~f:(fun n -> assert (Result.is_error (Timestamp.to_time_ns_int64 n)));
  let historic = Time_ns.of_string_with_utc_offset "1830-01-01 00:00:00Z" in
  assert (Int64.equal (Timestamp.of_time_ns_int64 historic) (-5364662400000000000L));
  assert (Exn.does_raise (fun () -> Timestamp.of_time_ns historic));
  assert (Result.is_error (Timestamp.to_time_ns_int64 3786825600000000000L));
  assert (Exn.does_raise (fun () -> Timestamp.to_time_ns 3786825600000000000));
  let contemporary = Time_ns.of_string_with_utc_offset "2026-01-01 00:00:00.000000001Z" in
  assert (
    Time_ns.equal contemporary (Timestamp.to_time_ns (Timestamp.of_time_ns contemporary)))
;;
