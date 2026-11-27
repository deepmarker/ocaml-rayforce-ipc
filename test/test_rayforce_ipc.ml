open Core
open Rayforce_ipc

let sym s = Value.Sym (Sym.intern s)
let syms_of l = Value.syms (Array.of_list_map l ~f:Sym.intern)
let syms = syms_of

let roundtrip v =
  let buf = Iobuf.create ~len:(Value.size v) in
  Value.fill buf v;
  Alcotest.(check int) "size is what fill wrote" 0 (Iobuf.length buf);
  Iobuf.flip_lo buf;
  let got = Value.consume buf in
  Alcotest.(check int) "consume read the whole value" 0 (Iobuf.length buf);
  got
;;

let check_same name v =
  let got = roundtrip v in
  Alcotest.(check string)
    name
    (Sexp.to_string (Value.sexp_of_t v))
    (Sexp.to_string (Value.sexp_of_t got))
;;

let batch ~rows =
  Value.table
    [ ( "ts_evt"
      , Value.timestamps
          (Array.init rows ~f:(fun i -> Int64.of_int (800_000_000_000_000_000 + i))) )
    ; ( "instrument"
      , Value.syms (Array.init rows ~f:(fun i -> Sym.intern (sprintf "S%d" (i % 7)))) )
    ; "px", Value.ints (Array.init rows ~f:(fun i -> Int64.of_int (i * 100)))
    ; "price", Value.floats (Array.init rows ~f:(fun i -> float_of_int i /. 8.))
    ; "f_last", Value.bools (Array.init rows ~f:(fun i -> i % 2 = 0))
    ]
;;

let test_atoms () =
  check_same "null" Value.Null;
  check_same "bool" (Value.Bool true);
  check_same "byte" (Value.Byte 200);
  check_same "short" (Value.Short (-3));
  check_same "int32" (Value.Int32 70_000);
  check_same "date" (Value.Date 9000);
  check_same "int" (Value.Int 4_294_967_304L);
  check_same "float" (Value.Float 1.5);
  check_same "timestamp" (Value.Timestamp 123L);
  check_same "sym" (sym "upd");
  check_same "string" (Value.String "(+ 1 2)");
  check_same "error" (Value.Error { code = "domain"; message = "" });
  check_same
    "error with message"
    (Value.Error { code = "type"; message = "expected i64, got sym" });
  check_same "float32 bits" (Value.Float32 0x7fc01234l);
  check_same "time" (Value.Time 1234);
  check_same "guid" (Value.Guid "0123456789abcdef");
  check_same "typed null flags" (Value.Atom_flags (1, Value.Bool false));
  check_same "quoted symbol" (Value.Atom_flags (32, sym "hello"));
  check_same
    "lambda"
    (Value.Lambda
       ( Value.List [||]
       , Value.Int 42L
       , Some (Value.Dict (Value.syms [||], Value.List [||])) ));
  check_same "unary" (Value.Unary "long_builtin_name");
  check_same "binary" (Value.Binary "+");
  check_same "variadic" (Value.Variadic "list")
;;

let test_vectors () =
  check_same "bools" (Value.bools [| true; false; true |]);
  check_same "shorts" (Value.shorts [| -32768; 32767 |]);
  check_same "int32s" (Value.int32s [| -2147483648; 2147483647 |]);
  check_same "dates" (Value.dates [| 0; -1 |]);
  check_same "times" (Value.times [| 0; 1234 |]);
  check_same "float32s" (Value.float32s [| 0l; 0x7fc01234l; Int32.min_value |]);
  check_same "guids" (Value.Guids [| "0123456789abcdef"; "fedcba9876543210" |]);
  check_same "ints" (Value.ints [| 0L; -1L; Int64.max_value |]);
  check_same "floats" (Value.floats [| 0.; -1.25; Float.max_value |]);
  check_same "timestamps" (Value.timestamps [| 0L; 1L |]);
  check_same "syms" (syms [ "BTCUSDT"; "ETHUSDT"; "BTCUSDT" ]);
  check_same "strings" (Value.Strings [| ""; "a"; "the quick brown fox" |]);
  check_same "empty syms" (Value.syms [||]);
  check_same "empty ints" (Value.ints [||])
;;

let test_columns () =
  let ints = Column.i64 ~capacity:1 () in
  let syms = Column.sym ~capacity:1 () in
  let flags = Column.bool ~capacity:1 () in
  let px = Column.f64 ~capacity:1 () in
  (* Capacity 1 with four rows: the growth path is the one that has to hold
     the rows it already had. *)
  List.iteri [ "BTCUSDT"; "ETHUSDT"; "BTCUSDT"; "SOLUSDT" ] ~f:(fun i s ->
    Column.add_int ints (i * 1000);
    Column.add_sym syms (Sym.intern s);
    Column.add_bool flags (i % 2 = 0);
    Column.add_float px (Float.of_int i /. 4.));
  Alcotest.(check int) "rows" 4 (Column.length ints);
  Alcotest.(check int) "value survives growth" 2000 (Column.get_int ints 2);
  Alcotest.(check string)
    "sym survives growth"
    "SOLUSDT"
    (Sym.to_string (Column.get_sym syms 3));
  Alcotest.(check bool) "bool" true (Column.get_bool flags 0);
  Alcotest.(check (float 0.)) "float" 0.75 (Column.get_float px 3);
  let table =
    Value.table
      [ "i", Value.Col ints
      ; "s", Value.Col syms
      ; "f", Value.Col flags
      ; "px", Value.Col px
      ]
  in
  let got = roundtrip table in
  Alcotest.(check string)
    "a built column decodes as the vector it is"
    (Sexp.to_string
       (Value.sexp_of_t
          (Value.table
             [ "i", Value.ints [| 0L; 1000L; 2000L; 3000L |]
             ; "s", syms_of [ "BTCUSDT"; "ETHUSDT"; "BTCUSDT"; "SOLUSDT" ]
             ; "f", Value.bools [| true; false; true; false |]
             ; "px", Value.floats [| 0.; 0.25; 0.5; 0.75 |]
             ])))
    (Sexp.to_string (Value.sexp_of_t got));
  Column.clear ints;
  Alcotest.(check int) "clear drops the rows" 0 (Column.length ints)
;;

let test_compound () =
  check_same "table" (batch ~rows:3);
  check_same "empty table" (batch ~rows:0);
  check_same "upd message" (Value.List [| sym "upd"; sym "depth"; batch ~rows:2 |]);
  check_same "dict" (Value.Dict (syms [ "a"; "b" ], Value.ints [| 1L; 2L |]));
  check_same "nested list" (Value.List [| Value.List [| Value.Int 1L |]; Value.Null |])
;;

let test_table_accessors () =
  let t = batch ~rows:4 in
  Alcotest.(check int) "rows" 4 (Value.nrows t);
  Alcotest.(check int) "cols" 5 (Value.ncols t);
  Alcotest.(check bool) "known column" true (Option.is_some (Value.column t "px"));
  Alcotest.(check bool) "unknown column" true (Option.is_none (Value.column t "nope"));
  Alcotest.check_raises
    "ragged columns are not a table"
    (Value.Protocol_error "table: column b has 1 rows, a has 2")
    (fun () ->
       ignore
         (Value.table [ "a", Value.ints [| 1L; 2L |]; "b", Value.ints [| 1L |] ]
          : Value.t))
;;

let test_frame () =
  let v = Value.List [| sym "upd"; sym "depth"; batch ~rows:2 |] in
  let buf = Iobuf.create ~len:(Wire.frame_size v) in
  Wire.fill_frame buf ~msgtype:Async v;
  Iobuf.flip_lo buf;
  Alcotest.(check int) "frame size" (Wire.frame_size v) (Iobuf.length buf);
  let header = Wire.consume_header buf in
  Alcotest.(check int) "payload length" (Value.size v) header.payload_len;
  Alcotest.(check bool) "not compressed" false header.compressed;
  Alcotest.(check bool) "async" true (Wire.Msgtype.equal Async header.msgtype);
  let got = Wire.consume_payload buf header in
  Alcotest.(check string)
    "payload"
    (Sexp.to_string (Value.sexp_of_t v))
    (Sexp.to_string (Value.sexp_of_t got))
;;

let test_bad_frames () =
  let bad bytes name =
    let buf = Iobuf.of_string bytes in
    Alcotest.(check bool)
      name
      true
      (match Wire.consume_header buf with
       | (_ : Wire.header) -> false
       | exception Value.Protocol_error _ -> true)
  in
  bad "\x00\x00\x00\x00\x03\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00" "wrong prefix";
  bad "\xfa\xde\xfa\xce\x02\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00" "wrong version";
  bad "\xfa\xde\xfa\xce\x03\x00\x01\x00\x08\x00\x00\x00\x00\x00\x00\x00" "big endian";
  bad "\xfa\xde\xfa\xce\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00" "empty payload"
;;

(* The compressor lives on the server side only -- this client never
   compresses. Transcribed from ray_ipc_compress_at so the decoder is
   checked against the coding it will actually meet. *)
let compress src =
  let len = Bigstring.length src in
  let delta = Bytes.create len in
  for i = 0 to len - 1 do
    let prev = if i = 0 then 0 else Char.to_int src.{i - 1} in
    Bytes.set delta i (Char.of_int_exn ((Char.to_int src.{i} - prev) land 0xff))
  done;
  let out = Buffer.create len in
  let si = ref 0 in
  while !si < len do
    let same =
      !si + 1 < len && Char.equal (Bytes.get delta !si) (Bytes.get delta (!si + 1))
    in
    if same
    then (
      let v = Bytes.get delta !si in
      let run = ref 1 in
      while
        !si + !run < len && Char.equal (Bytes.get delta (!si + !run)) v && !run < 127
      do
        incr run
      done;
      Buffer.add_char out (Char.of_int_exn !run);
      Buffer.add_char out v;
      si := !si + !run)
    else (
      let start = !si in
      let llen = ref 0 in
      let stop = ref false in
      while (not !stop) && !si < len && !llen < 128 do
        if !si + 1 < len && Char.equal (Bytes.get delta !si) (Bytes.get delta (!si + 1))
        then stop := true
        else (
          incr si;
          incr llen)
      done;
      Buffer.add_char out (Char.of_int_exn (- !llen land 0xff));
      Buffer.add_string out (Bytes.To_string.sub delta ~pos:start ~len:!llen))
  done;
  Bigstring.of_string (Buffer.contents out)
;;

let test_decompress () =
  let cases =
    [ "runs: " ^ String.make 300 'x'
    ; "mixed: " ^ String.make 40 'a' ^ "bcdefgh" ^ String.make 200 '\000'
    ; String.init 512 ~f:(fun i -> Char.of_int_exn (i % 251))
    ; "a"
    ]
  in
  List.iteri cases ~f:(fun i s ->
    let src = Bigstring.of_string s in
    let got = Wire.decompress (compress src) ~uncompressed_len:(String.length s) in
    Alcotest.(check string) (sprintf "case %d" i) s (Bigstring.to_string got))
;;

(* The narrow types are held at their own width, and a column of them goes
   out as one blit and comes back as a [Col], not as an array. *)
let narrow_columns () =
  let i16 = Column.i16 ()
  and i32 = Column.i32 ()
  and date = Column.date ()
  and time = Column.time ()
  and f32 = Column.f32 () in
  let i16_values = [ 0; 1; -1; 32767; -32768 ]
  and i32_values = [ 0; 1; -1; 2147483647; -2147483648 ]
  and f32_bits = [ 0x3f800000l; 0xbf800000l; 0x7fa00001l; 0x7fc00000l; 0xffffffffl ] in
  List.iter i16_values ~f:(Column.add_int16 i16);
  List.iter i32_values ~f:(Column.add_int32 i32);
  List.iter i32_values ~f:(Column.add_int32 date);
  List.iter i32_values ~f:(Column.add_int32 time);
  List.iter f32_bits ~f:(Column.add_float32_bits f32);
  let sizes = List.map [ i16; i32; date; time; f32 ] ~f:Column.wire_size in
  Alcotest.(check (list int)) "cells are 2 or 4 bytes" [ 10; 20; 20; 20; 20 ] sizes;
  let table =
    Value.table
      [ "i16", Col i16
      ; "i32", Col i32
      ; "date", Col date
      ; "time", Col time
      ; "f32", Col f32
      ]
  in
  let got = roundtrip table in
  let column name =
    match Value.column got name with
    | Some (Value.Col c) -> c
    | _ -> Alcotest.failf "%s did not decode to a Col" name
  in
  let read c ~f = List.init (Column.length c) ~f:(f c) in
  Alcotest.(check (list int)) "i16" i16_values (read (column "i16") ~f:Column.get_int16);
  Alcotest.(check (list int)) "i32" i32_values (read (column "i32") ~f:Column.get_int32);
  Alcotest.(check (list int)) "date" i32_values (read (column "date") ~f:Column.get_int32);
  Alcotest.(check (list int)) "time" i32_values (read (column "time") ~f:Column.get_int32);
  Alcotest.(check (list int32))
    "f32 bits survive, signaling NaN included"
    f32_bits
    (read (column "f32") ~f:Column.get_float32_bits);
  (* The same table written with the literal constructors is the same value. *)
  let as_arrays =
    Value.table
      [ "i16", Value.shorts (Array.of_list i16_values)
      ; "i32", Value.int32s (Array.of_list i32_values)
      ; "date", Value.dates (Array.of_list i32_values)
      ; "time", Value.times (Array.of_list i32_values)
      ; "f32", Value.float32s (Array.of_list f32_bits)
      ]
  in
  Alcotest.(check string)
    "a decoded table equals the one built from literals"
    (Sexp.to_string (Value.sexp_of_t as_arrays))
    (Sexp.to_string (Value.sexp_of_t got));
  (* And a vector written from an array decodes into a Col of the same cells. *)
  (match roundtrip (Value.int32s [| 7; -7 |]) with
   | Value.Col c -> Alcotest.(check int) "decoded int32s" (-7) (Column.get_int32 c 1)
   | _ -> Alcotest.fail "Int32s did not decode to a Col");
  List.iter
    [ Value.shorts [||]
    ; Value.int32s [||]
    ; Value.dates [||]
    ; Value.times [||]
    ; Value.float32s [||]
    ]
    ~f:(fun empty ->
      match roundtrip empty with
      | Value.Col c -> Alcotest.(check int) "empty column" 0 (Column.length c)
      | _ -> Alcotest.fail "empty narrow vector did not decode to a Col");
  Alcotest.(check (float 0.))
    "f32 as a float"
    1.5
    (let c = Column.f32 () in
     Column.add_float32 c 1.5;
     Column.get_float32 c 0)
;;

let () =
  Alcotest.run
    "rayforce-ipc"
    [ ( "values"
      , [ Alcotest.test_case "atoms" `Quick test_atoms
        ; Alcotest.test_case "vectors" `Quick test_vectors
        ; Alcotest.test_case "compound" `Quick test_compound
        ; Alcotest.test_case "table accessors" `Quick test_table_accessors
        ; Alcotest.test_case "columns" `Quick test_columns
        ; Alcotest.test_case "narrow columns" `Quick narrow_columns
        ] )
    ; ( "frames"
      , [ Alcotest.test_case "round trip" `Quick test_frame
        ; Alcotest.test_case "rejected headers" `Quick test_bad_frames
        ; Alcotest.test_case "decompression" `Quick test_decompress
        ] )
    ]
;;
