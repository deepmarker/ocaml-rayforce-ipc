open Core
open Rayforce_ipc

let sym s = Value.Sym (Sym.intern s)
let syms_of l = Value.Syms (Array.of_list_map l ~f:Sym.intern)
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
      , Value.Timestamps
          (Array.init rows ~f:(fun i -> Int64.of_int (800_000_000_000_000_000 + i))) )
    ; ( "instrument"
      , Value.Syms (Array.init rows ~f:(fun i -> Sym.intern (sprintf "S%d" (i % 7)))) )
    ; "px", Value.Ints (Array.init rows ~f:(fun i -> Int64.of_int (i * 100)))
    ; "price", Value.Floats (Array.init rows ~f:(fun i -> float_of_int i /. 8.))
    ; "f_last", Value.Bools (Array.init rows ~f:(fun i -> i % 2 = 0))
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
  check_same "error" (Value.Error "domain")
;;

let test_vectors () =
  check_same "bools" (Value.Bools [| true; false; true |]);
  check_same "ints" (Value.Ints [| 0L; -1L; Int64.max_value |]);
  check_same "floats" (Value.Floats [| 0.; -1.25; Float.max_value |]);
  check_same "timestamps" (Value.Timestamps [| 0L; 1L |]);
  check_same "syms" (syms [ "BTCUSDT"; "ETHUSDT"; "BTCUSDT" ]);
  check_same "strings" (Value.Strings [| ""; "a"; "the quick brown fox" |]);
  check_same "empty syms" (Value.Syms [||]);
  check_same "empty ints" (Value.Ints [||])
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
             [ "i", Value.Ints [| 0L; 1000L; 2000L; 3000L |]
             ; "s", syms_of [ "BTCUSDT"; "ETHUSDT"; "BTCUSDT"; "SOLUSDT" ]
             ; "f", Value.Bools [| true; false; true; false |]
             ; "px", Value.Floats [| 0.; 0.25; 0.5; 0.75 |]
             ])))
    (Sexp.to_string (Value.sexp_of_t got));
  Column.clear ints;
  Alcotest.(check int) "clear drops the rows" 0 (Column.length ints)
;;

let test_compound () =
  check_same "table" (batch ~rows:3);
  check_same "empty table" (batch ~rows:0);
  check_same "upd message" (Value.List [| sym "upd"; sym "depth"; batch ~rows:2 |]);
  check_same "dict" (Value.Dict (syms [ "a"; "b" ], Value.Ints [| 1L; 2L |]));
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
         (Value.table [ "a", Value.Ints [| 1L; 2L |]; "b", Value.Ints [| 1L |] ]
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

let () =
  Alcotest.run
    "rayforce-ipc"
    [ ( "values"
      , [ Alcotest.test_case "atoms" `Quick test_atoms
        ; Alcotest.test_case "vectors" `Quick test_vectors
        ; Alcotest.test_case "compound" `Quick test_compound
        ; Alcotest.test_case "table accessors" `Quick test_table_accessors
        ; Alcotest.test_case "columns" `Quick test_columns
        ] )
    ; ( "frames"
      , [ Alcotest.test_case "round trip" `Quick test_frame
        ; Alcotest.test_case "rejected headers" `Quick test_bad_frames
        ; Alcotest.test_case "decompression" `Quick test_decompress
        ] )
    ]
;;
