open! Core

type t =
  { mutable buf : Bigstring.t
  ; mutable len : int (* rows, not bytes *)
  ; mutable sym_bytes : int (* wire bytes of the symbols added so far *)
  ; width : int (* bytes per row in [buf] *)
  ; tag : int
  }

(* A sym is held as its id, eight bytes wide like an i64, and resolved to a
   string only when the column is written. *)
let create ?(capacity = 1024) ~width ~tag () =
  { buf = Bigstring.create (capacity * width); len = 0; sym_bytes = 0; width; tag }
;;

let i64 ?capacity () = create ?capacity ~width:8 ~tag:Tag.i64 ()
let f64 ?capacity () = create ?capacity ~width:8 ~tag:Tag.f64 ()
let timespan ?capacity () = create ?capacity ~width:8 ~tag:Tag.timespan ()
let timestamp ?capacity () = create ?capacity ~width:8 ~tag:Tag.timestamp ()
let bool ?capacity () = create ?capacity ~width:1 ~tag:Tag.bool ()
let i16 ?capacity () = create ?capacity ~width:2 ~tag:Tag.i16 ()
let i32 ?capacity () = create ?capacity ~width:4 ~tag:Tag.i32 ()
let date ?capacity () = create ?capacity ~width:4 ~tag:Tag.date ()
let time ?capacity () = create ?capacity ~width:4 ~tag:Tag.time ()
let f32 ?capacity () = create ?capacity ~width:4 ~tag:Tag.f32 ()
let sym ?capacity () = create ?capacity ~width:8 ~tag:Tag.sym ()
let length t = t.len

let clear t =
  t.len <- 0;
  t.sym_bytes <- 0
;;

let tag t = t.tag
let capacity t = Bigstring.length t.buf / t.width

let grow t =
  let bigger = Bigstring.create (Bigstring.length t.buf * 2) in
  Bigstring.blit ~src:t.buf ~src_pos:0 ~dst:bigger ~dst_pos:0 ~len:(t.len * t.width);
  t.buf <- bigger
;;

let next t =
  if t.len >= capacity t then grow t;
  let pos = t.len * t.width in
  t.len <- t.len + 1;
  pos
;;

let expect t tag what =
  if t.tag <> tag then failwithf "column holds %d, not %s" t.tag what ()
;;

let add_int t n = Bigstring.unsafe_set_int64_le t.buf ~pos:(next t) n
let add_int64 t n = Bigstring.unsafe_set_int64_t_le t.buf ~pos:(next t) n

let add_float t f =
  Bigstring.unsafe_set_int64_t_le t.buf ~pos:(next t) (Int64.bits_of_float f)
;;

(* The narrow types are written at their own width, so one cell is 2 or 4
   bytes in the buffer as on the wire. [add_int32] serves i32, date and time,
   which share a layout; an f32 cell is its IEEE-754 bits, never a float
   round-tripped through binary64, which may quiet a NaN payload. *)
let add_int16 t n = Bigstring.unsafe_set_int16_le t.buf ~pos:(next t) n
let add_int32 t n = Bigstring.unsafe_set_int32_le t.buf ~pos:(next t) n
let add_float32_bits t bits = Bigstring.unsafe_set_int32_t_le t.buf ~pos:(next t) bits
let add_float32 t f = add_float32_bits t (Int32.bits_of_float f)
let add_bool t b = Bigstring.unsafe_set_uint8 t.buf ~pos:(next t) (Bool.to_int b)

(* The wire length is accumulated as rows arrive so that sizing the column
   later is a read rather than a second pass over every cell. *)
let add_sym t s =
  t.sym_bytes <- t.sym_bytes + Sym.wire_len s;
  Bigstring.unsafe_set_int64_le t.buf ~pos:(next t) (Sym.to_int s)
;;

let check t i =
  if i < 0 || i >= t.len then invalid_argf "column index %d of %d" i t.len ()
;;

let get_int t i =
  check t i;
  Bigstring.unsafe_get_int64_le_exn t.buf ~pos:(i * t.width)
;;

let get_int64 t i =
  check t i;
  Bigstring.unsafe_get_int64_t_le t.buf ~pos:(i * t.width)
;;

let get_float t i =
  check t i;
  expect t Tag.f64 "floats";
  Int64.float_of_bits (Bigstring.unsafe_get_int64_t_le t.buf ~pos:(i * t.width))
;;

let get_int16 t i =
  check t i;
  Bigstring.unsafe_get_int16_le t.buf ~pos:(i * t.width)
;;

let get_int32 t i =
  check t i;
  Bigstring.unsafe_get_int32_le t.buf ~pos:(i * t.width)
;;

let get_float32_bits t i =
  check t i;
  expect t Tag.f32 "float32s";
  Bigstring.unsafe_get_int32_t_le t.buf ~pos:(i * t.width)
;;

let get_float32 t i = Int32.float_of_bits (get_float32_bits t i)

let get_bool t i =
  check t i;
  expect t Tag.bool "bools";
  Bigstring.unsafe_get_uint8 t.buf ~pos:i <> 0
;;

let get_sym t i =
  expect t Tag.sym "symbols";
  Sym.of_int_exn (get_int t i)
;;

(* Prints as the values it holds, named by its type, so a column reads the
   same way in a test failure as the vector it decodes back into. *)
let sexp_of_t t =
  let name, elem =
    match t.tag with
    | tag when tag = Tag.f64 -> "Floats", fun i -> [%sexp (get_float t i : float)]
    | tag when tag = Tag.bool -> "Bools", fun i -> [%sexp (get_bool t i : bool)]
    | tag when tag = Tag.sym ->
      "Syms", fun i -> [%sexp (Sym.to_string (get_sym t i) : string)]
    | tag when tag = Tag.i16 -> "Shorts", fun i -> [%sexp (get_int16 t i : int)]
    | tag when tag = Tag.i32 -> "Int32s", fun i -> [%sexp (get_int32 t i : int)]
    | tag when tag = Tag.date -> "Dates", fun i -> [%sexp (get_int32 t i : int)]
    | tag when tag = Tag.time -> "Times", fun i -> [%sexp (get_int32 t i : int)]
    | tag when tag = Tag.f32 ->
      "Float32s", fun i -> [%sexp (get_float32_bits t i : Int32.t)]
    | tag when tag = Tag.timespan ->
      "Timespans", fun i -> [%sexp (get_int64 t i : Int64.t)]
    | tag when tag = Tag.timestamp ->
      "Timestamps", fun i -> [%sexp (get_int64 t i : Int64.t)]
    | _ -> "Ints", fun i -> [%sexp (get_int64 t i : Int64.t)]
  in
  Sexp.List [ Sexp.Atom name; Sexp.List (List.init t.len ~f:elem) ]
;;

(* ===== Serialization ===== *)

(* Every column but a sym one is already in its wire layout: the payload is
   a prefix of the buffer, and writing it is one blit. A sym column pays for
   what the format costs -- the string of every cell. *)
let wire_size t = if t.tag <> Tag.sym then t.len * t.width else t.sym_bytes

(* The loop reads its own cells rather than going through [get_int]: the
   index cannot be out of range here, and the bounds check it repeats --
   once for the buffer, once for the symbol id, once for the name -- was
   measurable on a feed writing millions of cells. One [Fill.string] per
   cell, with no optional arguments to apply through. *)
let fill buf t =
  if t.tag <> Tag.sym
  then Iobuf.Fill.bigstring buf t.buf ~str_pos:0 ~len:(t.len * t.width)
  else
    for i = 0 to t.len - 1 do
      let id = Bigstring.unsafe_get_int64_le_exn t.buf ~pos:(i * t.width) in
      let s = Sym.wire (Sym.unsafe_of_int id) in
      Iobuf.Fill.string buf s ~str_pos:0 ~len:(String.length s)
    done
;;

(* ===== Decoding ===== *)

let width_of_tag tag =
  if tag = Tag.bool
  then 1
  else if tag = Tag.i16
  then 2
  else if tag = Tag.i32 || tag = Tag.date || tag = Tag.time || tag = Tag.f32
  then 4
  else 8
;;

(* The inverse of [fill], for a vector of [len] rows read off the wire. A
   fixed-width column is one blit out of the frame -- no box per cell, which
   is what the collector spent most of its time on when a decoded table was
   made of arrays. A sym column interns each string, which it must. The
   capacity is never zero, and the caller has checked that a fixed-width
   column fits in what is left of the frame, so that a column read empty can still grow. *)
let consume buf ~tag ~len ~consume_sym =
  let width = width_of_tag tag in
  let t = create ~capacity:(Int.max 1 len) ~width ~tag () in
  if tag = Tag.sym
  then
    for _ = 1 to len do
      add_sym t (consume_sym buf)
    done
  else (
    let bytes = len * width in
    Iobuf.Consume.To_bigstring.blit ~src:buf ~dst:t.buf ~dst_pos:0 ~len:bytes;
    t.len <- len);
  t
;;
