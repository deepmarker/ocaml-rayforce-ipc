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
let timestamp ?capacity () = create ?capacity ~width:8 ~tag:Tag.timestamp ()
let bool ?capacity () = create ?capacity ~width:1 ~tag:Tag.bool ()
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

let fill buf t =
  if t.tag <> Tag.sym
  then Iobuf.Fill.bigstring buf t.buf ~str_pos:0 ~len:(t.len * t.width)
  else
    for i = 0 to t.len - 1 do
      let s = Sym.to_string (Sym.of_int_exn (get_int t i)) in
      Iobuf.Fill.stringo buf s;
      Iobuf.Fill.char buf '\000'
    done
;;
