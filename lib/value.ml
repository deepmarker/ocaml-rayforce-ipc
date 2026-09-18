open! Core

(* Type tags, from rayforce's include/rayforce.h. A vector carries the tag
   itself, an atom of the same type carries its negation, and both 126 and
   127 are wire-only markers with no vector form. *)
module Tag = struct
  let list = 0
  let bool = 1
  let u8 = 2
  let i16 = 3
  let i32 = 4
  let i64 = 5
  let f64 = 7
  let date = 8
  let timestamp = 10
  let sym = 12
  let str = 13
  let table = 98
  let dict = 99

  (* Types this library does not decode. They are named so that meeting one
     reports what it is instead of reading its first bytes as a length. *)
  let undecoded =
    [ 6, "f32"
    ; 9, "time"
    ; 11, "guid"
    ; 97, "index"
    ; 100, "lambda"
    ; 101, "unary builtin"
    ; 102, "binary builtin"
    ; 103, "variadic builtin"
    ]
  ;;

  let null = 126
  let error = 127
end

(* The schema names of a table are the one place the C serializer writes a
   non-zero attrs byte: RAY_SYM_W64, the index width of the vector it came
   from. The decoder ignores it, but writing the same byte keeps our frames
   comparable with rayforce's own. *)
let sym_w64 = 3

type t =
  | Null
  | Bool of bool
  | Byte of int
  | Short of int
  | Int32 of int
  | Int of int64
  | Float of float
  | Timestamp of int64
  | Date of int
  | Sym of Sym.t
  | String of string
  | Bools of bool array
  | Bytes of Bigstring.t
  | Ints of int64 array
  | Floats of float array
  | Timestamps of int64 array
  | Syms of Sym.t array
  | Strings of string array
  | List of t array
  | Table of column array
  | Dict of t * t
  | Error of string

and column =
  { name : Sym.t
  ; data : t
  }
[@@deriving sexp_of]

exception Protocol_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Protocol_error s)) fmt

let rec length = function
  | Bools a -> Array.length a
  | Bytes b -> Bigstring.length b
  | Ints a | Timestamps a -> Array.length a
  | Floats a -> Array.length a
  | Syms a -> Array.length a
  | Strings a -> Array.length a
  | List a -> Array.length a
  | Table cols -> if Array.is_empty cols then 0 else length cols.(0).data
  | Dict (k, _) -> length k
  | _ -> 1
;;

let nrows = function
  | Table cols -> if Array.is_empty cols then 0 else length cols.(0).data
  | v -> fail "nrows: not a table (%s)" (Sexp.to_string (sexp_of_t v))
;;

let ncols = function
  | Table cols -> Array.length cols
  | v -> fail "ncols: not a table (%s)" (Sexp.to_string (sexp_of_t v))
;;

let column v name =
  match v with
  | Table cols ->
    Array.find_map cols ~f:(fun c ->
      Option.some_if (String.equal (Sym.to_string c.name) name) c.data)
  | v -> fail "column: not a table (%s)" (Sexp.to_string (sexp_of_t v))
;;

let table cols =
  let cols =
    Array.of_list_map cols ~f:(fun (name, data) -> { name = Sym.intern name; data })
  in
  Array.iter cols ~f:(fun c ->
    if length c.data <> length cols.(0).data
    then
      fail
        "table: column %s has %d rows, %s has %d"
        (Sym.to_string c.name)
        (length c.data)
        (Sym.to_string cols.(0).name)
        (length cols.(0).data));
  Table cols
;;

(* ===== Size ===== *)

(* An atom is type(1) + flags(1) + value; a vector type(1) + attrs(1) +
   len(8) + payload. *)
let atom n = 2 + n
let vec n = 10 + n
let cstring s = String.length s + 1
let sym_bytes a = Array.sum (module Int) a ~f:(fun s -> cstring (Sym.to_string s))

let rec size = function
  | Null -> 1
  | Error _ -> 1 + 8
  | Bool _ | Byte _ -> atom 1
  | Short _ -> atom 2
  | Int32 _ | Date _ -> atom 4
  | Int _ | Float _ | Timestamp _ -> atom 8
  | Sym s -> atom (cstring (Sym.to_string s))
  | String s -> atom (8 + String.length s)
  | Bools a -> vec (Array.length a)
  | Bytes b -> vec (Bigstring.length b)
  | Ints a | Timestamps a -> vec (Array.length a * 8)
  | Floats a -> vec (Array.length a * 8)
  | Syms a -> vec (sym_bytes a)
  | Strings a -> vec (Array.sum (module Int) a ~f:(fun s -> 8 + String.length s))
  | List a -> vec (Array.sum (module Int) a ~f:size)
  | Table cols ->
    2
    + vec (Array.sum (module Int) cols ~f:(fun c -> cstring (Sym.to_string c.name)))
    + vec (Array.sum (module Int) cols ~f:(fun c -> size c.data))
  | Dict (k, v) -> 2 + size k + size v
;;

(* ===== Encode ===== *)

let fill_tag buf tag = Iobuf.Fill.int8_trunc buf tag

let fill_atom_head buf tag =
  fill_tag buf (-tag);
  Iobuf.Fill.uint8_trunc buf 0
;;

let fill_vec_head ?(attrs = 0) buf tag len =
  fill_tag buf tag;
  Iobuf.Fill.uint8_trunc buf attrs;
  Iobuf.Fill.int64_le buf len
;;

let fill_cstring buf s =
  Iobuf.Fill.stringo buf s;
  Iobuf.Fill.char buf '\000'
;;

let fill_syms ?attrs buf a =
  fill_vec_head ?attrs buf Tag.sym (Array.length a);
  Array.iter a ~f:(fun s -> fill_cstring buf (Sym.to_string s))
;;

let rec fill buf t =
  match t with
  | Null -> fill_tag buf Tag.null
  | Error code ->
    fill_tag buf Tag.error;
    (* The code is 8 bytes of packed ASCII, NUL-padded. *)
    Iobuf.Fill.tail_padded_fixed_string ~padding:'\000' ~len:8 buf code
  | Bool b ->
    fill_atom_head buf Tag.bool;
    Iobuf.Fill.uint8_trunc buf (Bool.to_int b)
  | Byte n ->
    fill_atom_head buf Tag.u8;
    Iobuf.Fill.uint8_trunc buf n
  | Short n ->
    fill_atom_head buf Tag.i16;
    Iobuf.Fill.int16_le_trunc buf n
  | Int32 n ->
    fill_atom_head buf Tag.i32;
    Iobuf.Fill.int32_le_trunc buf n
  | Date n ->
    fill_atom_head buf Tag.date;
    Iobuf.Fill.int32_le_trunc buf n
  | Int n ->
    fill_atom_head buf Tag.i64;
    Iobuf.Fill.int64_t_le buf n
  | Timestamp n ->
    fill_atom_head buf Tag.timestamp;
    Iobuf.Fill.int64_t_le buf n
  | Float f ->
    fill_atom_head buf Tag.f64;
    Iobuf.Fill.int64_t_le buf (Int64.bits_of_float f)
  | Sym s ->
    fill_atom_head buf Tag.sym;
    fill_cstring buf (Sym.to_string s)
  | String s ->
    fill_atom_head buf Tag.str;
    Iobuf.Fill.int64_le buf (String.length s);
    Iobuf.Fill.stringo buf s
  | Bools a ->
    fill_vec_head buf Tag.bool (Array.length a);
    Array.iter a ~f:(fun b -> Iobuf.Fill.uint8_trunc buf (Bool.to_int b))
  | Bytes b ->
    fill_vec_head buf Tag.u8 (Bigstring.length b);
    Iobuf.Fill.bigstringo buf b
  | Ints a ->
    fill_vec_head buf Tag.i64 (Array.length a);
    Array.iter a ~f:(Iobuf.Fill.int64_t_le buf)
  | Timestamps a ->
    fill_vec_head buf Tag.timestamp (Array.length a);
    Array.iter a ~f:(Iobuf.Fill.int64_t_le buf)
  | Floats a ->
    fill_vec_head buf Tag.f64 (Array.length a);
    Array.iter a ~f:(fun f -> Iobuf.Fill.int64_t_le buf (Int64.bits_of_float f))
  | Syms a -> fill_syms buf a
  | Strings a ->
    fill_vec_head buf Tag.str (Array.length a);
    Array.iter a ~f:(fun s ->
      Iobuf.Fill.int64_le buf (String.length s);
      Iobuf.Fill.stringo buf s)
  | List a ->
    fill_vec_head buf Tag.list (Array.length a);
    Array.iter a ~f:(fill buf)
  | Table cols ->
    fill_tag buf Tag.table;
    Iobuf.Fill.uint8_trunc buf 0;
    fill_syms ~attrs:sym_w64 buf (Array.map cols ~f:(fun c -> c.name));
    fill_vec_head buf Tag.list (Array.length cols);
    Array.iter cols ~f:(fun c -> fill buf c.data)
  | Dict (k, v) ->
    fill_tag buf Tag.dict;
    Iobuf.Fill.uint8_trunc buf 0;
    fill buf k;
    fill buf v
;;

(* ===== Decode ===== *)

let consume_len buf ~what =
  let len = Iobuf.Consume.int64_le_exn buf in
  (* Every element costs at least one byte on the wire, so a count past the
     end of the frame is a malformed frame, not a large one -- reject it
     before allocating anything that big. *)
  if len < 0 || len > Iobuf.length buf
  then
    fail "%s: length %d is not possible in %d remaining bytes" what len (Iobuf.length buf);
  len
;;

let consume_cstring buf =
  match Iobuf.Peek.index buf '\000' with
  | None -> fail "symbol with no terminator in %d remaining bytes" (Iobuf.length buf)
  | Some len ->
    let s = Iobuf.Consume.stringo buf ~len in
    let (_ : char) = Iobuf.Consume.char buf in
    s
;;

let consume_float buf = Int64.float_of_bits (Iobuf.Consume.int64_t_le buf)

(* Every element reads from the same cursor, so the array must be filled in
   wire order -- which [Array.init] does not promise. *)
let init_in_order len ~f =
  if len = 0
  then [||]
  else (
    let a = Array.create ~len (f ()) in
    for i = 1 to len - 1 do
      a.(i) <- f ()
    done;
    a)
;;

let rec consume buf =
  let tag = Iobuf.Consume.int8 buf in
  if tag = Tag.null
  then Null
  else if tag = Tag.error
  then (
    let code = Iobuf.Consume.stringo buf ~len:8 in
    Error (String.rstrip code ~drop:(Char.equal '\000')))
  else if tag < 0
  then consume_atom buf ~tag:(-tag)
  else consume_vec buf ~tag

and consume_atom buf ~tag =
  (* flags bit 0 is rayforce's typed-null marker. The value bytes follow
     either way; a null atom is returned as its zero value, which is what
     the sentinel encoding means for every other producer too. *)
  let (_flags : int) = Iobuf.Consume.uint8 buf in
  match tag with
  | t when t = Tag.bool -> Bool (Iobuf.Consume.uint8 buf <> 0)
  | t when t = Tag.u8 -> Byte (Iobuf.Consume.uint8 buf)
  | t when t = Tag.i16 -> Short (Iobuf.Consume.int16_le buf)
  | t when t = Tag.i32 -> Int32 (Iobuf.Consume.int32_le buf)
  | t when t = Tag.date -> Date (Iobuf.Consume.int32_le buf)
  | t when t = Tag.i64 -> Int (Iobuf.Consume.int64_t_le buf)
  | t when t = Tag.timestamp -> Timestamp (Iobuf.Consume.int64_t_le buf)
  | t when t = Tag.f64 -> Float (consume_float buf)
  | t when t = Tag.sym -> Sym (Sym.intern (consume_cstring buf))
  | t when t = Tag.str ->
    let len = consume_len buf ~what:"string atom" in
    String (Iobuf.Consume.stringo buf ~len)
  | t ->
    (match List.Assoc.find Tag.undecoded t ~equal:Int.equal with
     | Some name -> fail "a %s atom is not a value this library decodes" name
     | None -> fail "atom type %d is not a rayforce type" t)

and consume_vec buf ~tag =
  (match List.Assoc.find Tag.undecoded tag ~equal:Int.equal with
   | None -> ()
   | Some name -> fail "a %s is not a value this library decodes" name);
  let (_attrs : int) = Iobuf.Consume.uint8 buf in
  match tag with
  | t when t = Tag.table ->
    (* A table is its schema -- a sym vector of column names -- followed by
       its columns as a list. The two are read back together because
       neither is a table on its own. *)
    let names =
      match consume buf with
      | Syms names -> names
      | Ints ids -> Array.map ids ~f:(fun id -> Sym.intern (Int64.to_string id))
      | v ->
        fail "table schema is a %s, not a symbol vector" (Sexp.to_string (sexp_of_t v))
    in
    let cols =
      match consume buf with
      | List cols -> cols
      | v -> fail "table columns are a %s, not a list" (Sexp.to_string (sexp_of_t v))
    in
    if Array.length names <> Array.length cols
    then
      fail
        "table has %d column names and %d columns"
        (Array.length names)
        (Array.length cols);
    Table (Array.map2_exn names cols ~f:(fun name data -> { name; data }))
  | t when t = Tag.dict ->
    let keys = consume buf in
    let vals = consume buf in
    Dict (keys, vals)
  | t ->
    let len = consume_len buf ~what:"vector" in
    (match t with
     | t when t = Tag.bool ->
       Bools (init_in_order len ~f:(fun () -> Iobuf.Consume.uint8 buf <> 0))
     | t when t = Tag.u8 -> Bytes (Iobuf.Consume.bigstringo buf ~len)
     | t when t = Tag.i64 ->
       Ints (init_in_order len ~f:(fun () -> Iobuf.Consume.int64_t_le buf))
     | t when t = Tag.timestamp ->
       Timestamps (init_in_order len ~f:(fun () -> Iobuf.Consume.int64_t_le buf))
     | t when t = Tag.f64 -> Floats (init_in_order len ~f:(fun () -> consume_float buf))
     | t when t = Tag.sym ->
       Syms (init_in_order len ~f:(fun () -> Sym.intern (consume_cstring buf)))
     | t when t = Tag.str ->
       Strings
         (init_in_order len ~f:(fun () ->
            let slen = consume_len buf ~what:"string element" in
            Iobuf.Consume.stringo buf ~len:slen))
     | t when t = Tag.list -> List (init_in_order len ~f:(fun () -> consume buf))
     | t -> fail "vector type %d is not a rayforce type" t)
;;
