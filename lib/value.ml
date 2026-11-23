open! Core

(* The schema names of a table are the one place the C serializer writes a
   non-zero attrs byte: RAY_SYM_W64, the index width of the vector it came
   from. The decoder ignores it, but writing the same byte keeps our frames
   comparable with rayforce's own. *)
let sym_w64 = 3

type t =
  | Null
  | Atom_flags of int * t
  | Bool of bool
  | Byte of int
  | Short of int
  | Int32 of int
  | Int of int64
  | Float of float
  | Float32 of int32
  | Time of int
  | Guid of string
  | Timespan of int64
  | Timestamp of int64
  | Date of int
  | Sym of Sym.t
  | String of string
  | Bools of bool array
  | Bytes of Bigstring.t
  | Shorts of int array
  | Int32s of int array
  | Dates of int array
  | Times of int array
  | Float32s of int32 array
  | Guids of string array
  | Ints of int64 array
  | Floats of float array
  | Timespans of int64 array
  | Timestamps of int64 array
  | Syms of Sym.t array
  | Strings of string array
  | Col of Column.t
  | List of t array
  | Table of column array
  | Dict of t * t
  | Lambda of t * t * t option
  | Unary of string
  | Binary of string
  | Variadic of string
  | Error of
      { code : string
      ; message : string
      }

and column =
  { name : Sym.t
  ; data : t
  }
[@@deriving sexp_of]

exception Protocol_error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Protocol_error s)) fmt

(* An error record's message is length-prefixed by one byte. *)
let error_message_len message = Int.min (String.length message) 255

let rec length = function
  | Bools a -> Array.length a
  | Bytes b -> Bigstring.length b
  | Shorts a | Int32s a | Dates a | Times a -> Array.length a
  | Float32s a -> Array.length a
  | Guids a -> Array.length a
  | Ints a | Timestamps a | Timespans a -> Array.length a
  | Floats a -> Array.length a
  | Syms a -> Array.length a
  | Strings a -> Array.length a
  | Col c -> Column.length c
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

(* [Array.sum (module Int)] costs an indirect call per element -- it reaches
   its addition through a first-class module, which nothing inlines. A
   column is sized once per row before it is written, so that call was
   showing up as several percent of a producer's whole runtime. *)
let sum_by a ~f =
  let total = ref 0 in
  for i = 0 to Array.length a - 1 do
    total := !total + f (Array.unsafe_get a i)
  done;
  !total
;;

let sym_bytes a = sum_by a ~f:Sym.wire_len

let rec size = function
  | Null -> 1
  | Atom_flags (_, v) -> size v
  | Error { code = _; message } -> 1 + 8 + 1 + error_message_len message
  | Bool _ | Byte _ -> atom 1
  | Short _ -> atom 2
  | Guid _ -> atom 16
  | Int32 _ | Date _ | Time _ | Float32 _ -> atom 4
  | Int _ | Float _ | Timestamp _ | Timespan _ -> atom 8
  | Sym s -> atom (cstring (Sym.to_string s))
  | String s -> atom (8 + String.length s)
  | Bools a -> vec (Array.length a)
  | Bytes b -> vec (Bigstring.length b)
  | Shorts a -> vec (Array.length a * 2)
  | Int32s a | Dates a | Times a -> vec (Array.length a * 4)
  | Float32s a -> vec (Array.length a * 4)
  | Guids a -> vec (Array.length a * 16)
  | Ints a | Timestamps a | Timespans a -> vec (Array.length a * 8)
  | Floats a -> vec (Array.length a * 8)
  | Syms a -> vec (sym_bytes a)
  | Strings a -> vec (sum_by a ~f:(fun s -> 8 + String.length s))
  | Col c -> vec (Column.wire_size c)
  | List a -> vec (sum_by a ~f:size)
  | Table cols ->
    2
    + vec (sum_by cols ~f:(fun c -> Sym.wire_len c.name))
    + vec (sum_by cols ~f:(fun c -> size c.data))
  | Dict (k, v) -> 2 + size k + size v
  | Lambda (params, body, closure) ->
    2 + size params + size body + Option.value_map closure ~default:0 ~f:size
  | Unary n | Binary n | Variadic n -> 1 + cstring n
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

(* Every loop below iterates by index rather than through [Array.iter]: the
   closure it would take is called once per row and is not inlined, which a
   column's worth of rows makes worth avoiding. *)
let fill_each a ~f =
  for i = 0 to Array.length a - 1 do
    f (Array.unsafe_get a i)
  done
;;

let fill_sym buf s =
  let bytes = Sym.wire s in
  Iobuf.Fill.string buf bytes ~str_pos:0 ~len:(String.length bytes)
;;

let fill_syms ?attrs buf a =
  fill_vec_head ?attrs buf Tag.sym (Array.length a);
  fill_each a ~f:(fill_sym buf)
;;

let rec fill buf t =
  match t with
  | Null -> fill_tag buf Tag.null
  | Atom_flags (flags, v) ->
    if flags < 0 || flags > 255 then fail "invalid atom flags %d" flags;
    let temp = Iobuf.create ~len:(size v) in
    fill temp v;
    Iobuf.flip_lo temp;
    let tag = Iobuf.Consume.int8 temp in
    if tag >= 0 then fail "atom flags require an atom";
    fill_tag buf tag;
    let (_ : int) = Iobuf.Consume.uint8 temp in
    Iobuf.Fill.uint8_trunc buf flags;
    Iobuf.Fill.stringo buf (Iobuf.Consume.stringo temp ~len:(Iobuf.length temp))
  | Error { code; message } ->
    fill_tag buf Tag.error;
    (* The code is 8 bytes of packed ASCII, NUL-padded; the message is one
       length byte and that many bytes, cut at 255 as rayforce cuts it. *)
    Iobuf.Fill.tail_padded_fixed_string ~padding:'\000' ~len:8 buf code;
    let len = error_message_len message in
    Iobuf.Fill.uint8_trunc buf len;
    Iobuf.Fill.stringo buf (String.prefix message len)
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
  | Time n ->
    fill_atom_head buf Tag.time;
    Iobuf.Fill.int32_le_trunc buf n
  | Float32 n ->
    fill_atom_head buf Tag.f32;
    Iobuf.Fill.int32_le_trunc buf (Int32.to_int_exn n)
  | Guid bytes ->
    if String.length bytes <> 16 then fail "GUID must contain 16 bytes";
    fill_atom_head buf Tag.guid;
    Iobuf.Fill.stringo buf bytes
  | Date n ->
    fill_atom_head buf Tag.date;
    Iobuf.Fill.int32_le_trunc buf n
  | Int n ->
    fill_atom_head buf Tag.i64;
    Iobuf.Fill.int64_t_le buf n
  | Timespan n ->
    fill_atom_head buf Tag.timespan;
    Iobuf.Fill.int64_t_le buf n
  | Timestamp n ->
    fill_atom_head buf Tag.timestamp;
    Iobuf.Fill.int64_t_le buf n
  | Float f ->
    fill_atom_head buf Tag.f64;
    Iobuf.Fill.int64_t_le buf (Int64.bits_of_float f)
  | Sym s ->
    fill_atom_head buf Tag.sym;
    fill_sym buf s
  | String s ->
    fill_atom_head buf Tag.str;
    Iobuf.Fill.int64_le buf (String.length s);
    Iobuf.Fill.stringo buf s
  | Bools a ->
    fill_vec_head buf Tag.bool (Array.length a);
    fill_each a ~f:(fun b -> Iobuf.Fill.uint8_trunc buf (Bool.to_int b))
  | Bytes b ->
    fill_vec_head buf Tag.u8 (Bigstring.length b);
    Iobuf.Fill.bigstringo buf b
  | Shorts a ->
    fill_vec_head buf Tag.i16 (Array.length a);
    fill_each a ~f:(Iobuf.Fill.int16_le_trunc buf)
  | (Int32s a | Dates a | Times a) as v ->
    let tag =
      match v with
      | Int32s _ -> Tag.i32
      | Dates _ -> Tag.date
      | _ -> Tag.time
    in
    fill_vec_head buf tag (Array.length a);
    fill_each a ~f:(Iobuf.Fill.int32_le_trunc buf)
  | Float32s a ->
    fill_vec_head buf Tag.f32 (Array.length a);
    fill_each a ~f:(fun n -> Iobuf.Fill.int32_le_trunc buf (Int32.to_int_exn n))
  | Guids a ->
    fill_vec_head buf Tag.guid (Array.length a);
    fill_each a ~f:(fun bytes ->
      if String.length bytes <> 16 then fail "GUID must contain 16 bytes";
      Iobuf.Fill.stringo buf bytes)
  | Ints a ->
    fill_vec_head buf Tag.i64 (Array.length a);
    fill_each a ~f:(fun n -> Iobuf.Fill.int64_t_le buf n)
  | Timespans a ->
    fill_vec_head buf Tag.timespan (Array.length a);
    fill_each a ~f:(fun n -> Iobuf.Fill.int64_t_le buf n)
  | Timestamps a ->
    fill_vec_head buf Tag.timestamp (Array.length a);
    fill_each a ~f:(fun n -> Iobuf.Fill.int64_t_le buf n)
  | Floats a ->
    fill_vec_head buf Tag.f64 (Array.length a);
    fill_each a ~f:(fun f -> Iobuf.Fill.int64_t_le buf (Int64.bits_of_float f))
  | Syms a -> fill_syms buf a
  | Strings a ->
    fill_vec_head buf Tag.str (Array.length a);
    fill_each a ~f:(fun s ->
      Iobuf.Fill.int64_le buf (String.length s);
      Iobuf.Fill.stringo buf s)
  | Col c ->
    fill_vec_head buf (Column.tag c) (Column.length c);
    Column.fill buf c
  | List a ->
    fill_vec_head buf Tag.list (Array.length a);
    fill_each a ~f:(fill buf)
  | Table cols ->
    fill_tag buf Tag.table;
    Iobuf.Fill.uint8_trunc buf 0;
    fill_syms ~attrs:sym_w64 buf (Array.map cols ~f:(fun c -> c.name));
    fill_vec_head buf Tag.list (Array.length cols);
    fill_each cols ~f:(fun c -> fill buf c.data)
  | Dict (k, v) ->
    fill_tag buf Tag.dict;
    Iobuf.Fill.uint8_trunc buf 0;
    fill buf k;
    fill buf v
  | Lambda (params, body, closure) ->
    fill_tag buf Tag.lambda;
    Iobuf.Fill.uint8_trunc buf (if Option.is_some closure then 1 else 0);
    fill buf params;
    fill buf body;
    Option.iter closure ~f:(fill buf)
  | (Unary n | Binary n | Variadic n) as v ->
    fill_tag
      buf
      (match v with
       | Unary _ -> Tag.unary
       | Binary _ -> Tag.binary
       | _ -> Tag.variadic);
    Iobuf.Fill.stringo buf n;
    Iobuf.Fill.uint8_trunc buf 0
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
    let len = Iobuf.Consume.uint8 buf in
    let message = Iobuf.Consume.stringo buf ~len in
    Error { code = String.rstrip code ~drop:(Char.equal '\000'); message })
  else if tag < 0
  then consume_atom buf ~tag:(-tag)
  else consume_vec buf ~tag

and consume_atom buf ~tag =
  (* Preserve atom flags as well as the payload, including typed nulls
     and quoted symbols. *)
  let flags = Iobuf.Consume.uint8 buf in
  let value =
    match tag with
    | t when t = Tag.bool -> Bool (Iobuf.Consume.uint8 buf <> 0)
    | t when t = Tag.u8 -> Byte (Iobuf.Consume.uint8 buf)
    | t when t = Tag.i16 -> Short (Iobuf.Consume.int16_le buf)
    | t when t = Tag.i32 -> Int32 (Iobuf.Consume.int32_le buf)
    | t when t = Tag.date -> Date (Iobuf.Consume.int32_le buf)
    | t when t = Tag.time -> Time (Iobuf.Consume.int32_le buf)
    | t when t = Tag.f32 -> Float32 (Int32.of_int_exn (Iobuf.Consume.int32_le buf))
    | t when t = Tag.guid -> Guid (Iobuf.Consume.stringo buf ~len:16)
    | t when t = Tag.i64 -> Int (Iobuf.Consume.int64_t_le buf)
    | t when t = Tag.timespan -> Timespan (Iobuf.Consume.int64_t_le buf)
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
  in
  if flags = 0 then value else Atom_flags (flags, value)

and consume_vec buf ~tag =
  (match List.Assoc.find Tag.undecoded tag ~equal:Int.equal with
   | None -> ()
   | Some name -> fail "a %s is not a value this library decodes" name);
  if tag = Tag.unary
  then Unary (consume_cstring buf)
  else if tag = Tag.binary
  then Binary (consume_cstring buf)
  else if tag = Tag.variadic
  then Variadic (consume_cstring buf)
  else (
    let attrs = Iobuf.Consume.uint8 buf in
    match tag with
    | t when t = Tag.lambda ->
      let params = consume buf in
      let body = consume buf in
      let closure = if attrs land 1 <> 0 then Some (consume buf) else None in
      Lambda (params, body, closure)
    | t when t = Tag.table ->
      (* A table is its schema -- a sym vector of column names -- followed by
       its columns as a list. The two are read back together because
       neither is a table on its own. *)
      let names =
        match consume buf with
        | Syms names -> names
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
       | t when t = Tag.i16 ->
         Shorts (init_in_order len ~f:(fun () -> Iobuf.Consume.int16_le buf))
       | t when t = Tag.i32 ->
         Int32s (init_in_order len ~f:(fun () -> Iobuf.Consume.int32_le buf))
       | t when t = Tag.date ->
         Dates (init_in_order len ~f:(fun () -> Iobuf.Consume.int32_le buf))
       | t when t = Tag.time ->
         Times (init_in_order len ~f:(fun () -> Iobuf.Consume.int32_le buf))
       | t when t = Tag.f32 ->
         Float32s
           (init_in_order len ~f:(fun () -> Int32.of_int_exn (Iobuf.Consume.int32_le buf)))
       | t when t = Tag.guid ->
         Guids (init_in_order len ~f:(fun () -> Iobuf.Consume.stringo buf ~len:16))
       | t when t = Tag.i64 ->
         Ints (init_in_order len ~f:(fun () -> Iobuf.Consume.int64_t_le buf))
       | t when t = Tag.timespan ->
         Timespans (init_in_order len ~f:(fun () -> Iobuf.Consume.int64_t_le buf))
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
       | t -> fail "vector type %d is not a rayforce type" t))
;;
