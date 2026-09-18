open! Core

module Msgtype = struct
  type t =
    | Async
    | Sync
    | Response
  [@@deriving compare, equal, sexp_of]

  let to_int = function
    | Async -> 0
    | Sync -> 1
    | Response -> 2
  ;;

  let of_int = function
    | 0 -> Async
    | 1 -> Sync
    | 2 -> Response
    | n -> Value.fail "message type %d is not one of async, sync, response" n
  ;;
end

type header =
  { msgtype : Msgtype.t
  ; compressed : bool
  ; payload_len : int
  }
[@@deriving sexp_of]

let prefix = 0xcefadefa
let wire_version = 3
let header_len = 16
let max_payload = 256 * 1024 * 1024
let flag_compressed = 0x01

(* 0 = little. A peer whose byte order differs rejects the frame rather than
   reading the columns backwards, which is the whole reason the field is
   there; we only ever write our own. *)
let endian_le = 0
let frame_size v = header_len + Value.size v

let fill_frame buf ~msgtype v =
  let payload_len = Value.size v in
  if payload_len > max_payload
  then
    Value.fail
      "payload of %d bytes is over the %d-byte frame limit"
      payload_len
      max_payload;
  Iobuf.Fill.uint32_le_trunc buf prefix;
  Iobuf.Fill.uint8_trunc buf wire_version;
  Iobuf.Fill.uint8_trunc buf 0;
  Iobuf.Fill.uint8_trunc buf endian_le;
  Iobuf.Fill.uint8_trunc buf (Msgtype.to_int msgtype);
  Iobuf.Fill.int64_le buf payload_len;
  Value.fill buf v
;;

let consume_header buf =
  let got_prefix = Iobuf.Consume.uint32_le buf in
  if got_prefix <> prefix
  then Value.fail "frame prefix %#x is not rayforce's %#x" got_prefix prefix;
  let version = Iobuf.Consume.uint8 buf in
  if version <> wire_version
  then
    Value.fail "peer speaks wire version %d, this client speaks %d" version wire_version;
  let flags = Iobuf.Consume.uint8 buf in
  let endian = Iobuf.Consume.uint8 buf in
  if endian <> endian_le then Value.fail "frame is big-endian; this client is not";
  let msgtype = Msgtype.of_int (Iobuf.Consume.uint8 buf) in
  let payload_len = Iobuf.Consume.int64_le_exn buf in
  if payload_len <= 0 || payload_len > max_payload
  then Value.fail "frame declares a %d-byte payload" payload_len;
  { msgtype; compressed = flags land flag_compressed <> 0; payload_len }
;;

(* Delta plus RLE, and the uncompressed size in the first four bytes. It is
   a sender-side choice with no negotiation, so this client never compresses
   -- but a server that does gets understood. *)
let decompress src ~uncompressed_len =
  let dst = Bigstring.create uncompressed_len in
  let clen = Bigstring.length src in
  let si = ref 0 in
  let di = ref 0 in
  (* Undo the RLE, then the delta, in one pass: the delta only ever refers
     to the byte before it, which is already final by the time we get here. *)
  let put byte =
    let b = if !di = 0 then byte else (byte + Char.to_int dst.{!di - 1}) land 0xff in
    dst.{!di} <- Char.of_int_exn b;
    incr di
  in
  while !si < clen && !di < uncompressed_len do
    let count = Char.to_int src.{!si} in
    let count = if count > 127 then count - 256 else count in
    incr si;
    if count > 0
    then (
      if !si >= clen then Value.fail "compressed frame: run with no value byte";
      if !di + count > uncompressed_len
      then Value.fail "compressed frame: run overruns the declared size";
      let v = Char.to_int src.{!si} in
      incr si;
      for _ = 1 to count do
        put v
      done)
    else (
      let n = -count in
      if !si + n > clen || !di + n > uncompressed_len
      then Value.fail "compressed frame: literal overruns the declared size";
      for _ = 1 to n do
        put (Char.to_int src.{!si});
        incr si
      done)
  done;
  if !di <> uncompressed_len
  then Value.fail "compressed frame: got %d bytes, expected %d" !di uncompressed_len;
  dst
;;

let consume_payload buf header =
  match header.compressed with
  | false -> Value.consume buf
  | true ->
    let uncompressed_len = Iobuf.Consume.uint32_le buf in
    if uncompressed_len <= 0 || uncompressed_len > max_payload
    then Value.fail "compressed frame declares %d uncompressed bytes" uncompressed_len;
    let src = Iobuf.Consume.bigstringo buf ~len:(header.payload_len - 4) in
    Value.consume (Iobuf.of_bigstring (decompress src ~uncompressed_len))
;;
