open! Core

(** Frames: a 16-byte header and a payload.

    The header names the byte order it was written in and the receiver
    refuses anything else, so the payload is native-endian -- little on
    every machine this runs on. *)

module Msgtype : sig
  type t =
    | Async (** fire and forget; the peer sends no response *)
    | Sync (** the peer evaluates and answers with a [Response] *)
    | Response
  [@@deriving compare, equal, sexp_of]
end

type header =
  { msgtype : Msgtype.t
  ; compressed : bool
  ; payload_len : int
  }
[@@deriving sexp_of]

val wire_version : int
val header_len : int
val max_payload : int

(** Bytes {!fill_frame} will write: header included. *)
val frame_size : Value.t -> int

val fill_frame : (read_write, Iobuf.seek) Iobuf.t -> msgtype:Msgtype.t -> Value.t -> unit

(** Raises {!Value.Protocol_error} on a frame this peer cannot be speaking:
    a bad prefix, a different wire version, or the other byte order. *)
val consume_header : ([> read ], Iobuf.seek) Iobuf.t -> header

(** Undo the delta-plus-RLE coding a server may apply to a frame. Exposed
    because it is the one piece of the format with no inverse here: this
    client never compresses, which the format allows -- compression is a
    per-frame sender-side decision with nothing to negotiate. *)
val decompress : Bigstring.t -> uncompressed_len:int -> Bigstring.t

(** Decode a payload of [header.payload_len] bytes, decompressing first if
    the header says so. The iobuf is advanced past the payload. *)
val consume_payload : ([> read ], Iobuf.seek) Iobuf.t -> header -> Value.t
