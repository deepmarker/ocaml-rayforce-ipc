open! Core
open! Async
open! Rayforce_ipc

module Peer = struct
  type t =
    { address : Socket.Address.Inet.t
    ; mutable user : string option
    ; reader : Reader.t
    ; writer : Writer.t
    ; mutable scratch : (read_write, Iobuf.seek) Iobuf.t
    }

  let scratch_len = 64 * 1024

  (* A frame bigger than this is encoded into a buffer of its own rather
     than leaving a response-sized one pinned to the connection. *)
  let scratch_keep = 4 * 1024 * 1024

  let create address reader writer =
    { address; user = None; reader; writer; scratch = Iobuf.create ~len:scratch_len }
  ;;

  let address t = t.address
  let user t = t.user
  let bytes_to_write t = Writer.bytes_to_write t.writer
  let is_closed t = Writer.is_closed t.writer || Reader.is_closed t.reader

  let close_finished t =
    Deferred.all_unit [ Writer.close_finished t.writer; Reader.close_finished t.reader ]
  ;;

  let close t =
    Monitor.protect
      (fun () -> Writer.close t.writer)
      ~finally:(fun () -> Reader.close t.reader)
  ;;

  (* The frame is encoded into the connection's scratch buffer and copied
     into the writer before this returns, so the scratch is free again at
     once: a response costs no allocation, no deferred and no pool, where
     handing the writer a buffer of its own cost all three. *)
  let frame_buffer t ~len =
    if len > scratch_keep
    then Iobuf.create ~len
    else (
      if Iobuf.capacity t.scratch < len
      then t.scratch <- Iobuf.create ~len:(Int.ceil_pow2 len);
      let buf = t.scratch in
      Iobuf.reset buf;
      Iobuf.resize buf ~len;
      buf)
  ;;

  let send t ~msgtype v =
    if not (Writer.is_closed t.writer)
    then (
      let buf = frame_buffer t ~len:(Wire.frame_size v) in
      Wire.fill_frame buf ~msgtype v;
      Iobuf.flip_lo buf;
      Writer.write_bigstring
        t.writer
        (Iobuf.Expert.buf buf)
        ~pos:(Iobuf.Expert.lo buf)
        ~len:(Iobuf.length buf))
  ;;

  let push t v = send t ~msgtype:Async v
end

type t =
  { server : (Socket.Address.Inet.t, int) Tcp.Server.t
  ; port : int
  }

let listening_port t = t.port
let close t = Tcp.Server.close t.server
let close_finished t = Tcp.Server.close_finished t.server

(* ===== Frames ===== *)

type request =
  | Request of Wire.Msgtype.t * Value.t
  | Undecodable of Wire.Msgtype.t * exn

(* Frames are decoded in place from the reader's own buffer: nothing is
   allocated to read them, a frame that has already arrived costs no
   deferred at all, and several pipelined frames are served in one wakeup.
   Decoding copies what it keeps, so the buffer can be handed back at once. *)
let decode_request buf ~pos (header : Wire.header) =
  let payload =
    Iobuf.of_bigstring buf ~pos:(pos + Wire.header_len) ~len:header.payload_len
  in
  match Wire.consume_payload payload header with
  | v -> Request (header.msgtype, v)
  | exception exn -> Undecodable (header.msgtype, exn)
;;

(* ===== Handshake ===== *)

let read_byte reader =
  Reader.read_char reader
  >>| function
  | `Ok c -> Some (Char.to_int c)
  | `Eof -> None
;;

(* The credentials are [user:password\000]; a password may itself hold a
   colon, a user name may not. *)
let split_credentials cred =
  let cred = String.chop_suffix_if_exists cred ~suffix:"\000" in
  String.lsplit2 cred ~on:':'
;;

let read_credentials reader =
  read_byte reader
  >>= function
  | None -> return None
  | Some len ->
    let buf = Bytes.create len in
    Reader.really_read reader buf
    >>| (function
     | `Eof _ -> None
     | `Ok -> split_credentials (Bytes.to_string buf))
;;

let check_credentials reader writer ~authenticate =
  read_credentials reader
  >>= function
  | None ->
    Writer.write_char writer '\001';
    return None
  | Some (user, password) ->
    authenticate ~user ~password
    >>| fun accepted ->
    Writer.write_char writer (if accepted then '\000' else '\001');
    if accepted then Some user else None
;;

type handshake =
  | Refused
  | Accepted of string option

let handshake reader writer ~authenticate =
  let hello = Bytes.create 2 in
  Reader.really_read reader hello
  >>= function
  | `Eof _ -> return Refused
  | `Ok ->
    (* Whatever version it spoke, we answer with ours: that is how a client
       finds out why it was turned away. *)
    let auth_required = Bool.to_int (Option.is_some authenticate) in
    Writer.write_bytes
      writer
      (Bytes.of_char_list
         [ Char.of_int_exn Wire.wire_version; Char.of_int_exn auth_required ]);
    if Char.to_int (Bytes.get hello 0) <> Wire.wire_version
    then return Refused
    else (
      match authenticate with
      | None -> return (Accepted None)
      | Some authenticate ->
        check_credentials reader writer ~authenticate
        >>| (function
         | None -> Refused
         | Some user -> Accepted (Some user)))
;;

(* ===== Serving ===== *)

let error_response ~what exn =
  Value.Error { code = "error"; message = sprintf "%s: %s" what (Exn.to_string exn) }
;;

let run_sync ~on_sync peer v =
  Monitor.try_with ~run:`Now ~rest:`Log (fun () -> on_sync peer v)
  >>| function
  | Ok response -> response
  | Error exn -> error_response ~what:"evaluation failed" exn
;;

let run_async ~on_async peer v =
  Monitor.try_with ~run:`Now ~rest:`Log (fun () -> on_async peer v)
  >>| function
  | Ok () -> ()
  | Error exn ->
    Log.Global.error
      "rayforce server: async handler for %s raised: %s"
      (Socket.Address.Inet.to_string (Peer.address peer))
      (Exn.to_string exn)
;;

(* Returns [`Continue] while the connection is worth reading from. *)
let handle_request ~on_async ~on_sync peer = function
  | Request (Wire.Msgtype.Async, v) -> run_async ~on_async peer v >>| fun () -> `Continue
  | Request (Sync, v) ->
    run_sync ~on_sync peer v
    >>| fun response ->
    Peer.send peer ~msgtype:Response response;
    `Continue
  | Request (Response, _) ->
    Log.Global.error "rayforce server: client sent a response frame";
    return `Stop
  | Undecodable (Sync, exn) ->
    Peer.send peer ~msgtype:Response (error_response ~what:"undecodable request" exn);
    return `Continue
  | Undecodable ((Async | Response), exn) ->
    Log.Global.error "rayforce server: undecodable frame: %s" (Exn.to_string exn);
    return `Continue
;;

(* What the frame at [pos] needs before it can be served. *)
type next =
  | Frame of Wire.header * int (* the header, and the frame's total bytes *)
  | Need of int
  | Bad_header

let next_frame buf ~pos ~len =
  if len < Wire.header_len
  then Need Wire.header_len
  else (
    match Wire.consume_header (Iobuf.of_bigstring buf ~pos ~len:Wire.header_len) with
    | exception exn ->
      Log.Global.error "rayforce server: bad frame header: %s" (Exn.to_string exn);
      Bad_header
    | header ->
      let total = Wire.header_len + header.payload_len in
      if len < total then Need total else Frame (header, total))
;;

(* Serves every complete frame in the chunk, not just the first: the reader
   hands back bytes it already holds only when more arrive, so a burst whose
   last frame is a sync call would otherwise leave that call unanswered.
   A header that cannot be trusted ends the connection, since nothing after
   it can be framed; a payload that fails to decode has been read in full,
   so the stream is still in step and the connection carries on. *)
let handle_chunk ~on_async ~on_sync peer buf ~pos ~len =
  let handle = handle_request ~on_async ~on_sync peer in
  let rec serve ~used =
    match next_frame buf ~pos:(pos + used) ~len:(len - used) with
    | Bad_header -> return (`Stop ())
    | Need n -> return (`Consumed (used, `Need n))
    | Frame (header, total) ->
      handle (decode_request buf ~pos:(pos + used) header)
      >>= (function
       | `Continue -> serve ~used:(used + total)
       | `Stop -> return (`Stop_consumed ((), used + total)))
  in
  serve ~used:0
;;

let serve_requests ~on_async ~on_sync peer =
  Reader.read_one_chunk_at_a_time
    peer.Peer.reader
    ~handle_chunk:(handle_chunk ~on_async ~on_sync peer)
  >>| fun (_ : [ `Eof | `Eof_with_unconsumed_data of string | `Stopped of unit ]) -> ()
;;

let serve_peer
      ~authenticate
      ~on_connect
      ~on_disconnect
      ~on_async
      ~on_sync
      ~handshake_timeout
      address
      reader
      writer
  =
  let peer = Peer.create address reader writer in
  Clock_ns.with_timeout handshake_timeout (handshake reader writer ~authenticate)
  >>= function
  | `Timeout | `Result Refused -> Peer.close peer
  | `Result (Accepted user) ->
    peer.user <- user;
    on_connect peer;
    serve_requests ~on_async ~on_sync peer
    >>= fun () ->
    on_disconnect peer;
    Peer.close peer
;;

let create
      ?authenticate
      ?(on_connect = ignore)
      ?(on_disconnect = ignore)
      ?max_connections
      ?(handshake_timeout = Time_ns.Span.of_int_sec 5)
      ~where_to_listen
      ~on_async
      ~on_sync
      ()
  =
  let serve =
    serve_peer
      ~authenticate
      ~on_connect
      ~on_disconnect
      ~on_async
      ~on_sync
      ~handshake_timeout
  in
  let on_client address reader writer = serve address reader writer in
  Tcp.Server.create ?max_connections ~on_handler_error:`Ignore where_to_listen on_client
  >>| fun server -> { server; port = Tcp.Server.listening_on server }
;;
