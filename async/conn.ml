open! Core
open! Async
open! Rayforce_ipc

type t =
  { reader : Reader.t
  ; writer : Writer.t
  ; (* Frames are handed to the writer rather than copied into it, so a
       buffer stays spoken for until the writer says it is done with it and
       cannot be refilled before then. Hence a pool: a steady producer
       settles on two or three and allocates no more. *)
    mutable free : (read_write, Iobuf.seek) Iobuf.t list
  ; sync : unit Sequencer.t
  }

let initial_buf = 64 * 1024
let close t = Writer.close t.writer >>= fun () -> Reader.close t.reader
let is_closed t = Writer.is_closed t.writer || Reader.is_closed t.reader

let close_finished t =
  Deferred.all_unit [ Writer.close_finished t.writer; Reader.close_finished t.reader ]
;;

let bytes_to_write t = Writer.bytes_to_write t.writer

(* ===== Frames ===== *)

(* Take a buffer of at least [len] from the pool, or make one. A feed sends
   frames of a similar size forever, so after the first few this neither
   allocates nor copies. *)
let take t ~len =
  match List.find t.free ~f:(fun b -> Iobuf.capacity b >= len) with
  | Some b ->
    t.free <- List.filter t.free ~f:(fun x -> not (phys_equal x b));
    Iobuf.reset b;
    Iobuf.resize b ~len;
    b
  | None -> Iobuf.create ~len:(Int.max initial_buf (Int.ceil_pow2 len))
;;

let send t ~msgtype v =
  let buf = take t ~len:(Wire.frame_size v) in
  Wire.fill_frame buf ~msgtype v;
  Iobuf.flip_lo buf;
  (* The writer takes the buffer rather than copying it -- a batch is
     hundreds of kilobytes and copying it once more per send was worth
     several percent of a feed's CPU. [schedule_iobuf_consume] says when
     this particular buffer is free again, so it goes back in the pool
     then rather than when the writer happens to have flushed everything. *)
  don't_wait_for
    (Writer.schedule_iobuf_consume t.writer buf
     >>| fun () -> if not (Writer.is_closed t.writer) then t.free <- buf :: t.free)
;;

let send_async t v = send t ~msgtype:Async v

let read_exactly t ~len =
  let buf = Iobuf.create ~len in
  Reader.really_read_bigsubstring
    t.reader
    (Bigsubstring.create (Iobuf.Expert.buf buf) ~pos:0 ~len)
  >>| function
  | `Ok -> Ok buf
  | `Eof n -> Or_error.errorf "rayforce: connection closed after %d of %d bytes" n len
;;

let read_frame t =
  read_exactly t ~len:Wire.header_len
  >>=? fun buf ->
  match Wire.consume_header buf with
  | exception exn -> return (Or_error.of_exn exn)
  | header ->
    read_exactly t ~len:header.payload_len
    >>|? fun payload ->
    (match Wire.consume_payload payload header with
     | v -> header.msgtype, v
     | exception exn -> Exn.reraise exn "rayforce: undecodable payload")
;;

let send_sync ?(on_push = fun (_ : Value.t) -> ()) t v =
  (* One outstanding request at a time: the protocol has no request id, so
     a response belongs to whoever asked last. *)
  Throttle.enqueue t.sync (fun () ->
    match send t ~msgtype:Sync v with
    | exception exn -> return (Or_error.of_exn exn)
    | () ->
      Deferred.repeat_until_finished () (fun () ->
        read_frame t
        >>| function
        | Error _ as e -> `Finished e
        | Ok (Wire.Msgtype.Response, v) -> `Finished (Ok v)
        | Ok ((Async | Sync), v) ->
          on_push v;
          `Repeat ()))
;;

(* ===== Handshake ===== *)

let handshake t ~user ~password =
  Writer.write_bytes
    t.writer
    (Bytes.of_char_list [ Char.of_int_exn Wire.wire_version; '\000' ]);
  let resp = Bytes.create 2 in
  Reader.really_read t.reader resp
  >>= function
  | `Eof _ ->
    return (Or_error.error_string "rayforce: server closed during the handshake")
  | `Ok ->
    let version = Char.to_int (Bytes.get resp 0) in
    if version <> Wire.wire_version
    then
      return
        (Or_error.errorf
           "rayforce: server speaks wire version %d, this client speaks %d"
           version
           Wire.wire_version)
    else (
      match Char.to_int (Bytes.get resp 1) with
      | 0 -> return (Ok ())
      | 1 ->
        (match password with
         | None ->
           return
             (Or_error.error_string
                "rayforce: server requires credentials and none were given")
         | Some password ->
           let cred = sprintf "%s:%s\000" (Option.value user ~default:"") password in
           let len = String.length cred in
           if len > 255
           then
             return
               (Or_error.errorf
                  "rayforce: credentials are %d bytes; the wire allows 255"
                  len)
           else (
             Writer.write_char t.writer (Char.of_int_exn len);
             Writer.write t.writer cred;
             let verdict = Bytes.create 1 in
             Reader.really_read t.reader verdict
             >>| function
             | `Eof _ ->
               Or_error.error_string "rayforce: server closed while authenticating"
             | `Ok ->
               if Char.to_int (Bytes.get verdict 0) = 0
               then Ok ()
               else Or_error.error_string "rayforce: server rejected the credentials"))
      | n -> return (Or_error.errorf "rayforce: handshake auth byte %d is not 0 or 1" n))
;;

let connect ?user ?password ?(timeout = Time_ns.Span.of_int_sec 5) ~host ~port () =
  Monitor.try_with_or_error ~rest:`Log (fun () ->
    Tcp.connect
      ~timeout:(Time_ns.Span.to_span_float_round_nearest timeout)
      (Tcp.Where_to_connect.of_host_and_port { host; port }))
  >>=? fun (socket, reader, writer) ->
  Socket.setopt socket Socket.Opt.nodelay true;
  let t =
    { reader; writer; free = []; sync = Sequencer.create ~continue_on_error:true () }
  in
  (* The handshake gets the same budget as the connect: a server that
     accepts and then says nothing must not hold a client forever. *)
  Clock_ns.with_timeout timeout (handshake t ~user ~password)
  >>= function
  | `Result (Ok ()) -> return (Ok t)
  | `Result (Error _ as e) -> close t >>| fun () -> e
  | `Timeout ->
    close t
    >>| fun () -> Or_error.errorf "rayforce: %s:%d did not answer the handshake" host port
;;
