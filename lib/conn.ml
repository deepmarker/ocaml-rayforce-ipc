open! Core
module Unix = Core_unix

type t =
  { fd : Unix.File_descr.t
  ; mutable out : (read_write, Iobuf.seek) Iobuf.t
  ; mutable in_ : (read_write, Iobuf.seek) Iobuf.t
  ; mutable closed : bool
  }

let initial_buf = 64 * 1024

(* Both buffers are kept and reused: a feed sends batches of a similar size
   forever, so the second one already fits. *)
let ensure buf ~len =
  if Iobuf.capacity buf >= len then buf else Iobuf.create ~len:(Int.ceil_pow2 len)
;;

let is_closed t = t.closed

let close t =
  if not t.closed
  then (
    t.closed <- true;
    try Unix.close t.fd with
    | _ -> ())
;;

let write_all t buf =
  while Iobuf.length buf > 0 do
    Iobuf_unix.write buf t.fd
  done
;;

let read_exact t ~len =
  t.in_ <- ensure t.in_ ~len;
  let buf = t.in_ in
  Iobuf.reset buf;
  Iobuf.resize buf ~len;
  while Iobuf.length buf > 0 do
    match Iobuf_unix.read buf t.fd with
    | Ok -> ()
    | Eof -> Value.fail "connection closed with %d bytes still to read" (Iobuf.length buf)
  done;
  Iobuf.flip_lo buf;
  buf
;;

(* ===== Handshake ===== *)

let with_deadline fd ~timeout_ms ~f =
  let secs = Float.of_int timeout_ms /. 1000. in
  Unix.setsockopt_float fd SO_RCVTIMEO secs;
  Unix.setsockopt_float fd SO_SNDTIMEO secs;
  Exn.protect ~f ~finally:(fun () ->
    Unix.setsockopt_float fd SO_RCVTIMEO 0.;
    Unix.setsockopt_float fd SO_SNDTIMEO 0.)
;;

let connect_with_timeout fd addr ~timeout_ms =
  Unix.set_nonblock fd;
  (try Unix.connect fd ~addr with
   | Unix.Unix_error ((EINPROGRESS | EWOULDBLOCK | EAGAIN), _, _) ->
     let { Unix.Select_fds.write; _ } =
       Unix.select
         ~read:[]
         ~write:[ fd ]
         ~except:[]
         ~timeout:(`After (Time_ns.Span.of_int_ms timeout_ms))
         ()
     in
     if List.is_empty write then raise (Unix.Unix_error (ETIMEDOUT, "connect", ""));
     (* The socket is writable either way; a second connect is what tells
        the two apart -- EISCONN means it landed, anything else is the
        error the first attempt deferred. *)
     (try Unix.connect fd ~addr with
      | Unix.Unix_error (EISCONN, _, _) -> ()));
  Unix.clear_nonblock fd
;;

let handshake t ~user ~password =
  let buf = Iobuf.create ~len:2 in
  Iobuf.Fill.uint8_trunc buf Wire.wire_version;
  Iobuf.Fill.uint8_trunc buf 0;
  Iobuf.flip_lo buf;
  write_all t buf;
  let resp = read_exact t ~len:2 in
  let version = Iobuf.Consume.uint8 resp in
  if version <> Wire.wire_version
  then
    Value.fail
      "server speaks wire version %d, this client speaks %d"
      version
      Wire.wire_version;
  match Iobuf.Consume.uint8 resp with
  | 0 -> ()
  | 1 ->
    let password =
      match password with
      | Some p -> p
      | None -> Value.fail "server requires credentials and none were given"
    in
    let cred = sprintf "%s:%s\000" (Option.value user ~default:"") password in
    let len = String.length cred in
    if len > 255 then Value.fail "credentials are %d bytes; the wire allows 255" len;
    let buf = Iobuf.create ~len:(len + 1) in
    Iobuf.Fill.uint8_trunc buf len;
    Iobuf.Fill.stringo buf cred;
    Iobuf.flip_lo buf;
    write_all t buf;
    if Iobuf.Consume.uint8 (read_exact t ~len:1) <> 0
    then Value.fail "server rejected the credentials"
  | n -> Value.fail "server answered the handshake with an auth byte of %d" n
;;

let connect ?user ?password ?(timeout_ms = 5000) host port =
  let addr = Unix.ADDR_INET (Unix.Inet_addr.of_string_or_getbyname host, port) in
  let fd = Unix.socket ~domain:PF_INET ~kind:SOCK_STREAM ~protocol:0 () in
  let t =
    { fd
    ; out = Iobuf.create ~len:initial_buf
    ; in_ = Iobuf.create ~len:initial_buf
    ; closed = false
    }
  in
  (try
     connect_with_timeout fd addr ~timeout_ms;
     Unix.setsockopt fd TCP_NODELAY true;
     with_deadline fd ~timeout_ms ~f:(fun () -> handshake t ~user ~password)
   with
   | exn ->
     close t;
     raise exn);
  t
;;

(* ===== Messages ===== *)

let send t ~msgtype v =
  if t.closed then Value.fail "send on a closed connection";
  t.out <- ensure t.out ~len:(Wire.frame_size v);
  let buf = t.out in
  Iobuf.reset buf;
  Wire.fill_frame buf ~msgtype v;
  Iobuf.flip_lo buf;
  write_all t buf
;;

let send_async t v = send t ~msgtype:Async v

let read_frame t =
  let header = Wire.consume_header (read_exact t ~len:Wire.header_len) in
  let payload = read_exact t ~len:header.payload_len in
  header.msgtype, Wire.consume_payload payload header
;;

let send_sync ?(on_push = fun (_ : Value.t) -> ()) t v =
  send t ~msgtype:Sync v;
  let rec await () =
    match read_frame t with
    | Wire.Msgtype.Response, v -> v
    | (Async | Sync), v ->
      on_push v;
      await ()
  in
  await ()
;;
