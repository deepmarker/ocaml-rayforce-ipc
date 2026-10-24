open Core
open Async
open Rayforce_ipc
module Conn = Rayforce_ipc_async.Conn

let sym text = Value.Sym (Sym.intern text)

let deadline operation =
  Clock_ns.with_timeout (Time_ns.Span.of_int_sec 3) operation
  >>= function
  | `Result result -> return result
  | `Timeout -> failwith "engine did not respond after cancellation"
;;

let rec await_marker path =
  Sys.file_exists path
  >>= function
  | `Yes -> Deferred.unit
  | `No | `Unknown ->
    Clock_ns.after (Time_ns.Span.of_int_ms 5) >>= fun () -> await_marker path
;;

let busy_query marker =
  Value.List
    [| sym "do"
     ; Value.List [| sym "write"; Value.String marker; Value.String "ready" |]
     ; Value.List [| sym "while"; Value.Bool true; Value.Int 0L |]
    |]
;;

let connect port = Conn.connect ~host:"127.0.0.1" ~port () >>| Or_error.ok_exn

let verify_cancel port =
  Unix.mkdtemp "/tmp/rayforce-ipc-cancel-XXXXXX"
  >>= fun directory ->
  let marker = Filename.concat directory "running" in
  connect port
  >>= fun busy ->
  connect port
  >>= fun probe ->
  connect port
  >>= fun idle ->
  let cleanup () =
    Conn.cancel_and_close busy
    >>= fun _ ->
    Conn.close probe
    >>= fun () ->
    Conn.close idle
    >>= fun () ->
    Sys.file_exists marker
    >>= (function
     | `Yes -> Unix.unlink marker
     | _ -> Deferred.unit)
    >>= fun () -> Unix.rmdir directory
  in
  let exercise () =
    let running =
      Monitor.try_with_or_error (fun () -> Conn.send_sync busy (busy_query marker))
    in
    deadline (await_marker marker)
    >>= fun () ->
    (* Cancelling a different socket must not interrupt the running query. *)
    Conn.cancel_and_close idle
    >>= fun cancelled ->
    Or_error.ok_exn cancelled;
    Clock_ns.after (Time_ns.Span.of_int_ms 30)
    >>= fun () ->
    if Deferred.is_determined running then failwith "idle socket cancelled another query";
    Conn.cancel_and_close busy
    >>= fun cancelled ->
    Or_error.ok_exn cancelled;
    deadline running
    >>= fun _ ->
    (* A closed local socket is not evidence of cancellation. A different
       connection must now get a result while the infinite loop would still run. *)
    deadline (Conn.send_sync probe (Value.String "(+ 1 2)"))
    >>= fun reply ->
    (match Or_error.ok_exn reply with
     | Value.Int n when Int64.equal n 3L -> ()
     | _ -> failwith "probe after cancellation failed");
    Conn.cancel_and_close busy >>| Or_error.ok_exn
  in
  Monitor.protect exercise ~finally:cleanup
;;

let verify_buffered_cancel () =
  let finish = Ivar.create () in
  let handler _address input output =
    let bytes = Bytes.create 2 in
    Reader.really_read input bytes
    >>= fun _ ->
    Writer.write output "\003\000";
    Writer.flushed output >>= fun () -> Ivar.read finish
  in
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.bind_to Localhost On_port_chosen_by_os)
    handler
  >>= fun server ->
  connect (Tcp.Server.listening_on server)
  >>= fun connection ->
  let cleanup () =
    Ivar.fill_if_empty finish ();
    Conn.close connection
    >>= fun () -> Tcp.Server.close ~close_existing_connections:true server
  in
  let exercise () =
    Conn.send_async connection (Value.String (String.make (8 * 1024 * 1024) 'x'));
    if Conn.bytes_to_write connection = 0 then failwith "expected buffered bytes";
    Clock_ns.with_timeout (Time_ns.Span.of_int_ms 500) (Conn.cancel_and_close connection)
    >>| function
    | `Timeout -> failwith "cancellation waited for buffered output to flush"
    | `Result result ->
      Or_error.ok_exn result;
      if not (Conn.is_closed connection) then failwith "cancelled connection still open"
  in
  Monitor.protect exercise ~finally:cleanup
;;

let verify_all port = verify_cancel port >>= fun () -> verify_buffered_cancel ()

let () =
  let port = Int.of_string (Sys.get_argv ()).(1) in
  don't_wait_for
    (Monitor.try_with (fun () -> verify_all port)
     >>= function
     | Ok () ->
       printf "PASS Async IPC engine cancellation\n";
       Shutdown.exit 0
     | Error exn ->
       eprintf "%s\n" (Exn.to_string exn);
       Shutdown.exit 1);
  never_returns (Scheduler.go ())
;;
