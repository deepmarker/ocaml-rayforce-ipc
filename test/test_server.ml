open! Core
open! Async
open! Rayforce_ipc
module Client = Rayforce_ipc_async.Conn
module Server = Rayforce_ipc_server.Server

let seen_async = Ivar.create ()

let on_async (_ : Server.Peer.t) v =
  Ivar.fill_if_empty seen_async v;
  return ()
;;

let on_sync (_ : Server.Peer.t) = function
  | Value.String "boom" -> failwith "boom"
  | v -> return v
;;

let authenticate ~user ~password =
  return (String.equal user "u" && String.equal password "p")
;;

let start ?authenticate () =
  Server.create
    ?authenticate
    ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
    ~on_async
    ~on_sync
    ()
;;

let connect ?user ?password server =
  Client.connect ?user ?password ~host:"127.0.0.1" ~port:(Server.listening_port server) ()
;;

let check b msg = if not b then failwith msg

let main () =
  let%bind server = start () in
  let%bind client = connect server >>| Or_error.ok_exn in
  let table =
    Value.table
      [ "x", Value.ints [| 1L; 2L |]
      ; "s", Value.syms [| Sym.intern "a"; Sym.intern "b" |]
      ]
  in
  let%bind echoed = Client.send_sync client table >>| Or_error.ok_exn in
  check (Value.nrows echoed = 2 && Value.ncols echoed = 2) "table echo";
  Client.send_async client (Value.Int 7L);
  let%bind got = Ivar.read seen_async in
  check
    (match got with
     | Value.Int 7L -> true
     | _ -> false)
    "async delivered";
  let%bind err = Client.send_sync client (String "boom") >>| Or_error.ok_exn in
  check
    (match err with
     | Value.Error _ -> true
     | _ -> false)
    "handler exception is an Error value";
  let%bind again = Client.send_sync client (Int 1L) >>| Or_error.ok_exn in
  check
    (match again with
     | Value.Int 1L -> true
     | _ -> false)
    "connection survives an error";
  let%bind () = Client.close client in
  let%bind () = Server.close server in
  (* credentials *)
  let%bind server = start ~authenticate () in
  let%bind bad = connect server ~user:"u" ~password:"nope" in
  check (Or_error.is_error bad) "bad password refused";
  let%bind none = connect server in
  check (Or_error.is_error none) "missing credentials refused";
  let%bind good = connect server ~user:"u" ~password:"p" >>| Or_error.ok_exn in
  let%bind r = Client.send_sync good (Int 3L) >>| Or_error.ok_exn in
  check
    (match r with
     | Value.Int 3L -> true
     | _ -> false)
    "authenticated call";
  let%bind () = Client.close good in
  Server.close server
;;

let () =
  Thread_safe.block_on_async_exn (fun () ->
    let%map () = main () in
    print_endline "test_server: ok")
;;
