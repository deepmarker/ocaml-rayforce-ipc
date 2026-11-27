open! Core
open! Async
open! Rayforce_ipc
module Server = Rayforce_ipc_server.Server

(* Echo server for benchmarking against a rayfall client: sync frames are
   answered with their own payload, async frames are counted. Sending the
   symbol `stats` as a sync frame answers the async count so far. *)

let async_count = ref 0

let on_async (_ : Server.Peer.t) (_ : Value.t) =
  incr async_count;
  return ()
;;

let is_stats = function
  | Value.Sym s | Atom_flags (_, Sym s) -> String.equal (Sym.to_string s) "stats"
  | _ -> false
;;

let on_sync (_ : Server.Peer.t) v =
  match v with
  | v when is_stats v -> return (Value.Int (Int64.of_int !async_count))
  | v -> return v
;;

let main port =
  let%bind server =
    Server.create
      ~where_to_listen:(Tcp.Where_to_listen.of_port port)
      ~on_async
      ~on_sync
      ()
  in
  printf "listening on %d\n%!" (Server.listening_port server);
  Server.close_finished server
;;

let () =
  let port = Int.of_string (Sys.get_argv ()).(1) in
  don't_wait_for (main port);
  never_returns (Scheduler.go ())
;;
