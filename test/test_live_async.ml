(* The Async client against a real server, the same way test_live.ml
   checks the blocking one: every value here is encoded by this client and
   read back through rayforce's own evaluator.

   What this covers that the blocking test cannot is the part that made an
   Async client worth having -- a send that returns without waiting on the
   socket, and a connection that a scheduler can hold. *)
open Core
open Async
open Rayforce_ipc
module Conn = Rayforce_ipc_async.Conn

let sym s = Value.Sym (Sym.intern s)

let check_eval conn expr expected =
  Conn.send_sync conn (Value.String expr)
  >>| Or_error.ok_exn
  >>| fun got ->
  Alcotest.(check string)
    expr
    (Sexp.to_string (Value.sexp_of_t expected))
    (Sexp.to_string (Value.sexp_of_t got))
;;

let rows = 4

let batch () =
  let ts = Column.timestamp () in
  let instrument = Column.sym () in
  let px = Column.i64 () in
  let f_last = Column.bool () in
  Array.iteri [| "BTCUSDT"; "ETHUSDT"; "BTCUSDT"; "SOLUSDT" |] ~f:(fun i s ->
    Column.add_int
      ts
      (Timestamp.of_time_ns
         (Time_ns.of_string_with_utc_offset (sprintf "2026-09-18 12:00:0%d.5Z" i)));
    Column.add_sym instrument (Sym.intern s);
    Column.add_int px ((i + 1) * 1_000_000_000);
    Column.add_bool f_last (i % 2 = 0));
  Value.table
    [ "ts_evt", Value.Col ts
    ; "instrument", Value.Col instrument
    ; "px", Value.Col px
    ; "f_last", Value.Col f_last
    ]
;;

let main port =
  Conn.connect ~timeout:(Time_ns.Span.of_int_sec 5) ~host:"127.0.0.1" ~port ()
  >>| Or_error.ok_exn
  >>= fun conn ->
  Monitor.protect
    ~finally:(fun () -> Conn.close conn)
    (fun () ->
       check_eval conn "(+ 1 2)" (Value.Int 3L)
       >>= fun () ->
       check_eval
         conn
         "(do (set .ipc.on.async (fn [m] (set last_upd m))) null)"
         Value.Null
       >>= fun () ->
       (* The point of the Async client: this returns without waiting on the
         socket, so a caller on the scheduler is never held by a slow
         server. What it wrote is still on its way out. *)
       Conn.send_async conn (Value.List [| sym "upd"; sym "depth"; batch () |]);
       (* And the sync call that follows can only be answered after the
         server has processed what went before it. *)
       check_eval conn "(count (at last_upd 2))" (Value.Int (Int64.of_int rows))
       >>= fun () ->
       check_eval
         conn
         "(get (at last_upd 2) 'instrument)"
         (Value.Syms
            (Array.map [| "BTCUSDT"; "ETHUSDT"; "BTCUSDT"; "SOLUSDT" |] ~f:Sym.intern))
       >>= fun () ->
       check_eval
         conn
         "(get (at last_upd 2) 'px)"
         (Value.Ints [| 1_000_000_000L; 2_000_000_000L; 3_000_000_000L; 4_000_000_000L |])
       >>= fun () ->
       check_eval
         conn
         "(get (at last_upd 2) 'f_last)"
         (Value.Bools [| true; false; true; false |])
       >>= fun () ->
       check_eval
         conn
         "(as 'STR (first (get (at last_upd 2) 'ts_evt)))"
         (Value.String "2026.09.18D12:00:00.500000000")
       >>= fun () ->
       (* A batch well past the frame's compression threshold, written
         without the caller ever blocking on the socket. *)
       let big = Column.i64 () in
       for i = 0 to 4999 do
         Column.add_int big i
       done;
       Conn.send_async
         conn
         (Value.List [| sym "upd"; sym "big"; Value.table [ "i", Value.Col big ] |]);
       check_eval conn "(sum (get (at last_upd 2) 'i))" (Value.Int 12_497_500L)
       >>= fun () ->
       Alcotest.(check int) "writer drained" 0 (Conn.bytes_to_write conn);
       Deferred.unit)
;;

let () =
  let argv = Sys.get_argv () in
  let port = if Array.length argv > 1 then Int.of_string argv.(1) else 16555 in
  Alcotest.run
    ~argv:[| "test_live_async" |]
    "rayforce-ipc-async live"
    [ ( "server"
      , [ Alcotest.test_case "encode and read back" `Quick (fun () ->
            Thread_safe.block_on_async_exn (fun () -> main port))
        ] )
    ]
;;
