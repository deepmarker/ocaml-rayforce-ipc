(* The client against a real server. `dune runtest` drives this through
   run_live_test.sh, which starts a throwaway `rayforce -p <port>` and
   passes that port as argv.(1); run it by hand against a server of your
   own with `dune exec lib/rayforce-ipc/test/test_live.exe -- 16555`.

   A round trip through our own decoder proves only that we are
   self-consistent. What matters is that the other end agrees, so every
   check here is a value this client encoded, read back through rayforce's
   own evaluator. *)
open Core
open Rayforce_ipc

let sym s = Value.Sym (Sym.intern s)
let eval conn s = Conn.send_sync conn (Value.String s)

let check_eval conn expr expected =
  let got = eval conn expr in
  Alcotest.(check string)
    expr
    (Sexp.to_string (Value.sexp_of_t expected))
    (Sexp.to_string (Value.sexp_of_t got))
;;

let rows = 4

let depth_batch () =
  Value.table
    [ ( "ts_evt"
      , Value.Timestamps
          (Array.init rows ~f:(fun i ->
             Timestamp.of_time_ns
               (Time_ns.of_string_with_utc_offset (sprintf "2026-09-18 12:00:0%d.5Z" i))))
      )
    ; ( "instrument"
      , Value.Syms
          (Array.map [| "BTCUSDT"; "ETHUSDT"; "BTCUSDT"; "SOLUSDT" |] ~f:Sym.intern) )
    ; ( "px"
      , Value.Ints (Array.init rows ~f:(fun i -> Int64.of_int ((i + 1) * 1_000_000_000)))
      )
    ; "price", Value.Floats (Array.init rows ~f:(fun i -> 1.5 +. float_of_int i))
    ; "f_last", Value.Bools (Array.init rows ~f:(fun i -> i % 2 = 0))
    ]
;;

let main port =
  let conn = Conn.connect ~timeout_ms:5000 "127.0.0.1" port in
  Exn.protect
    ~finally:(fun () -> Conn.close conn)
    ~f:(fun () ->
      (* Sync: the server parses and evaluates the string we sent. *)
      check_eval conn "(+ 1 2)" (Value.Int 3L);
      check_eval conn "(til 4)" (Value.Ints [| 0L; 1L; 2L; 3L |]);
      (* Async: an ingest hook of the shape the tickerplant uses, so the
         batch is stored under the name the message carries rather than
         evaluated. *)
      (* The `null` is not decoration: `set` answers with the value it
         bound, and a lambda is not a value this client decodes. *)
      check_eval conn "(do (set .ipc.on.async (fn [m] (set last_upd m))) null)" Value.Null;
      Conn.send_async conn (Value.List [| sym "upd"; sym "depth"; depth_batch () |]);
      (* Async has no reply to wait on, and the hook runs on the server's
         own loop -- so read the result back with a sync call, which the
         server can only answer after it has processed what came before. *)
      check_eval conn "(at last_upd 0)" (sym "upd");
      check_eval conn "(at last_upd 1)" (sym "depth");
      check_eval conn "(count (at last_upd 2))" (Value.Int (Int64.of_int rows));
      check_eval
        conn
        "(cols (at last_upd 2))"
        (Value.Syms
           (Array.map [| "ts_evt"; "instrument"; "px"; "price"; "f_last" |] ~f:Sym.intern));
      check_eval
        conn
        "(get (at last_upd 2) 'instrument)"
        (Value.Syms
           (Array.map [| "BTCUSDT"; "ETHUSDT"; "BTCUSDT"; "SOLUSDT" |] ~f:Sym.intern));
      check_eval
        conn
        "(get (at last_upd 2) 'px)"
        (Value.Ints [| 1_000_000_000L; 2_000_000_000L; 3_000_000_000L; 4_000_000_000L |]);
      check_eval
        conn
        "(get (at last_upd 2) 'price)"
        (Value.Floats [| 1.5; 2.5; 3.5; 4.5 |]);
      check_eval
        conn
        "(get (at last_upd 2) 'f_last)"
        (Value.Bools [| true; false; true; false |]);
      (* The epoch offset is the easiest thing in the format to get wrong,
         and a server that disagrees says so in its own calendar. *)
      check_eval
        conn
        "(as 'STR (first (get (at last_upd 2) 'ts_evt)))"
        (Value.String "2026.09.18D12:00:00.500000000");
      (* A batch past the 2000-byte compression threshold, to prove the
         size headroom and that nothing in the frame depends on being
         small. *)
      let big =
        Value.table
          [ "i", Value.Ints (Array.init 5000 ~f:Int64.of_int)
          ; ( "s"
            , Value.Syms
                (Array.init 5000 ~f:(fun i -> Sym.intern (sprintf "s%d" (i % 97)))) )
          ]
      in
      Conn.send_async conn (Value.List [| sym "upd"; sym "big"; big |]);
      check_eval conn "(count (at last_upd 2))" (Value.Int 5000L);
      check_eval conn "(sum (get (at last_upd 2) 'i))" (Value.Int 12_497_500L))
;;

let () =
  let port =
    if Array.length (Sys.get_argv ()) > 1
    then Int.of_string (Sys.get_argv ()).(1)
    else 16555
  in
  Alcotest.run
    ~argv:[| "test_live" |]
    "rayforce-ipc live"
    [ "server", [ Alcotest.test_case "encode and read back" `Quick (fun () -> main port) ]
    ]
;;
