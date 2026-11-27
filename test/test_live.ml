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

let int64s = function
  | Value.Col c -> Array.init (Column.length c) ~f:(Column.get_int64 c)
  | _ -> failwith "not a column"
;;

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
      , Value.timestamps
          (Array.init rows ~f:(fun i ->
             Int64.of_int
               (Timestamp.of_time_ns
                  (Time_ns.of_string_with_utc_offset
                     (sprintf "2026-09-18 12:00:0%d.5Z" i))))) )
    ; ( "instrument"
      , Value.syms
          (Array.map [| "BTCUSDT"; "ETHUSDT"; "BTCUSDT"; "SOLUSDT" |] ~f:Sym.intern) )
    ; ( "px"
      , Value.ints (Array.init rows ~f:(fun i -> Int64.of_int ((i + 1) * 1_000_000_000)))
      )
    ; "price", Value.floats (Array.init rows ~f:(fun i -> 1.5 +. float_of_int i))
    ; "f_last", Value.bools (Array.init rows ~f:(fun i -> i % 2 = 0))
    ]
;;

let main port =
  let conn = Conn.connect ~timeout_ms:5000 "127.0.0.1" port in
  Exn.protect
    ~finally:(fun () -> Conn.close conn)
    ~f:(fun () ->
      (* Sync: the server parses and evaluates the string we sent. *)
      check_eval conn "(+ 1 2)" (Value.Int 3L);
      check_eval conn "(til 4)" (Value.ints [| 0L; 1L; 2L; 3L |]);
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
        (Value.syms
           (Array.map [| "ts_evt"; "instrument"; "px"; "price"; "f_last" |] ~f:Sym.intern));
      check_eval
        conn
        "(get (at last_upd 2) 'instrument)"
        (Value.syms
           (Array.map [| "BTCUSDT"; "ETHUSDT"; "BTCUSDT"; "SOLUSDT" |] ~f:Sym.intern));
      check_eval
        conn
        "(get (at last_upd 2) 'px)"
        (Value.ints [| 1_000_000_000L; 2_000_000_000L; 3_000_000_000L; 4_000_000_000L |]);
      check_eval
        conn
        "(get (at last_upd 2) 'price)"
        (Value.floats [| 1.5; 2.5; 3.5; 4.5 |]);
      check_eval
        conn
        "(get (at last_upd 2) 'f_last)"
        (Value.bools [| true; false; true; false |]);
      (* The epoch offset is the easiest thing in the format to get wrong,
         and a server that disagrees says so in its own calendar. *)
      check_eval
        conn
        "(as 'STR (first (get (at last_upd 2) 'ts_evt)))"
        (Value.String "2026.09.18D12:00:00.500000000");
      (* Timespan atoms, vectors, null sentinels and column builders through
         the engine's evaluator and serializer, not just our own decoder. *)
      check_eval conn "0D00:00:00.000000001" (Value.Timespan 1L);
      check_eval conn "-0D00:00:00.000000001" (Value.Timespan (-1L));
      check_eval conn "1D" (Value.Timespan 86400000000000L);
      check_eval conn "(do (set temporal_echo (fn [x] x)) null)" Value.Null;
      (Conn.send_sync
         conn
         (Value.List [| sym "temporal_echo"; Value.Timespan 9007199254740993L |])
       |> fun got ->
       Alcotest.(check string)
         "timespan precision"
         "(Timespan 9007199254740993)"
         (Sexp.to_string (Value.sexp_of_t got)));
      let spans = [| 0L; -1L; 86400000000000L; 9007199254740993L; Int64.min_value |] in
      let got =
        Conn.send_sync conn (Value.List [| sym "temporal_echo"; Value.timespans spans |])
      in
      Alcotest.(check (array int64)) "timespan vector" spans (int64s got);
      let null =
        Conn.send_sync conn (Value.List [| sym "nil?"; Value.Timespan Int64.min_value |])
      in
      (match null with
       | Value.Bool true -> ()
       | _ -> failwith "timespan atom null");
      let empty =
        Conn.send_sync conn (Value.List [| sym "temporal_echo"; Value.timespans [||] |])
      in
      Alcotest.(check int) "empty timespan vector" 0 (Array.length (int64s empty));
      let col = Column.timespan () in
      Array.iter spans ~f:(Column.add_int64 col);
      let got =
        Conn.send_sync
          conn
          (Value.List [| sym "temporal_echo"; Value.table [ "span", Value.Col col ] |])
      in
      Alcotest.(check (array int64))
        "timespan column"
        spans
        (int64s (Option.value_exn (Value.column got "span")));
      (* The server deserializes these values from the async message and
         serializes them again in the synchronous reply. *)
      let extra =
        Value.table
          [ "short", Value.shorts [| -7; 12 |]
          ; "i32", Value.int32s [| -42; 123456 |]
          ; "date", Value.dates [| 0; 1 |]
          ; "time", Value.times [| 0; 1234 |]
          ; "f32", Value.float32s [| 0x3f800000l; 0x40000000l |]
          ; "guid", Value.Guids [| "0123456789abcdef"; "fedcba9876543210" |]
          ]
      in
      Conn.send_async conn extra;
      check_eval conn "last_upd" extra;
      check_eval conn "0Nl" (Value.Atom_flags (1, Value.Int Int64.min_value));
      let check_function expr =
        let value = eval conn expr in
        (match value with
         | Value.Lambda _ | Value.Unary _ | Value.Binary _ | Value.Variadic _ -> ()
         | _ -> failwith "function response");
        Conn.send_async conn value;
        check_eval conn "last_upd" value
      in
      check_function "(fn [x] x)";
      check_function "+";
      (* A batch past the 2000-byte compression threshold, to prove the
         size headroom and that nothing in the frame depends on being
         small. *)
      let big =
        Value.table
          [ "i", Value.ints (Array.init 5000 ~f:Int64.of_int)
          ; ( "s"
            , Value.syms
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
