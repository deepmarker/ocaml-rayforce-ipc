# rayforce-ipc

RayforceDB's IPC protocol in OCaml, written against the wire format rather
than against `librayforce`. No C stubs, no embedded VM, no symbol table
shared with a server, and nothing in the process that cares which thread it
runs on.

```ocaml
let conn = Rayforce_ipc.Conn.connect ~timeout_ms:5000 "127.0.0.1" 5010 in
let batch =
  Rayforce_ipc.Value.table
    [ "ts_evt", Timestamps ts
    ; "instrument", Syms instruments
    ; "px", Ints px
    ]
in
Rayforce_ipc.Conn.send_async conn (List [| Sym (Sym.intern "upd"); Sym (Sym.intern "depth"); batch |])
```

## What is here

| Module | What it is |
|---|---|
| `Sym` | Local intern table. A symbol travels as a string, so an id is ours alone — it exists so a column holds one int per row. |
| `Value` | The value model, sized and filled straight into an `Iobuf`. A column is an ordinary OCaml array. |
| `Wire` | The 16-byte frame header, and the delta+RLE decoder for a compressed frame. |
| `Conn` | A blocking client: connect, handshake, `send_async`, `send_sync`. |
| `Rayforce_ipc_async.Conn` | The same, on the Async scheduler (package `rayforce-ipc-async`). A send hands the frame to the writer and returns, so a slow server backs up in a buffer rather than stalling the caller; and it satisfies `Persistent_connection_kernel.Closable`. |
| `Timestamp` | Checked conversion between Core Unix time and Rayforce's 2000-01-01 epoch. |
| `Timespan` | Checked conversion between signed int64 nanoseconds and Core `Time_ns.Span`. |

## The format

Wire version 3, as of rayforce v2.7. The authority is
`src/store/serde.{h,c}` and `src/core/ipc.{h,c}` in the rayforce tree.

- **Handshake** — the client sends `{3, 0}`; the server answers
  `{version, auth_required}`, and if it asked for credentials, one length
  byte plus `user:password\0` and a one-byte verdict.
- **Frame** — `prefix 0xcefadefa`, version, flags, endian, message type,
  and an `i64` payload length. The payload is native-endian, and a peer
  whose byte order differs refuses the frame rather than reading columns
  backwards.
- **Values** — a vector is `type, attrs, i64 length, payload`; an atom is
  `type, flags, value` with the type negated. A table is a symbol vector of
  column names followed by a list of columns.
- **Symbols** are NUL-terminated strings on the wire. The id a server hands
  out indexes its own intern table, which is why `Sym` can keep its own and
  why two processes need no coordination to agree on a name.
- **Compression** is a per-frame, sender-side decision with nothing to
  negotiate, so this client never compresses and always decompresses.
  Rayforce never compresses to a loopback or UNIX-domain peer anyway.

A wire-version bump is a connect-time error here, not a silent
misparse — the handshake compares versions before anything is sent.

## Which client

`rayforce-ipc-async` in any program that already runs Async, which is the
one to reach for: a blocking write to a server that has stopped draining
stalls whatever thread called it, and in a feed that means the ingest
loop. The blocking `Conn` stays for programs with no scheduler. Both speak
the same format and share the encoder.

## Tests

`dune runtest` runs the round-trip tests, and then the one that matters:
`test_live.ml` against a real `rayforce -p <port>`, reading every encoded
value back through rayforce's own evaluator. It needs `rayforce` on PATH.

## Exact timestamps and timespans

`Value.Timespan` and `Value.Timespans` carry signed int64 nanoseconds using
Rayforce tag 16. `Column.timespan` supports the reusable column-builder path
with `Column.add_int64`. No epoch offset is applied to spans. Raw values retain
the entire engine range; `INT64_MIN` denotes null.

Use `Timestamp.of_time_ns_int64` and `Timestamp.to_time_ns_int64` when bridging
Core time and wire timestamps. The checked constructor returns an error for
null or values outside Core's signed 63-bit range. `Timespan.of_span` and
`Timespan.to_span` do the equivalent span conversion without an epoch offset.
No floating-point conversion is involved. The existing int timestamp helpers
remain available but now raise rather than wrap for an unrepresentable value.
Contemporary timestamps are unchanged; historical Core times can have a
Rayforce epoch offset too negative for an OCaml int.

The temporal tests cover raw atoms/vectors, empty and nullable columns,
nanosecond precision above 2^53, Core conversion limits and both overflow
regressions. The live tests verify timespan values against a real engine
through both blocking and Async clients.

## Async query cancellation

`Rayforce_ipc_async.Conn.cancel_and_close` sends Rayforce's POSIX TCP urgent
cancel byte and tears down the connection, discarding unsent buffered output.
The urgent send runs through Async's managed nonblocking file-descriptor API;
ordinary connections, I/O and cleanup remain on Async. A failed send is
returned as an error but does not skip cleanup. Repeated calls after closure
are harmless. The blocking client API is unchanged.

Cancellation is best effort: socket acceptance of the byte does not acknowledge
server interruption, and the engine's urgent-data mechanism is POSIX-specific.
The connection is deliberately not reusable: reply frames have no request IDs,
and neither a late reply nor a queued urgent byte may affect a subsequent call.
Reconnect for later work. Closing alone does not guarantee engine interruption.

The dedicated Async live test starts a query that writes a readiness marker
and loops indefinitely, cancels it, and verifies that another connection then
gets a prompt result. It also checks idle-connection isolation, idempotent
teardown and cancellation with buffered output. The workbench separately
checks urgent-byte delivery for user cancellation and request deadlines.
