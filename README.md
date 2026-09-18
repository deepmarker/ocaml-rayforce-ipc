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
| `Timestamp` | The 2000-01-01 epoch the format counts nanoseconds from. |

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
