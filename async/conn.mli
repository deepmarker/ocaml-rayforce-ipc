open! Core
open! Async
open! Rayforce_ipc

(** A rayforce connection on the Async scheduler.

    Prefer this to {!Rayforce_ipc.Conn} in any program that already runs
    Async. A send costs a copy into the writer's buffer and returns: a
    server that is slow to drain backs up in that buffer rather than
    stalling the caller, which for a feed means the difference between
    falling behind and stopping. The blocking client remains for programs
    with no scheduler.

    The connection satisfies [Persistent_connection_kernel.Closable], so a
    link that must survive the server restarting can be handed to
    [Persistent_connection] rather than reconnected by hand. *)

type t

(** Connects, performs the version handshake, and sends credentials if the
    server asks for them. [timeout] bounds the connect and the handshake
    together; it does not bound later sends. *)
val connect
  :  ?user:string
  -> ?password:string
  -> ?timeout:Time_ns.Span.t
  -> host:string
  -> port:int
  -> unit
  -> t Deferred.Or_error.t

val close : t -> unit Deferred.t
val is_closed : t -> bool
val close_finished : t -> unit Deferred.t

(** Fire and forget: the value is encoded into the writer's buffer and this
    returns. It never raises for a peer that has gone away -- that surfaces
    as the connection closing. *)
val send_async : t -> Value.t -> unit

(** Bytes encoded but not yet written to the socket. A producer that must
    not outrun its server checks this and drops rather than buffering
    without end -- the buffer is the only place the backlog can go. *)
val bytes_to_write : t -> int

(** Send, and wait for the response frame. Calls are serialized, so
    concurrent callers cannot take each other's answers. Frames the server
    pushes while one is outstanding go to [on_push] (dropped by default).

    Reads only happen inside this call: a connection used purely for
    {!send_async} never reads, so a server pushing to it will eventually
    fill the socket. *)
val send_sync : ?on_push:(Value.t -> unit) -> t -> Value.t -> Value.t Deferred.Or_error.t
