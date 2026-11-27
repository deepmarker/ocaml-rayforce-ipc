open! Core
open! Async
open! Rayforce_ipc

(** A rayforce IPC server on the Async scheduler: the other end of
    {!Rayforce_ipc_async.Conn}, speaking the same wire format.

    The protocol has no request ids, so each connection is served strictly in
    order: one frame is handled to completion before the next is read, and a
    response always answers the request before it. Different connections run
    concurrently. *)

(** A connected client. *)
module Peer : sig
  type t

  val address : t -> Socket.Address.Inet.t

  (** The user the client authenticated as; [None] when the server asks for
      no credentials. *)
  val user : t -> string option

  (** Push a value to the client as an async frame, without waiting. Never
      raises for a client that has gone away; that shows as the connection
      closing. Interleaves with responses, which the client's [on_push]
      receives while a sync call is outstanding. *)
  val push : t -> Value.t -> unit

  (** Bytes encoded but not yet written to this client's socket. *)
  val bytes_to_write : t -> int

  val close : t -> unit Deferred.t
  val is_closed : t -> bool
  val close_finished : t -> unit Deferred.t
end

type t

(** Starts listening.

    - [on_async] handles a fire-and-forget frame. An exception is logged and
      the connection carries on.
    - [on_sync] computes the response to a sync frame. An exception, or a
      frame whose payload cannot be decoded, is answered with a
      {!Value.Error} rather than by dropping the connection.
    - [authenticate], when given, makes the server ask clients for
      credentials; a client it rejects is disconnected after the verdict
      byte. Without it no credentials are asked for.
    - [on_connect] and [on_disconnect] observe each client that completed
      the handshake.

    A frame that is not valid wire format, a client speaking another wire
    version, or one that sends a response frame closes that connection
    only. *)
val create
  :  ?authenticate:(user:string -> password:string -> bool Deferred.t)
  -> ?on_connect:(Peer.t -> unit)
  -> ?on_disconnect:(Peer.t -> unit)
  -> ?max_connections:int
  -> ?handshake_timeout:Time_ns.Span.t
  -> where_to_listen:(Socket.Address.Inet.t, int) Tcp.Where_to_listen.t
  -> on_async:(Peer.t -> Value.t -> unit Deferred.t)
  -> on_sync:(Peer.t -> Value.t -> Value.t Deferred.t)
  -> unit
  -> t Deferred.t

(** The port actually bound, useful after asking for port 0. *)
val listening_port : t -> int

(** Stops accepting and disconnects every client. *)
val close : t -> unit Deferred.t

val close_finished : t -> unit Deferred.t
