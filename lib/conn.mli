open! Core

(** A blocking client connection to a rayforce server.

    Blocking is deliberate: the callers this was written for send from a
    polling loop that owns its own thread, and a batch is one write of a
    buffer they have already built. Nothing here touches a scheduler, and
    the connection has no state of its own beyond the socket -- which is
    what makes it safe to use from whichever thread happens to be running,
    unlike an embedded VM. *)

type t

(** Connects, then performs the version handshake and, if the server asks
    for credentials, sends them. Raises on refusal, on a timeout, and on a
    server speaking a different wire version.

    [timeout_ms] bounds the connect and the handshake together; it does not
    bound later sends. *)
val connect : ?user:string -> ?password:string -> ?timeout_ms:int -> string -> int -> t

val close : t -> unit
val is_closed : t -> bool

(** Fire and forget: the server evaluates the value through its
    [.ipc.on.async] hook and sends nothing back. *)
val send_async : t -> Value.t -> unit

(** Send and wait for the response frame. Frames the server pushes while we
    wait are decoded and passed to [on_push] (dropped by default) rather
    than mistaken for the answer. *)
val send_sync : ?on_push:(Value.t -> unit) -> t -> Value.t -> Value.t
