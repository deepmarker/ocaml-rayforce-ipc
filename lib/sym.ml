open Core

module T = struct
  type t = int [@@deriving compare, equal, hash, sexp_of]
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

(* [names] is the id -> string direction and is only ever appended to, so an
   id stays valid for the life of the process. Core's queue is array-backed
   and indexes in constant time, which is what the serializer needs. *)
let ids : int String.Table.t = String.Table.create ()
let names : string Queue.t = Queue.create ()

(* The wire length of each name, kept beside it: a sym column is sized once
   per row before it is written, and that must not cost a string fetch. *)
let wire_lens : int Queue.t = Queue.create ()

let intern s =
  Hashtbl.find_or_add ids s ~default:(fun () ->
    let id = Queue.length names in
    Queue.enqueue names s;
    Queue.enqueue wire_lens (String.length s + 1);
    id)
;;

let of_int_exn id =
  if id < 0 || id >= Queue.length names then invalid_argf "no symbol with id %d" id ();
  id
;;

let to_string t = Queue.get names t
let to_int t = t
let wire_len t = Queue.get wire_lens t
