open! Core

module T = struct
  type t = int [@@deriving compare, equal, hash, sexp_of]
end

include T
include Comparable.Make_plain (T)
include Hashable.Make_plain (T)

(* Everything a symbol costs at serialization time, worked out once when it
   is interned: the name, the bytes that actually go on the wire (the name
   and its terminator, so writing a cell is one blit rather than two), and
   their length, so sizing a column is a read. *)
type entry =
  { name : string
  ; wire : string
  ; wire_len : int
  }

let ids : int String.Table.t = String.Table.create ()

(* Append-only, indexed by id: an id stays valid for the life of the
   process, and resolving one is an array read on the serializer's hot
   path. Grown by doubling rather than kept in a queue, whose bounds check
   showed up in a profile of a feed writing millions of cells. *)
let store = ref (Array.create ~len:64 { name = ""; wire = "\000"; wire_len = 1 })
let count = ref 0

let intern s =
  Hashtbl.find_or_add ids s ~default:(fun () ->
    let id = !count in
    if id >= Array.length !store
    then (
      let bigger = Array.create ~len:(Array.length !store * 2) !store.(0) in
      Array.blit ~src:!store ~src_pos:0 ~dst:bigger ~dst_pos:0 ~len:id;
      store := bigger);
    let wire = s ^ "\000" in
    !store.(id) <- { name = s; wire; wire_len = String.length wire };
    count := id + 1;
    id)
;;

let of_int_exn id =
  if id < 0 || id >= !count then invalid_argf "no symbol with id %d" id ();
  id
;;

let unsafe_of_int id = id
let entry t = Array.unsafe_get !store t
let to_string t = (entry t).name
let to_int t = t
let wire_len t = (entry t).wire_len
let wire t = (entry t).wire
