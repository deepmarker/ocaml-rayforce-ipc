(** Type tags, from rayforce's [include/rayforce.h]. A vector carries the tag
    itself, an atom of the same type carries its negation, and both 126 and
    127 are wire-only markers with no vector form. *)

open! Core

let list = 0
let bool = 1
let u8 = 2
let i16 = 3
let i32 = 4
let i64 = 5
let f32 = 6
let f64 = 7
let date = 8
let time = 9
let timestamp = 10
let guid = 11
let timespan = 16
let sym = 12
let str = 13
let table = 98
let dict = 99
let lambda = 100
let unary = 101
let binary = 102
let variadic = 103

(* Types this library does not decode. They are named so that meeting one
   reports what it is instead of reading its first bytes as a length. *)
let undecoded = [ 97, "index" ]
let null = 126
let error = 127
