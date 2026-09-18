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
let f64 = 7
let date = 8
let timestamp = 10
let sym = 12
let str = 13
let table = 98
let dict = 99

(* Types this library does not decode. They are named so that meeting one
   reports what it is instead of reading its first bytes as a length. *)
let undecoded =
  [ 6, "f32"
  ; 9, "time"
  ; 11, "guid"
  ; 97, "index"
  ; 100, "lambda"
  ; 101, "unary builtin"
  ; 102, "binary builtin"
  ; 103, "variadic builtin"
  ]
;;

let null = 126
let error = 127
