(* $Id$ *)

open Term.Funs 

val compilepredicate :
  (term -> bool) -> (term -> term list option) -> term -> term option
val discardzeroarities :
  (term * (term list * term list list) list) list ->
    (term * (term list * term list list) list) list
val findpredicates :
  (term -> bool) -> term list ->
    term * (term * (term list * term list list) list) list ->
    (term * (term list * term list list) list) list option
val findpredicatevars :
  (term list * term list list) list -> term list option
val interpretpredicates : bool ref
val matchpredicate :
  bool -> (term -> bool) -> term -> (term * term list) option
val predicatedebug : bool ref
val predicatebindingstring :
  (term * (term list * term list list) list) list -> string

exception Predicate_ of string list
