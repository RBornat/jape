(* $Id$ *)

open Context.Cxt
open Term.Funs
open Proviso
open Mappingfuns

val checkprovisos : cxt -> cxt option
val deferrable : cxt -> term * term -> bool
val expandFreshProviso :
  bool -> bool * bool * bool * term -> term -> term -> visproviso list ->
    visproviso list
val groundedprovisos :
  term list -> visproviso list -> visproviso list option
val remapproviso : (term, term) mapping -> proviso -> proviso
val verifyprovisos : cxt -> cxt

exception Verifyproviso_ of proviso
