(* $Id$ *)

open Term.Funs
open Context.Cxt

val _Oracle : string -> cxt -> term -> term -> string -> string list -> cxt option
val resetoracle : unit -> unit
