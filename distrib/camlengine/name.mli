(* $Id$ *)

open Term.Type

type name = Nametype.name

val namefrom : string -> name
val nameorder : name -> name -> bool
val namestring : name -> string
val parseablenamestring : name -> string
val term2name : term -> name option

