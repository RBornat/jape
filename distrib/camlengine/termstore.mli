open Termtype
open Idclass

val registerId : vid * idclass -> term
val registerUnknown : vid * idclass -> term
val registerApp : term * term -> term
val registerTup : string * term list -> term
val registerLiteral : litcon -> term
val registerFixapp : string list * term list -> term
val registerSubst : bool * term * (term * term) list -> term
val registerBinding :
  (term list * term list * term list) * 
  (term * (int * int)) list *
  term -> term
val registerCollection : idclass * element list -> term
val registerElement : resnum * term -> element
val registerSegvar : term list * term -> element
val resettermstore : unit -> unit

val hashterm : term -> int option
val hashelement : element -> int option
val termhashing : bool ref
