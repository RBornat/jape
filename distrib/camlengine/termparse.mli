(* $Id$ *)

open Term.Funs
open Symbol

val check : symbol -> unit
val ignore : symbol -> unit
val canstartTerm : symbol -> bool
val canstartidentifier : symbol -> bool
val parseVariable : unit -> term
val parseidentifier : unit -> term
val parseTerm : symbol -> term
val parseBindingpattern : unit -> term
val parseCollection : idclass -> term
val parseElementList :
  (symbol -> bool) -> (symbol -> term) -> symbol -> idclass option ->
    idclass option * element list
val parseList : (symbol -> bool) -> (symbol -> 'a) -> symbol -> 'a list
val parseUnsepList : (symbol -> bool) -> (symbol -> 'a) -> 'a list
val parsecurriedarglist : unit -> term list
val tryparse : (symbol -> 'a) -> string -> 'a
val tryparse_dbug : (symbol -> 'a) -> ('a -> string) -> string -> 'a
val asTactic : ('a -> term) -> 'a -> term
(* parse with no notice of name classification, non-vars allowed in Substs *)
val checkTacticTerm : term -> unit
(* raises Tacastrophe_ (really) if argument isn't a pukka term *)

val term_of_string : string -> term
val tactic_of_string : string -> term
val declareOutRightfix : symbol list -> symbol -> unit
val declareLeftMidfix : symbol list -> unit
val resettermparse : unit -> unit
