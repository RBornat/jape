(* $Id$ *)

type tree = Prooftree.Tree.Vistree.prooftree
and structurerule = Thing.M.structurerule
and font = Displayfont.displayfont
and sequent = Sequent.Funs.seq
and reason
and text = Text.M.text
and term = Term.Funs.term
and element = Term.Funs.element

val allTipConcs : tree -> int list -> (int list * element list) list
val comma : unit -> text
val element2text : (element -> string) -> element -> text
val explode : sequent -> string * element list * element list
val isStructureRulenode : tree -> structurerule -> bool
val ishiddencut : tree -> bool
val ismultistep : tree -> bool
val matched : tree -> element list * element list
val reason : tree -> reason option
val reason2fontNstring : reason -> font * string
val reason2text : reason -> text
val sequent : tree -> sequent
val stillopen : tree -> int list -> bool
val subtrees : tree -> tree list
val term2text : (term -> string) -> term -> text
val tip : tree -> int list -> sequent option
val turnstile : string -> text
val validconc : tree -> element -> int list -> bool
val validhyp : tree -> element -> int list -> bool
