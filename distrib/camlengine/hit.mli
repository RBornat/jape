(* $Id$ *)

open Term.Type

(* Hits are what drawing packages (treedraw, boxdraw, and one day etc.) translate
 * clicks into.  They are also used to locate text selections.  Only conclusions are
 * treated with the transitivity transformation, so only conclusion clicks can be
 * sided.  
 *
 * Sels are what we translate collections of Hits into.
 * 
 * The interface ('server') is never supposed to report an ambiguous click, but 
 * a text selection in an ambiguous formula must be reported somehow, so we can't
 * get rid of AmbigHit as I would like.
 *
 * ''a is a path -- there are various kinds.
 *)

type 'a hit = FormulaHit of 'a fhit | ReasonHit of 'a

and 'a fhit =
    ConcHit of ('a * (element * side option))
  | HypHit of ('a * element)
  | AmbigHit of (('a * (element * side option)) * ('a * element))

and side = Left | Right 

(* In box display at least, a single displayed element may play more than one 
 * role. hitkind (should perhaps be hitpathkind) allows us to choose which we want.
 *)

type hitkind = HitPath | PrunePath | LayoutPath

(* Selections uniquely identify a single sequent.  
 * They are either conclusion+hypotheses, reason or nothing at all.
 * At present I countenance either a single conclusion selection, or none.
 *
 * Interfaces are supposed to sort out the user's clicks so that it's reasonable
 * to say that either a reason is selected, or some formulae are selected, or neither.
 *
 * Text selections are rolled up into formula selections, because why not?
 *
 * We distinguish 'text selection only' selections.
 *) 

type 'a sel =
    FormulaSel of
      ('a * (element * side option) option * element list *
         ((element * side option) * string list) list *
         (element * string list) list * string list)
  | TextSel of (('a fhit * string list) list * string list)
  | ReasonSel of 'a

val fhitpath : 'a fhit -> 'a option
val fhitstring : ('a -> string) -> 'a fhit -> string
val hitkindstring : hitkind -> string
val hitpath : 'a hit -> 'a option
val hitstring : ('a -> string) -> 'a hit -> string
val selpath : 'a sel -> 'a option
val selstring : ('a -> string) -> 'a sel -> string
val sidestring : side -> string
val tranfhitpath : ('a -> 'b) -> 'a fhit -> 'b fhit
val tranhitpath : ('a -> 'b) -> 'a hit -> 'b hit
val transelpath : ('a -> 'b) -> 'a sel -> 'b sel
