(* $Id$ *)

open Term.Funs
open Mappingfuns

(* everything you can say in a LAYOUT tactic *)
type treelayout =
	HideRootLayout
  | HideCutLayout
  | CompressedLayout of (term * term option)
  | NamedLayout of (term * term option)
                  (* fmt * list of subtrees to show *)

val treelayoutstring : treelayout -> string
val smltreelayoutstring : treelayout -> string
val remaptreelayout : (term, term) mapping -> treelayout -> treelayout
