(* $Id$ *)

open Text
open Displayfont
open Treeformat.VisFmt
open Prooftree.Tree
open Prooftree.Tree.Vistree
open Sml

let commasymbol       = Symbol.commasymbol
let debracket         = Term.Funs.debracket
let explodeCollection = Term.Funs.explodeCollection
let isstructurerule   = Thing.isstructurerule
let proved            = Proofstore.proved
let seqexplode        = Sequent.Funs.seqexplode
let symbolstring      = Symbol.symbolstring

let termstring_invisbracketed    = Term.Termstring.termstring_invisbracketed
let elementstring_invisbracketed = Term.Termstring.elementstring_invisbracketed

type structurerule = Thing.structurerule
 and tree          = Prooftree.Tree.Vistree.prooftree
 and sequent       = Sequent.Funs.seq
 and reason        = string
 and element       = Term.Funs.element
 and text          = Text.text
 and term          = Term.Funs.term
 and font          = Displayfont.displayfont

let sequent = Vistree.sequent
let subtrees = Vistree.subtrees

let explode =
  (fun (st, hs, gs) -> st, explodeCollection hs, explodeCollection gs) <*>
  seqexplode
let isStructureRulenode node rule =
  match Vistree.rule node with
    Some r -> isstructurerule rule r
  | None -> false
let matched = thinned
let allTipConcs tree ns =
  try
    let concs = Vistree.allTipConcs (followPath tree (VisPath ns)) in
    List.map (fun (VisPath cpath, conc) -> ns @ cpath, conc) concs
  with
    FollowPath_ _ -> []
let tip tree ns =
  try Some (findTip tree (VisPath ns)) with
    _ -> None
let comma () =
  Text [Syllable (TermFont, symbolstring commasymbol ^ " ")]
let turnstile st = Text [Syllable (TermFont, (" " ^ st) ^ " ")]
let reason2text why = Text [Syllable (ReasonFont, why)]
let reason2fontNstring why = ReasonFont, why
let element2text elementstring e =
  Text [Syllable (TermFont, elementstring e)]
let term2text termstring t = Text [Syllable (TermFont, termstring t)]
let validhyp t el ns = Vistree.validhyp t el (VisPath ns)
let validconc t el ns = Vistree.validconc t el (VisPath ns)
let stillopen t = Vistree.stillopen t <*> (fun v->VisPath v)
let ismultistep t =
  match format t with VisFormat (b, _) -> b
let ishiddencut t =
  match format t with VisFormat (_, b) -> b
let reason = reason proved
