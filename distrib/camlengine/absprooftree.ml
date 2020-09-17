(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
        richard@bornat.me.uk
        sufrin@comlab.ox.ac.uk

    This file is part of the jape proof engine, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*)

open Text
open Displayfont
open Treeformat.VisFmt
open Prooftree.Tree
open Prooftree.Tree.Vistree
open Sml

let commasymbol       = Symbol.commasymbol
let debracket         = Termfuns.debracket
let explodeCollection = Termfuns.explodeCollection
let isstructurerule   = Thing.isstructurerule
let proved            = Proofstore.proved
let seqexplode        = Sequent.seqexplode
let string_of_symbol  = Symbol.string_of_symbol

type structurerule = Thing.structurerule
 and tree          = Prooftree.Tree.Vistree.prooftree
 and sequent       = Sequent.seq
 and reason        = string
 and element       = Termtype.element
 and text          = Text.text
 and term          = Termtype.term
 and font          = Displayfont.displayfont
 and proviso       = Proviso.proviso
 and paraparam     = Paraparam.paraparam

let sequent = Vistree.sequent
let subtrees = Vistree.subtrees
let params = Vistree.params
let args = Vistree.args
let stepprovisos = Vistree.stepprovisos

let explode =
  (fun (st, hs, gs) -> st, explodeCollection hs, explodeCollection gs) <.>
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
  Text [Syllable (TermFont, string_of_symbol commasymbol ^ " ")]

let turnstile st = Text [Syllable (TermFont, (" " ^ st) ^ " ")]

let text_of_reason why = Text [Syllable (ReasonFont, why)]

let fontNstring_of_reason why = ReasonFont, why

let text_of_element string_of_element e =
  Text [Syllable (TermFont, string_of_element e)]

let text_of_term string_of_term t = Text [Syllable (TermFont, string_of_term t)]

let validhyp t el ns = Vistree.validhyp t el (VisPath ns)

let validconc t el ns = Vistree.validconc t el (VisPath ns)

let stillopen t = Vistree.stillopen t <.> (fun v->VisPath v)

let ismultistep t =
  match format t with VisFormat (b, _, _) -> b

let ishiddencut t =
  match format t with VisFormat (_, b, _) -> b
  
let assumptiontail t =
  match format t with VisFormat (_, _, None  ) -> ""
                 |    VisFormat (_, _, Some s) -> " " ^ s

let reason = reason proved  (* VisTree.reason, I think (and hope). Plucks out Some j.why from Join j, None from Tip.
                               'proved' is ignored, because (says VisTree) it's already happened. RB 08/2020
                             *)

let reason_of_string s = s (* because the type reason is hidden *)

let stepparamsprovisos t = 
  match rule t with
  | Some r ->
      (match Thing.thinginfo r with
       | Some (Thing.Rule ((params, provisos, _, _), _), _) -> Some (params, provisos)
       | Some (Thing.Theorem (params, provisos, _), _)      -> Some (params, provisos)
       | _                                       -> 
           raise (Miscellaneous.Catastrophe_ 
                     ["Absprooftree.stepparamsprovisos can't find thing named ";
                                Name.string_of_name r])
      )
  | None   -> None
