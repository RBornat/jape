(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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

(* Functions for use with drawing packages (e.g. treedraw, boxdraw) *)

open Box 
open Japeserver 
open Listfuns
open Mappingfuns 
open Miscellaneous
open Optionfuns
open Sml
open Text

type term       = Termtype.term
 and element    = Termtype.element
 and reason     = Absprooftree.reason
 and textlayout = Text.textlayout
 and text       = Text.text
 and textalign  = Text.textalign
 and textsize   = Box.textsize
 and pos        = Box.pos
 and size       = Box.size
 and box        = Box.box
 and textbox    = Box.textbox
 and font       = Text.font
 and displayclass = Displayclass.displayclass
 
let fontNstring_of_reason = Absprooftree.fontNstring_of_reason
let minwaste              = Minwaste.minwaste
let proofpane             = Displayfont.ProofPane
let string_of_element     = Termstring.string_of_element
let string_of_pair        = Stringfuns.string_of_pair
let string_of_term        = Termstring.string_of_term
let string_of_triple      = Stringfuns.string_of_triple
let text_of_element       = Absprooftree.text_of_element
let text_of_reason        = Absprooftree.text_of_reason
let text_of_term          = Absprooftree.text_of_term

let fontinfo = fontinfo
and blacken = blacken
and greyen = greyen
and drawLine = drawLine

type class__ = int (* could be Japeserver.class__, if I played it right ... *)
    
let rec drawinproofpane () = Japeserver.drawinpane proofpane

type 'a plan = Formulaplan of (textlayout * textbox * 'a)

let rec debugstring_of_plan f = (* was string_of_plan  *)
  fun (Formulaplan plan) ->
    "Formulaplan" ^ string_of_triple string_of_textlayout string_of_textbox f "," plan

let rec string_of_plan  =
  function
    Formulaplan (Textlayout [_, _, s], _, _) -> s
  | p -> raise
            (Catastrophe_
               ["multisyllable text in string_of_plan  -- ";
                debugstring_of_plan (fun _ -> "...") p])

let textlayout_of_plan = fun (Formulaplan (tl, _, _)) -> tl

let textbox_of_plan = fun (Formulaplan (_, b, _)) -> b

let textbox_of_planlist ps = List.fold_left (+|-|+) emptytextbox (List.map textbox_of_plan ps)

let info_of_plan = fun (Formulaplan (_, _, i)) -> i

let textsize_of_plan p = tbS (textbox_of_plan p)

let textsize_of_planlist ps = tbS (textbox_of_planlist ps)
  
let viewBox () = Japeserver.getPaneGeometry Displayfont.ProofPane

let rec clearView () = Japeserver.clearPane Displayfont.ProofPane

let highlight = Japeserver.highlight

let drawBox = Japeserver.drawRect

let rec linethickness leading =
  (* width of the lines (box, selection) we draw *)
  let r = max ((leading + 2) / 3) 1 in(* consolereport["leading ", string_of_int leading, "; thickness ", string_of_int r]; *)
   r

let setproofparams = Japeserver.setproofparams

let rec measuretext ta t = Text.measuretext Japeserver.measurestring ta t
(* note fixed alignment, so don't use for folded/multiline texts *)

let textinfo_of_text = measuretext FirstLine

let rec mktextinfo f = textinfo_of_text <.> f

let rec textinfo_of_string f = mktextinfo (text_of_string f)

let rec textinfo_of_element string_of_element =
  mktextinfo (text_of_element string_of_element)

let rec textinfo_of_term string_of_term = mktextinfo (text_of_term string_of_term)

let textinfo_of_reason = mktextinfo text_of_reason

let textsize_of_textlayout (Textlayout ts) =
  let strbox (p, f, str) = 
    textbox p ((uncurry3 textsize) (Japeserver.measurestring f str)) in 
  let box = List.fold_left (+|-|+) emptytextbox (List.map strbox ts) in
  tbS box

let rec textinfo_procrustes w p (size, Textlayout ts as inf) =
  if not !truncatereasons || tsW size<=w then false, inf else
  let ellipsis = "..." in
  let crusty (p', f, text) =
    let w' = w - (posX p' - posX p) in
    let text' = Japeserver.procrustes w' ellipsis f text in
    if text'=text then None else Some [(p, f, text')]
  in
  let rec crust ts =
    match ts with
      []     -> None
    | t::ts' -> crusty t |~~
                (fun _ -> crust ts' &~~ (fun ts'' -> Some (t::ts'')))
  in
  match crust ts with
    None     -> false, inf
  | Some ts' -> let layout' = Textlayout ts' in true, (textsize_of_textlayout layout', layout')
  
  (* let (rf, rs) = fontNstring_of_reason r in
  textinfo_of_string rf (Japeserver.procrustes w " ..." rf rs) *)

let rec plan_of_textinfo (size, layout) info p =
  Formulaplan (layout, textbox p size, info)

let rec plan_of_string font s info p =
  plan_of_textinfo (textinfo_of_string font s) info p

let rec plan_of_element ef element info p =
  plan_of_textinfo (textinfo_of_element ef element) info p
(* I'd much rather say val plan_of_element = plan_of_textinfo o textinfo_of_element, but
 * I seem to remember that SML 109 can't cope.
 *)
 
let planOffset (Formulaplan (layout, box, thing)) pos =
    Formulaplan (layout, tbOffset box pos, thing)

let plans_of_plan (Formulaplan (_, b, _) as plan) = [plan], b

(* to make a comma-separated list without heap churning ... can't be hard, surely?
 * But I have made a right meal of it at least twice (and who knows how many other
 * attempts are in the CVS tree, or never even made the tree?).
 *
 * And why should I care so much? Let's start again ... again.
 *)
(* and now -- oh dear -- I want to abstract from it, to make a newline-separated
   list of lists ...
 *)
 
let plancons (Formulaplan (_, tb1, _) as plan) (plans, tb2) =
    plan :: plans, tb1 +|-|+ tb2

let nextright_of_plan (Formulaplan (_, tb, _)) = nextright_of_textbox tb

let planfollowedby plan moref =
  plancons plan (moref (nextright_of_plan plan))
  
let planunwind tail stuff = 
  let plancons' stuff plan = plancons plan stuff in
  List.fold_left plancons' tail stuff
             
let rec planfold thingf sepf (p, tb, plans as res) =
    function []              -> res
    |        [thing]         -> let plan = thingf thing p in
                                nextright_of_plan plan, tb +|-|+ textbox_of_plan plan, plan :: plans
    |        thing :: things ->
               let thingplan = thingf thing p in
               let sepplan = sepf (nextright_of_plan thingplan)  in
               planfold thingf sepf 
                (nextright_of_plan sepplan, 
                 tb +|-|+ textbox_of_plan thingplan +|-|+ textbox_of_plan sepplan,
                 sepplan :: thingplan :: plans) 
                things

let plans_of_things thingf sepf moref things p  =
  let p', _, stuff = planfold thingf sepf (p, emptytextbox, []) things in
  planunwind (moref p') stuff
    
let rec drawplan f p =
  fun (Formulaplan (Textlayout tl, b, info)) ->
    Japeserver.drawmeasuredtext (f info) tl (p +->+ tbP b)

let rec findfirstplanhit p =
  findfirst (fun pl -> if withintb p (textbox_of_plan pl) then Some pl else None)

let string_of_textinfo = string_of_pair string_of_textsize string_of_textlayout ","

