(*
    $Id$

    Copyright (C) 2003-4 Richard Bornat & Bernard Sufrin
     
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
open Mappingfuns 
open Sml
open Text

exception Catastrophe_ = Miscellaneous.Catastrophe_

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
 

let (&~~) = Optionfuns.(&~~)
let (|~~) = Optionfuns.(|~~)
let consolereport = Miscellaneous.consolereport
let text_of_element  = Absprooftree.text_of_element
let string_of_element = Termstring.string_of_element
let findfirst     = Optionfuns.findfirst
let interpolate   = Listfuns.interpolate
let string_of_option  = Optionfuns.string_of_option
let string_of_pair    = Stringfuns.string_of_pair
let proofpane     = Displayfont.ProofPane
let text_of_reason   = Absprooftree.text_of_reason
let fontNstring_of_reason = Absprooftree.fontNstring_of_reason
let text_of_term     = Absprooftree.text_of_term
let string_of_term    = Termstring.string_of_term
let string_of_triple  = Stringfuns.string_of_triple

let fontinfo = fontinfo
and blacken = blacken
and greyen = greyen
and drawLine = drawLine

type class__ = int (* could be Japeserver.class__, if I played it right ... *)
    
let rec drawinproofpane () = Japeserver.drawinpane proofpane

type 'a plan = Formulaplan of (textlayout * textbox * 'a)

let rec debugstring_of_plan f =
  fun (Formulaplan plan) ->
    "Formulaplan" ^ string_of_triple string_of_textlayout string_of_textbox f "," plan
(* was string_of_plan  *)

let rec string_of_plan  =
  function
    Formulaplan (Textlayout [_, _, s], _, _) -> s
  | p ->
      raise
        (Catastrophe_
           ["multisyllable text in string_of_plan  -- ";
            debugstring_of_plan (fun _ -> "...") p])

let textlayout_of_plan = fun (Formulaplan (tl, _, _)) -> tl

let textbox_of_plan = fun (Formulaplan (_, b, _)) -> b

let textbox_of_planlist ps = nj_fold (+|-|+) (List.map textbox_of_plan ps) emptytextbox

let info_of_plan = fun (Formulaplan (_, _, i)) -> i

let textsize_of_plan p = textsize_of_textbox (textbox_of_plan p)

let textsize_of_planlist ps = textsize_of_textbox (textbox_of_planlist ps)
  
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
  let strbox (p, f, str) = textbox (p, textsize(Japeserver.measurestring f str)) in 
  let box = nj_fold (+|-|+) (List.map strbox ts) emptytextbox in
  textsize_of_textbox box

let rec textinfo_procrustes w p (size, Textlayout ts as inf) =
  if tsW size<=w then inf else
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
    None     -> inf
  | Some ts' -> let layout' = Textlayout ts' in textsize_of_textlayout layout', layout'
  
  (* let (rf, rs) = fontNstring_of_reason r in
  textinfo_of_string rf (Japeserver.procrustes w " ..." rf rs) *)

let rec plan_of_textinfo (size, layout) info p =
  Formulaplan (layout, textbox (p, size), info)

let rec plan_of_string font s info p =
  plan_of_textinfo (textinfo_of_string font s) info p

let rec plan_of_element ef element info p =
  plan_of_textinfo (textinfo_of_element ef element) info p
(* I'd much rather say val plan_of_element = plan_of_textinfo o textinfo_of_element, but
 * I seem to remember that SML 109 can't cope.
 *)
 
(* to make a comma-separated list without heap churning ... can't be hard, surely?
 * But I have made a right meal of it in the past.
 *
 * Ok, start again ... since we have to take the element and make a plan out of it,
 * it's best to give an element function which does the whole thing.  Users can
 * make it out of plan_of_element easily.
 *
 * Then we need commatx and commainf, and we have the whole thing, I think, with
 * just a sort of foldr.  Surely.  We need a zero _function_ rather than a zero
 * _value_ because other people need to take the final position and draw the rest
 * of the sequent, for example
 *
 * But if we use foldr there is heap churning because all the conses take place
 * afterwards -- in SML, anyway.  Never mind: this works, at least.
 *)


let rec plancons =
  fun (Formulaplan (_, b1, _) as plan) moref ->
    let (plans, b2) = moref (rightby (tbPos b1, tsW (textsize_of_textbox b1))) in
    plan :: plans, ( +|-|+ ) (b1, b2)

let rec plans_of_plan = fun (Formulaplan (_, b, _) as plan) -> [plan], b
(* I bet this churns like buggery! Still, it never reverses :-) *)

let rec plans_of_things thingf sepf moref things zp =
  let rec f a1 a2 =
    match a1, a2 with
      [], p -> moref p
    | [el], p -> plancons (thingf el p) moref
    | el :: things, p ->
        plancons (thingf el p) (fun p' -> plancons (sepf p') (f things))
  in
  f things zp

let rec plans_of_sequent hs hf cs cf commaf stilef sp =
  let rec rhs rp =
    plans_of_things cf commaf (fun _ -> [], emptytextbox) cs rp
  in
  plans_of_things hf commaf (fun p' -> plancons (stilef p') rhs) hs sp
(*   (* the plan comes out of mktlp in reverse order; it's regularised in 
    * mkelementlistplan and mksequentplan (this is necessary for plan-linearisers 
    * and for the minimum-waste split algorithm).  It isn't done here because it would 
    * cause too much hoohah - see, for example, mksequentplan
    *)
   (* the 'plans' argument to mktlp has to be backwards as well, so that the 
    * whole thing gets built backwards ...
    *)
   fun mktlp elf els comminf commaclass startinf =
     let fun addcomma (p, plans, size) =
           let val (p', txplan, commasize) = plan_of_textinfo comminf commaclass p
           in
               (p', txplan::plans, size+-+commasize)
           end
         fun addelement element (p, plans, size) =
           let val (p', elementplan, elementsize) = elf element p
           in
               (p', elementplan::plans, size+-+elementsize)
           end
         fun S []        z = z
         |   S [el]      z = addelement el z
         |   S (el::els) z = S els (addcomma (addelement el z)) (* isn't this just foldl? *)
         
     in
         S els startinf (* don't reverse it: trust your users *)
     end

   fun p_of_s mkp hs hclass cs cclass stileinf stileclass comminf commaclass (p,plans,size) =
     let val (p,hplans,hsize) = mktlp (mkp hclass) hs comminf commaclass (p,List.rev plans,size)
         val (p',stileplan, stilesize) = plan_of_textinfo stileinf stileclass p
         val (p'',allplans,allsize) = 
           mktlp (mkp cclass) cs comminf commaclass (p',stileplan::hplans,hsize+-+stilesize)
     in
         (p'',List.rev allplans,allsize)
     end
   ;
                     
   val plans_of_sequent = 
     let fun mkp elf el p = plan_of_element el elf p in p_of_s mkp end

(*    val plans_of_sequentntexts = let fun mkp class eli p = 
     plan_of_elementinfo (elnoside eli) class p in p_of_s mkp end
   ;
*)                                                 
*)


let rec planOffset =
  fun (Formulaplan (layout, box, thing)) pos ->
    Formulaplan (layout, tbOffset box pos, thing)

let rec drawplan f p =
  fun (Formulaplan (Textlayout tl, b, info)) ->
    Japeserver.drawmeasuredtext (f info) tl (p +->+ tbPos b)

let rec findfirstplanhit p =
  findfirst (fun pl -> if withintb (p, textbox_of_plan pl) then Some pl else None)

let string_of_textinfo = string_of_pair string_of_textsize string_of_textlayout ","

