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

(* draw a sequent - used in treedraw and disproof *)

open Box 
open Draw 
open Displayclass

let comma = Absprooftree.comma
let string_of_element = Termstring.string_of_element
let explode = Absprooftree.explode
let turnstile = Absprooftree.turnstile

type planclass =
  ElementClass of (element * displayclass) | PunctClass | ReasonClass

let rec string_of_planclass =
  function
    ElementClass (el, c) -> "ElementClass(\"" ^ string_of_element el ^ "\"," ^ string_of_displayclass c ^ ")"
  | PunctClass           -> "PunctClass"
  | ReasonClass          -> "ReasonClass"

let rec displayclass_of_planclass =
  function ElementClass (_, i) -> i
  |        PunctClass          -> DisplayPunct
  |        ReasonClass         -> DisplayReason

let rec makeelementplan string_of_element c el =
  plan_of_element string_of_element el (ElementClass (el, c))

(* sequents are drawn
                              C - if lhs empty and rhs a single element
   [ H {, H } ] |- [ C {, C } ] - otherwise

   that knowledge is entirely encapsulated in the makeseqplan function.
 *)

let rec makeseqplan string_of_element showturnstiles p seq =
  let comma = comma () in
  let (commasize, _ as comminf) = textinfo_of_text comma in
  let mkcommaplan = plan_of_textinfo comminf PunctClass in
  let rec makeelementplan c el =
    plan_of_element string_of_element el (ElementClass (el, c))
  in
  let rec rhs cs =
    plans_of_things (makeelementplan DisplayConc) mkcommaplan
      (fun _ -> [], emptytextbox) cs
  in
  match explode seq, showturnstiles with
    (st, [], ([c] as cs)), false -> rhs cs p
  | (st, hs, cs), _ ->
      let turnstile = turnstile st in
      let (turnstilesize, _ as stileinf) = textinfo_of_text turnstile in
      let mkstileplan = plan_of_textinfo stileinf PunctClass in
      plans_of_things (makeelementplan DisplayHyp) mkcommaplan
        (fun p -> plancons (mkstileplan p) (rhs cs)) hs p

let rec seqdraw p seqbox seqplan =
  List.iter (drawplan displayclass_of_planclass (p +->+ tbPos seqbox)) seqplan

let rec seqelementpos p seqbox plan =
  p +->+ tbPos seqbox +->+ tbPos (textbox_of_plan plan)
