(*
	$Id$

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
let elementstring = Term.Termstring.elementstring
let explode = Absprooftree.explode
let turnstile = Absprooftree.turnstile

type planclass =
  ElementClass of (element * displayclass) | PunctClass | ReasonClass

let rec planclassstring =
  function
    ElementClass (el, c) ->
      (("ElementClass(" ^ elementstring el) ^ displayclassstring c) ^ ")"
  | PunctClass -> "PunctClass"
  | ReasonClass -> "ReasonClass"

let rec planclass2displayclass =
  function
    ElementClass (_, i) -> i
  | PunctClass -> DisplayPunct
  | ReasonClass -> DisplayReason

let rec makeelementplan elementstring c el =
  element2plan elementstring el (ElementClass (el, c))

(* sequents are drawn
                                C - if lhs empty and rhs a single element
   [ H {, H } ] |- [ C {, C } ] - otherwise

   that knowledge is entirely encapsulated in the makeseqplan function.
 *)

let rec makeseqplan elementstring showturnstiles p seq =
  let comma = comma () in
  let (commasize, _ as comminf) = text2textinfo comma in
  let mkcommaplan = textinfo2plan comminf PunctClass in
  let rec makeelementplan c el =
    element2plan elementstring el (ElementClass (el, c))
  in
  let rec rhs cs =
    things2plans (makeelementplan DisplayConc) mkcommaplan
      (fun _ -> [], emptytextbox) cs
  in
  match explode seq, showturnstiles with
    (st, [], ([c] as cs)), false -> rhs cs p
  | (st, hs, cs), _ ->
      let turnstile = turnstile st in
      let (turnstilesize, _ as stileinf) = text2textinfo turnstile in
      let mkstileplan = textinfo2plan stileinf PunctClass in
      things2plans (makeelementplan DisplayHyp) mkcommaplan
        (fun p -> plancons (mkstileplan p) (rhs cs)) hs p

let rec seqdraw p seqbox seqplan =
  List.iter (drawplan planclass2displayclass (p +->+ tbPos seqbox)) seqplan

let rec seqelementpos p seqbox plan =
  p +->+ tbPos seqbox +->+ tbPos (plantextbox plan)
