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
open List

let (<*) = Listfuns.(<*)
let (<.>) = Sml.(<.>)
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
   [ H {, H } ] |- [ C {, C } ] - otherwise.

   that knowledge is entirely encapsulated in the makeseqplan function.
 
   Left and right sides are stacked, if leading>0.
   
   p is position at which sequent must be drawn.
 *)
let rec makeseqplan leading string_of_element showturnstiles p seq =
  let comma = comma () in
  let (commasize, _ as comminf) = textinfo_of_text comma in
  let mkcommaplan = plan_of_textinfo comminf PunctClass in
  let rec makeelementplan c el =
    plan_of_element string_of_element el (ElementClass (el, c))
  in
  let rhs cs =
    plans_of_things (makeelementplan DisplayConc) mkcommaplan
                    (fun _ -> [], emptytextbox) cs
  in
  match explode seq, showturnstiles with
    (st, [], ([c] as cs)), false -> rhs cs p
  | (st, hs, cs), _ ->
      let turnstile = turnstile st in
      let (turnstilesize, _ as stileinf) = textinfo_of_text turnstile in
      let mkstileplan = plan_of_textinfo stileinf PunctClass (* probably needs more space when stacked *)
      in 
      if leading>0 && (length hs>1 || length cs>1) then
       let stackedside p eclass es =
          let eplans = (fun e -> makeelementplan eclass e origin) <* es in
          let commaplan = mkcommaplan origin in
          let listmax = fold_left max 0 in
          let width = tsW <.> textsize_of_plan in
          let height = tsH <.> textsize_of_plan in
          let commaw = width commaplan in
          let h = listmax (height <* eplans) + leading in
          let w = listmax (width <* eplans) + commaw in
          let lines = Minwaste.minwaste (fun plan -> width plan + commaw) w eplans in
          let linef p = 
            planfold planOffset (planOffset commaplan) (p, emptytextbox, [])
          in
          let rec stackfold (p, tb, rs as res) =
              function []            -> res
              |        [line]        -> let _, tbr, r = linef p line in
                                        downby p h, tb +|-|+ tbr, (r,tbr) :: rs
              |        line :: lines ->
                         let p', tbl, r = linef p line in
                         let complan = planOffset commaplan p' in
                         let r' = complan :: r in
                         let tbl' = tbl +|-|+ textbox_of_plan complan in
                         stackfold (downby p h, tb +|-|+ tbl', (r', tbl') :: rs) lines
          in
          let _, tb, rs = stackfold (p,emptytextbox,[]) lines in
          rs, tb
        in
        let stackunwind tail stack =
          let planunwind' tail line = planunwind tail (fst line) in
          List.fold_left planunwind' tail (fst stack)
        in
        let lhsstack = stackedside p DisplayHyp hs in
        let tbrs, tbl = lhsstack in
        let lw = tbW tbl in
        let rightj (line, tb) = 
          let offset = lw - tbW tb in
          let poff = pos offset 0 in
          let rj plan = planOffset plan poff in
          rj <* line, tb
        in
        let lhsstack = rightj <* tbrs, tbl in
        (* We first align the stileplan with the hyps *)
        let ps = rightby p (tbW tbl) in
        let stileplan = mkstileplan ps in
        let soffset = (tbH tbl - tbH (textbox_of_plan stileplan)) / 2 in
        let stileplan = mkstileplan (downby ps soffset) in 
        let tbs = textbox_of_plan stileplan in
        let tbls = tbl +|-|+ tbs in
        (* now we find the size of the rhs stack *)
        let pr = rightby p (tbW tbls) in
        let rhsstack = stackedside pr DisplayConc cs in
        let roffset = (tbH tbls - tbH (snd rhsstack)) / 2 in
        (* now do it again in the right place *)
        let rhsstack = stackedside (downby pr roffset) DisplayConc cs in
        (* and put it all together *)
        stackunwind (plancons stileplan (stackunwind ([],emptytextbox) rhsstack)) lhsstack
      else
        plans_of_things (makeelementplan DisplayHyp) mkcommaplan
                        (fun p -> planfollowedby (mkstileplan p) (rhs cs)) hs p 

let rec seqdraw p seqbox seqplan =
  iter (drawplan displayclass_of_planclass (p +->+ tbP seqbox)) seqplan

let rec seqelementpos p seqbox plan =
  p +->+ tbP seqbox +->+ tbP (textbox_of_plan plan)
