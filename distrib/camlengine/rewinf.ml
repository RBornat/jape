(*
    Copyright (C) 2003-17 Richard Bornat & Bernard Sufrin
     
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

open Stringfuns
open Termstring
open Listfuns
open Optionfuns
open Termfuns

type term = Termtype.term
 and vid  = Termtype.vid

(* see rewrite.sml for an explanation of this data structure *)
type rewinf = Rewinf of (term list * vid list * int list * int option)

let nullrewinf = Rewinf ([], [], [], None)

let mkrewinf v = Rewinf v

let rec rawinf_of_rew = fun (Rewinf r) -> r
let rec rewinf_vars = fun (Rewinf (vars, _, _, _)) -> vars
let rec rewinf_uVIDs = fun (Rewinf (_, uVIDs, _, _)) -> uVIDs
let rec rewinf_badres = fun (Rewinf (_, _, badres, _)) -> badres
let rec rewinf_psig = fun (Rewinf (_, _, _, psig)) -> psig
let rec rewinf_setvars =
  fun (Rewinf (_, uVIDs, badres, psig)) vars ->
    Rewinf (vars, uVIDs, badres, psig)
let rec rewinf_setuVIDs =
  fun (Rewinf (vars, _, badres, psig)) uVIDs ->
    Rewinf (vars, uVIDs, badres, psig)
let rec rewinf_setbadres =
  fun (Rewinf (vars, uVIDs, _, psig)) badres ->
    Rewinf (vars, uVIDs, badres, psig)
let rec rewinf_setpsig =
  fun (Rewinf (vars, uVIDs, badres, _)) psig ->
    Rewinf (vars, uVIDs, badres, psig)
let rec rewinf_addvars =
  fun (Rewinf (vars, uVIDs, badres, psig)) vars' ->
    Rewinf (vars' @ vars, uVIDs, badres, psig)
let rec rewinf_adduVIDs =
  fun (Rewinf (vars, uVIDs, badres, psig)) uVIDs' ->
    Rewinf (vars, uVIDs' @ uVIDs, badres, psig)
let rec rewinf_addbadres =
  fun (Rewinf (vars, uVIDs, badres, psig)) badres' ->
    Rewinf (vars, uVIDs, badres' @ badres, psig)
let rec string_of_rewinf =
  fun (Rewinf r) ->
    "Rewinf" ^
      string_of_quadruple string_of_termlist (bracketed_string_of_list string_of_vid ",")
        (bracketed_string_of_list string_of_int ",") (string_of_option string_of_int) ","
        r
let rec rewinf_merge =
  fun
    (Rewinf (allvars, uVIDs, badres, psig),
     Rewinf (allvars', uVIDs', badres', psig')) ->
    Rewinf
      (mergevars allvars allvars', mergeVIDs uVIDs uVIDs',
       sortedmerge (<) badres badres',
       (match psig, psig' with
          Some n, Some n' -> Some (min n n')
        | Some _, None -> psig
        | _ -> psig'))
