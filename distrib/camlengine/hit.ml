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

open Sml
	
type element = Term.Type.element

let bracketedliststring = Listfuns.bracketedliststring
let elementstring       = Term.Termstring.smlelementstring Term.Termstring.termstring
let optionmap           = Optionfuns.optionmap
let optionstring        = Optionfuns.optionstring
let pairstring          = Stringfuns.pairstring
let triplestring        = Stringfuns.triplestring
let sextuplestring      = Stringfuns.sextuplestring

type side = Left | Right
type 'a hit = FormulaHit of 'a fhit | ReasonHit of 'a
and 'a fhit =
	ConcHit of ('a * (element * side option))
  | HypHit of ('a * element)
  | AmbigHit of (('a * (element * side option)) * ('a * element))
type hitkind = HitPath | PrunePath | LayoutPath
type 'a sel =
	FormulaSel of
	  ('a * (element * side option) option * element list *
		 ((element * side option) * string list) list *
		 (element * string list) list * string list)
  | TextSel of (('a fhit * string list) list * string list)
  | ReasonSel of 'a
let rec fhitpath =
  function
	ConcHit (p, _) -> Some p
  | HypHit (p, _) -> Some p
  | AmbigHit _ -> None
let rec hitpath =
  function
	FormulaHit h -> fhitpath h
  | ReasonHit p -> Some p
let rec selpath =
  function
	FormulaSel f -> Some (fst_of_6 f)
  | TextSel (ths, _) ->
	  begin match
		optionmap (fhitpath <*> fst) ths
	  with
		Some (p :: ps) ->
		  if not (List.exists (fun p' -> p <> p') ps) then Some p else None
	  | _ -> None
	  end
  | ReasonSel p -> Some p
let rec tranfhitpath a1 a2 =
  match a1, a2 with
	f, ConcHit (p, c) -> ConcHit (f p, c)
  | f, HypHit (p, h) -> HypHit (f p, h)
  | f, AmbigHit ((p1, s1), (p2, s2)) -> AmbigHit ((f p1, s1), (f p2, s2))
let rec tranhitpath a1 a2 =
  match a1, a2 with
	f, FormulaHit h -> FormulaHit (tranfhitpath f h)
  | f, ReasonHit p -> ReasonHit (f p)
let rec transelpath a1 a2 =
  match a1, a2 with
	f, FormulaSel (p, c, hs, tc, ths, tg) ->
	  FormulaSel (f p, c, hs, tc, ths, tg)
  | f, TextSel (ths, tg) ->
	  TextSel (List.map (fun (th, ss) -> tranfhitpath f th, ss) ths, tg)
  | f, ReasonSel p -> ReasonSel (f p)
let rec sidestring =
  function
	Left -> "Left"
  | Right -> "Right"
let rec hitkindstring =
  function
	HitPath -> "HitPath"
  | PrunePath -> "PrunePath"
  | LayoutPath -> "LayoutPath"
let elsistring = pairstring elementstring (optionstring sidestring) ","
let rec fhitstring pathstring s =
  let pelsistring = pairstring pathstring elsistring "," in
  let pelstring = pairstring pathstring elementstring "," in
  match s with
	ConcHit pc -> "ConcHit" ^ pelsistring pc
  | HypHit ph -> "HypHit" ^ pelstring ph
  | AmbigHit pch -> "AmbigHit" ^ pairstring pelsistring pelstring "," pch
let rec hitstring a1 a2 =
  match a1, a2 with
	pathstring, FormulaHit h ->
	  ("FormulaHit(" ^ fhitstring pathstring h) ^ ")"
  | pathstring, ReasonHit p -> ("ReasonHit(" ^ pathstring p) ^ ")"
let sstring = bracketedliststring (fun s -> s) ","
let rec selstring a1 a2 =
  match a1, a2 with
	pathstring, FormulaSel f ->
	  "FormulaSel" ^
		sextuplestring pathstring (optionstring elsistring)
		  (bracketedliststring elementstring ",")
		  (bracketedliststring (pairstring elsistring sstring ",") ",")
		  (bracketedliststring (pairstring elementstring sstring ",") ",")
		  sstring "," f
  | pathstring, TextSel t ->
	  "TextSel" ^
		pairstring
		  (bracketedliststring
			 (pairstring (fhitstring pathstring) sstring ",") ",")
		  sstring "," t
  | pathstring, ReasonSel p -> ("ReasonSel(" ^ pathstring p) ^ ")"

