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

open Sml

let (<*) = Listfuns.(<*)
let idf  = fun x -> x
    
type element = Termtype.element

let bracketedstring_of_list = Listfuns.bracketedstring_of_list
let string_of_element       = Termstring.debugstring_of_element Termstring.string_of_term
let optionmap               = Optionfuns.optionmap
let string_of_option        = Optionfuns.string_of_option
let string_of_pair          = Stringfuns.string_of_pair
let string_of_triple        = Stringfuns.string_of_triple
let string_of_sextuple      = Stringfuns.string_of_sextuple

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
         ('a * (element * side option) * string list) list *
         ('a * element * string list) list * string list)
  | TextSel of (('a fhit * string list) list * string list)
  | ReasonSel of 'a

let rec fhitpath =
  function
    ConcHit (p, _) -> Some p
  | HypHit  (p, _) -> Some p
  | AmbigHit _     -> None

let rec hitpath =
  function
    FormulaHit h -> fhitpath h
  | ReasonHit p -> Some p

let rec selpath =
  function
    FormulaSel f -> Some (fst_of_6 f)
  | TextSel (ths, _) ->
      begin match
        optionmap (fhitpath <.> fst) ths
      with
        Some (p :: ps) ->
          if not (List.exists (fun p' -> p <> p') ps) then Some p else None
      | _ -> None
      end
  | ReasonSel p -> Some p

let rec tranfhitpath f hit =
  match hit with
    ConcHit  (p, c)               -> ConcHit (f p, c)
  | HypHit   (p, h)               -> HypHit (f p, h)
  | AmbigHit ((p1, s1), (p2, s2)) -> AmbigHit ((f p1, s1), (f p2, s2))

let rec tranhitpath f hit =
  match hit with
    FormulaHit h -> FormulaHit (tranfhitpath f h)
  | ReasonHit  p -> ReasonHit (f p)

let rec transelpath f sel =
  let f3 ts = (fun (p, a, b) -> (f p, a, b)) <* ts in 
  match sel with
    FormulaSel (p, c, hs, tcs, ths, tg) -> FormulaSel (f p, c, hs, f3 tcs, f3 ths, tg)
  | TextSel    (ths, tg)                ->
      TextSel (List.map (fun (th, ss) -> tranfhitpath f th, ss) ths, tg)
  | ReasonSel  p                       -> ReasonSel (f p)

let rec string_of_side =
  function
    Left -> "Left"
  | Right -> "Right"

let rec string_of_hitkind =
  function
    HitPath -> "HitPath"
  | PrunePath -> "PrunePath"
  | LayoutPath -> "LayoutPath"

let string_of_elsi = string_of_pair string_of_element (string_of_option string_of_side) ","

let rec string_of_fhit string_of_path s =
  let string_of_pelsi = string_of_pair string_of_path string_of_elsi "," in
  let string_of_pel = string_of_pair string_of_path string_of_element "," in
  match s with
    ConcHit pc -> "ConcHit" ^ string_of_pelsi pc
  | HypHit ph -> "HypHit" ^ string_of_pel ph
  | AmbigHit pch -> "AmbigHit" ^ string_of_pair string_of_pelsi string_of_pel "," pch

let rec string_of_hit a1 a2 =
  match a1, a2 with
    string_of_path, FormulaHit h ->
      ("FormulaHit(" ^ string_of_fhit string_of_path h) ^ ")"
  | string_of_path, ReasonHit p -> ("ReasonHit(" ^ string_of_path p) ^ ")"

let sstring = bracketedstring_of_list idf ","

let rec string_of_sel a1 a2 =
  match a1, a2 with
    string_of_path, FormulaSel f ->
      "FormulaSel" ^
        string_of_sextuple string_of_path (string_of_option string_of_elsi)
          (bracketedstring_of_list string_of_element ",")
          (bracketedstring_of_list (string_of_triple string_of_path string_of_elsi sstring ",") ",")
          (bracketedstring_of_list (string_of_triple string_of_path string_of_element sstring ",") ",")
          sstring "," f
  | string_of_path, TextSel t ->
      "TextSel" ^
        string_of_pair
          (bracketedstring_of_list
             (string_of_pair (string_of_fhit string_of_path) sstring ",") ",")
          sstring "," t
  | string_of_path, ReasonSel p -> ("ReasonSel(" ^ string_of_path p) ^ ")"

