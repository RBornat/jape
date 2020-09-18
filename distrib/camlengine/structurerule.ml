(*
    Copyright (C) 2003-20 Richard Bornat & Bernard Sufrin
     
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

open Name
open Listfuns

type structurerule =
  | CutRule
  | LeftWeakenRule
  | RightWeakenRule
  | IdentityRule
  | TransitiveRule
  | ReflexiveRule

let rec string_of_structurerule sr =
  match sr with
  | CutRule         -> "CutRule"
  | LeftWeakenRule  -> "LeftWeakenRule"
  | RightWeakenRule -> "RightWeakenRule"
  | IdentityRule    -> "IdentityRule"
  | TransitiveRule  -> "TransitiveRule"
  | ReflexiveRule   -> "ReflexiveRule"

let structurerules : (structurerule * name) list ref = ref []

let addstructurerule kind name = structurerules := (kind, name) :: !structurerules

let getstructurerules () = !structurerules

let clearstructurerules () = structurerules := []

let rec erasestructurerule name =
  structurerules := ((fun (_, n) -> n <> name) <| !structurerules)

let isstructurerule kind name =
  List.exists (fun (k, n) -> (k, n) = (kind, name)) !structurerules

