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

open Listfuns
open Symbol
open Termfuns
open Termstore

type vid = Termtype.vid
 and idclass = Idclass.idclass

type paraparam = Objectparam      of (vid * idclass)
               | Ordinaryparam    of (vid * idclass)
               | Unknownparam     of (vid * idclass)
               | Abstractionparam of (vid * idclass)

(* ABSTRACTION P *) (*huh?*)
 
let rec catelim_paraparamstring p tail =
  match p with
    Objectparam (v, _) -> "OBJECT " :: string_of_vid v :: tail
  | Ordinaryparam (v, _) -> string_of_vid v :: tail
  | Unknownparam (v, _) -> metachar :: string_of_vid v :: tail
  | Abstractionparam (v, _) -> "ABSTRACTION " :: string_of_vid v :: tail

let paraparamstring = catelim2stringfn catelim_paraparamstring

let rec paramidbits p =
  match p with
    Objectparam vc -> vc
  | Ordinaryparam vc -> vc
  | Unknownparam vc -> vc
  | Abstractionparam vc -> vc

let rec paramvar p =
  match p with
    Objectparam vc -> registerId vc
  | Ordinaryparam vc -> registerId vc
  | Unknownparam vc -> registerUnknown vc
  | Abstractionparam vc -> registerId vc

