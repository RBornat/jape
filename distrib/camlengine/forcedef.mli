(*
	$Id$

    Copyright (C) 2003 Richard Bornat & Bernard Sufrin
     
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

open Seqtype
open Termtype

type forcedef = ForcePrim of term
              | ForceBoth of (forcedef * forcedef)
              | ForceEither of (forcedef * forcedef)
              | ForceIf of (forcedef * forcedef)
              | ForceEverywhere of forcedef
              | ForceNowhere of forcedef
              | ForceAll of (term * term list * forcedef)
              | ForceSome of (term * term list * forcedef)

val forcedef2term: forcedef -> term option (* really, is it ForcePrim? *)

val catelim_forcedefstring : forcedef -> string list -> string list
val forcedefstring : forcedef -> string

val existsinforcedef : (term -> bool) -> forcedef -> bool
val findinforcedef : (term -> 'a option) -> forcedef -> 'a option
val mapforcedefterms : (term -> term option) -> forcedef -> forcedef
val parseForceDef : unit -> forcedef

type coordinate = Coord of (int * int)

and world = World of (coordinate * coordinate list * term list)

and model = Model of world list

val catelim_modelstring : (seq * model) option -> string list -> string list
val parsemodel : unit -> (seq * model) option
