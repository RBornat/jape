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

open Termtype
open Idclass

val registerId : vid * idclass -> term
val registerUnknown : vid * idclass -> term
val registerApp : term * term -> term
val registerTup : string * term list -> term
val registerLiteral : litcon -> term
val registerFixapp : string list * term list -> term
val registerSubst : bool * term * (term * term) list -> term
val registerBinding :
  (term list * term list * term list) * 
  (term * (int * int)) list *
  term -> term
val registerCollection : idclass * element list -> term
val registerElement : resnum * term -> element
val registerSegvar : term list * term -> element
val resettermstore : unit -> unit

val hashterm : term -> int option
val hashelement : element -> int option
val termhashing : bool ref
