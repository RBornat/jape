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

open Termtype
open Facts
open Prooftree.Tree.Fmttree
open Seqtype

type forcedef = Forcedef.forcedef
 and model = Forcedef.model
 and universe

val catelim_universestring : string -> universe -> string list -> string list
val universestring : string -> universe -> string

val emptyworld : unit -> universe
val isemptyworld : universe -> bool
val addworldlabel : universe -> int * int -> term -> universe option
val deleteworldlabel : universe -> int * int -> term -> universe option

(* semantics *)

val addforcedef : term * forcedef -> unit
val clearforcedefs : unit -> unit
val hasforcedefs : unit -> bool
(* and when I can work out how to fix termstring ...
   val analyse : term -> world -> string
   val seq_analyse : seq -> world -> string list * string * string list
 *)

(* states of the interaction *)

type disproofstate

val catelim_disproofstatestring : disproofstate -> string list -> string list
val disproofstatestring : disproofstate -> string

val disproofstate_seq      : disproofstate -> seq
val disproofstate_universe : disproofstate -> universe
val disproofstate_selected : disproofstate -> (int * int) list

val disproofstate_conclusive   : disproofstate -> bool
val disproofstate_countermodel : disproofstate -> bool

(* because of the need for facts when evaluating, these functions don't evaluate.
 * So you should use evaldisproofstate once you've set it up
 *)
val withdisproofuniverse : disproofstate -> universe -> disproofstate

val addchild        : universe -> int * int -> int * int -> universe option
val addchildtolink  : universe -> int * int -> int * int -> int * int -> int * int -> universe option
val deletelink      : universe -> int * int -> int * int -> universe option
val deleteworld     : disproofstate -> int * int -> disproofstate option
val moveworld       : disproofstate -> int * int -> int * int -> disproofstate option
val moveworldtolink : disproofstate -> int * int -> int * int -> int * int -> int * int -> disproofstate option
val newtile         : disproofstate -> term -> disproofstate option
val splitlink       : universe -> int * int -> int * int -> int * int -> universe option
val worldselect     : disproofstate -> (int * int) list -> disproofstate option

val evaldisproofstate : facts -> prooftree -> disproofstate -> disproofstate

val disproof_start : facts -> prooftree -> path option -> element list -> disproofstate
val disproof_minimal : disproofstate option -> bool

(* models and disproofstates *)

val disproofstate2model : disproofstate option -> (seq * model) option
val model2disproofstate : facts -> prooftree -> (seq * model) option 
                       -> disproofstate option
val checkdisproof : facts -> prooftree -> (seq * model) option -> bool

exception Disproof_ of string list

(* stuff to display disproofs *)

val showdisproof : disproofstate -> unit
val cleardisproof : unit -> unit
val disproofdebug : bool ref
