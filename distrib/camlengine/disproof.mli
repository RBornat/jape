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

open Box
open Termtype
open Facts
open Prooftree.Tree.Fmttree
open Seqtype

type forcedef = Forcedef.forcedef
 and model = Forcedef.model
 and universe

val catelim_string_of_universe : string -> universe -> string list -> string list
val string_of_universe : string -> universe -> string

val simplestuniverse : unit -> universe
val issimplestuniverse : universe -> bool
val addworldlabel : universe -> int * int -> term -> universe option
val deleteworldlabel : universe -> int * int -> term -> universe option
val moveworldlabel : universe -> int * int -> int * int -> term -> universe option

(* semantics *)

val addforcedef : term * forcedef -> unit
val clearforcedefs : unit -> unit
val hasforcedefs : unit -> bool
(* and when I can work out how to fix string_of_term ...
   val analyse : term -> world -> string
   val seq_analyse : seq -> world -> string list * string * string list
 *)

(* states of the interaction *)

type disproofstate

val catelim_string_of_disproofstate : disproofstate -> string list -> string list
val string_of_disproofstate : disproofstate -> string

val disproofstate_seq        : disproofstate -> seq
val disproofstate_selections : disproofstate -> pos list * (pos * string list) list
val disproofstate_universe   : disproofstate -> universe
val disproofstate_selected   : disproofstate -> (int * int) list

val disproofstate_conclusive   : disproofstate -> bool
val disproofstate_countermodel : disproofstate -> bool

(* because of the need for facts when evaluating, these functions don't evaluate.
 * So you should use evaldisproofstate once you've set it up
 *)
val withdisproofuniverse   : disproofstate -> universe -> disproofstate
val withdisproofselections : disproofstate -> pos list * (pos * string list) list 
                          -> disproofstate
                          
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

val model_of_disproofstate : disproofstate option -> (seq * model) option
val disproofstate_of_model : facts -> prooftree -> (seq * model) option 
                       -> disproofstate option
val checkdisproof : facts -> prooftree -> (seq * model) option -> bool

exception Disproof_ of string list

(* stuff to display disproofs *)

val showdisproof : disproofstate -> unit
val cleardisproof : unit -> unit
val disproofdebug : bool ref
