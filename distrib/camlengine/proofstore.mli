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


open Cxttype
open Forcedef 
open Name
open Proofstage
open Prooftree.Tree.Fmttree
open Proviso
open Seqtype
open Thing

val saveable : unit -> bool
val saved : unit -> bool
val freezesaved : unit -> unit
val thawsaved : unit -> unit

val saveproof : out_channel -> name -> proofstage -> prooftree 
             -> proviso list -> seq list -> (seq * model) option -> unit
val saveproofs : out_channel -> unit

val proved            : name -> bool
val disproved         : name -> bool
val provedordisproved : name -> (bool * bool) option

val proofnamed : name ->
                 (bool * prooftree * (bool * proviso) list * seq list * bool * (seq * model) option) option

val addproof : (string list -> unit) ->                          (* alert *)
               (string list * string * string * int -> bool) ->  (* query *)
               name -> bool -> prooftree -> seq list -> cxt ->   (* name proved proof givens cxt *)
               bool -> (seq * model) option -> bool              (* disproved disproof -> success *)

val clearproofs : unit -> unit
val proofnames : unit -> name list
val thingswithproofs : bool -> name list
val namedthingswithproofs : bool -> name list -> name list

(* bad naming here *)
val needsProof : name -> thing -> bool
val lacksProof : name -> bool
val thmLacksProof : name -> bool
