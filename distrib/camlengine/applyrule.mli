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

type term = Termtype.term
and seq = Seqtype.seq
and rewinf = Prooftree.Tree.rewinf
and element = Termtype.element
and cxt = Cxttype.cxt
and prooftree = Prooftree.Tree.Fmttree.prooftree
and possmatch
and resnum = Termtype.resnum
and visproviso = Proviso.visproviso
and prooftree_step = Prooftree.Tree.prooftree_step
and name = Name.name

val applydebug : int ref
val beforeOfferingDo : (unit -> unit) -> unit
val failOfferingDo : (unit -> unit) -> unit
val succeedOfferingDo : (unit -> unit) -> unit

(* apply is now the only matcher:  
 * args are checker, filter, taker, selhyps, selconcs, name, stuff, reason, conjecture, cxt
 *)
val apply :
  (term * term -> cxt -> cxt list) -> (possmatch -> possmatch option) ->
    (possmatch -> 'a option) -> element list -> element list ->
    string * (bool * bool) * prooftree_step * term list *
      (resnum list * resnum list) * seq list * seq * visproviso list ->
    string -> cxt -> seq * rewinf -> 'a option
 (* reason    cxt    problem      *)

(* filters *)
val nofilter : possmatch -> possmatch option
val bymatch : possmatch -> possmatch option
val sameprovisos : possmatch -> possmatch option

(* discriminators *)
val takefirst : possmatch -> (cxt * prooftree) option
val takeonlyone : possmatch -> (cxt * prooftree) option
val offerChoice : possmatch -> (cxt * prooftree) option

(* and one more *)
val takethelot : possmatch -> (cxt * prooftree) list
