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

type ('c, 'r) searchtree and ('c, 'r) fsm

type ('r, 's) searchresult = Found of ('r * 's) | NotFound of 's

val addtotree       : ('r * 'r -> bool) -> ('c, 'r) searchtree -> 'c list * 'r * bool
				   -> ('c, 'r) searchtree
val deletefromtree  : ('r * 'r -> bool) -> ('c, 'r) searchtree -> 'c list * 'r * bool
                   -> ('c, 'r) searchtree
val emptysearchtree : (('c * ('c, 'r) fsm) list -> ('c, 'r) fsm -> 'c -> ('c, 'r) fsm)
				   -> ('c, 'r) searchtree
val summarisetree   : ('c, 'r) searchtree -> ('c list * 'r * bool) list

val rootfsm         : ('c, 'r) searchtree ref -> ('c, 'r) fsm
val fsmpos          : ('c, 'r) fsm -> 'c list -> ('c, 'r) fsm option

val scanfsm         : (unit -> 'c) -> ('c, 'r) fsm -> 'c list -> 'c 
                   -> ('r, 'c list) searchresult
val scanstatefsm    : ('a -> 'c) -> ('a -> 'a) -> ('c, 'r) fsm -> 'a 
                   -> ('r, 'a) searchresult
val searchfsm       : ('c, 'r) fsm -> 'c list -> ('r, 'c list) searchresult

val catelim_fsmstring : ('c -> string list -> string list)
					 -> ('r -> string list -> string list) -> ('c, 'r) fsm -> string list
					 -> string list

exception DeleteFromTree_
