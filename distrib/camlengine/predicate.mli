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

open Termtype

val compilepredicate :
  (term -> bool) -> (term -> term list option) -> term -> term option
val discardzeroarities :
  (term * (term list * term list list) list) list ->
    (term * (term list * term list list) list) list
val findpredicates :
  (term -> bool) -> term list ->
    term * (term * (term list * term list list) list) list ->
    (term * (term list * term list list) list) list option
val findpredicatevars :
  (term list * term list list) list -> term list option
val interpretpredicates : bool ref
val matchpredicate :
  bool -> (term -> bool) -> term -> (term * term list) option
val predicatedebug : bool ref
val predicatebindingstring :
  (term * (term list * term list list) list) list -> string

exception Predicate_ of string list
