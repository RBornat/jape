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

val (<.>) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c (* compose -- can't do without it *)

val chars_of_string : string -> char list
val explode : string -> string list
val fSome : 'a ->'a option
val fst_of_3 : ('a * 'b * 'c) -> 'a
val fst_of_6 : ('a * 'b * 'c * 'd * 'e * 'f) -> 'a
val fst_of_7 : ('a * 'b * 'c * 'd * 'e * 'f * 'g) -> 'a
val implode : string list -> string
val nj_fold : ('b * 'a -> 'a) -> 'b list -> 'a -> 'a
val nj_revfold : ('b * 'a -> 'a) -> 'b list -> 'a -> 'a
val null : 'a list -> bool
val ord : string -> int
val ordof : string -> int -> int
val revapp : ('a -> unit) -> 'a list -> unit
val snd_of_3 : ('a * 'b * 'c) -> 'b
val string_of_chars : char list -> string
val thrd : ('a * 'b * 'c) -> 'c

exception OrdOf_ of string * int
