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

val (&~) : ('a -> 'b option) -> ('b -> 'c option) -> 'a -> 'c option  (* andthen  *)
val (&~~) : 'a option -> ('a -> 'b option) -> 'b option               (* andthenr *)
val (|~) : ('a -> 'b option) -> ('a -> 'b option) -> 'a -> 'b option  (* ortry    *)
val (|~~) : 'a option -> (unit -> 'a option) -> 'a option             (* ortryr   *)

val _The  : 'a option -> 'a                              (* _The None raises None_ *)
val _Some : 'a -> 'a option                              (* Some as a function *)

val anyway : ('a -> 'a option) -> 'a -> 'a
val failpt : ('a -> 'a option) -> 'a -> 'a

val findbest  : ('a -> 'b option) -> ('b -> 'b -> 'b) -> 'a list -> 'b option
val findfirst : ('a -> 'b option) -> 'a list -> 'b option

val bool_of_opt : 'a option -> bool

val optioncompose : ('b -> 'c) * ('a -> 'b option) -> 'a -> 'c option
val optionfilter  : ('a -> 'b option) -> 'a list -> 'b list
val option_foldl  : ('b -> 'a -> 'b option) -> 'b -> 'a list -> 'b option
val option_foldr  : ('a -> 'b -> 'b option) -> 'b -> 'a list -> 'b option
val option_njfold : ('a * 'b  -> 'b option) -> 'a list -> 'b -> 'b option
val optionmap     : ('a -> 'b option) -> 'a list -> 'b list option

val optordefault : 'a option * 'a -> 'a
val somef : ('a -> 'a option) -> 'a -> 'a option
val stripoption : 'a option option -> 'a option
val try__ : ('a -> 'b) -> 'a option -> 'b option

(* save space when rewriting structures *)
val option_rewrite2 : ('a -> 'a option) -> ('b -> 'b option) 
                   -> 'a * 'b -> ('a * 'b) option
val option_rewrite3 : ('a -> 'a option) -> ('b -> 'b option) 
                   -> ('c -> 'c option) -> 'a * 'b * 'c -> ('a * 'b * 'c) option
val option_rewritelist : ('a -> 'a option) -> 'a list -> 'a list option

val catelim_string_of_option : ('a -> string list -> string list) 
                        -> 'a option -> string list -> string list
val string_of_option : ('a -> string) -> 'a option -> string

exception None_
