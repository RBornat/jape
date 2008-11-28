(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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

type ('a, 'b) mapping

(* infixr   9   at
   infixr   8   |->  
   infixr   7   ++
   infixr   7   --
 *)

val ( ++  ) : ('a, 'b) mapping -> ('a, 'b) mapping -> ('a, 'b) mapping
val ( --  ) : ('a, 'b) mapping -> 'a list -> ('a, 'b) mapping
val ( |-> ) : 'a -> 'b -> ('a, 'b) mapping

val aslist : ('a, 'b) mapping -> ('a * 'b) list
val (<@>)  : ('a, 'b) mapping -> 'a -> 'b option
val dom : ('a, 'b) mapping -> 'a list
val empty : ('a, 'b) mapping
val formappingpairs : ('a * 'b -> unit) * ('a, 'b) mapping -> unit
val isempty : ('a, 'b) mapping -> bool
val lfold : (('a * 'b) * 'c -> 'c) -> 'c -> ('a, 'b) mapping -> 'c
val mapped : ('a * 'a -> bool) -> ('a, 'b) mapping -> 'a -> 'b option
val mkmap : ('a * 'b) list -> ('a, 'b) mapping
val ran : ('a, 'b) mapping -> 'b list
val rawaslist : ('a, 'b) mapping -> ('a * 'b) list
val rawdom : ('a, 'b) mapping -> 'a list
val rawran : ('a, 'b) mapping -> 'b list
val remapping : ('a * 'b -> 'c * 'd) * ('a, 'b) mapping -> ('c, 'd) mapping

val catelim_string_of_mapping :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) -> string -> ('a, 'b) mapping ->
    string list -> string list
val string_of_mapping :
  ('a -> string) -> ('b -> string) -> ('a, 'b) mapping -> string
