(*
    Copyright (C) 2003-19 Richard Bornat & Bernard Sufrin
     
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

type prestring =
  Prestr of string | Prestrs of string list | Prepres of prestring list

val pre_List : ('a -> prestring) -> 'a list -> prestring
val pre_Set : ('a -> prestring) -> 'a list -> prestring
val pre_Tuple : ('a -> prestring) -> 'a list -> prestring
val pre__comma : prestring
val pre__nil : prestring
val pre__space : prestring
val pre_app : (string -> unit) -> prestring -> unit
val pre_array : ('a -> prestring) -> 'a array -> prestring
val pre_bool : bool -> prestring
val pre_implode : prestring -> string
val pre_int : int -> prestring
val pre_list : ('a -> prestring) -> 'a list -> prestring
val pre_option : ('a -> prestring) -> 'a option -> prestring
val pre_real : float -> prestring
val pre_string : string -> prestring
val pre_unit : unit -> prestring
val pre_vector : ('a -> prestring) -> 'a array -> prestring

val pre_Ascii : string -> string
