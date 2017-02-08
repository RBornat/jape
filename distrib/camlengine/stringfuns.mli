(*
    Copyright (C) 2003-17 Richard Bornat & Bernard Sufrin
     
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

val isQuoted : string -> bool
val disQuote : string -> string
val enQuote : string -> string
val enCharQuote : string -> string
val lowercase : string -> string
val uppercase : string -> string
val string_of_pair :
  ('a -> string) -> ('b -> string) -> string -> 'a * 'b -> string
val string_of_triple :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> string ->
    'a * 'b * 'c -> string
val string_of_quadruple :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    string -> 'a * 'b * 'c * 'd -> string
val string_of_quintuple :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    ('e -> string) -> string -> 'a * 'b * 'c * 'd * 'e -> string
val string_of_sextuple :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    ('e -> string) -> ('f -> string) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f -> string
val string_of_septuple :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    ('e -> string) -> ('f -> string) -> ('g -> string) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f * 'g -> string
val string_of_octuple :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    ('e -> string) -> ('f -> string) -> ('g -> string) ->
    ('h -> string) -> string -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h ->
    string
val catelim_string_of_pair :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) -> string -> 'a * 'b ->
    string list -> string list
val catelim_string_of_triple :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) -> string -> 'a * 'b * 'c ->
    string list -> string list
val catelim_string_of_quadruple :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) -> string -> 'a * 'b * 'c * 'd ->
    string list -> string list
val catelim_string_of_quintuple :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) ->
    ('e -> string list -> string list) -> string ->
    'a * 'b * 'c * 'd * 'e -> string list -> string list
val catelim_string_of_sextuple :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) ->
    ('e -> string list -> string list) ->
    ('f -> string list -> string list) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f -> string list -> string list
val catelim_string_of_septuple :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) ->
    ('e -> string list -> string list) ->
    ('f -> string list -> string list) ->
    ('g -> string list -> string list) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f * 'g -> string list -> string list
val catelim_string_of_octuple :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) ->
    ('e -> string list -> string list) ->
    ('f -> string list -> string list) ->
    ('g -> string list -> string list) ->
    ('h -> string list -> string list) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> string list -> string list
val catelim_string_of_array :
  ('a -> string list -> string list) -> string -> 'a array ->
    string list -> string list
val string_of_array : ('a -> string) -> string -> 'a array -> string

val quotedstring_of_char : char -> string

(* the hexstring functions don't put the 0x on the front *)
val hexstring_of_int : int -> string
val fixedwidth_hexstring_of_int: int (*width*) -> int -> string