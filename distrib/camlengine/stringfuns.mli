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

val isQuoted : string -> bool
val disQuote : string -> string
val enQuote : string -> string
val words : string -> string list
val respace : string list -> string
val lowercase : string -> string
val uppercase : string -> string
val pairstring :
  ('a -> string) -> ('b -> string) -> string -> 'a * 'b -> string
val triplestring :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> string ->
    'a * 'b * 'c -> string
val quadruplestring :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    string -> 'a * 'b * 'c * 'd -> string
val quintuplestring :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    ('e -> string) -> string -> 'a * 'b * 'c * 'd * 'e -> string
val sextuplestring :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    ('e -> string) -> ('f -> string) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f -> string
val septuplestring :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    ('e -> string) -> ('f -> string) -> ('g -> string) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f * 'g -> string
val octuplestring :
  ('a -> string) -> ('b -> string) -> ('c -> string) -> ('d -> string) ->
    ('e -> string) -> ('f -> string) -> ('g -> string) ->
    ('h -> string) -> string -> 'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h ->
    string
val catelim_pairstring :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) -> string -> 'a * 'b ->
    string list -> string list
val catelim_triplestring :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) -> string -> 'a * 'b * 'c ->
    string list -> string list
val catelim_quadruplestring :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) -> string -> 'a * 'b * 'c * 'd ->
    string list -> string list
val catelim_quintuplestring :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) ->
    ('e -> string list -> string list) -> string ->
    'a * 'b * 'c * 'd * 'e -> string list -> string list
val catelim_sextuplestring :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) ->
    ('e -> string list -> string list) ->
    ('f -> string list -> string list) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f -> string list -> string list
val catelim_septuplestring :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) ->
    ('e -> string list -> string list) ->
    ('f -> string list -> string list) ->
    ('g -> string list -> string list) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f * 'g -> string list -> string list
val catelim_octuplestring :
  ('a -> string list -> string list) ->
    ('b -> string list -> string list) ->
    ('c -> string list -> string list) ->
    ('d -> string list -> string list) ->
    ('e -> string list -> string list) ->
    ('f -> string list -> string list) ->
    ('g -> string list -> string list) ->
    ('h -> string list -> string list) -> string ->
    'a * 'b * 'c * 'd * 'e * 'f * 'g * 'h -> string list -> string list
val catelim_arraystring :
  ('a -> string list -> string list) -> string -> 'a array ->
    string list -> string list
val arraystring : ('a -> string) -> string -> 'a array -> string

val quotedstring_of_char : char -> string
