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

exception Malformed_

val next_utf8  : char Stream.t -> int
val next_utf16 : bool (* bigendian *) -> char Stream.t -> int
val next_utf32 : bool (* bigendian *) -> char Stream.t -> int

val utf8_of_int  : int -> char list
val utf16_of_int : bool (* bigendian *) -> int -> char list
val utf32_of_int : bool (* bigendian *) -> int -> char list

val utf8_of_utfchannel  : in_channel -> char Stream.t                (* respects BOMs; utf8 default *)
val utf8_of_utfNchannel : int -> bool -> in_channel -> char Stream.t (* skips BOM *)
val open_out_utf8       : string -> out_channel                      (* writes utf8BOM *)

val utf8width_from_header : char -> int

val utf8_sub    : string -> int -> string
val utf8_presub : string -> int -> string

val utf8_peek : char Stream.t -> string option
val utf8_junk : char Stream.t -> unit

val utf8_explode : string -> string list