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

type ucode = int

val uEOF : ucode

exception MalformedUTF_ of string list

val utf8_get   : string -> int -> ucode
val utf8_next  : char Stream.t -> ucode
val utf16_next : bool (* bigendian *) -> char Stream.t -> ucode
val utf32_next : bool (* bigendian *) -> char Stream.t -> ucode

val of_utfchannel  : in_channel -> ucode Stream.t                (* respects BOMs; utf8 default *)
val stream_of_utfNinchannel : int -> bool -> in_channel -> ucode Stream.t (* skips BOM *)
val open_out_utf8  : string -> out_channel                       (* writes utf8 BOM *)

val utf_stdin : ucode Stream.t

val stream_of_utf8string : string -> ucode Stream.t

val utf8width_from_header : char -> int
val utf8width_from_ucode  : ucode -> int

val utf8_sub    : string -> int -> ucode
val utf8_presub : string -> int -> ucode

val words   : string -> string list
val respace : string list -> string

val utf8_of_ucode  : ucode -> string

val utf8_explode : string -> ucode list
val utf8_implode : ucode list -> string

val charpred   : string -> (ucode -> bool) * (ucode * bool -> unit)

val njunk : int -> 'a Stream.t -> unit

val isdigit    : ucode -> bool
val islcletter : ucode -> bool
val isletter   : ucode -> bool
val isucletter : ucode -> bool

val utf8LSQUOTE : string
val utf8RSQUOTE : string