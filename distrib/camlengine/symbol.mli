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

open UTF

type symbol = Symboltype.symbol
and associativity = Symboltype.associativity
and idclass = Idclass.idclass

val symboldebug : bool ref
val enter : string * symbol -> unit
val lookup : string -> symbol (* default is ID *)
val lookupassoc : string -> (bool * associativity) option (* (curried, assoc) - when INFIX... *)

val declareIdPrefix : idclass -> string -> (string * idclass) list
val declareIdClass : idclass -> string -> (string * idclass) option
val symclass : string -> idclass
exception Symclass_ of string
val isnumber : string -> bool
val isextensibleID : string -> bool
val symbolstring : symbol -> string
val smlsymbolstring : symbol -> string

(* aid for prettyprinters *)
val metachar_as_string : string
val mustseparate       : string * string -> bool

(* aid for parsers *)
val scansymb : unit -> unit
val currsymb : unit -> symbol
val currnovelsymb : unit -> symbol (* so that user can define new symbols, punctuation marks, whatever *)
val canstartnovelsymb : symbol -> bool
val peeksymb : unit -> symbol
val putbacksymb : symbol -> unit (* for primitive backtracking *)

type savedlex
val pushlex : string -> ucode Stream.t -> savedlex
val poplex  : savedlex -> unit

val showInputError : (string list -> unit) -> string list -> unit
val resetSymbols   : unit -> unit
val escapechar : ucode -> ucode
val commasymbol : symbol

val appfix     : int ref
val substfix   : int ref
val substsense : bool ref

(* aid for keyboardists *)
val get_oplist : unit -> string list
val set_oplist : string list -> unit

(* aid for name inventors *)
val autoID : idclass -> string -> string
