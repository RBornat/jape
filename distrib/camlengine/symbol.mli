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

open UTF

type symbol = Symboltype.symbol

and associativity = Symboltype.associativity

and idclass = Idclass.idclass

and ucode = UTF.ucode

val symboldebug : bool ref

val enter : string -> int option -> associativity option -> symbol -> unit

val lookup : string -> symbol (* default is ID(s, None) *)

val assoc : symbol -> associativity

val prio : symbol -> int

val lookupassoc : string -> (bool * associativity) option

(* (curried, assoc) - when INFIX... *)

val declareIdPrefix : idclass -> string -> (string * idclass) list

val declareIdClass : idclass -> string -> (string * idclass) option

val symclass : string -> idclass

exception Symclass_ of string

val isnumber : string -> bool

val isextensibleID : string -> bool

val string_of_symbol : symbol -> string

val debugstring_of_symbol : symbol -> string

(* aid for prettyprinters *)
val metachar_as_string : string

val mustseparate : string * string -> bool

(* aid for parsers *)
val scansymb : unit -> unit

val currsymb : unit -> symbol

val currnovelsymb : unit -> symbol

(* so that user can define new symbols, punctuation marks, whatever *)

val canstartnovelsymb : symbol -> bool

val peeksymb : unit -> symbol

val putbacksymb : symbol -> unit (* for primitive backtracking *)

val check : symbol -> unit

val ignore : symbol -> unit

type savedlex

val pushlex : string -> ucode Stream.t -> savedlex

val poplex : savedlex -> unit

val tryparse : (symbol -> 'a) -> string -> 'a

val tryparse_dbug : (symbol -> 'a) -> ('a -> string) -> string -> 'a

val pushSyntax : string -> unit

val popSyntax : unit -> unit

val popAllSyntaxes : unit -> unit

val showInputError : (string list -> unit) -> string list -> unit

val resetSymbols : unit -> unit

val escapechar : ucode -> ucode

val commasymbol : symbol

val appfix : int ref

val substfix : int ref

val substsense : bool ref

(* aid for keyboardists *)
val get_oplist : unit -> string list

val set_oplist : string list -> unit

(* aid for name inventors *)
val autoID : idclass -> string -> string
