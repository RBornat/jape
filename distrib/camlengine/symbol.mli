(* $Id$ *)

type symbol = Symboltype.symbol
and associativity = Symboltype.associativity
and idclass = Idclass.M.idclass

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
val metachar : string
val mustseparate : string * string -> bool

(* aid for parsers *)
val scansymb : unit -> unit
val currsymb : unit -> symbol
val currnovelsymb : unit -> symbol (* so that user can define new symbols, punctuation marks, whatever *)
val canstartnovelsymb : symbol -> bool
val peeksymb : unit -> symbol
val putbacksymb : symbol -> unit (* for primitive backtracking *)

type savedlex
val pushlex : string -> char Stream.t -> savedlex
val poplex : savedlex -> unit

val showInputError : (string list -> unit) -> string list -> unit
val resetSymbols : unit -> unit
val escapechar : string -> string
val unescapechar : string -> string
val commasymbol : symbol
val appfix : int ref
val substfix : int ref
val substsense : bool ref

(* aid for keyboardists *)
val get_oplist : unit -> string list
val set_oplist : string list -> unit

(* aid for name inventors *)
val autoID : idclass -> string -> string
