(* $Id$ *)

val atoi : string -> int
exception AtoI_
val error : string list -> 'a
exception Error_
val observe : string list -> unit
val sum : int list -> int
val charpred : string -> (string -> bool) * (string * bool -> unit)
val isdigit : string -> bool
val islcletter : string -> bool
val isletter : string -> bool
val isucletter : string -> bool
val iter : (int -> 'a) -> int * int -> unit
val curry2 : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
val uncurry2 : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val create_reportfile : string -> unit
val close_reportfile : unit -> unit
val consolereport : string list -> unit
val consolequery : string list * string * string * int -> bool
val refstring : ('a -> string) -> 'a ref -> string
val earlierpair :
  ('a -> 'a -> bool) -> ('b -> 'b -> bool) -> ('a * 'b) -> ('a * 'b) -> bool
val onbra : char
val onket : char
val offbra : char
val offket : char
val outbra : char
val outket : char
val lockbra : char
val lockket : char
val invisible : string -> bool
val lemmacount : int ref
(* number of lemmas during THIS proof *)
val applyconjectures : bool ref
(* conjectures allowed in proofs *)
val applyderivedrules : bool ref
(* derived rules allowed in proofs *)
val autoselect : bool ref
(* show 'goal' when printing proofs *)
val givenMenuTactic : string ref
(* what to use when the interface says applygiven *)
val foldformulae : bool ref
(* whether to fold long lines in boxdraw *)
val truncatereasons : bool ref
(* whether to shorten reasons in boxdraw *)
val seektipselection : bool ref
(* look for a tip to work on in boxdraw *)
val textselectionmode : string ref
(* how to press-and-drag over text *)

val screenpositiondebug : bool ref
exception Catastrophe_ of string list
exception ParseError_ of string list
exception Tacastrophe_ of string list

