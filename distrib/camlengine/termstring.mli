open Termtype

val termstring                : term -> string
val termstring_invisbracketed : bool -> term -> string (* first arg sets bracketing *)
val termstring_invischoose    : (term -> string) -> (term -> string) -> term -> string

val catelim_termstring                : term -> string list -> string list
val catelim_termstring_invisbracketed : bool -> term -> string list -> string list
val catelim_termstring_invischoose    : (term -> string) -> (term -> string) -> term
									 -> string list -> string list

val argstring : term -> string

val smltermstring         : term -> string
val catelim_smltermstring : term -> string list -> string list

val vtsstring : (term * term) list -> string

(* bracketed for use as args in curried functions *)
val collectionstring                : string -> term -> string
val collectionstring_invisbracketed : bool -> string -> term -> string

(* for those who don't want to see the details *)
val termOrCollectionstring                : string -> term -> string
val termOrCollectionstring_invisbracketed : bool -> string -> term -> string

val catelim_termOrCollectionstring                : string -> term -> string list -> string list
val catelim_termOrCollectionstring_invisbracketed : bool -> string -> term -> string list -> string list

val elementstring                : element -> string
val elementstring_invisbracketed : bool -> element -> string
val elementstring_invischoose    : (term -> string) -> (term -> string) -> element -> string

val catelim_elementstring                : element -> string list -> string list
val catelim_elementstring_invisbracketed : bool -> element -> string list -> string list

val smlelementstring         : (term -> string) -> element -> string
val catelim_smlelementstring : (term -> string list -> string list) -> element 
							-> string list -> string list

val resnumstring : resnum -> string

val termliststring : term list -> string

val catelim_vtsstring : (term * term) list -> string list -> string list
val catelim_argstring : term -> string list -> string list

val catelim_collectionstring                : string -> term -> string list -> string list
val catelim_collectionstring_invisbracketed : bool -> string -> term -> string list -> string list

val catelim_resnumstring : resnum -> string list -> string list
val catelim_termliststring : term list -> string list -> string list
val debracketapplications : bool ref

val remake : ((term -> term option) -> term -> 'a) ->
int option * (term list * term list * term list) *
(term * (int * int)) list * term -> 'a (* this is internals showing.  Sorry. RB *)
