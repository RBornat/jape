(* $Id$ *)

open Answer
open Facts
open Term.Funs
 
(* Oh at this point I wish for a type which distinguishes
   variables from terms.  Even though it might be a pain everywhere
   else. RB
 *)
     
val substdebug : bool ref
val varmappedbyq : facts -> term -> (term * term) list -> answer
val varboundbyq : facts -> term -> term list -> answer
val varoccursinq : facts -> term -> term -> answer
val varmappedto : facts -> term -> (term * term) list -> term option
val simplifySubstAnyway : facts -> (term * term) list -> term -> term
val simplifySubst : facts -> (term * term) list -> term -> term option
val simplifysubstmap :
  facts -> term -> (term * term) list -> (term * term) list option
val substmapdom : (term * term) list -> term list
val substmapran : (term * term) list -> term list
val restrictsubstmap :
  facts -> (term * term) list -> term list -> term list ->
    (term * term) list option
val plussubstmap :
  facts -> (term * term) list -> (term * term) list ->
    (term * term) list option
(* some useful survivors *)
val vtmetacount : (term * term) list -> int
val vtminus :
  facts -> (term * term) list -> (term * term) list -> (term * term) list
val vtsplit :
  facts -> (term * term) list -> term list ->
    (term * term) list * (term * term) list * (term * term) list
