(* $Id$ *)

open Mappingfuns
open Term.Type

val matchdebug : bool ref

(* first arg is matchbra: whether to match brackets exactly (true) 
   or debracket before matching (false)
 *)
val matchterm :
  bool -> term -> term -> (term, term) mapping list ->
    (term, term) mapping list
val matchtermvars :
  bool -> (term -> bool) -> term -> term -> (term, term) mapping list ->
    (term, term) mapping list
val match__ :
  bool -> term -> term -> (term, term) mapping ->
    (term, term) mapping option
val matchvars :
  bool -> (term -> bool) -> term -> term -> (term, term) mapping ->
    (term, term) mapping option

(* remapping doesn't have that problem, I think ... *)
val remapterm : (term, term) mapping -> term -> term
val option_remapterm : (term, term) mapping -> term -> term option
val simplepat : term -> term

(* straight into the vein *)
type matchresult =
  Certain of (term, term) mapping | Uncertain of (term, term) mapping
val match3term :
  bool -> term -> term -> matchresult list -> matchresult list
val match3termvars :
  bool -> (term -> bool) -> term -> term -> matchresult list ->
    matchresult list
val match3 : bool -> term -> term -> matchresult -> matchresult option
val match3vars :
  bool -> (term -> bool) -> term -> term -> matchresult ->
    matchresult option
