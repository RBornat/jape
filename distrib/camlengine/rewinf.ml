(* $Id$ *)

open Stringfuns
open Term.Termstring
open Listfuns
open Optionfuns
open Term.Funs

   (* see rewrite.sml for an explanation of this data structure *)
type rewinf = Rewinf of (term list * vid list * int list * int option)
let nullrewinf = Rewinf ([], [], [], None)
let mkrewinf v = Rewinf v
let rec rew2rawinf = fun (Rewinf r) -> r
let rec rewinf_vars = fun (Rewinf (vars, _, _, _)) -> vars
let rec rewinf_uVIDs = fun (Rewinf (_, uVIDs, _, _)) -> uVIDs
let rec rewinf_badres = fun (Rewinf (_, _, badres, _)) -> badres
let rec rewinf_psig = fun (Rewinf (_, _, _, psig)) -> psig
let rec rewinf_setvars =
  fun (Rewinf (_, uVIDs, badres, psig)) vars ->
    Rewinf (vars, uVIDs, badres, psig)
let rec rewinf_setuVIDs =
  fun (Rewinf (vars, _, badres, psig)) uVIDs ->
    Rewinf (vars, uVIDs, badres, psig)
let rec rewinf_setbadres =
  fun (Rewinf (vars, uVIDs, _, psig)) badres ->
    Rewinf (vars, uVIDs, badres, psig)
let rec rewinf_setpsig =
  fun (Rewinf (vars, uVIDs, badres, _)) psig ->
    Rewinf (vars, uVIDs, badres, psig)
let rec rewinf_addvars =
  fun (Rewinf (vars, uVIDs, badres, psig)) vars' ->
    Rewinf (vars' @ vars, uVIDs, badres, psig)
let rec rewinf_adduVIDs =
  fun (Rewinf (vars, uVIDs, badres, psig)) uVIDs' ->
    Rewinf (vars, uVIDs' @ uVIDs, badres, psig)
let rec rewinf_addbadres =
  fun (Rewinf (vars, uVIDs, badres, psig)) badres' ->
    Rewinf (vars, uVIDs, badres' @ badres, psig)
let rec rewinfstring =
  fun (Rewinf r) ->
    "Rewinf" ^
      quadruplestring termliststring (bracketedliststring string_of_vid ",")
        (bracketedliststring string_of_int ",") (optionstring string_of_int) ","
        r
let rec rewinf_merge =
  fun
    (Rewinf (allvars, uVIDs, badres, psig),
     Rewinf (allvars', uVIDs', badres', psig')) ->
    Rewinf
      (mergevars allvars allvars', mergeVIDs uVIDs uVIDs',
       sortedmerge (<) badres badres',
       (match psig, psig' with
          Some n, Some n' -> Some (min (n) (n'))
        | Some _, None -> psig
        | _ -> psig'))
