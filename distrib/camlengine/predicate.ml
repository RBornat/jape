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

open Idclass 
open Termtype
open Termstore
open Termstring
open Termfuns
open Listfuns
open Miscellaneous
open Stringfuns
open Optionfuns
open Sml

let consolereport = Miscellaneous.consolereport

let interpretpredicates = ref false
let predicatedebug = ref false
exception Predicate_ of string list
(* aid for debuggers *)
let string_of_predicatebinding =
  bracketedstring_of_list
    (string_of_pair string_of_term
       (bracketedstring_of_list
          (string_of_pair string_of_termlist
             (bracketedstring_of_list string_of_termlist ",") ",")
          ",")
       ", ")
    "; "

(* check that a formula is an application which can be viewed as a predicate *)
(* FormulaClass stops operators becoming predicates when interpretpredicates is on ... *)
(* but we need PredicateClass ... *)
(* Experimenting with demanding round brackets in predicate applications ... *)
let rec matchpredicate all isabstraction t =
  match t with
    App (_, (Id (_, _, c) as pp), (Fixapp (_, ["("; ")"], _) as arg)) ->
      if !interpretpredicates && c = FormulaClass || isabstraction pp then
        Some
          (pp,
           (match debracket arg with
              Tup (_, ",", ts) -> ts
            | arg              -> [arg]))
      else None
  | Id (_, v, c) ->
      if all &&
         (!interpretpredicates && c = FormulaClass || isabstraction t)
      then
        Some (t, [])
      else None
  | _ -> None
(* given a mapping from names to lists of variables, translate predicates into substitutions *)
let rec compilepredicate isabstraction env t =
  match matchpredicate false isabstraction t with
    Some (pp, ts) ->
      let _ =
        if !predicatedebug then
          consolereport
            ["compilepredicate spotted "; string_of_term pp; "(";
             string_of_termlist ts; ")"]
      in
      let ts = (mapterm (compilepredicate isabstraction env) <* ts) in
      begin match env pp with
        Some vs ->
          let res =
            Some
              (if vs = ts then pp
               else
                 try registerSubst (true, pp, (vs ||| ts)) with
                   Zip_ ->
                     raise (Catastrophe_ ["Zip_ in compilepredicates"]))
          in
          if !predicatedebug then
            consolereport
              ["compilepredicate "; string_of_term pp; "("; string_of_termlist ts;
               ") => "; string_of_option string_of_term res];
          res
      | None -> raise (Catastrophe_ ["bad env in compilepredicates"])
      end
  | None -> None

(* find predicates, record their arguments and the bindings which enclose them.
   Produces a list of predicates, each paired with a list of arguments, each
   paired with a list of binding contexts in which that predicate/argument pair
   occurs. Does an arity check. Designed to be foldtermed.
 *)
(* this ought to use a PredicateClass notion -- or maybe that should be in the 
   isabstraction argument. At present any application is taken as a predicate
   application -- at any rate if it uses round brackets (which covers a numbe
   of sins ...)
 *)
let rec findpredicates isabstraction bs (t, pbs) =
  let rec addbinding newbs bs =
    sortedmerge earliervar (sort earliervar newbs) bs
  in
  let rec insertinlist eq f k kvs =
    let rec g =
      function
        [] -> [k, f []]
      | (k', v') :: kvs ->
          if eq (k, k') then (k, f v') :: kvs else (k', v') :: g kvs
    in
    g kvs
  in
  let rec inserttbs =
    fun pp ((ts : term list), bs) ->
      insertinlist
        (fun (ts, ts') ->
           if List.length ts <> List.length ts' then
             raise
               (Predicate_
                  ["predicate "; string_of_term pp;
                   " is used inconsistently: "; "sometimes with ";
                   string_of_int (List.length ts); ", sometimes with ";
                   string_of_int (List.length ts'); " arguments."])
           else ts = ts')
        (fun bs' -> sortedmerge (earlierlist earliervar) bs bs') ts
  in
  let rec insertP =
    fun ((pp : term), tbs) ->
      insertinlist (fun (x, y) -> x = y)
        (fun tbss -> inserttbs pp tbs tbss) pp
  in
  let fp = findpredicates isabstraction in
  match t with
    Binding (_, (newbs, ss, us), _, _) ->
      Some
        (nj_fold (nj_foldterm (fp (addbinding newbs bs))) ss
           (nj_fold (nj_foldterm (fp bs)) us pbs))
  | Subst (_, r, pp, vts) ->
      Some
        (foldterm (fp (addbinding ((fst <* vts)) bs))
           (nj_fold (nj_foldterm (fp bs)) ((snd <* vts)) pbs) pp)
  | _ ->
      match matchpredicate true isabstraction t with
        Some (pp, ts) ->
          Some
            (insertP (pp, (ts, [bs])) (nj_fold (nj_foldterm (fp bs)) ts pbs))
      | _ -> None

(* discard zero-arity 'predicates' -- only necessary for arity check *)
let rec discardzeroarities pbs =
  (fun (_, (abss : (term list * 'a) list)) -> List.exists (not <.> null <.> fst) abss) <| pbs

(* To make a mapping from predicates to default args we prefer binding variables, if
 * suitable examples can be found.
 *)
let rec findpredicatevars abss =
  findfirst
    (fun (ts, bss) ->
       if List.exists (fun bs -> eqbags eqterms (ts, bs)) bss then Some ts
       else None)
    abss
