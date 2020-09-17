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

open Answer
open Cxttype
open Cxtexterior
open Cxtfuns
open Facts
open Idclass
open Listfuns
open Mappingfuns
open Match
open Miscellaneous
open Optionfuns
open Provisotype (* ok, of course. RB *)
open Proviso
open Rewrite
open Sequent
open Sml
open Stringfuns
open Substmapfuns
open Termfuns
open Termstore
open Termstring
open Termtype

let consolereport = Miscellaneous.consolereport

let baseseqsides cxt =
  match getexterior cxt with
  | Exterior((_,Seq(_,left,right)),_,fvi) -> 
      (left,right,
       (match fvi with Some{bhfvs=bhfvs;bcfvs=bcfvs} -> Some(bhfvs,bcfvs)
        | None                                       -> None
       )
      )
  | NoExterior -> raise (Catastrophe_ ["baseseqsides"])
   

let vv = bracketed_string_of_list string_of_visproviso " AND "
(* just turn a proviso into a list of simpler provisos 
   (function designed for folding over proviso list) 
 *)
(* oh, and by the way, uncurried constructors and fold in SML are a PAIN. RB *)

let rec simplifyProviso facts (p, cats) =
  let parent = provisoparent p in
  let vis = provisovisible p in
  let rec new__ p' = mkparentedvisproviso parent (vis, p') in
  let rec np v (t, cats) =
    let rec addone p' ps = new__ p' :: ps in
    let rec def () = Some (addone (NotinProviso (v, t)) cats) in
    let rec fnp (t, cats) = foldterm (np v) cats t in
    match t with
    | Id _ -> def ()
    | Unknown _ -> def ()
    | Binding (_, (bs, ss, us), _, _) ->
        let rs = (substeqvarsq facts v <* bs) in
        if List.exists qDEF rs then Some (nj_fold fnp us cats)
        else if not (List.exists qUNSURE rs) then
          Some (nj_fold fnp ss (nj_fold fnp us cats))
        else def ()
    | Subst (_, r, _P, vts) ->
        let rs = (substeqvarsq facts v <* (fst <* vts)) in
        if List.exists qDEF rs then
          Some (nj_fold fnp ((snd <* vts)) cats)
        else if not (List.exists qUNSURE rs) then
          match vts with
          | [v', t'] ->
              (match varoccursinq facts v t' with
               | Yes -> Some (foldterm (np v') (fnp (_P, cats)) _P)
               | No -> Some (fnp (_P, cats))
               | Maybe -> def ()
              )
          | _ ->
              Some
                (nj_fold fnp
                   ((fun vt -> registerSubst (true, _P, [vt])) <* vts)
                   cats)
        else def ()
    | Collection (_, c, es) ->
        let rec npe (e, cats) =
          match e with
          | Segvar (_, _, v') ->
              addone (NotinProviso (v, registerCollection (c, [e]))) cats
          | Element (_, _, t) -> fnp (t, cats)
        in
        Some (nj_fold npe es cats)
    | _ -> None
  in
  let rec nop =
    fun (vs, pat, _C as n) ->
      let rec expand env (v, cats) =
        match (env <@> v) with
        | Some v' ->
            simplifyProviso facts (new__ (NotinProviso (v, v')), cats)
        | None -> raise (Catastrophe_ ["simplifyProviso nop"])
      in
      let rec simp t cats =
        match match3 false pat t (Certain empty) with
        | Some (Certain env) -> Some (nj_fold (expand env) vs cats)
        | Some (Uncertain env) -> None
        | None -> Some cats
      in
      match _C with
      | Collection (_, class__, es) ->
          let rec doel (e, (defers, cats)) =
            match e with
            | Element (_, _, t) ->
                (match simp t cats with
                 | Some cats' -> defers, cats'
                 | None -> e :: defers, cats
                )
            | _ -> e :: defers, cats
          in
          let (defers, cats') = nj_fold doel es ([], cats) in
          if null defers then cats'
          else
            new__
              (NotoneofProviso
                 (vs, pat, registerCollection (class__, defers))) ::
              cats'
      | t ->
          match simp t cats with
          | Some cats' -> cats'
          | _ -> new__ (NotoneofProviso n) :: cats
  in
  match provisoactual p with
  | NotinProviso (v, t) -> foldterm (np v) cats t
  | NotoneofProviso n -> nop n
  | _ -> p :: cats

let rec deferrable cxt (t1, t2) =
  simterms (t1, t2) &&
  (match t1, t2 with
   | Subst _, Subst _ -> true
   | Subst (_, _, (Unknown _ as _P1), vts), _ ->
       not (qDEF (varoccursinq cxt _P1 t2)) ||
       specialisesto (idclass _P1, VariableClass) &&
       List.exists (fun t1' -> deferrable cxt (t1', t2))
         ((snd <* vts))
   | _, Subst _ -> deferrable cxt (t2, t1)
   | _ -> true)

(* Is this proviso obviously satisfied (Yes) or obviously violated (No)
 * or can't we tell (Maybe)?
 * Make use of other, more basic, provisos as facts to help you decide, 
 * where appropriate - but be sure that you don't eliminate two mutually 
 * equivalent provisos (e.g. at this point don't use NOTINs to remove other NOTINs).
 *)

let rec _PROVISOq facts p =
  match p with
  | FreshProviso _      -> (* can occur in a derived rule *) Maybe
  | NotinProviso (v, t) -> notq (varoccursinq facts v t)
  | DistinctProviso vs -> 
      let rec dp = function []    -> Yes
                   |        v::vs -> andalsoq (notq (existsq (varoccursinq facts v) vs)) 
                                              (fun _ -> dp vs)
      in dp vs              
  | NotoneofProviso _   -> Maybe
  | UnifiesProviso (_P1, _P2) ->
      if eqterms (_P1, _P2) then Yes
      else if not (deferrable facts (_P1, _P2)) then No
      else(* if termoccursin (debracket _P2) _P1 
         orelse termoccursin (debracket _P1) _P2 then No
      else *)  Maybe
  | SingleDischargeProviso rts -> Maybe
  
let rec expandFreshProviso b (h, g, r, v) left right ps =
  nj_fold
    (function
     | (true, side), ps -> mkvisproviso (b, NotinProviso (v, side)) :: ps
     | (false, side), ps -> ps)
    [h, left; g, right] ps

exception Verifyproviso of proviso

(* take out the first occurrence *)

let rec (--) xs ys =
  match (xs, ys) with
  | x :: xs, y -> if x = y then xs else x :: (xs -- y)
  | []     , y -> []

(* We find out which provisos from the set ps are independent of the set qs.
 * If op-- is the function above, p is deleted from the set qs before we start,
 * but there are other things we might want to do ... it's called with mm below
 *)

let rec checker cxt (--) ps qs =
  let rec ch a1 a2 =
    if !provisodebug then
      consolereport ["Provisofuns.checker.ch"; 
                     " "; bracketed_string_of_list string_of_visproviso  " AND " a1; 
                     " "; bracketed_string_of_list string_of_visproviso  " AND " a2];
    match a1, a2 with
    | [], qs -> []
    | p :: ps, qs ->
        let pp = provisoactual p in
        let qs' = (qs -- p) in (* check p against the _other_ provisos, 
                                  when (--) is (--). Ok, but why would you _not_ do this?.
                                *)
        if List.exists (fun q' -> pp = provisoactual q') qs' 
        then ch ps qs' (* discard p if there is a proviso in qs' which differs only in visibility *)
        else
          let verdict = _PROVISOq (facts qs' cxt) pp in
          if !provisodebug then
            consolereport ["Provisofuns.checker "; Cxtstring.string_of_cxt cxt;
                           " "; string_of_visproviso p; 
                           " "; bracketed_string_of_list string_of_visproviso  " AND " qs'; 
                           " => "; string_of_answer verdict];
          match verdict with
          | Yes   -> ch ps qs'
          | No    -> raise (Verifyproviso (provisoparent p))
          | Maybe -> p :: ch ps qs (* qs not qs', cos qs is the database you are verifying against *)
  in
  ch ps qs

(* remdups deals with consecutive items only. This does everything, and is parameterised too.
   It discards things left-to-right -- i.e. it keeps the rightmost instance
 *)

let rec remalldups eq ps =
  let rec rad ps =
    match ps with
    | []      -> None
    | p :: ps -> if List.exists (fun p' -> eq p p') ps then Some(anyway rad ps)
                 else rad ps &~~ (fun ps' -> Some(p::ps'))
  in
    anyway rad ps
    
(* verifyprovisos appears to give back a minimal list of provisos that aren't trivially true
   or dependent on each other. If it finds a violated proviso it raises Verifyproviso.
   RB 2012
 *)
 
(* The correct interpretation of a FRESH proviso is given by expandFreshProviso above.  
 * But elsewhere -- see exterioreqvarsq -- I have taken FRESH to mean: doesn't appear 
 * free in the base sequent.  That second interpretation applies when proving theorems:
 * it's over-enthusiastic (too restrictive), but mixing them won't do: it causes us to
 * often have to add extra, strictly necessary, provisos to bolster FRESH.  Using the 
 * restrictive version includes those extra provisos all the time. 
 *
 * So now I use the restrictive version in verifycxtprovisos.
 * RB 20/i/00
 *)

let rec verifycxtprovisos cxt =
  try
    let cxt = rewritecxt cxt in
    let (left, right, fvopt) = baseseqsides cxt in
    let rec efp (h, g, r, v as f) =
      match fvopt with
      | Some (bhfvs, bcfvs) ->
          let notins a1 a2 a3 =
            match a1, a2, a3 with
            | true, fvs, ps ->
                nj_fold
                  (fun (fv, ps) ->
                     mkvisproviso (true, NotinProviso (v, fv)) :: ps)
                  fvs ps
            | false, _, ps -> ps
          in
          notins h bhfvs (notins g bcfvs [])
      | None -> expandFreshProviso true f left right []
    in
    (* the checker function above is wierd.  But it checks the list of provisos
     * left-to-right, eliminating the ones that are a consequence of what is left,
     * so we want the _visible_ provisos first
     *)
    let (vis, invis) = split provisovisible (provisos cxt) in
    (* FreshProvisos are never invisible, I hope, so I just look at vis *)
    let (fresh, unfresh) =
      nj_fold
        (fun (p, (fs, us)) ->
           match provisoactual p with
           | FreshProviso f -> let ns = efp f in (f, ns) :: fs, ns @ us
           | _              -> fs, p :: us)
        vis ([], [])
    in
    let pros =
      let ps = unfresh @ invis in
      let simpleps = remalldups (fun p p' -> provisoactual p = provisoactual p') 
                                (nj_fold (simplifyProviso (facts ps cxt)) ps []) in
      let _ =
        if !provisodebug then consolereport ["simpleps = "; vv simpleps]
      in
      (* I think this is right: no need to delete provisos from cxt, because
         facts are derived from second argument
       *)
      checker cxt (--) simpleps simpleps
    in
    (* FreshProviso now interacts with other provisos in a number of ways.
     * If we have FRESH/IMPFRESH z, but z doesn't appear in the base sequent or the givens,
     * then we can replace it by just the provisos which derive from it.
     * If after that we still have FRESH z, then we can strip out all the provisos which 
     * derive from it.
     * If we still have IMPFRESH z, and we have all the provisos which derive from
     * it, then we can strip them out and use FRESH z.
     * If we already have a FRESH/IMPFRESH proviso then we can augment it to allow this one
     *)
    let ps =
      let rec dofresh (((h, g, r, v as f), ns), pros) =
        let news = nj_fold (simplifyProviso (facts (pros @ ns) cxt)) ns [] in
        let rec mm xs y = xs in
        let rec def () = checker cxt mm pros news in
        let rec push f ps =
          if List.exists
               ((function | FreshProviso (_, _, _, v') -> v = v' | _ -> false) <.> provisoactual)
               ps
          then
            (fun vp ->
               match provisoactual vp with
               | FreshProviso (h', g', r', v') ->
                   if v = v' then
                     mkvisproviso (true, FreshProviso (h || h', g || g', r && r', v))
                   else vp
               | _ -> vp) 
            <* ps
          else mkvisproviso (true, FreshProviso f) :: ps
        in
        if knownproofvar (facts pros cxt) v then def () (* no need for it at all *)
        else 
        if r then
          (* IMPFRESH - check if news are needed *)
          let news' = try checker cxt mm news pros 
                      with Verifyproviso _ -> news
          in
          if null news' then
            (* we have the lot: just say FRESH *)
            push (h, g, false, v) (def ())
          else
            (* something missing: use IMPFRESH *)
            push f pros
        else (* FRESH *)
          (* take out copies of the new ones *)
          push f (def ()) 
      in
      nj_fold dofresh fresh pros
    in
    if !provisodebug then
      consolereport
        ["verifyprovisos "; vv (provisos cxt); " (pros = "; vv pros; ") ";
         " (vis = "; vv vis; ") "; " (invis = "; vv invis; ") ";
         " (fresh = ";
         bracketed_string_of_list
           (fun (f, ns) ->
              string_of_pair string_of_proviso vv "," (FreshProviso f, ns))
           "," fresh;
         ") "; " (unfresh = "; vv unfresh; ") "; " => "; vv ps];
    rewritecxt (withprovisos cxt ps)
  with
  | Verifyproviso p ->
      if !provisodebug then
        consolereport
          ["proviso "; string_of_proviso p; " failed in verifyprovisos"];
      raise (Verifyproviso p)

type prooftree = Prooftree.Tree.Fmttree.prooftree

let verifytreeprovisos prooftree cxt = cxt (* for now *)

let checkcxtprovisos cxt  =
  try Some (verifycxtprovisos cxt) with
  | Verifyproviso _ -> None

let checkprovisos prooftree cxt =
  try checkcxtprovisos cxt &~~ (_Some <.> verifytreeprovisos prooftree) with
  | Verifyproviso _ -> None

(* the drag and drop mapping is a list of (source,target) pairs, derived from
 * UnifiesProvisos thus:
 * if we have to unify two bag collections, either of which includes an unknown 
 * bag variable, then each such variable is a target and each of the formulae on 
 * the other side of the proviso is a source for that target.
 *)

(* and currently it is a dead duck.  RB 30/vii/01 *)
(* but now I'm reviving it. RB 17.iii.05 *)
let draganddropmapping ps =
  let istarget = function (Segvar(_,_,Unknown _)) -> true
                 |        _                       -> false
  in
  let getsts =
    function (UnifiesProviso(Collection(_,BagClass FormulaClass, xs),
                             Collection(_,BagClass FormulaClass, ys)
                            ),
              rs
             ) -> (xs,istarget<|ys)::(ys,istarget<|xs)::rs
    |        (_,rs) -> rs
  in 
  let sts = (not <.> null <.> snd) <| nj_fold getsts ps [] in
  (* We need to run Warshall's algorithm to get the transitive closure.
   * This implementation is slow, but these collections don't get large 
   *)
  let ists = (not <.> null <.> fst) <| ((fun (xs,ys) -> (istarget<|xs,ys)) <* sts) in
  let ists = nj_fold (fun ((ss,ts),rs) -> nj_fold (fun (s,rs) -> (s,ts)::rs) ss rs) ists [] in
  let sts = nj_fold (fun ((s,ts),sts) -> 
                        (fun (ss',ts') -> 
                            (ss',if List.exists (fun t' -> s=t') ts' then ts'@ts else ts')
                        ) <* sts
                    ) ists sts
  in 
    (* nj_fold (fun ((ss,ts),rs) -> (ss><ts)@rs) sts [] *) 
    sts

(* for export *)

let deferrable cxt = deferrable (facts (provisos cxt) cxt)
