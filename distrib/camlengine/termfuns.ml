(*
	$Id$

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

open Termtype
open Termstore
open Termstring

open Miscellaneous
open Stringfuns
open Optionfuns
open Listfuns
open Mappingfuns
open Optionfuns
open Symboltype
open Symbol
open Idclass
open Idclassfuns
open Answer
open Sml

let bracketed = Termtype.bracketed
let debracket = Termtype.debracket
let resnum2int = Termtype.resnum2int

(* --------------------- hack to help binding matches go faster --------------------- *)

let rec termkind t =
  match t with
    Id _ -> 0
  | Unknown _ -> 1
  | App _ -> 2
  | Tup _ -> 3
  | Literal _ -> 4
  | Fixapp _ -> 5
  | Subst _ -> 6
  | Binding _ -> 7
  | Collection _ -> 8

let termkindmax = 8

(**** (possibly temporary) additions to ease transition to Collection use ****)

let rec elementnumbers =
  function
    Collection (_, _, es) ->
      let rec f =
        function
          Element (_, r, _) -> Some r
        | _ -> None
      in
      optionfilter f es
  | _ -> []
let rec elementnumbered a1 a2 =
  match a1, a2 with
    Collection (_, _, es), r ->
      findfirst
        (function
           Element (_, r', t) -> if r = r' then Some t else None
         | _ -> None)
        es
  | _, _ -> None
let rec isProperResnum =
  function
    Resnum _ -> true
  | _ -> false
let rec collectionkind =
  function
    Collection (_, c, _) -> Some c
  | _ -> None
(* ------------------------------ generic functions on terms and elements ------------------------------ *)

(* These functions mop up after you have picked out the things of
* interest with a special argument function (Some for caught, None for ignored).  
* They go right through bindings and substitutions - unless the argument function catches that.
* In fact they will even rewrite bindings, so watch out!
*)
let rec option_mapterm f t =
  (f t |~~
     (fun _ ->
        let mtff = option_mapterm f in
        let mtfl = option_rewritelist mtff in
        match t with
          Id _ -> None
        | Unknown _ -> None
        | App (_, f, a) ->
              (option_rewrite2 mtff mtff (f, a) &~~
               (fSome <.> registerApp))
        | Tup (_, s, ts) ->
            (mtfl ts &~~ (fun ts' -> Some (registerTup (s, ts'))))
        | Literal _ -> None
        | Fixapp (_, ss, ts) ->
            (mtfl ts &~~ (fun ts' -> Some (registerFixapp (ss, ts'))))
        | Subst (_, r, p_, vts) ->
              (option_rewrite2 mtff
                 (option_rewritelist (option_rewrite2 mtff mtff))
                 (p_, vts) &~~
               (fun (p_', vts') -> Some (registerSubst (r, p_', vts'))))
        | Binding (_, bs_ss_us, env, pat) ->
            (option_rewrite3 mtfl mtfl mtfl bs_ss_us &~~
               (fun bs_ss_us' ->
                  Some (registerBinding (bs_ss_us', env, pat))))
        | Collection (_, k, es) ->
            (option_mapelements f es &~~
               (fun es' -> Some (registerCollection (k, es'))))))
and option_mapelement a1 a2 =
  match a1, a2 with
    f, Segvar (_, ps, v) ->
      (option_mapterm f v &~~ (fun v' -> Some (registerSegvar (ps, v'))))
  | f, Element (_, r, t) ->
      (option_mapterm f t &~~ (fun t' -> Some (registerElement (r, t'))))
(* yes, it should really be r *)

and option_mapelements f = option_rewritelist (option_mapelement f)
let rec mapterm f = anyway (option_mapterm f)
let rec mapelements f = anyway (option_mapelements f)
let rec foldterm f z t =
  match f (t, z) with
    Some v -> v
  | None ->
      let rec ff z ts = nj_fold (nj_foldterm f) ts z in
      match t with
        App (_, f, a) -> ff z [f; a]
      | Tup (_, _, ts) -> ff z ts
      | Fixapp (_, _, ts) -> ff z ts
      | Subst (_, _, p_, vts) ->
          ff (ff z ((snd <* vts)))
             (p_ :: (fst <* vts))
      | Binding (_, (bs, ss, us), _, _) -> ff (ff (ff z us) ss) bs
      | Collection (_, k, es) -> foldelements f z es
      | _ -> z
and nj_foldterm f (t, z) = foldterm f z t
and foldelements f z es =
  let rec fe z e =
    match e with
      Element (_, _, t) -> foldterm f z t
    | Segvar (_, ms, v) -> nj_fold (nj_foldterm f) (v :: ms) z
  in
  (* not sure that's right ... *)
  nj_fold (fun (e, z) -> fe z e) es z
let rec findterm g t =
  match g t with
    None ->
      let fx = findfirst (findterm g) in
      begin match t with
        App (_, f, a) -> fx [f; a]
      | Tup (_, s, ts) -> fx ts
      | Fixapp (_, ss, ts) -> fx ts
      | Binding (_, (bs, ss, us), _, _) ->
          ((fx bs |~~ (fun _ -> fx ss)) |~~ (fun _ -> fx us))
      | Subst (_, r, p_, vts) ->
          (findterm g p_ |~~
             (fun _ -> findfirst (fun (v, t) -> fx [v; t]) vts))
      | Collection (_, k, es) ->
          let rec fe =
            function
              Element (_, _, t) -> findterm g t
            | Segvar (_, ms, v) -> findfirst (findterm g) (v :: ms)
          in
          (* not sure that's right *)
          findfirst fe es
      | _ -> None
      end
  | v -> v
(* what on earth does findhole do?  Some very clever person must have written this ... *)
let rec findhole g t =
  let rec fh h t =
    let rec fhs sel build h xs =
      let rec fhx a1 a2 =
        match a1, a2 with
          h, [] -> None
        | h, x :: xs ->
            (fh (h <.> (fun t -> build x t :: xs)) (sel x) |~~
               (fun _ -> fhx (h <.> (fun xs -> x :: xs)) xs))
      in
      fhx h xs
    in
    let rec selt t = t in
    let rec buildt _ t = t in
    match g h t with
      None ->
        begin match t with
          App (_, f, a) ->
              (fh (h <.> (fun f -> registerApp (f, a))) f |~~
               (fun _ ->
                  fh (h <.> (fun a -> registerApp (f, a)))
                     a))
        | Tup (_, s, ts) ->
            fhs selt buildt
              (h <.> (fun ts -> registerTup (s, ts))) ts
        | Fixapp (_, ss, ts) ->
            fhs selt buildt
              (h <.> (fun ts -> registerFixapp (ss, ts))) ts
        | Binding (_, (bs, ss, us), env, pat) ->
              (fhs selt buildt
                 (
                    h <.> (fun ss ->
                          registerBinding ((bs, ss, us), env, pat))
                        )
                 ss |~~
               (fun _ ->
                  fhs selt buildt
                    (
                       h <.> (fun us ->
                             registerBinding ((bs, ss, us), env, pat))
                           )
                    us))
        | Subst (_, r, p_, vts) ->
              (fh (
                     h <.> (fun p_ -> registerSubst (r, p_, vts)))
                  p_ |~~
               (fun _ ->
                  fhs (fun (v, t) -> t) (fun (v, _) t -> v, t)
                    (
                       h <.> (fun vts -> registerSubst (r, p_, vts)))
                    vts))
        | Collection (_, k, es) ->
            let rec sele =
              function
                Element (_, _, t) -> t
              | Segvar (_, ms, v) -> v
            in
            (* not really satisfactory *)
            let rec builde a1 a2 =
              match a1, a2 with
                Element (_, r, _), t -> registerElement (r, t)
              | Segvar (_, ps, _), v -> registerSegvar (ps, v)
            in
            fhs sele builde
              (h <.> (fun es -> registerCollection (k, es)))
              es
        | _ -> None
        end
    | res -> res
  in
  fh (fun t -> t) t
let rec searchterm g z t =
  match findterm g t with
    Some v -> v
  | None -> z
let rec existsterm g t =
  opt2bool (findterm (fun t -> if g t then Some true else None) t)
(* at present we have a very specialised type hierarchy *)
(* c1 specialisesto c2 if something of class c1 can, by process of unification
 * and/or instantiation, be made into a thing of class c1.  
 * If you like, it tests whether a c2-thing is a kind of c1-thing.
 *)

let rec specialisesto (c1, c2) =
  c1 = c2 ||
  (match c1, c2 with
     FormulaClass, VariableClass -> true
   | FormulaClass, ConstantClass -> true
   | FormulaClass, NumberClass -> true
   | ConstantClass, NumberClass -> true
   | FormulaClass, StringClass -> true
   | ConstantClass, StringClass -> true
   | FormulaClass, OperatorClass -> true
   | ConstantClass, OperatorClass -> true
   | BagClass c1, BagClass c2 ->(* the lines above are essential at present, to allow (=), for example, to 
      * be unified with a FormulaClass unknown.  But I'm vaguely unhappy.  I 
      * think we need to be a bit more careful with connectives, and I guess I 
      * think that (=) should be ConstantClass or something, while (logand) 
      * can be a connective, and not at all a function.
      * RB May 95
      *)
      specialisesto (c1, c2)
   | ListClass c1, ListClass c2 -> specialisesto (c1, c2)
   | SubstClass, _ ->(* I think we need these too ... *)
      specialisesto (FormulaClass, c2)
   | _, SubstClass -> specialisesto (c1, FormulaClass)
   | _ -> false)
(* specialises to is not adequate to catch canoccurfreein, because of Collection
 * formulae.  In future it will be even worse, when we have typed formulae ...
 *)

let rec canoccurfreein (c1, c2) =
  specialisesto (c2, c1) ||
  (* this is wrong, actually: all they need is a common ancestor *)
  (match c2 with
     BagClass c -> canoccurfreein (c1, c)
   | ListClass c -> canoccurfreein (c1, c)
   | _ -> false)
let rec idclass t =
  match debracket t with
    Id (_, _, c) -> c
  | Unknown (_, _, c) -> c
  | App _ -> FormulaClass
  | Tup _ -> FormulaClass
  | Literal (_, Number _) -> NumberClass
  | Literal (_, String _) -> StringClass
  | Fixapp _ -> FormulaClass
  | Subst (_, _, t, vts) ->
      begin match idclass t with
        VariableClass ->
          (* ohmygod *) 
          if _All
               (fun t -> idclass t = VariableClass)
               (List.map snd vts)
          then
            VariableClass
          else SubstClass
      | c ->
          match debracket t with
            Unknown _ ->
              if specialisesto (c, VariableClass) then SubstClass else c
          | _ -> c
      end
  | Binding _ -> FormulaClass
  | Collection (_, k, _) -> k
let rec isSubstClass t = idclass t = SubstClass
(* parse that, you bastards *)
let rec replaceelement a1 a2 a3 =
  match a1, a2, a3 with
    Collection (_, c, es), (Element (_, r, _) as el), t ->
      let newel = registerElement (r, t) in
      let rec rep =
        function
          (Element (_, r', _) as el) :: es ->
            if r = r' then newel :: es else el :: rep es
        | el :: es -> el :: rep es
        | [] ->
            raise
              (Catastrophe_
                 ["replaceelement: collection ";
                  bracketedliststring (smlelementstring termstring) ","
                    es;
                  " doesn't contain "; smlelementstring termstring el])
      in
      newel, registerCollection (c, rep es)
  | c, el, t ->
      raise
        (Catastrophe_
           ["replaceelement ("; smltermstring c; ") (";
            smlelementstring termstring el; ") ("; termstring t; ")"])

let rec uniqueVID class__ sortedVIDs extraVIDs vid =
  let str = string_of_vid vid in
  let rec stemNstern s =
    let s' = String.sub s 0 (String.length s - 1) in
    let n = String.sub s (String.length s - 1) 1 in
    if isdigit n && s' <> "" && isextensibleID s' &&
       symclass s' = class__
    then
      let (s'', n') = stemNstern s' in s'', n' ^ n
    else s, ""
  in
  let (stem_str, stern_str) = stemNstern str in
  let rec next_vid n = vid_of_string (stem_str ^ string_of_int (n + 1)) in
  let rec e_ vid n =
    if member (vid, extraVIDs) || symclass (string_of_vid vid) <> class__ then
      u_ (next_vid n) (n + 1) sortedVIDs
    else vid
  and u_ a1 a2 a3 =
    match a1, a2, a3 with
      vid, n, [] -> e_ vid n
    | vid, n, vid1 :: vids ->
        if vid < vid1 then e_ vid n
        else if vid = vid1 then
          let vid' = next_vid n in
          if vid1 < vid' then u_ vid' (n + 1) vids
          else u_ vid' (n + 1) sortedVIDs
        else u_ vid n vids
  in
  if isextensibleID str then
    if symclass str <> class__ then
      raise
        (Catastrophe_
           ["uniqueVID "; idclassstring class__; " ... "; str;
            " (which is "; idclassstring (symclass str); ")"])
    else u_ vid (if stern_str = "" then 0 else atoi stern_str) sortedVIDs
  else
    raise
      (Catastrophe_ ["uniqueVID "; idclassstring class__; " ... "; str])

let mergeVIDs = sortedmerge (<)

let rec earliervar t1 t2 =
  let rec ordinal =
    function
      Id _ -> 0
    | Unknown _ -> 1
    | _ -> 2
  in
  match debracket t1, debracket t2 with
    Id (_, v1, _), Id (_, v2, _) -> v1 < v2
  | Unknown (_, v1, _), Unknown (_, v2, _) -> v1 < v2
  | t1, t2 -> ordinal t1 < ordinal t2

let rec mergevars xs ys = sortedmerge earliervar xs ys

(* In order to be able to compare or unify two maps,
   we must have a canonical order (sigh!).
 *)

let rec canonicalsubstmap vts =
  let rec earliervt (v, _) (v', _) = earliervar v v' in
  sort earliervt vts
let rec isconstantID (_, _, c) =
  match c with
    ConstantClass -> true
  | NumberClass -> true
  | StringClass -> true
  | OperatorClass -> true
  | _ -> false
(* used (now only in proviso) to tell if we have a 'variable' or a 'constant' *)
let rec isconstant t =
  match debracket t with
    Id s -> isconstantID s
  | Unknown s -> isconstantID s
  | _ -> false
(* This is a replacement for uses of isconstant, isoperator and the like which
   attempted to tell the difference between 'made up' names like x, y, z and 
   'constants' like map, fold, (+).
   
   A 'meta variable' is an implicitly (meta-) quantified
   variable which occurs in a rule or theorem. We look for them
   in (at least) the following situations:
   
     when matching terms (for example in the UNFOLD/FOLD search)
     when doing LET bindings
     when (in EVALUATE) finding the fresh unknowns and object variables
     in a sequent (things which appear in the consequent but not in
     the hypothesis).
   
   An operator or an identifier which has been declared to be a constant
   can not be a meta variable.
   
   An identifier which has been declared to be of
   some identifier CLASS is a meta variable.
   
   The implementation of Symbol distinguishes
   between CONSTANT foo and CLASS CONSTANT foo only by
   making the former answer false to isextensibleID whereas
   the latter answers true. 
   
   BAS September 5th 1996 (slightly edited RB 10/ix/96).
 *)

let rec ismetav t =
  match debracket t with
    Id (_, v, c) -> isextensibleID (string_of_vid v)
  | Unknown _ -> true
  | _ -> false
let rec isextensibleId t =
  match debracket t with
    Id (_, v, c) -> isextensibleID (string_of_vid v)
  | _ -> false
let rec isId t =
  match debracket t with
    Id _ -> true
  | _ -> false
let rec isUnknown t =
  match debracket t with
    Unknown _ -> true
  | _ -> false
let rec isVariable t =
  match debracket t with
    Id (_, _, VariableClass) -> true
  | Unknown (_, _, VariableClass) -> true
  | _ -> false
let rec isleaf t =
  match t with
    Id _ -> true
  | Unknown _ -> true
  | Literal _ -> true
  | _ -> false
let rec isleafelement e =
  match e with
    Segvar (_, [], v) -> isleaf v
  | Element (_, _, t) -> isleaf t
  | _ -> false
let rec isidentifier t =
  match t with
    Id _ -> true
  | Unknown _ -> true
  | _ -> false
let rec issegvar e =
  match e with
    Segvar _ -> true
  | _ -> false
let rec isCollection =
  function
    Collection _ -> true
  | _ -> false
let rec isemptycollection =
  function
    Collection (_, _, []) -> true
  | _ -> false
let rec isselectionSubst e =
  match e with
    Element (_, _, Subst (_, false, _, _)) -> true
  | _ -> false
let rec emptycollection k = registerCollection (k, [])
let rec element2term =
  function
    Element (_, _, t) -> Some t
  | _ -> None
let rec collection2term =
  function
    Collection (_, _, [el]) -> element2term el
  | _ -> None
(* This function would be simple equality, were it not for the debracketing.
 * Now perhaps the right thing would be a stripbracket function ... but no.
 * It might be more economically expressed if it used |||, but this way 
 * (perhaps) is faster.
 *)

let rec eqterms (t1, t2) =
  let rec fEQs =
    function
      t1 :: t1s, t2 :: t2s -> eqterms (t1, t2) && fEQs (t1s, t2s)
    | [], [] -> true
    | _ -> false
  in
  match debracket t1, debracket t2 with
    Id (_, s1, c1), Id (_, s2, c2) ->(* first the ones which alter the interpretation *)
     s1 = s2 && c1 = c2
  | Unknown (_, s1, c1), Unknown (_, s2, c2) -> s1 = s2 && c1 = c2
  | App (_, f1, a1), App (_, f2, a2) ->
      eqterms (f1, f2) && eqterms (a1, a2)
  | Literal (_, k1), Literal (_, k2) -> k1 = k2
  | Tup (_, s1, t1s), Tup (_, s2, t2s) -> s1 = s2 && fEQs (t1s, t2s)
  | Fixapp (_, ss1, t1s), Fixapp (_, ss2, t2s) ->
      ss1 = ss2 && fEQs (t1s, t2s)
  | Subst (_, _, p1, vts1), Subst (_, _, p2, vts2) ->
      let vts1 = canonicalsubstmap vts1 in
      let vts2 = canonicalsubstmap vts2 in
      let rec fEQvts =
        function
          (v1, t1) :: vts1, (v2, t2) :: vts2 ->
            (eqterms (v1, v2) && eqterms (t1, t2)) && fEQvts (vts1, vts2)
        | [], [] -> true
        | _ -> false
      in
      eqterms (p1, p2) && fEQvts (vts1, vts2)
  | Binding (_, (bs, ss, us), _, pat),
    Binding (_, (bs', ss', us'), _, pat') ->
      ((pat = pat' && fEQs (sort earliervar bs, sort earliervar bs')) &&
       fEQs (ss, ss')) &&
      fEQs (us, us')
  | Collection (_, BagClass k1, es1), Collection (_, BagClass k2, es2) ->
      k1 = k2 && eqbags (eqelements eqterms) (es1, es2)
  | Collection (_, ListClass k1, es1),
    Collection (_, ListClass k2, es2) ->
      k1 = k2 && eqlists (eqelements eqterms) (es1, es2)
  | _ -> false
and eqelements eq (e1, e2) =
  match e1, e2 with
    Segvar (_, p1s, v1), Segvar (_, p2s, v2) ->
      eqlists eqterms (p1s, p2s) && eq (v1, v2)
  | Element (_, r1, t1), Element (_, r2, t2) -> eq (t1, t2)
  | _ ->(* ignore resource numbers *)
     false
let rec sameresource (e1, e2) =
  match e1, e2 with
    Element (_, r1, _), Element (_, r2, _) -> r1 = r2
  | _ -> e1 = e2
(* we don't analyse Segvars *)
  
let rec earlierresource e1 e2 =
  match e1, e2 with
    Element (_, r1, _), Element (_, r2, _) ->
      begin match r1, r2 with
        Resnum i1, Resnum i2 -> i1 < i2
      | ResUnknown i1, ResUnknown i2 -> i1 < i2
      | Resnum _, Nonum -> true
      | Resnum _, ResUnknown _ -> true
      | ResUnknown _, Nonum -> true
      | _ -> false
      end
  | _ -> false
(* who cares ? *)
(* this is an alpha-conversion-capable version of eqterms.
   tbs is a binding (a mapping from vars to numbers).
   Because substitutions are a kind of binding, an earlier version tried
   to simplify them on the fly: it wasn't a success.  This version will
   work provided that all the substitutions with which it is presented are 
   maximally reduced.
   
   It occurs to me that we could, now this function only considers 
   alpha-conversion, modify it so that it is independent of the order in
   which variables are declared in a binding.  But one step at a time ...
 *)
let eqalphadebug = ref false
let rec eqalphaterms (t1, t2) =
  let count = ref 0 in
  let rec nxb _ = incr count; !count in
  (* infix at confuses OCaml *)
  let rec (<@>) tb v =
    match tb with
      (v', n) :: tb -> if v = v' then Some n else (tb <@> v)
    | [] -> None
  in
  let rec eq t1bs t2bs (t1, t2) =
    let fEQ = eq t1bs t2bs in
    let rec fEQs =
      function
        t1 :: t1s, t2 :: t2s -> fEQ (t1, t2) && fEQs (t1s, t2s)
      | [], [] -> true
      | _ -> false
    in
    let rec doublev () =
      match (t1bs <@> t1), (t2bs <@> t2) with
        Some n1, Some n2 -> n1 = n2
      | None, None -> t1 = t2
      | _ -> false
    in
    match debracket t1, debracket t2 with
      Id _, _ ->(* first the ones which alter the interpretation *)
       doublev ()
    | _, Id _ -> doublev ()
    | Unknown _, _ -> doublev ()
    | _, Unknown _ -> doublev ()
    | App (_, f1, a1), App (_, f2, a2) -> fEQ (f1, f2) && fEQ (a1, a2)
    | Literal (_, k1), Literal (_, k2) -> k1 = k2
    | Tup (_, s1, t1s), Tup (_, s2, t2s) -> s1 = s2 && fEQs (t1s, t2s)
    | Fixapp (_, ss1, t1s), Fixapp (_, ss2, t2s) ->
        ss1 = ss2 && fEQs (t1s, t2s)
    | Subst (_, _, p1, vts1), Subst (_, _, p2, vts2) ->
        let vts1 = canonicalsubstmap vts1 in
        let vts2 = canonicalsubstmap vts2 in
        let rec fEQvts =
          function
            (v1, t1) :: vts1, (v2, t2) :: vts2 ->
              (fEQ (v1, v2) && fEQ (t1, t2)) && fEQvts (vts1, vts2)
          | [], [] -> true
          | _ -> false
        in
        fEQ (p1, p2) && fEQvts (vts1, vts2)
    | Binding (_, (bs, ss, us), _, pat),
      Binding (_, (bs', ss', us'), _, pat') ->
        let ns = (nxb <* bs) in
        begin try
          (pat = pat' && fEQs (us, us')) &&
          _All
            (eq ((bs  ||| ns) @ t1bs) ((bs' ||| ns) @ t2bs))
            (ss ||| ss')
        with
          Zip_ -> false
        end
    | Collection (_, BagClass k1, es1),
      Collection (_, BagClass k2, es2) ->
        (* handling may not be necessary for the moment ...*)
        k1 = k2 && eqbags (eqelements fEQ) (es1, es2)
    | Collection (_, ListClass k1, es1),
      Collection (_, ListClass k2, es2) ->
        k1 = k2 && eqlists (eqelements fEQ) (es1, es2)
    | _ -> false
  in
  let r = t1 = t2 || eq [] [] (t1, t2) in(* if !eqalphadebug then
      consolereport["eqalphaterms (", termstring t1, ",",
                    termstring t2, ") => ", string_of_int r
                   ]
     else ();
   *)
   r
(* we are only interested in a definite answer here, in order to turn a Maybe 
 * into a Yes or a No.
 * This function doesn't use binding structure: it is looking for an occurrence 
 * of a sub-tree, for use in unification and equality testing.  It tells you 
 * whether in *every* unification/instantiation, t occurs in P.
 * RB 20/i/93
 *)
let rec termoccursin t = fun p_ -> existsterm (curry2 eqterms t) p_
(* termvars takes no note of bindings, and makes no interpretation of maps. 
 * No longer cat-eliminated, produces a sorted list of names.
 *)

let tmerge = sortedmerge earliervar
let rec termvars t =
  sortunique earliervar
    (foldterm
       (function
          (Id _ as v), vs -> Some (v :: vs)
        | (Unknown _ as v), vs -> Some (v :: vs)
        | _ -> None)
       [] t)
(*-------------------------- stuff about bindings ------------------------------ *)

(* The functions which follow are intended to help in minimising the need for 
 * NOTIN provisos.  To begin with we consider what binds what in varbindings. 
 * RB 15/iv/96
 *)

let varbindingsdebug = ref false
let bcstring = bracketedliststring termliststring ","
let bcliststring = bracketedliststring bcstring ","
let varinfstring = pairstring termstring bcliststring ","
let varbindingsresstring = bracketedliststring varinfstring ","
let bvsorder = earlierlist earliervar
let bcorder = earlierlist bvsorder
let rec relorder (v1, _) (v2, _) = earliervar v1 v2
let rec combinebindings (x, bcs) (_, bcs') =
  x, sortedmerge bcorder bcs bcs'
let bmerge = sortedmergeandcombine relorder combinebindings
(* This computes a list of variables in t, each with various binding
 * contexts. A binding context is a list of lists of binders, so each
 * variable is paired with a list of lists of lists of binders.
 * RB 19/xi/97
 *)
let rec varbindings t =
  let rec doit outers inners t = foldterm (f outers inners) [] t
  and f outers inners (t, ps) =
    let binders =
      if null outers then if null inners then [] else [inners]
      else inners :: outers
    in
    let dothem = List.map (doit outers inners) in
    let rec v () =
      if canoccurfreein (VariableClass, idclass t) then
        Some (bmerge [t, [binders]] ps)
      else Some ps
    in
    let rec doit2 bs = doit binders (sort earliervar bs) in
    let r =
      match t with
        Id _ -> v ()
      | Unknown _ -> v ()
      | Subst (_, _, p_, vts) ->
          Some
            (nj_fold (uncurry2 bmerge)
               (doit2 ((fst <* vts)) p_ ::
                  dothem ((snd <* vts)))
               ps)
      | Binding (_, (bs, ss, us), _, _) ->
          Some
            (nj_fold (uncurry2 bmerge) ((doit2 bs <* ss))
               (nj_fold (uncurry2 bmerge) (dothem us) ps))
      | _ -> None
    in
    if !varbindingsdebug then
      begin match r with
        Some _ ->
          consolereport
            ["(varbindings) f "; termliststring inners; " ";
             bracketedliststring termliststring "," outers; " ";
             pairstring termstring varbindingsresstring "," (t, ps);
             " => "; optionstring varbindingsresstring r]
      | None -> ()
      end;
    r
  in
  doit [] [] t
let combinemappings (x, vs) (_, vs') = x, tmerge vs vs'
let mmerge = sortedmergeandcombine relorder combinemappings
let rec fmerge (fvs, m) (fvs', m') = tmerge fvs fvs', mmerge m m'
(* this function finds identifiers that occur free, those of which we have
 * 'lost control', and what binds what otherwise.
 * In the case of bindings which are distinct only because of the order of
 * binding (e.g. Ax.Ay.P and Ay.Ax.P) it is necessary to find which variable
 * 'dominates' which in each list, and to add binding pairs accordingly.
 * Phew.  RB 18/iv/96
 *)

 (* This thing needed a rewrite, because it was obscure and uncommented and didn't
  * work.  So it got one.  Here is what it does, and there are comments about how it
  * does it. (Please note that the treatment here was developed before the
  * introduction of 'hidden provisos' generated from the interpretation of binding
  * structures and predicate notation. That later treatment means that some of the
  * data gathered here is over-cautious, but that doesn't matter, because when we
  * have a proviso saying that two names are independent, we believe it no matter
  * what the data structure prepared here says.  However, steps 3 and 4 below are 
  * far too conservative when we are interpreting predicates, and lead to far too 
  * many binders being treated as 'bad'.  So we have added a parameter which 
  * details those NOTIN provisos which are in force.)
  * 
  * The data structure built here is added to the context by rewritecxt (see
  * rewrite.sml).  It's interpreted in exterioreqvarsq (see facts.sml).
  * 
  * We get (indirectly from varbindings) a list of names, each paired with a list of
  * binding contexts; each binding context is a list of lists of names.  Each element
  * of a binding context is a 'parallel binding' - what you get from something like
  * Ax,y.P.  So if you have Ax,y.Eu,v.P what is recorded for P is the binding context
  * [[u,v],[x,y]].
  * 
  * Now the question that we want to solve is this: are there names in the 'exterior'
  * of a theorem/derived rule -- that is, in the base sequent plus any givens --
  * which can be treated as independent, because if we get an instance of the theorem
  * or rule in which they happen to be the same, we can alpha-convert them apart?
  * 
  * We solve this question by constructing a data structure which tells us the names
  * that are not necessarily independent.  Unfortunately the question applies not
  * only to variables like x, y, z but also to formulas like P, Q, R.  That
  * complicates the issue: we need to know not only when x and z can be considered
  * distinct, but also when x can be considered not to occur free in P.  Luckily
  * these questions are closely related, but it does mean that you have to pay rather
  * close attention to some details.
  * 
  * Step 1. Clearly, names which appear free are not independendent. So we should
  *     accumulate names which appear free.  We can add to the free names those
  *     formula names which appear bound: if we have free z and also we have Ax.P,
  *     then we can't say that z doesn't appear free in P.
  * 
  * Step 2. If we have Ax.P, then clearly P may contain occurrences of x: we record a
  *     relation between binders like x and the names which they 'dominate' like P.
  * 
  * Step 3. If we have Ax.Ay.P(x,y) and also Ay.Ax.P(x,y) then there is a problem. We
  *     can envisage an instance of the theorem/rule in which x and y happen to be
  *     the same, but then we won't be able to alpha-convert them apart, because in
  *     one case the ys are captured and in the other the xs are captured.  So if
  *     there is one binding in which x dominates y and another in which y dominates
  *     x, both pairs are added to the mapping of step 2.
  * 
  * Step 4. If we have Ax.P and Ay.P then y can occur free (case 1) and so can x
  *     (case 2).  This doesn't apply, clearly, to Ax.z and Ay.z, so we restrict it
  *     to cases in which what is dominated isn't a variable.  Anyway, if we find it
  *     we add x and y to the list of 'free' variables from step 1.
  * 
  * The result is a list of 'free' variables -- if two names are in that list then
  * they can't be judged independent -- plus a 'dominates' relation -- if x dominates
  * P then x NOTIN P can't be ignored.  But if x and y are two variables which aren't
  * both in the 'free' variable list and neither dominates the other, then they can
  * be treated as independent and distinct.  And if x and P aren't both in the free
  * variable list and x doesn't dominate P, then x NOTIN P can be considered
  * satisfied.  And that's really the point: to allow simplification of substitutions
  * involving schematic but different names like x and y, and to eliminate provisos
  * ditto.
  * 
  * RB 20/xi/97
  *)

let rec freevarsfrombindings inf notins =
  (* we assume that inf is sorted by variable and that each bcs is sorted by bcorder *)
  let rec vvorder (a, b) (c, d) =
    earliervar a c || a = c && earliervar b d
  in
  let rec closed v bc = List.exists (fun bvs -> member (v, bvs)) bc in
  (* step 1 - find all free and semi-free names *)
  let rec freev (v, bcs) = List.exists (not <.> closed v) bcs in
  let freevs = (fst <* (freev <| inf)) in
  (* step 2 - find the dominates relation from all the bindings *)
  let rec domf (v, bcs) =
    let rec truncate bc =
      takewhile (fun bvs -> not (member (v, bvs))) bc
    in
    let bcs = (truncate <* bcs) in
    let dominators = nj_fold (fun (bc, ds) -> nj_fold (uncurry2 tmerge) bc ds) bcs [] in
    ((fun bv -> bv, [v]) <* dominators)
  in
  let domrel = nj_fold (uncurry2 mmerge) (domf <* inf) [] in
  (* step 3 - find all the pairs of names which occur in bindings
   * both ways round, add to domrel
   *)
  let allbindings =
    nj_fold (uncurry2 (sortedmerge bcorder)) ((snd <* inf)) []
  in
  let rec allbpairs bc =
    let vpss =
      List.map (fun (us, vs) -> sort vvorder (( >< ) us vs)) (allpairs bc)
    in
    List.filter (fun (x, y) -> x <> y) (nj_fold (uncurry2 (sortedmerge vvorder)) vpss [])
  in
  let vps =
    nj_fold (uncurry2 (sortedmerge vvorder)) (List.map allbpairs allbindings) []
  in
  let rvps = sort vvorder ((fun (x, y) -> y, x) <* vps) in
  let commonpairs =
       (fun (x, y) ->
          not (member ((x, y), notins) || member ((y, x), notins)))
        <| sortedsame vvorder vps rvps
  in
  let domrel =
    nj_fold (uncurry2 mmerge) ((fun (x, y) -> [x, [y]; y, [x]]) <* commonpairs)
      domrel
  in
  (* step 4 - find pairs of bindings which aren't the same, add their 
   * differences to domrel.
   *
   * This could be simplified by using VariableClass - we aren't interested in
   * bindings where the dominated is simply a variable - but the code here
   * will be more robust when the notion of idclass gets more intricate
   *)
  let rec badbindings (v, bcs) =
    let bcs = (not <.> closed v) <| bcs in
    let vclass = idclass v in
    let rec allbvs bc =
      let rec filterbv bv =
        let bvclass = idclass bv in
        bvclass <> vclass && canoccurfreein (bvclass, vclass)
      in
      nj_fold (uncurry2 tmerge) (((fun bvs -> filterbv <| bvs) <* bc)) []
    in
    let bvss = (allbvs <* bcs) in
    let rec escapers bvs =
      (fun bv -> not (member ((bv, v), notins))) <| bvs
    in
    let rec b2 ((bv1s, bv2s), bads) =
      nj_fold (uncurry2 tmerge)
        [escapers (sorteddiff earliervar bv1s bv2s);
         escapers (sorteddiff earliervar bv2s bv1s)]
        bads
    in
    nj_fold b2 (allpairs bvss) []
  in
  let freevs = nj_fold (uncurry2 tmerge) ((badbindings <* inf)) freevs in
  (* and there we have it *)
  let r = freevs, domrel in
  (* stuff to persuade ourselves we have it right :-) *)
  let showin = varinfstring in
  let showout =
    pairstring termliststring
      (mappingstring termstring termliststring <.> mkmap) ","
  in
  if !varbindingsdebug then
    consolereport
      ["freevarsfrom bindings "; bracketedliststring showin ", " inf; " ";
       bracketedliststring (pairstring termstring termstring ",") ","
         notins;
       " => "; showout r];
  r
let orderVIDs = sortunique (<)
(* should be identifiertoVID *)
let rec vid_of_var =
  function
    Id (_, v, _) -> v
  | Unknown (_, v, _) -> v
  | t -> raise (Catastrophe_ ["vid_of_var "; argstring t])
let rec termVIDs t = orderVIDs ((vid_of_var <* termvars t))
let rec conVIDs ts =
  nj_fold
    (function
       Id (_, v, c), vs -> v :: vs
     | _, vs -> vs)
    ts []
(* --------------------------------------------------------------------- *)

   (* could t1 be changed by unification/substitution so that it becomes the same term as t2, 
    * and/or vice-versa? 
    *)
let rec simterms (t1, t2) =
  let rec similar t1subst t1 t2subst t2 =
    let (t1, t2) = debracket t1, debracket t2 in
    let rec sim t1 t2 = similar t1subst t1 t2subst t2 in
    let rec sims t1s t2s =
      try _All (uncurry2 sim) (t1s ||| t2s) with
        Zip_ -> false
    in
    let rec reverse () = similar t2subst t2 t1subst t1 in
    let rec lsub () = t1subst && idclass t1 = VariableClass in
    let rec rsub () = t2subst && idclass t2 = VariableClass in
    let rec luni () =
      specialisesto (idclass t1, idclass t2) ||
      t1subst && specialisesto (idclass t1, VariableClass)
    in
    let rec runi () =
      specialisesto (idclass t2, idclass t1) ||
      t2subst && specialisesto (idclass t2, VariableClass)
    in
    if t1 = t2 then true
    else
      match t1, t2 with
        Id _, Id _ -> lsub () || rsub ()
      | Id _, Unknown _ -> lsub () || runi ()
      | Id _, Subst (_, _, p2, _) -> lsub () || similar t1subst t1 true p2
      | Id _, _ -> lsub ()
      | Unknown _, Id _ -> reverse ()
      | Unknown _, Unknown _ -> true
      | Unknown _, Subst (_, _, p2, _) ->
          (* need a notion of common subclass to go farther, don't have it *)
          (luni () || lsub ()) || similar t1subst t1 true p2
      | Unknown _, _ -> luni ()
      | Subst _, Id _ -> reverse ()
      | Subst _, Unknown _ -> reverse ()
      | Subst (_, _, p1, _), Subst (_, _, p2, _) ->
          similar true p1 true p2
      | Subst (_, _, p1, _), _ -> similar true p1 t2subst t2
      | _, Id _ -> reverse ()
      | _, Unknown _ -> reverse ()
      | _, Subst _ -> reverse ()
      | App (_, f1, a1), App (_, f2, a2) -> sim f1 f2 && sim a1 a2
      | Tup (_, s1, t1s), Tup (_, s2, t2s) -> s1 = s2 && sims t1s t2s
      | Literal (_, l1), Literal (_, l2) -> l1 = l2
      | Fixapp (_, ss1, t1s), Fixapp (_, ss2, t2s) ->
          (* false by now, really, but who cares? *)
          ss1 = ss2 && sims t1s t2s
      | Binding stuff, Binding stuff' ->
          sim (remake mapterm stuff) (remake mapterm stuff')
      | Collection (_, k, _), Collection (_, k', _) -> k = k'
      | _ ->(* otherwise too hard *)
         false
  in
  similar false t1 false t2
(*------------------------------------------------------------------------------*)

(* find the 'function' and 'arguments' in a curried or uncurried 'application'; 
 * strip brackets from all; give back a pair of function and argument list. 
 * Even works for null-argument 'applications'. It's sketchy, and doesn't protect
 * against silly arguments.
 * Now augmented with its converse.
 *)

let rec explodeApp curry t =
  let rec unApp t rs =
    match t with
      App (_, l, r) -> unApp (debracket l) (debracket r :: rs)
    | _ -> t, rs
  in
  match curry, unApp (debracket t) [] with
    true, (f, [Tup (_, ",", rs)]) -> f, (debracket <* rs)
  | _, res -> res
let rec implodeApp curry (t, args) =
  if curry then registerApp (t, registerTup (",", args))
  else nj_revfold (fun (r, l) -> registerApp (l, r)) args t
(* find the various ways in which a term can be a binary operation (sigh) *)
let rec explodebinapp t =
  match debracket t with
    App (_, Id (_, v, OperatorClass), Tup (_, ",", [e; f])) ->
      Some (e, string_of_vid v, f)
  | App (_, App (_, Id (_, v, OperatorClass), e), f) -> Some (e, string_of_vid v, f)
  | Tup (_, ",", _) -> None
  | Tup (_, s, [e; f]) -> Some (e, s, f)
  | _ -> None

(* ---------- for export ------------ *)

let rec int2term (i : int) = registerLiteral (Number i)
let rec term2int t =
  try
    match debracket t with
      Literal (_, Number n) -> n
    | App (_, Id (_, v, NoClass), t') -> 
        (match string_of_vid v with "~" -> -term2int t'
         |                          _   -> raise (Catastrophe_ []))
    | _ ->(* can happen, and NoClass is important ... *)
       raise (Catastrophe_ ["term2int"])
  with
    _ -> raise (Catastrophe_ ["term2int "; smltermstring t])
let rec enbracket t = registerFixapp (["("; ")"], [t])
let rec explodeCollection =
  function
    Collection (_, _, es) -> es
  | t -> [registerElement (Nonum, t)]
let rec augmentCollection a1 a2 =
  match a1, a2 with
    Collection (_, kind, es), els ->
      Some (registerCollection (kind, es @ els))
  | _, _ -> None
let rec decodeSubst =
  function
    Subst (_, r, p_, vts) -> Some (r, p_, vts)
  | _ -> None
let rec decodeBinding =
  function
    Binding (_, info, _, _) -> Some info
  | _ -> None
let rec decodeBracketed =
  function
    Fixapp (_, ["("; ")"], [t]) -> Some t
  | _ -> None

let string_of_vid = string_of_vid
let vid_of_string = vid_of_string
