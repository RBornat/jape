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

open Provisotype

open Listfuns 
open Miscellaneous
open Sml
open Symbol
open Symboltype 
open Termfuns 
open Termstore
open Termstring
open Termtype
open Termparse

let mkBag els = 
  registerCollection(Idclass.BagClass Idclass.FormulaClass,els)

let listclass = Idclass.ListClass Idclass.FormulaClass

type proviso = Provisotype.proviso

let provisodebug = ref false

let catelim_invisbracketedstring_of_proviso b p tail =
  match p with
  | FreshProviso (h, g, r, v) ->
      (if r then "IMP" else "") ::
        (match h, g with
         | true, true -> ""
         | true, false -> "HYP"
         | false, true -> "CONC"
         | false, false -> "????") ::
        "FRESH " :: catelim_invisbracketedstring_of_term b v tail
  | UnifiesProviso (t1, t2) ->
      let good =
        List.exists
          (function
           | Segvar _ -> true
           | _ -> false)
      in
      let easy =
        match t1, t2 with
        | Collection (_, _, e1s), Collection (_, _, s_of_e) ->
            good e1s || good s_of_e
        | Collection (_, _, e1s), _ -> good e1s
        | _, Collection (_, _, s_of_e) -> good s_of_e
        | _ -> true
      in
      let tstring =
        if easy then catelim_invisbracketedstring_of_termOrCollection b ","
        else catelim_invisbracketedstring_of_term b
      in
      tstring t1 (" UNIFIESWITH " :: tstring t2 tail)
  | NotinProviso (t1, t2) ->
      catelim_invisbracketedstring_of_term b t1
        (" NOTIN " :: catelim_invisbracketedstring_of_termOrCollection b "," t2 tail)
  | NotoneofProviso (vs, pat, _C) ->
      (* this madness IS used! *)
      catelim_string_of_list (catelim_invisbracketedstring_of_term b) "," vs
        (" IN " ::
           catelim_invisbracketedstring_of_term b pat
             (" NOTONEOF " :: catelim_invisbracketedstring_of_collection b"," _C tail))
  | DistinctProviso vs ->
      "DISTINCT " ::
         catelim_string_of_list (catelim_invisbracketedstring_of_term b) ", " vs tail
  | DischargeProviso (nt,sing,rts) -> (* we don't show the resnum *)
      (match nt,sing with
       | true , true  -> "SNTDISCHARGE"
       | true , false -> "NTDISCHARGE"
       | false, false -> "DISCHARGE"
       | false, true  -> "??SDICHARGE??"
      ) :: " " :: catelim_string_of_list (fun (r,t) -> catelim_invisbracketedstring_of_term b t) ", " rts tail
  
let invisbracketedstring_of_proviso = stringfn_of_catelim <.> catelim_invisbracketedstring_of_proviso

let catelim_string_of_proviso = catelim_invisbracketedstring_of_proviso false
let string_of_proviso = stringfn_of_catelim catelim_string_of_proviso

let rec isFreshProviso = function | FreshProviso _ -> true
                                  | _              -> false

type visrec = { visible : bool; parent : proviso; actual : proviso }
type visproviso = VisProviso of visrec

let rec mkvisproviso (vis, pro) = 
  VisProviso { visible=vis; parent=pro; actual=pro }
let rec mkparentedvisproviso parent (vis, pro) =
  VisProviso { visible=vis; parent=parent; actual=pro }

let rec provisovisible (VisProviso v) = v.visible
let rec provisoparent (VisProviso v) = v.parent
let rec provisoactual (VisProviso v) = v.actual

let rec provisoresetactual (VisProviso v) actual = 
    VisProviso {v with actual=actual}
let rec provisoselfparent (VisProviso v) =
    VisProviso {v with parent=v.actual}

let rec catelim_invisbracketedstring_of_visproviso b a1 a2 =
  match a1, a2 with
  | VisProviso {visible = true; actual = p}, tail ->
      catelim_invisbracketedstring_of_proviso b p tail
  | VisProviso {visible = false; actual = p}, tail ->
      "<<" :: catelim_invisbracketedstring_of_proviso b p (">>" :: tail)
let invisbracketedstring_of_visproviso = stringfn_of_catelim <.> catelim_invisbracketedstring_of_visproviso

let catelim_string_of_visproviso = catelim_invisbracketedstring_of_visproviso false
let string_of_visproviso = stringfn_of_catelim catelim_string_of_visproviso

let rec catelim_detailedstring_of_visproviso =
  fun (VisProviso {visible = visible; parent = parent; actual = actual})
    tail ->
    "{" :: string_of_bool visible :: ", actual=" ::
      catelim_string_of_proviso actual
        (", parent=" :: catelim_string_of_proviso parent ("}" :: tail))

let detailedstring_of_visproviso = stringfn_of_catelim catelim_detailedstring_of_visproviso

let rec stripElement =
  function
  | Element (_, _, t) -> t
  | s ->
      raise
        (Catastrophe_
           ["stripElement (proviso) called on Segvar ";
            string_of_term (mkBag [s])])

(* this is for renaming provisos when a rule/theorem is instantiated *)
let rec remapproviso env p =
  let _T = Match.remapterm env in
  match p with
  | FreshProviso (h, g, r, v)       -> FreshProviso (h, g, r, _T v)
  | NotinProviso (v, t)             -> NotinProviso (_T v, _T t)
  | DistinctProviso vs              -> DistinctProviso (_T <* vs)
  | NotoneofProviso (vs, pat, _C)   -> NotoneofProviso ((_T <* vs), _T pat, _T _C)
  | UnifiesProviso (_P1, _P2)       -> UnifiesProviso (_T _P1, _T _P2)
  | DischargeProviso (nt,sing,rts)  -> DischargeProviso (nt,sing,List.map (fun (r, t) -> r, _T t) rts)

let checkNOTINvars pname vars =
  List.iter
    (fun t ->
       if isVariable t || isconstant t then ()
       else
         raise
           (ParseError_
              [string_of_term t; " in "; pname; " proviso is not a variable or constant"]))
    vars

let rec parseNOTINvars pname =
  let _ = scansymb () in
  let vs = parseList (fun _ -> true) parseTerm commasymbol in
  checkNOTINvars pname vs; vs

let rec parseProvisos () =
  let rec parseNOTONEOF vars =
    let bad ss = raise (ParseError_ ("in NOTONEOF proviso, " :: ss)) in
    if List.exists (not <.> ismetav) vars then
      bad ["not all the names "; bracketed_string_of_list string_of_term ", " vars;
             " are schematic"];
    let _ = check (SHYID "IN") in
    let pat = parseBindingpattern () in
    let varsinpat = ismetav <| termvars pat in
    if not (subset (vars, varsinpat)) then
      bad
        ["not all the names "; bracketed_string_of_list string_of_term ", " vars;
         " appear in the pattern "; string_of_term pat];
    let _ = check (SHYID "NOTONEOF") in
    let (classopt, els) =
      parseElementList canstartTerm parseTerm commasymbol None
    in
    let class__ =
      match classopt with
      | Some c -> c
      | None -> listclass
    in
    NotoneofProviso (vars, pat, registerCollection (class__, els))
  in
  let parseNOTIN vars =
    let _ = checkNOTINvars "NOTIN" vars in
    let _ = check (SHYID "NOTIN") in
    let (class__, els) =
      parseElementList canstartTerm parseTerm commasymbol None
    in
    let terms =
      match class__ with
      | None -> (stripElement <* els)
      | Some k -> [registerCollection (k, els)]
    in
    ((fun v -> NotinProviso v) <* (vars >< terms))
  in
  let rec freshp p h g r v = p (h, g, r, v) in
  let dp nt sing =
    scansymb();
    let ts = parseList canstartTerm parseTerm commasymbol in
    [DischargeProviso (nt,sing,List.map (fun t -> Nonum, t) ts)]
  in
  match currsymb () with
  | SHYID "FRESH" ->
      freshp _FreshProviso true true false <* parseNOTINvars "FRESH"
  | SHYID "HYPFRESH" ->
      freshp _FreshProviso true false false <* parseNOTINvars "HYPFRESH"
  | SHYID "CONCFRESH" ->
      freshp _FreshProviso false true false <* parseNOTINvars "CONCFRESH"
  | SHYID "IMPFRESH" ->
      (freshp _FreshProviso true true true <* parseNOTINvars "FRESH")
  | SHYID "IMPHYPFRESH" ->
      (freshp _FreshProviso true false true <* parseNOTINvars "HYPFRESH")
  | SHYID "IMPCONCFRESH" ->
      freshp _FreshProviso false true true <* parseNOTINvars "CONCFRESH"
  | SHYID "DISTINCT" ->
      [DistinctProviso (parseNOTINvars "DISTINCT")]
  | SHYID "SNTDISCHARGE" -> dp true  true
  | SHYID "NTDISCHARGE"  -> dp true  false
  | SHYID "DISCHARGE"    -> dp false false
  | sy ->
      if canstartTerm sy then
        let (class__, els) =
          parseElementList canstartTerm parseTerm commasymbol None
        in
        let rec bk =
          function
          | [el] -> string_of_term (mkBag [el])
          | els -> ("[" ^ string_of_term (mkBag els)) ^ "]"
        in
        let rec collbad s =
          raise
            (ParseError_ ["when parsing a proviso, symbol "; s;
                          " found after collection "; bk els])
        in
        match class__, currsymb () with
        | None, SHYID "NOTIN"    -> parseNOTIN ((stripElement <* els))
        | None, SHYID "IN"       -> [parseNOTONEOF ((stripElement <* els))]
        | _, SHYID "NOTIN"       -> collbad "NOTIN"
        | _, SHYID "IN"          -> collbad "IN"
        | _, SHYID "UNIFIESWITH" ->
            let _ = scansymb () in
            let (class', els') =
              parseElementList canstartTerm parseTerm commasymbol class__
            in
            let (t1, t2) =
              match class', els, els' with
              | Some k, _, _ ->
                  registerCollection (k, els),
                  registerCollection (k, els')
              | None, [el], [el'] -> stripElement el, stripElement el'
              | _ ->
                  raise
                    (ParseError_
                       ["can't determine collection class of "; bk els;
                        " and "; bk els'])
            in
            [UnifiesProviso (t1, t2)]
        | _ ->
            raise
              (ParseError_
                 ["comma, IN, NOTIN or UNIFIESWITH expected after ";
                  string_of_list string_of_element ", " els; " in provisos"])
      else
        raise
          (ParseError_
             ["Proviso -- FRESH.. or HYPFRESH.. or CONCFRESH.. \
               or formula UNIFIESWITH formula or ids NOTIN terms or \
               DISTINCT ids or \
              var IN pattern NOTONEOF collection -- expected, \
              found "; debugstring_of_symbol sy])

(* function for sorting proviso lists so that they are nice and readable *)
let earlierproviso p1 p2 =
  let lin1 =
    function
    | FreshProviso (h, g, r, v)         -> (if h then 10 else 0) + (if g then 20 else 0) + (if r then 40 else 0)
    | DistinctProviso vs                -> 300
    | NotinProviso (v, t)               -> 400
    | NotoneofProviso (vs, pat, _C)     -> 500
    | UnifiesProviso (t, t')            -> 600
    | DischargeProviso (nt,sing,rts)    -> 700 + (if nt then 10 else 0) + (if sing then 10 else 0)
  in
  let lin2 =
    function
    | FreshProviso (h, g, r, v)     -> [string_of_term v]
    | DistinctProviso vs            -> string_of_term <* vs
    | NotinProviso (v, t)           -> [string_of_term v; string_of_term t]
    | NotoneofProviso (vs, pat, _C) ->
        nj_fold (fun (v, ss) -> string_of_term v :: ss) vs
                [string_of_term pat; string_of_term _C]
    | UnifiesProviso (t, t')        -> [string_of_term t; string_of_term t']
    | DischargeProviso (nt,sing,rts)    -> List.map (string_of_term <.> snd) rts
  in
  let n1 = lin1 p1 in
  let n2 = lin1 p2 in
  n1 < n2 || n1 = n2 && earlierlist (<) (lin2 p1) (lin2 p2)

let provisovars termvars tmerge p =
  match p with
  | FreshProviso (_, _, _, t)       -> termvars t
  | UnifiesProviso (t1, t2)         -> tmerge (termvars t1) (termvars t2)
  | NotinProviso (t1, t2)           -> tmerge (termvars t1) (termvars t2)
  | DistinctProviso vs              -> nj_fold (uncurry2 tmerge) (termvars <* vs) []
  | NotoneofProviso (vs, pat, _C)   -> nj_fold (uncurry2 tmerge) (termvars <* vs) (termvars _C)
  | DischargeProviso (nt,sing,rts)  -> nj_fold (uncurry2 tmerge) ((termvars <.> snd) <* rts) []

let provisoVIDs p =
  orderVIDs ((vid_of_var <* provisovars termvars tmerge p))

let maxprovisoresnum p =
  let rec f t n =
    nj_fold (uncurry2 max) ((int_of_resnum <* elementnumbers t)) n
  in
  match p with
  | FreshProviso (_, _, _, t)       -> f t 0
  | UnifiesProviso (t1, t2)         -> f t1 (f t2 0)
  | NotinProviso (v, t)             -> f v (f t 0)
  | DistinctProviso vs              -> nj_fold (uncurry2 f) vs 0
  | NotoneofProviso (vs, pat, _C)   -> nj_fold (uncurry2 f) vs (f _C 0)
  | DischargeProviso (nt,sing,rts)  -> (* an element is not a collection, whatever the type says. So we just look for max
                                          in the resnums of rts
                                        *)
                                       nj_fold (uncurry2 max) ((int_of_resnum <.> fst) <* rts) 0

(* make DISTINCT out of several NOTINs and vice-versa; 
   make a single FRESH out of several
 *)

let expandProvisos ps =
  let ep p ps = 
    match p with
    | DistinctProviso vs -> 
        let rec dp = 
          function 
          | []    -> ps
          | v::vs -> foldr (fun v' ps -> DistinctProviso[v;v']::ps) (dp vs) vs
        in dp vs
    | p -> p :: ps
  in
  foldr ep [] ps

let compressProvisos ps =
  let ps = expandProvisos ps in
  (* filter out the variable NOTINs and try to put them into groups *)
  (* This algorithm -- generate all the subsets, filter out the non-solutions --
     suggested by Peter O'Hearn 2/vii/2004
     Bernard Sufrin suggested filtering during construction, same date.
   *)
  let xys, xts, others =
    foldl (fun (xys, xts, others) ->
             function (NotinProviso(v,t)) ->
                         if isVariable t then (false,[v;t])::xys, xts, others
                                   else xys, (v,t)::xts, others
           |          (DistinctProviso[v;v']) -> (true,[v;v'])::xys, xts, others
           |          p                       -> xys, xts, p::others) 
          ([],[],[]) ps
  in
  (* remove duplication *)
  let xys = (fun (tf,xs) -> tf, sort earliervar xs) <* xys in
  let xys = sortunique (fun (_,xs) (_,ys) -> earlierlist earliervar xs ys) xys in
  let xy2s = List.map snd xys in
  let names = foldl tmerge [] xy2s in
  (* make all supported subsets *)
  let rec subsets =
    function 
    | [] -> [[]]
    | x :: xs -> 
        let xss = subsets xs in
        foldr (fun ys yss -> if all (fun y -> List.mem [x;y] xy2s) ys
                             then (x::ys)::yss
                             else yss) 
              xss xss
  in
  let sets = (fun xs -> List.length xs>=2) <| subsets names in
  let sets = sort (fun xs ys -> List.length xs>List.length ys) sets in
  let rec comb =
    function 
    | []        -> []
    | xs :: xss -> xs :: comb ((fun xs' -> sorteddiff earliervar xs' xs != []) <| xss)
  in
  let distincts = (function ([x;y] as xy) -> if List.mem (false,xy) xys 
                                             then _NotinProviso(x,y) else _DistinctProviso xy
                   |        xs            -> _DistinctProviso xs) <* comb sets in 
  (* One day I'll try to join the xts into larger groups *)
  (* and do something about FreshProviso *)
  distincts @ (_NotinProviso <* xts) @ others
  
let doVisProvisos f ps =
  let vis, invis = split (fun (VisProviso p) -> p.visible) ps in
  let vis = f (provisoactual <* vis) in
  let invis = f (provisoactual <* invis) in
  (curry2 mkvisproviso true <* vis) @ (curry2 mkvisproviso false <* invis)

let compressVisProvisos = doVisProvisos compressProvisos
let expandVisProvisos = doVisProvisos expandProvisos

(* I suspect that groundedprovisos identifies provisos that don't have 
   anything to do with 'names'. It appears to give you Some ps iff ps are those
   provisos which are grounded, and the rest must be thrown away. So None means
   don't change anything. It's called from prooftree when rewriting a proof tree.
   RB 2012
 *)

(* the list of names must be sorted, which it will be if it comes out of termvars *)
(* because of hidden provisos, this thing now gets a visproviso list *)
(* iso means isolated, I think; so isot is an isolated term, isop an isolated proviso *)

let vv = bracketed_string_of_list string_of_visproviso " AND "

let rec groundedprovisos names provisos =
  let rec isot t =
    let vs = ismetav <| termvars t in
    let rec diff a1 a2 =
      match a1, a2 with
      | x :: xs, y :: ys ->
          earliervar x y && diff xs (y :: ys) ||
          earliervar y x && diff (x :: xs) ys
      | _, _ -> true
    in
    let r = diff vs names in
    if !provisodebug then
      consolereport
        ["isot checking "; string_of_term t; " against ";
         string_of_termlist names; " => "; string_of_bool r];
    r
  in
  let rec isop p =
    let r =
      match p with
      | FreshProviso (_, _, _, v)       -> ismetav v && isot v
      | NotinProviso (v, t)             -> ismetav v && isot v || isot t
      | DistinctProviso vs              -> not (List.exists (not <.> isot) vs)
      | NotoneofProviso (vs, pat, _C)   -> not (List.exists (not <.> isot) vs) || isot _C
      | UnifiesProviso (_P1, _P2)       -> isot _P1 && isot _P2
      | DischargeProviso (nt,sing,rts)  -> List.for_all (isot <.> snd) rts
    in
    if !provisodebug then
      consolereport
        ["isop checking "; string_of_proviso p; " => "; string_of_bool r];
    r
  in
  let r =
    if null provisos then None
    else
      (* keep them? throw them away? *) 
      match split (isop <.> provisoactual) provisos with
      | [], _ -> None
      | _, [] -> Some [] (* we kept them all *)
      | isos, uses -> (* we threw them all away *)
          match
            groundedprovisos
              (nj_fold (uncurry2 tmerge)
                       ((provisovars termvars tmerge <.> provisoactual) <* uses)
                       names)
              isos
          with
          | Some more -> Some (uses @ more)
          | None      -> None (* we kept some *)
             
  in
  if !provisodebug then
    consolereport
      ["groundedprovisos "; string_of_termlist names; " ";
       bracketed_string_of_list string_of_visproviso " AND " provisos; " => ";
       Optionfuns.string_of_option vv r];
  r

(* This is useful when fabricating lemmas *)

let relevantprovisos seq ps =
  let lemmavars = Sequent.seqvars termvars tmerge seq in
  (fun p -> Listfuns.sorteddiff earliervar (provisovars termvars tmerge p) lemmavars=[]) <| ps
  
