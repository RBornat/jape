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


(* The disproof calculator.  Commenced 4.vii.01. RB *)
(* It implements disproof in Kripke semantics (made classical by restriction to a single
   world, if necessary).  Predicates operate only on individuals, mostly because I don't
   yet understand terms.  It seems reasonable to me that if there is a disproof, then there
   is one which only uses individuals, so I'm not too unhappy yet.
   RB 
 *)

open Box
open Idclass
open Forcedef
open Japeserver
open Listfuns
open Mappingfuns
open Optionfuns
open Predicate
open Seqdraw
open Sequent.Funs
open Sml
open Term.Funs
open Term.Store
open Term.Termstring

type forcedef = Forcedef.forcedef
 and model = Forcedef.model

let atoi = Miscellaneous.atoi
let rec ask ss bs def =
  Alert.ask (Alert.defaultseverity bs) (implode ss) bs def
let askChoice = Alert.askChoice
let base_sequent = Prooftree.Tree.Fmttree.sequent
let catelim_seqstring = Sequent.Funs.catelim_seqstring
let consolereport = Miscellaneous.consolereport
let drawindisproofpane () = drawinpane Displayfont.DisproofPane
let getsemanticturnstile = Sequent.Funs.getsemanticturnstile
let isdigit = Miscellaneous.isdigit
let isextensibleID = Symbol.isextensibleID
let lowercase = Stringfuns.lowercase
let matchdebug = Match.matchdebug
let matchvars = Match.matchvars 
let mkSeq = Sequent.Funs.mkSeq
let onbraket = String.make 1 Miscellaneous.onbra, String.make 1 Miscellaneous.onket
let offbraket = String.make 1 Miscellaneous.offbra, String.make 1 Miscellaneous.offket
let outbraket = String.make 1 Miscellaneous.outbra, String.make 1 Miscellaneous.outket
let lockbraket = String.make 1 Miscellaneous.lockbra, String.make 1 Miscellaneous.lockket
let option_remapterm = Match.option_remapterm
let parseTerm = Termparse.term_of_string
let pairstring = Stringfuns.pairstring
let planinfo = Draw.planinfo
let seqexplode = Sequent.Funs.seqexplode
let seqstring = Sequent.Funs.seqstring
let seqvars = Sequent.Funs.seqvars termvars tmerge
let showAlert = Alert.showAlert Alert.defaultseverity_alert <*> implode
let simplifySubst = Substmapfuns.simplifySubst
let smlseqstring = Sequent.Funs.smlseqstring
let subtree = Prooftree.Tree.Fmttree.followPath
let triplestring = Stringfuns.triplestring
let uncurry2 = Miscellaneous.uncurry2

let rec term2binding t =
  match Binding.bindingstructure t with
    Some t' -> Some (registerBinding t')
  | None -> None
    
let catelim_pairstring = Stringfuns.catelim_pairstring
let catelim_triplestring = Stringfuns.catelim_triplestring

exception Catastrophe_ = Miscellaneous.Catastrophe_
exception ParseError_ = Miscellaneous.ParseError_
exception Tacastrophe_ = Miscellaneous.Tacastrophe_

let rec catelim_intstring i ss = string_of_int i :: ss
let rec catelim_boolstring b ss = string_of_bool b :: ss
let disproofdebug = ref false
let sameelement = eqelements eqterms
let rec dosubst facts =
  option_mapterm
       (decodeSubst &~ (fun (_, _P, vts) -> simplifySubst facts vts _P))
let rec patvar t = (isleaf t && isId t) && isextensibleID (string_of_vid (vid_of_var t))
let rec patvars t = patvar <| termvars t
let rec matchit pat vs t =
  let res = matchvars true (fun v -> member (v, vs)) pat t empty in
  if !matchdebug then
    consolereport
      ["matchit (disproof) matching "; termstring pat; " ";
       bracketedliststring
         (fun v ->
            pairstring termstring (idclassstring <*> idclass)
              "," (v, v))
         ", " vs;
       " against "; termstring t; " => ";
       optionstring (mappingstring termstring termstring) res];
  res
let rec isBagClass t =
  match collectionkind t with
    Some (BagClass _) -> true
  | _ -> false
let rec my_seqexplode s =
  let (st, h, c) = seqexplode s in
  st, isBagClass h, explodeCollection h, isBagClass c, explodeCollection c
(* *********************** worlds and situations *********************** *)

(* a world has coordinates, labels (terms), and a list of child worlds.  
 * This defines a DAG.  We can do DAGs with refs, but printing it is
 * hard, so it's simpler to do it with a table, and use indices rather than pointers.  
 * I started with an array, but because we need to be able (for example) to
 * keep histories of disproof attempts, I had to use a mapping.  Oh well.
 *)

type coord = int * int
type world = term list * coord list
(* labels, children *)
   
type universe = (coord, world) mapping
let catelim_coordstring =
  catelim_pairstring catelim_intstring catelim_intstring ","
let rec coordstring c = implode (catelim_coordstring c [])
let catelim_worldstring =
  catelim_pairstring (catelim_bracketedliststring catelim_termstring ",")
    (catelim_bracketedliststring catelim_coordstring ",") ", "
let rec worldstring w = implode (catelim_worldstring w [])
let rec catelim_universestring sep u ss =
  "Universe " ::
    catelim_mappingstring catelim_coordstring catelim_worldstring
      ("++" ^ sep) u ss
let rec universestring sep u = implode (catelim_universestring sep u [])
let rec universe2list u = List.map (fun (c, (ts, cs)) -> c, ts, cs) (aslist u)
let rec emptyworld () = ( |-> ) ((0, 0), ([], []))
let rec isemptyworld u =
  match ran u with
    [[], []] -> true
  | _ -> false
let rec getworld u c =
  try _The (at (u, c)) with
    _ ->
      raise
        (Catastrophe_
           ["(getworld) no world at "; coordstring c; "; ";
            universestring "" u])
(* enforce monotonicity *)
let rec domono pts u c =
  let (ts, cs) = getworld u c in
  match listsub eqterms pts ts with
    [] -> u
  | ts' -> foldl (domono ts') (( ++ ) (u, ( |-> ) (c, (ts' @ ts, cs)))) cs
let rec addworldlabel u c t =
  match at (u, c) with
    None ->
      raise
        (Tacastrophe_
           ["(addworldlabel) no world at "; coordstring c; "; ";
            universestring "" u])
  | Some (pts, pcs) ->
      if member (t, pts) then None
      else
        Some
          (foldl (domono [t]) (( ++ ) (u, ( |-> ) (c, (t :: pts, pcs))))
             pcs)
let rec deleteworldlabel u c t =
  match at (u, c) with
    None ->
      raise
        (Tacastrophe_
           ["(deleteworldlabel) no world at "; coordstring c; "; ";
            universestring "" u])
  | Some (pts, pcs) ->
      if not (member (t, pts)) then None
      else
        let rec islabelledparent u c c' =
          let (ts, cs) = getworld u c' in member (c, cs) && member (t, ts)
        in
        let rec doself u c =
          let (ts, cs) = getworld u c in
          if member (t, ts) then
            ( ++ ) (u, ( |-> ) (c, (listsub eqterms ts [t], cs)))
          else u
        in
        let rec doparents u c =
          foldl doparents (doself u c)
            (islabelledparent u c <| dom u)
        in
        Some (doparents u c)

(* *********************** semantics *********************** *)

type definition = term * term list * bool * forcedef
(* lhs, args, hassubst, rhs *)
   
   (* for 'occurrence' read 'individual', I think: things like actual i and so on *)
let forcedefs : definition list ref = ref []
let occurrences : (term * term list) list ref = ref []
(* occurrence forms *)
   
let rec isoccurrence t =
  List.exists
    (opt2bool <*> (fun (occ, occvs) -> matchit occ occvs t))
    !occurrences
let rec newoccurrence v =
  match
       (function
          t, [v] -> true
        | _ -> false) <|
       !occurrences
  with
    (occ, [occv]) :: _ ->
      _The (option_remapterm (( |-> ) (v, occv)) occ)
  | _ ->
      raise
        (Catastrophe_
           ["(newoccurrence) no single-variable occurrence forms recorded"])
(* filters out previous versions of t -- so we only keep actual i, for example,
   instead of actual i, actual j, actual k etc.
 *)
let rec nodups t vs patf xs =
  t, (not <*> opt2bool <*> matchit t vs <*> patf) <| xs
let rec newocc t vs os =
  let rec patf (t, vs) = t in
  let (t, os') = nodups t vs patf os in (t, vs) :: os'
let rec variables t = isVariable <| termvars t
let rec findoccs f os =
  match f with
    ForcePrim _ -> os
  | ForceBoth (f1, f2) -> findoccs f1 (findoccs f2 os)
  | ForceEither (f1, f2) -> findoccs f1 (findoccs f2 os)
  | ForceIf (f1, f2) -> findoccs f1 (findoccs f2 os)
  | ForceEverywhere f -> findoccs f os
  | ForceNowhere f -> findoccs f os
  | ForceAll (t, vs, f) -> findoccs f (newocc t vs os)
  | ForceSome (t, vs, f) -> findoccs f (newocc t vs os)
(* translate predicates, avoid duplicates *)
let rec addforcedef (t, fd) =
  try
    let rec notabst _ = false in
    let pbs =
      discardzeroarities (foldterm (findpredicates notabst []) [] t)
    in
    let rec pvars =
      fun (_P, abss) ->
        match findpredicatevars abss with
          Some vs -> _P, vs
        | None ->
            raise
              (Predicate_
                 ["Semantics must be simple! In the semantic pattern ";
                  termstring t; ", predicate "; termstring _P;
                  " doesn't have bound variables as arguments"])
    in
    let env = mkmap (List.map pvars pbs) in
    let comp = compilepredicate notabst (fun v -> at (env, v)) in
    let (t, fd) = mapterm comp t, mapforcedefterms comp fd in
    let vs = patvars t in
    let (t, fds) = nodups t vs (fun (t', _, _, _) -> t') !forcedefs in
    let hassubst = opt2bool <*> findinforcedef decodeSubst in
    forcedefs := (t, vs, hassubst fd, fd) :: fds;
    occurrences := findoccs fd !occurrences
  with
    Predicate_ ss -> raise (ParseError_ ss)
let rec clearforcedefs () = forcedefs := []; occurrences := []
let rec hasforcedefs () = not (null !forcedefs)
(* matching a formula against the definitions *)
let rec semantics facts t =
  match
    findfirst
      (fun (pat, vs, hassubst, fd) ->
           (matchit pat vs t &~~ (fun env -> Some (env, hassubst, fd))))
      !forcedefs
  with
    Some (env, hassubst, fd) ->
      let fd =
        mapforcedefterms (option_remapterm env) (uniquebinders t fd)
      in
      Some (if hassubst then mapforcedefterms (dosubst facts) fd else fd)
  | None -> None
(* avoid variable capture in semantics function *)
(* really this ought to look inside the forcedef, but one step at a time ... *)
and uniquebinders t fd =
  let rec newoccenv ocvs =
    let vids = orderVIDs (List.map vid_of_var (variables t)) in
    let rec newvar (ocvids, env) ocv =
      let ocvid = vid_of_var ocv in
      let ocvid' = uniqueVID VariableClass vids ocvids ocvid in
      ocvid' :: ocvids,
      (if ocvid = ocvid' then env
       else
         ( ++ ) (( |-> ) (ocv, registerId (ocvid', VariableClass)), env))
    in
    let r = snd (foldl newvar ([], empty) ocvs) in
    if !disproofdebug then
      consolereport
        ["semantics matched "; termstring t; " and "; forcedefstring fd;
         "; rewrite with "; mappingstring termstring termstring r];
    r
  in
  let rec doit ocvs =
    mapforcedefterms (option_remapterm (newoccenv ocvs)) fd
  in
  match fd with
    ForceAll (_, ocvs, _) -> doit ocvs
  | ForceSome (_, ocvs, _) -> doit ocvs
  | _ -> fd
(* finding the subformulae which aren't semantically defined. Designed to be foldl'd *)
let rec semanticfringe facts ts t =
  let rec tf ts t =
    match semantics facts t with
      None ->
        begin match decodeBracketed t with
          Some t' -> tf ts t'
        | None -> t :: ts
        end
    | Some fd -> ff ts fd
  and ff ts fd =
    match fd with
      ForcePrim t -> tf ts t
    | ForceBoth pair -> pf ts pair
    | ForceEither pair -> pf ts pair
    | ForceIf pair -> pf ts pair
    | ForceEverywhere fd -> ff ts fd
    | ForceNowhere fd -> ff ts fd
    | ForceAll (t, _, fd) -> ff (t :: ts) fd
    | ForceSome (t, _, fd) -> ff (t :: ts) fd
  and pf ts (fd1, fd2) = ff (ff ts fd1) fd2 in
  tf ts t
(* is it forced? *)
(* rewritten to be memoised, both for 'efficiency' and for de-emphasising irrelevant subformulae
   -- i.e. quantified subformulae
 *)
let rec unfixedforced facts u =
  let rec ff f (c, t) =
    let rec labels c = fst (getworld u c) in
    let rec children c = snd (getworld u c) in
    let rec lookup c t = member (t, labels c), true in
    (* emphasis locked *)
                   (* forced logic -- we force evaluation of subformulae because otherwise things go grey which shouldn't *)
    let rec logNot =
      function
        true, _ -> false, false
      | _ -> true, false
    in
    let rec logAnd a1 a2 =
      match a1, a2 with
        (true, _), (true, _) -> true, false
      | _, _ -> false, false
    in
    let rec logOr a1 a2 =
      match a1, a2 with
        (false, _), (false, _) -> false, false
      | _, _ -> true, false
    in
    let rec logImp a1 a2 =
      match a1, a2 with
        (true, _), (false, _) -> false, false
      | _, _ -> true, false
    in
    let rec logAll f = foldl logAnd (true, false) <*> List.map f in
    let rec logExists f = foldl logOr (false, false) <*> List.map f in
    let rec indiv_fd (oc, vs, fd) i =
      (* fun remap env t = (* written with andthenr, ortryr, because I don't trust 0.93 with the functional version *)
                             option_remapterm env t  &~~ 
                             (fn t' => dosubst facts t' |~~ (fn () => Some t'))
                            *)
      let r =
          (matchit oc vs i &~~
           (fun env ->
              Some
                (mapforcedefterms
                   ( (option_remapterm env &~ somef (dosubst facts)))
                   fd)))
      in
      if !disproofdebug then
        begin
          consolereport
            ["indiv_fd ";
             triplestring termstring (bracketedliststring termstring ",")
               forcedefstring "," (oc, vs, fd);
             " "; termstring i; " => "; optionstring forcedefstring r];
          consolereport
            ["matchit oc vs i => ";
             optionstring (mappingstring termstring termstring)
               (matchit oc vs i)];
          match matchit oc vs i with
            None -> ()
          | Some env ->
              consolereport
                ["mapforcedefterms (option_remapterm env) fd => ";
                 forcedefstring
                   (mapforcedefterms (option_remapterm env) fd)];
              consolereport
                ["mapforcedefterms (option_remapterm env  &~ (somef (dosubst facts))) fd => ";
                 forcedefstring
                   (mapforcedefterms
                      (
                         (option_remapterm env &~ somef (dosubst facts)))
                      fd)]
        end;
      r
    in
    let rec interp fd c =
      match fd with
        ForcePrim t' -> f (c, t')
      | ForceBoth (fd1, fd2) -> logAnd (interp fd1 c) (interp fd2 c)
      | ForceEither (fd1, fd2) -> logOr (interp fd1 c) (interp fd2 c)
      | ForceIf (fd1, fd2) -> logImp (interp fd1 c) (interp fd2 c)
      | ForceEverywhere fd' ->
          logAnd (interp fd' c) (logAll (interp fd) (children c))
      | ForceNowhere fd' ->
          logAnd (logNot (interp fd' c)) (logAll (interp fd) (children c))
      | ForceAll tvsfd ->
          logAll
            (fun lab ->
               match indiv_fd tvsfd lab with
                 Some fd' -> interp fd' c
               | _ -> true, false)
            (labels c)
      | ForceSome tvsfd ->
          logExists
            (fun lab ->
               match indiv_fd tvsfd lab with
                 Some fd' -> interp fd' c
               | _ -> false, false)
            (labels c)
    in
    let _ =
      if !disproofdebug then
        consolereport ["evaluating "; coordstring c; "; "; termstring t]
    in
    let result =
      match semantics facts t with
        None ->
          begin match decodeBracketed t with
            Some t' -> f (c, t')
          | None -> lookup c t
          end
      | Some fd -> interp fd c
    in
    if !disproofdebug then
      consolereport
        ["forced ("; coordstring c; ", "; termstring t; ") => ";
         pairstring string_of_bool string_of_bool "," result];
    result
  in
  ff
let rec seq_forced forced c s =
  let rec doit e =
    match element2term e with
      None -> false, false
    | Some t -> forced (c, t)
  in
  let (_, _, hyps, _, concs) = my_seqexplode s in
  List.map doit hyps, List.map doit concs
(* *********************** interaction states *********************** *)

type disproofstaterec =
      { seq : seq; universe : universe; tiles : term list;
        selected : coord list;
        forcemap : ((coord * term), (bool * bool)) Hashtbl.t;
        conclusive : bool; countermodel : bool }
type disproofstate = Disproofstate of disproofstaterec
let catelim_forcedstring =
  catelim_pairstring catelim_boolstring catelim_boolstring ","

let rec catelim_Hashtblstring astring bstring sepstr mapstr store ss =
  let mapping = Hashtbl.fold (fun k v kvs -> (k,v)::kvs) store [] in
  "<<:" ::
    catelim_liststring
      (fun (a, b) ss -> "(" :: astring a (sepstr :: bstring b (")" :: ss)))
      sepstr mapping (":>>" :: ss)
let rec _Hashtblstring a b =
  catelim2stringfn
    (catelim_Hashtblstring (stringfn2catelim a) (stringfn2catelim b) "+:+" "|:->")

let rec catelim_disproofstatestring =
  fun
    (Disproofstate
       {seq = seq; universe = universe; selected = selected; tiles = tiles;
        forcemap = forcemap; conclusive = conclusive; countermodel = countermodel})
    ss ->
    let cbs = catelim_bracketedliststring catelim_boolstring "," in
    "Disproofstate{seq = " ::
      catelim_seqstring seq
        (",\nworld = " ::
           catelim_universestring "" universe
             (",\nselected = " ::
                catelim_bracketedliststring catelim_coordstring ","
                  selected
                  (",\ntiles = " ::
                     catelim_bracketedliststring catelim_termstring ","
                       tiles
                       (", forcemap = " ::
                          catelim_Hashtblstring
                            (catelim_pairstring catelim_coordstring
                               catelim_termstring ",")
                            catelim_forcedstring "+:+" "|:->" forcemap
                            (", conclusive = " ::
                               catelim_boolstring conclusive
                                 (", countermodel = " ::
                                    catelim_boolstring countermodel
                                      ("}" :: ss)))))))
let rec disproofstatestring d = implode (catelim_disproofstatestring d [])
let rec disproofstate_seq = fun (Disproofstate {seq = seq}) -> seq
let rec disproofstate_universe =
  fun (Disproofstate {universe = universe}) -> universe
let rec disproofstate_selected =
  fun (Disproofstate {selected = selected}) -> selected
let rec disproofstate_conclusive =
  fun (Disproofstate {conclusive = conclusive}) -> conclusive
let rec disproofstate_countermodel =
  fun (Disproofstate {countermodel = countermodel}) -> countermodel

(* because of the need for facts, these don't evaluate! *)
let rec withdisproofuniverse (Disproofstate s) universe =
    Disproofstate {s with universe = universe}
let rec withdisproofselected (Disproofstate s) selected =
    Disproofstate {s with selected = selected}
let rec withdisprooftiles (Disproofstate s) tiles =
    Disproofstate {s with tiles = tiles}
    
let rec disproof_minimal =
  function
    None -> true
  | Some (Disproofstate {universe = universe}) -> isemptyworld universe

let newforcemap () = Hashtbl.create 17 

let rec evaldisproofstate facts tree =
  fun (Disproofstate
         {seq = seq; universe = universe; selected = selected; tiles = tiles}) ->
    let (basestyle, hypsbag, basehyps, concsbag, baseconcs) =
      my_seqexplode (base_sequent tree)
    in
    let (style, _, hyps, _, concs) = my_seqexplode seq in
    let conclusive =
      ((match getsemanticturnstile basestyle with
          Some semstyle -> semstyle = style
        | None -> basestyle = style) &&
       (* since styles are equal, so is baggishness *)
       (eqlists sameelement (basehyps, hyps) ||
        hypsbag && null (listsub sameelement basehyps hyps))) &&
      (eqlists sameelement (baseconcs, concs) ||
       concsbag && null (listsub sameelement baseconcs concs))
    in
    match selected with
      [root] ->
        let forcemap = newforcemap () in
        (* fun mf a v = pairstring coordstring smltermstring "," a^"|->"^(catelim2stringfn catelim_forcedstring) v *)
        let forced = Fix.memofix forcemap (unfixedforced facts universe) in
        let (hs, cs) = seq_forced forced root seq in
        let countermodel =
          _All fst hs && not (List.exists fst cs)
        in
        Disproofstate
          {seq = seq; universe = universe; tiles = tiles; selected = selected;
           forcemap = forcemap; conclusive = conclusive; countermodel = countermodel}
    | _ ->
        Disproofstate
          {seq = seq; universe = universe; tiles = tiles; selected = selected;
           forcemap = newforcemap (); conclusive = conclusive; countermodel = false}
exception Disproof_ of string list
(* sort, eliminating duplicates.  This sort is no longer case sensitive *)
let rec alphasort sel ts =
  List.map sel
    (sortunique (fun (s1, _) (s2, _) -> lowercase s1 < lowercase s2)
       ((List.map termstring ts ||| ts)))
let tilesort = List.rev <*> alphasort snd
let labelsort = alphasort fst 
let rec seq2tiles facts seq =
  let (_, _, hyps, _, concs) = my_seqexplode seq in
  (* check for an absence of silliness *)
  let rec existselement f e =
    match element2term e with
      None -> false
    | Some t -> existsterm f t
  in
  let _ =
    if List.exists
         (fun e ->
            if issegvar e then
              raise
                (Disproof_
                   ["Forcing semantics can only deal with actual formulae in sequents.";
                    " Segment variables (e.g. "; elementstring e;
                    ") aren't allowed "; " in disproof sequents."])
            else false)
         (hyps @ concs)
    then
      ()
    else if
      (* can't happen *)
      List.exists
        (existselement
           (fun t ->
              if isUnknown t then
                raise
                  (Disproof_
                     ["Unknowns (e.g. "; termstring t; " in sequent ";
                      seqstring seq; ") make disproof somewhat tricky.  Try again when you have resolved the ";
                      " unknown formulae."])
              else false))
        (concs @ hyps)
    then
      ()
    else if
      (* can't happen *)
      List.exists
        (existselement
           (fun t ->
              match decodeSubst t with
                Some _ ->
                  raise
                    (Disproof_
                       ["Substitutions (e.g. "; termstring t;
                        " in sequent "; seqstring seq; ") aren't part of the forcing semantics.  Try again when you have ";
                        " simplified the substitution(s)."])
              | _ -> false))
        (concs @ hyps)
    then
      ()
  in
  (* find the fringe *)
  let hypterms = optionfilter element2term hyps in
  let concterms = optionfilter element2term concs in
  let ts =
    foldl (semanticfringe facts)
      (foldl (semanticfringe facts) [] hypterms) concterms
  in
  (* add occurrence formulae if necessary *)
  let ts =
    (if List.exists (opt2bool <*> decodeBinding) hypterms ||
        List.exists (opt2bool <*> decodeBinding) concterms
     then
       List.map fst 
             ((fun (occ, vs) ->
                not (List.exists (opt2bool <*> (matchit occ vs)) ts)) <| !occurrences)
     else []) @ ts
  in
  (* eliminate all individuals which don't appear free in the sequent, 
   * but if there aren't any such individuals, keep just one
   *)
  let svs = seqvars seq in
  match isVariable <| nj_fold (uncurry2 tmerge) (List.map patvars ts) [] with
    [] -> ts
  | tvs ->
      let tvs' =
        match (fun i -> member (i, svs)) <| tvs with
          [] -> [List.hd tvs]
        | vs -> vs
      in
      let badvs = listsub (fun (x, y) -> x = y) tvs tvs' in
      let rec changevars i =
        if member (i, badvs) then Some (List.hd tvs') else None
      in
      List.map (mapterm changevars) ts
let rec disproofstate2model =
  function
    None -> None
  | Some (Disproofstate {seq = seq; universe = universe}) ->
      Some
        (seq,
         Model
           (List.map (fun (c, (ts, chs)) -> World (Coord c, List.map (fun v->Coord v) chs, ts))
              (aslist universe)))
let rec model2disproofstate a1 a2 a3 =
  match a1, a2, a3 with
    facts, tree, None -> None
  | facts, tree, Some (seq, model) ->
      let rec coord = fun (Coord c) -> c in
      let (Model worlds) = model in
      let ulist =
        List.map (fun (World (c, chs, ts)) -> coord c, (ts, List.map coord chs))
          worlds
      in
      let utiles = nj_fold (fun (x, y) -> x @ y) (List.map (fst <*> snd) ulist) [] in
      let uvars = nj_fold (uncurry2 tmerge) (List.map variables utiles) [] in
      let tiles = List.map newoccurrence uvars @ seq2tiles facts seq @ utiles in
      let minc =
        foldr
          (fun ((_, cy as c), _) (_, miny as minc) ->
             if cy < miny then c else minc)
          (0, 0) ulist
      in
      let u = mkmap ulist in
      let ws = List.map fst ulist in
      List.iter
        (fun ((_, cy as c), (ts, chs)) ->
           List.iter
             (fun (_, chy as ch) ->
                if chy <= cy then
                  raise
                    (ParseError_
                       ["(model2disproofstate) non-ascending link ";
                        coordstring c; " to "; coordstring ch; " in ";
                        universestring "" u])
                else
                  match at (u, ch) with
                    Some (chts, _) ->
                      if not
                           (null (listsub (fun (x, y) -> x = y) ts chts))
                      then
                        raise
                          (ParseError_
                             ["(model2disproofstate) non-monotonic link ";
                              coordstring c; " to "; coordstring ch;
                              " in "; universestring "" u])
                  | None ->
                      raise
                        (ParseError_
                           ["(model2disproofstate) broken link ";
                            coordstring c; " to "; coordstring ch; " in ";
                            universestring "" u]))
             chs)
        ulist;
      Some
        (evaldisproofstate facts tree
           (Disproofstate
              {seq = seq; universe = mkmap ulist; tiles = tilesort tiles;
               selected = [minc]; conclusive = false; forcemap = newforcemap ();
               countermodel = false}))
let rec checkdisproof facts tree disproofopt =
  match model2disproofstate facts tree disproofopt with
    Some (Disproofstate {countermodel = countermodel}) -> countermodel
  | _ -> false
let rec disproof_start facts tree pathopt hyps =
  let rec replace_hyps s hyps =
    let (style, _, _, _, concs) = my_seqexplode s in
    mkSeq (style, hyps, concs)
  in
  let rec res s' =
    let (syn, _, hyps, _, concs) = my_seqexplode s' in
    let tiles = seq2tiles facts s' in
    let seq =
      match getsemanticturnstile syn with
        Some sem -> mkSeq (sem, hyps, concs)
      | None -> s'
    in
    evaldisproofstate facts tree
      (Disproofstate
         {seq = seq; universe = emptyworld (); tiles = tilesort tiles;
          selected = [0, 0]; conclusive = false; forcemap = newforcemap ();
          countermodel = false})
  in
  match pathopt with
    None ->
      let s = base_sequent tree in
      begin match hyps with
        [] -> res s
      | hs -> res (replace_hyps s hs)
      end
  | Some path ->
      let tree' = subtree tree path in
      let s' = base_sequent tree' in
      res (if null hyps then s' else replace_hyps s' hyps)
let rec newtile =
  fun (Disproofstate {tiles = tiles} as d) t ->
    let rec indivs t =
      let varstrings = List.map termstring (variables t) in
      let longest =
        foldl
          (fun long v ->
             if String.length v > String.length long then v else long)
          "" varstrings
      in
      let appendix = ref "" in
      let template =
        mapterm
          (fun t ->
             if not (bracketed t) && isVariable t then
               begin
                 appendix := !appendix ^ "'";
                 Some (parseTerm (longest ^ !appendix))
               end
             else None)
          t
      in
      template, variables template
    in
    let rec newtile v v' =
        (option_remapterm (( |-> ) (v, v')) t &~~
         (fun t' -> if member (t', tiles) then None else Some t'))
    in
    let rec newoccurrence v =
      let cs = explode (termstring v) in
      let ds = List.rev (takewhile isdigit (List.rev cs)) in
      let stem = implode (take (List.length cs - List.length ds) cs) in
      let stern = if null ds then 1 else atoi (implode ds) + 1 in
      let rec freshtile stem stern =
        let v' = parseTerm (stem ^ string_of_int stern) in
        (newtile v v'|~~ (fun _ -> freshtile stem (stern + 1)))
      in
      freshtile stem stern
    in
    let (template, tvs) = indivs t in
    let occs = isoccurrence <| tiles in
    let occvs = nj_fold (uncurry2 tmerge) (List.map variables occs) [] in
    if null tvs then None
    else
        ((if member (t, occs) then
            match variables t with
              [v] -> newoccurrence v
            | vs ->
                raise
                  (Catastrophe_
                     ["(newtile) occurrence tile "; termstring t;
                      " has termvars ";
                      bracketedliststring termstring "," vs])
          else
            let rec nlists a1 a2 =
              match a1, a2 with
                xs, 0 -> ([[]] : term list list)
              | (xs : term list), n ->
                 (let ys : term list list = nlists xs (n - 1) in
                   let rec insert a1 a2 =
                     match a1, a2 with
                       i, [] -> [[i]]
                     | i, j :: js ->
                         (i :: j :: js) ::
                           List.map (fun js -> j :: js) (insert i js)
                   in
                   nj_fold (fun (x, y) -> x @ y)
                      (nj_fold (fun (x, y) -> x @ y)
                         (List.map
                            (fun y ->
                               (List.map (fun x -> insert x y) xs : term list list list))
                            ys : term list list list list)
                         [])
                      [] : term list list)
            in
            let args = set (nlists occvs (List.length tvs)) in
            let possibles =
                (fun t -> not (member (t, tiles))) <|
                 List.map
                   (fun vs ->
                      _The
                        (option_remapterm (mkmap ((tvs ||| vs)))
                           template))
                   args
            in
            match possibles with
              [] ->
                showAlert
                  ["Can't make a new tile from "; termstring t;
                   ", because you can only use ";
                   liststring2 termstring ", " " and " occs];
                None
            | [p] -> Some p
            | _ ->
                try
                    (askChoice
                       ("Choose your new tile",
                        List.map (fun t -> [t])
                          (sort (<) (List.map termstring possibles))) 
                     &~~ (fun i -> Some (List.nth (possibles) (i))))
                with
                  Failure "nth" -> raise (Catastrophe_ ["(newtile) Failure \"nth\" ..."]))  
         &~~
         (fun tile ->
            Some (withdisprooftiles d (tilesort (tile :: tiles)))))
let rec addchild u (_, py as pc) (_, cy as cc) =
  if py >= cy then None
  else
    match at (u, pc) with
      None ->
        raise
          (Catastrophe_
             ["(addchild) no parent world at "; coordstring pc])
    | Some (pts, pcs) ->
        if member (cc, pcs) then None
        else
          (* already there *)
          let u' =
            ( ++ )
              (u,
               ( |-> ) (pc, (pts, (if py < cy then cc :: pcs else pcs))))
          in
          match at (u', cc) with
            None -> Some (( ++ ) (u', ( |-> ) (cc, (pts, []))))
          | Some _ -> Some (domono pts u' cc)
let rec deleteworld =
  fun (Disproofstate {universe = universe; selected = selected} as state)
    c ->
    match at (universe, c) with
      None -> None
    | Some _ ->
        (* a garbage-collection job! But actually we just leave the floaters floating ... *)
        let ws = listsub (fun (x, y) -> x = y) (dom universe) [c] in
        match ws with
          [] -> None
        | _ ->
            (* we don't delete the last world *)
            let rec doit w =
              let (ts, cs) = getworld universe w in
              w, (ts, listsub (fun (x, y) -> x = y) cs [w])
            in
            let u' = mkmap (List.map doit ws) in
            Some
              (withdisproofselected
                 (withdisproofuniverse state u')
                 (listsub (fun (x, y) -> x = y) selected [c]))
let rec worldselect =
  fun (Disproofstate {selected = selected} as d) cs ->
    if null (listsub (fun (x, y) -> x = y) selected cs) &&
       null (listsub (fun (x, y) -> x = y) cs selected)
    then
      None
    else Some (withdisproofselected d cs)
let rec addlink u from to__ =
  let (ts, cs) = getworld u from in
  if member (to__, cs) then u
  else ( ++ ) (u, ( |-> ) (from, (ts, to__ :: cs)))
let rec deletelink u from to__ =
  let (ts, cs) = getworld u from in
  if member (to__, cs) then
    ( ++ )
      (u, ( |-> ) (from, (ts, listsub (fun (x, y) -> x = y) cs [to__])))
  else u
let rec children u c = snd (getworld u c)
let rec parents u c = (fun p -> member (c, children u p)) <| dom u
let rec moveworld =
  fun (Disproofstate {universe = u; selected = sels} as d) from
    (_, toy as to__) ->
    if from = to__ then None
    else
      match at (u, from) with
        None ->
          raise
            (Catastrophe_
               ["(moveworld) no world at "; coordstring from; " in ";
                universestring "" u])
      | Some (fromts, fromcs) ->
          (* split all (c->from) contrary links to point parent->from and from->child *)
          let rec splitlink fromcs u (_, cy as c) =
            let rec linkparent u (_, py as p) =
              if py < toy then addlink u p from
              else foldl linkparent u (parents u p)
            in
            let rec linkchild u ch = addlink u from ch in
            if cy >= toy && member (from, children u c) then
              foldl linkchild
                (deletelink (foldl linkparent u (parents u c)) c from)
                fromcs
            else u
          in
          let u =
            foldl (splitlink fromcs) u
              (listsub (fun (x, y) -> x = y) (dom u) [from])
          in
          (* all (from->c) contrary links split into parent->c and c->child *)
          let rec splitlink2 fromps (u, cs) (_, cy as c) =
            if c = to__ then u, cs
            else if(* no self links, please *)  toy >= cy then
              let rec linkparent u p = addlink u p c in
              let rec linkchild u (_, chy as ch) =
                if chy < toy then addlink u c ch
                else foldl linkchild u (children u ch)
              in
              foldl linkchild (foldl linkparent u fromps) (children u c),
              cs
            else u, c :: cs
          in
          let (u, fromcs) =
            foldl (splitlink2 (parents u from)) (( -- ) (u, [from]), [])
              fromcs
          in
          let rec swing c = if c = from then to__ else c in
          let ulist = (fun (c, _) -> c <> from) <| aslist u in
          let u =
            match at (u, to__) with
              None ->
                (* it's a new position: swing links from->to *)
                ( ++ )
                  (mkmap
                     (List.map (fun (c, (ts, cs)) -> c, (ts, List.map swing cs))
                        ulist),
                   ( |-> ) (to__, (fromts, fromcs)))
            | Some (tots, tocs) ->
                (* it's already there; delete unnecessary ->from links and unite the worlds *)
                let u =
                  mkmap
                    (List.map
                       (fun (c, (ts, cs)) ->
                          c,
                          (ts,
                           (if member (to__, cs) then
                              (fun c -> c <> from) <| cs
                            else List.map swing cs)))
                       ulist)
                in
                domono fromts
                  (( ++ )
                     (u,
                      ( |-> )
                        (to__,
                         (tots,
                          tocs @
                            listsub (fun (x, y) -> x = y) fromcs tocs))))
                  to__
          in
          let sels =
            if member (to__, sels) then
              listsub (fun (x, y) -> x = y) sels [from]
            else List.map swing sels
          in
          Some (withdisproofselected (withdisproofuniverse d u) sels)
let rec showdisproof =
  fun
    (Disproofstate
       {seq = seq; universe = universe; tiles = tiles;
        selected = selected; forcemap = forcemap}) ->
    (*  val _ = consolereport["showdisproof: seq is ", smlseqstring seq, 
                          "\nforcemap is ", mappingstring (pairstring coordstring smltermstring ",") 
                                                          (catelim2stringfn catelim_forcedstring) forcemap
                     ]
              *)
    let root =
      match selected with
        [root] -> root
      | _ -> 0, 0
    in
    (* the value doesn't matter: forcemap will be empty *)
    let rec realterm t =
      match term2binding t with
        Some t' -> t'
      | _ -> t
    in
    let rec ivb t =
      let t = realterm t in
      (* consolereport ["ivb ", smltermstring t, " => ", 
           optionstring (catelim2stringfn catelim_forcedstring) (forcemap at (root,t))]; 
       *)
      try let (on, lock) = Hashtbl.find forcemap (root, t) in
          fst (if on then onbraket else offbraket) ^ (if lock then fst lockbraket else "")
      with Not_found -> fst outbraket
    in
    let rec ivk t =
      let t = realterm t in
      (* consolereport ["ivk ", smltermstring t, " => ", string_of_int (forced facts universe root t)]; *)
      try let (on, lock) = Hashtbl.find forcemap (root, t) in
          (if lock then snd lockbraket else "") ^ snd (if on then onbraket else offbraket)
      with Not_found -> snd outbraket
    in
    let (seqplan, seqbox) =
      makeseqplan (elementstring_invischoose ivb ivk) true origin seq
    in
    let seqsize = tbSize seqbox in
    let seqboxpos = pos (- (tsW seqsize / 2), - tsD seqsize) in
    drawindisproofpane ();
    setdisproofseqbox (textbox2box (tbOffset seqbox seqboxpos));
    seqdraw seqboxpos seqbox seqplan;
    setdisprooftiles (List.map termstring tiles);
    setdisproofworlds selected
      (List.map (fun (c, labels, ws) -> c, labelsort labels, ws)
         (universe2list universe));
    (* put the emphasis in *)
    List.iter
      (fun plan ->
         match planinfo plan with
           ElementClass (el, _) ->
             let emph =
               match element2term el with
                 None -> false
               | Some t ->
                   try let (v, _) = Hashtbl.find forcemap (root, t) in v
                   with Not_found -> false
             in
             emphasise (seqelementpos seqboxpos seqbox plan) emph
         | _ -> ())
      seqplan
let cleardisproof = clearDisproofPane
(* for export, with slightly altered semantics *)
let rec deletelink u from to__ =
  let (ts, cs) = getworld u from in
  if member (to__, cs) then
    Some
      (( ++ )
         (u,
          ( |-> ) (from, (ts, listsub (fun (x, y) -> x = y) cs [to__]))))
  else None
