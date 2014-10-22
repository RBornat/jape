(*
    $Id$

    Copyright (C) 2003-8 Richard Bornat & Bernard Sufrin
     
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


(* The disproof calculator.  Commenced 4.vii.01. RB *)
(* It implements disproof in Kripke semantics (made classical by restriction to a single
   world, if necessary).  Predicates operate only on individuals, mostly because I don't
   yet understand terms.  It seems reasonable to me that if there is a disproof, then there
   is one which only uses individuals, so I'm not too unhappy.
   RB 
 *)

open Box
open Draw
open Idclass
open Forcedef
open Invisibles
open Japeserver
open Listfuns
open Mappingfuns
open Optionfuns
open Predicate
open Seqdraw
open Sequent
open Sml
open Termfuns
open Termstore
open Termstring
open UTF

type forcedef  = Forcedef.forcedef
 and model     = Forcedef.model

let atoi = Miscellaneous.atoi
let askChoice = Alert.askChoice
let base_sequent = Prooftree.Tree.Fmttree.sequent
let catelim_string_of_seq = Sequent.catelim_string_of_seq
let consolereport = Miscellaneous.consolereport
let drawindisproofpane () = drawinpane Displayfont.DisproofPane
let getsemanticturnstile = Sequent.getsemanticturnstile
let idf = fun x -> x
let isdigit = isdigit
let isextensibleID = Symbol.isextensibleID
let lowercase = Stringfuns.lowercase
let matchdebug = Match.matchdebug
let matchvars = Match.matchvars 
let mkSeq = Sequent.mkSeq
let option_remapterm = Match.option_remapterm
let parseTerm = Termparse.term_of_string
let string_of_pair = Stringfuns.string_of_pair
let seqexplode = Sequent.seqexplode
let string_of_seq = Sequent.string_of_seq
let seqvars = Sequent.seqvars termvars tmerge
let showAlert = Alert.showAlert Alert.defaultseverity_alert <.> implode
let simplifySubst = Substmapfuns.simplifySubst
let debugstring_of_seq = Sequent.debugstring_of_seq
let subtree = Prooftree.Tree.Fmttree.followPath
let string_of_triple = Stringfuns.string_of_triple
let uncurry2 = Miscellaneous.uncurry2
let catelim_string_of_pair = Stringfuns.catelim_string_of_pair
let catelim_string_of_triple = Stringfuns.catelim_string_of_triple

let ask ss bs def = Alert.ask (Alert.defaultseverity bs) (implode ss) bs def

let onbraket = onbra_as_string, onket_as_string
let offbraket = offbra_as_string, offket_as_string
let outbraket = outbra_as_string, outket_as_string
let lockbraket = lockbra_as_string, lockket_as_string

let binding_of_term t =
  match Binding.bindingstructure t with
    Some t' -> Some (registerBinding t')
  | None    -> None

exception Catastrophe_ = Miscellaneous.Catastrophe_
exception ParseError_ = Miscellaneous.ParseError_
exception Tacastrophe_ = Miscellaneous.Tacastrophe_

let rec catelim_string_of_int i ss = string_of_int i :: ss
let rec catelim_string_of_bool b ss = string_of_bool b :: ss

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
      ["matchit (disproof) matching "; string_of_term pat; " ";
       bracketedstring_of_list
         (fun v ->
            string_of_pair string_of_term (string_of_idclass <.> idclass)
              "," (v, v))
         ", " vs;
       " against "; string_of_term t; " => ";
       string_of_option (string_of_mapping string_of_term string_of_term) res];
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

(* nowadays worlds are also coloured, if the user provides some selections of interest, to
 * say if all those selections are forced at the world or not
 *)

type disproofemphasis = Forced | Unforced | Irrelevant

let stringofdisproofemphasis e =
  match e with
    Forced     -> "Forced"
  | Unforced   -> "Unforced"
  | Irrelevant -> "Irrelevant"
  
let intofdisproofemphasis e =
  match e with
    Irrelevant -> 0
  | Unforced   -> 1
  | Forced     -> 2

type term = Termtype.term
type coord = int * int
type world = disproofemphasis * (disproofemphasis * term) list * coord list
          (* colour,            labels,                          children *)

let emphasisofworld = fst_of_3
let labelsofworld   = snd_of_3
let childrenofworld = thrd

type universe = (coord, world) mapping

let catelim_string_of_emph = catelim_of_stringfn stringofdisproofemphasis

let catelim_string_of_coord =
  catelim_string_of_pair catelim_string_of_int catelim_string_of_int ","

let catelim_string_of_label =
  catelim_string_of_pair catelim_string_of_emph catelim_string_of_term ","
  
let rec string_of_coord c = implode (catelim_string_of_coord c [])

let catelim_string_of_world =
  catelim_string_of_triple 
    catelim_string_of_emph
    (catelim_bracketedstring_of_list catelim_string_of_label ",")
    (catelim_bracketedstring_of_list catelim_string_of_coord ",") ", "

let rec string_of_world w = implode (catelim_string_of_world w [])

let rec catelim_string_of_universe sep u ss =
  "Universe " ::
    catelim_string_of_mapping catelim_string_of_coord catelim_string_of_world
      ("++" ^ sep) u ss

let rec string_of_universe sep u = implode (catelim_string_of_universe sep u [])

let rec list_of_universe u = List.map (fun (c, (e, ts, cs)) -> c, e, ts, cs) (aslist u)

let rec simplestuniverse () = ((0, 0) |-> (Unforced, [], []))

let rec issimplestuniverse u =
  match ran u with
    [(_, [], [])] -> true
  | _             -> false

let getworld u c =
  try _The ((u <@> c)) with
    _ -> raise (Catastrophe_ ["(Disproof.getworld) no world at "; string_of_coord c; 
                              "; "; string_of_universe "" u])

(* world-changing operations don't preserve emphasis *)

let eqlabels ((_,t1),(_,t2)) = eqterms (t1,t2)

let rec domono pts u c =
  let (_, ts, cs) = getworld u c in
  match listsub eqlabels pts ts with
    []  -> u
  | ts' -> foldl (domono ts') ((u ++ (c |-> (Unforced, ts' @ ts, cs)))) cs

let label_member ((_,t),ls) = member (t,List.map snd ls)

let rec addworldlabel u c t =
  match (u <@> c) with
    None ->
      raise
        (Tacastrophe_ ["(addworldlabel) no world at "; string_of_coord c; "; "; string_of_universe "" u])
  | Some (_, pts, pcs) ->
      let l = Unforced,t in
      if label_member (l, pts) then None
      else Some (foldl (domono [l]) ((u ++ (c |-> (Unforced, l :: pts, pcs)))) pcs)

let rec deleteworldlabel u c t =
  match (u <@> c) with
    None ->
      raise
        (Tacastrophe_ ["(deleteworldlabel) no world at "; string_of_coord c; "; "; string_of_universe "" u])
  | Some (_, pts, pcs) ->
      let l = Unforced,t in
      if not (label_member (l, pts)) then None
      else
        let rec islabelledparent u c c' =
          let (_, ts, cs) = getworld u c' in member (c, cs) && label_member (l, ts)
        in
        let rec doself u c =
          let (_, ts, cs) = getworld u c in
          if label_member (l, ts) then
            (u ++ (c |-> (Unforced, listsub eqlabels ts [l], cs)))
          else u
        in
        let rec doparents u c =
          foldl doparents (doself u c)
            (islabelledparent u c <| dom u)
        in
        Some (doparents u c)

let moveworldlabel u w c t =
  deleteworldlabel u w t &~~ (fun u' -> addworldlabel u' c t)
  
(* *********************** semantics *********************** *)

type definition = term * term list * bool * forcedef
(* lhs, args, hassubst, rhs *)
   
(* for 'occurrence' read 'individual', I think: things like actual i and so on *)

let forcedefs : definition list ref = ref []

let occurrences : (term * term list) list ref = ref [] (* occurrence forms *)

let string_of_occurrences = 
  bracketedstring_of_list 
    (string_of_pair string_of_term (bracketedstring_of_list string_of_term ";") ",")
    "; "
    
let rec isoccurrence t =
  List.exists
    (bool_of_opt <.> (fun (occ, occvs) -> matchit occ occvs t))
    !occurrences

let rec newoccurrence v =
  match (function | t, [_] -> true
                  | _      -> false
        ) <| !occurrences
  with
  | (occ, [occv]) :: _ ->
      _The (option_remapterm (occv |-> v) occ)
  | _ ->
      raise (Catastrophe_ ["(newoccurrence) no single-variable occurrence forms recorded"])

(* filters out previous versions of t -- so we only keep actual i, for example,
   instead of actual i, actual j, actual k etc.
 *)
let rec nodups t vs patf xs =
  t, (not <.> bool_of_opt <.> matchit t vs <.> patf) <| xs

let rec newocc t vs os =
  let rec patf (t, vs) = t in
  let (t, os') = nodups t vs patf os in (t, vs) :: os'

let rec variables t = isVariable <| termvars t

let rec findoccs f os =
  match f with
    ForceAlways          -> os
  | ForceNever           -> os
  | ForcePrim _          -> os
  | ForceBoth (f1, f2)   -> findoccs f1 (findoccs f2 os)
  | ForceEither (f1, f2) -> findoccs f1 (findoccs f2 os)
  | ForceIf (f1, f2)     -> findoccs f1 (findoccs f2 os)
  | ForceEverywhere f    -> findoccs f os
  | ForceNowhere f       -> findoccs f os
  | ForceAll (t, vs, f)  -> findoccs f (newocc t vs os)
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
                  string_of_term t; ", predicate "; string_of_term _P;
                  " doesn't have bound variables as arguments"])
    in
    let env = mkmap (List.map pvars pbs) in
    let comp = compilepredicate notabst (fun v -> (env <@> v)) in
    let (t, fd) = mapterm comp t, mapforcedefterms comp fd in
    let vs = patvars t in
    let (t, fds) = nodups t vs (fun (t', _, _, _) -> t') !forcedefs in
    let hassubst = bool_of_opt <.> findinforcedef decodeSubst in
    forcedefs := (t, vs, hassubst fd, fd) :: fds;
    occurrences := findoccs fd !occurrences
  with
    Predicate_ ss -> raise (ParseError_ ss)

let rec clearforcedefs () = forcedefs := []; occurrences := []

let rec hasforcedefs () = not (null !forcedefs)

(* matching a formula against the definitions *)

let rec semantics facts t =
  findfirst
    (fun (pat, vs, hassubst, fd) -> (matchit pat vs t &~~ (fun env -> Some (env, hassubst, fd))))
    !forcedefs
  &~~
  (fun (env, hassubst, fd) ->
     let fd' = mapforcedefterms (option_remapterm env) (uniquebinders t fd) in
     Some (if hassubst then mapforcedefterms (dosubst facts) fd' else fd'))
  
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
       else ((ocv |-> registerId (ocvid', VariableClass)) ++ env))
    in
    let r = snd (foldl newvar ([], empty) ocvs) in
    if !disproofdebug then
      consolereport
        ["semantics matched "; string_of_term t; " and "; string_of_forcedef fd;
         "; rewrite with "; string_of_mapping string_of_term string_of_term r];
    r
  in
  let rec doit ocvs =
    mapforcedefterms (option_remapterm (newoccenv ocvs)) fd
  in
  match fd with
    ForceAll (_, ocvs, _)  -> doit ocvs
  | ForceSome (_, ocvs, _) -> doit ocvs
  | _                      -> fd
  
(* finding the subformulae which aren't semantically defined. Designed to be foldl'd *)

let rec semanticfringe facts ts t =
  (* for efficiency's sake, tf does it the wrong way round: a failing search backed up
     with a check for bracketing. But I guess that one day somebody might say that 
     brackets mean something. If so, then aargghh, because all of Jape assumes the
     opposite. Still, the wrong way round is the way it does it for now.
   *)
  let rec tf ts t =
    match semantics facts t with
      None    -> (match decodeBracketed t with
                    Some t' -> tf ts t'
                  | None    -> t :: ts)
    | Some fd -> ff ts fd
  and ff ts fd =
    match fd with
      ForceAlways          -> ts
    | ForceNever           -> ts
    | ForcePrim       t    -> tf ts t
    | ForceBoth       pair -> pf ts pair
    | ForceEither     pair -> pf ts pair
    | ForceIf         pair -> pf ts pair
    | ForceEverywhere fd   -> ff ts fd
    | ForceNowhere    fd   -> ff ts fd
    | ForceAll  (t, _, fd) -> ff (t :: ts) fd
    | ForceSome (t, _, fd) -> ff (t :: ts) fd
  and pf ts (fd1, fd2) = ff (ff ts fd1) fd2 in
  tf ts t

(* This, I think, instantiates a binding fd if i matches the 'individual' pattern oc *)
let rec indiv_fd facts (oc, vs, fd) i =
  (* fun remap env t = (* written with andthenr, ortryr, because I don't trust 0.93 with the functional version *)
                         option_remapterm env t  &~~ 
                         (fn t' => dosubst facts t' |~~ (fn () => Some t'))
                        *)
  let r =
       matchit oc vs i &~~
       (fun env ->
          Some (mapforcedefterms (option_remapterm env &~ somef (dosubst facts)) fd))
  in
  if !disproofdebug then begin
      consolereport
        ["indiv_fd ";
         string_of_triple string_of_term (bracketedstring_of_list string_of_term ",")
           string_of_forcedef "," (oc, vs, fd);
         " "; string_of_term i; " => "; string_of_option string_of_forcedef r];
      consolereport
        ["matchit oc vs i => ";
         string_of_option (string_of_mapping string_of_term string_of_term)
           (matchit oc vs i)];
      match matchit oc vs i with
        None     -> ()
      | Some env ->
          consolereport
            ["mapforcedefterms (option_remapterm env) fd => ";
             string_of_forcedef
               (mapforcedefterms (option_remapterm env) fd)];
          consolereport
            ["mapforcedefterms (option_remapterm env  &~ (somef (dosubst facts))) fd => ";
             string_of_forcedef
               (mapforcedefterms
                  (
                     (option_remapterm env &~ somef (dosubst facts)))
                  fd)]
  end;
  r
    

(* is it forced? *)
(* rewritten to be memoised, both for 'efficiency' and for de-emphasising irrelevant subformulae
   -- i.e. quantified subformulae. Should also be useful when it comes to highlighting worlds 
   which force a formula.
 *)

let rec unfixedforced facts u =
  let rec ff f (c, t) =
    let labelterms = List.map snd <.> labelsofworld <.> getworld u in
    let children = childrenofworld <.> getworld u in
    let rec lookup c t = member (t, labelterms c), true in
    (* result is (forced, locked) *)
    (* forced logic -- we force evaluation of subformulae because otherwise things go grey 
       which shouldn't.
     *)
    let rec logNot =
      function
        true, _ -> false, false
      | _       -> true , false
    in
    let rec logAnd a1 a2 =
      match a1, a2 with
        (true, _), (true, _) -> true , false
      | _        , _         -> false, false
    in
    let rec logOr a1 a2 =
      match a1, a2 with
        (false, _), (false, _) -> false, false
      | _         , _          -> true , false
    in
    let rec logImp a1 a2 =
      match a1, a2 with
        (true, _), (false, _) -> false, false
      | _        , _          -> true , false
    in 
    let rec logAll f = foldl logAnd (true, false) <.> List.map f in
    let rec logExists f = foldl logOr (false, false) <.> List.map f in
    
    let rec interp fd c =
      match fd with
        ForceAlways            -> true, false
      | ForceNever             -> false, false
      | ForcePrim t'           -> f (c, t')
      | ForceBoth (fd1, fd2)   -> logAnd (interp fd1 c) (interp fd2 c)
      | ForceEither (fd1, fd2) -> logOr (interp fd1 c) (interp fd2 c)
      | ForceIf (fd1, fd2)     -> logImp (interp fd1 c) (interp fd2 c)
      | ForceEverywhere fd'    -> logAnd (interp fd' c) (logAll (interp fd) (children c))
      | ForceNowhere fd'       -> logAnd (logNot (interp fd' c)) (logAll (interp fd) (children c))
      | ForceAll tvsfd         -> logAll (fun lab ->
                                            match indiv_fd facts tvsfd lab with
                                              Some fd' -> interp fd' c
                                            | None     -> true, false)
                                         (labelterms c)
      | ForceSome tvsfd        -> logExists (fun lab ->
                                               match indiv_fd facts tvsfd lab with
                                                 Some fd' -> interp fd' c
                                               | None     -> false, false)
                                            (labelterms c)
    in
    if !disproofdebug then
      consolereport ["evaluating "; string_of_coord c; "; "; string_of_term t];
    let result =
      match semantics facts t with
        None    -> (match decodeBracketed t with
                     Some t' -> f (c, t')
                   | None    -> lookup c t)
      | Some fd -> interp fd c
    in
    if !disproofdebug then
      consolereport
        ["forced ("; string_of_coord c; ", "; string_of_term t; ") => ";
         string_of_pair string_of_bool string_of_bool "," result];
    result
  in
  ff

let rec seq_forced forced c s =
  let rec doit e = match term_of_element e with
                     None   -> false, false
                   | Some t -> forced (c, t)
  in
  let (_, _, hyps, _, concs) = my_seqexplode s in
  List.map doit hyps, List.map doit concs

(* *********************** interaction states *********************** *)

type seq = Seqtype.seq

type disproofstaterec =
      { seq          : seq; 
        selections   : pos list                 (* element selection coordinates *) *
                       (pos * string list) list (* text selection coordinates and description *);
        seqplan      : (planclass plan list * textbox) option;
        universe     : universe; 
        tiles        : term list;
        selected     : coord list;
        forcemap     : ((coord * term), (bool * bool)) Hashtbl.t;
        conclusive   : bool; 
        countermodel : bool }

(* we don't store a forced (coord * term -> bool * bool) function in the state rec
 * because, rather niftily, forcemap is being used for two separate purposes. One is to 
 * memofun-optimise the calculation of forcing; the other is to check if a formula's
 * force-state has ever been investigated. The body of a quantifier like All x.(P(x)->Q(x)),
 * for example, will never be evaluated -- though P(i)->Q(i), P(j)->Q(j), etc., will be.
 *
 * The map gives three-valued results; a forced function can only give two.
 *
 * But we use forcemap via memofix (see evaldisproofstate) to tint the universe. Provided we 
 * never allow text-selection of an irrelevant formula, no harm will come (I hope).
 *)
 
type disproofstate = Disproofstate of disproofstaterec

let catelim_string_of_forced =
  catelim_string_of_pair catelim_string_of_bool catelim_string_of_bool ","

let rec catelim_string_of_Hashtbl astring bstring sepstr mapstr store ss =
  let mapping = Hashtbl.fold (fun k v kvs -> (k,v)::kvs) store [] in
  "<<:" ::
    catelim_string_of_list
      (fun (a, b) ss -> "(" :: astring a (sepstr :: bstring b (")" :: ss)))
      sepstr mapping (":>>" :: ss)

let rec _Hashtblstring a b =
  stringfn_of_catelim
    (catelim_string_of_Hashtbl (catelim_of_stringfn a) (catelim_of_stringfn b) "+:+" "|:->")

let rec catelim_string_of_disproofstate =
  fun
    (Disproofstate
       {seq = seq; universe = universe; selected = selected; tiles = tiles;
        forcemap = forcemap; conclusive = conclusive; countermodel = countermodel})
    ss ->
    (* unused let cbs = catelim_bracketedstring_of_list catelim_string_of_bool "," in *)
    "Disproofstate{seq = " ::
      catelim_string_of_seq seq
        (",\nworld = " ::
           catelim_string_of_universe "" universe
             (",\nselected = " ::
                catelim_bracketedstring_of_list catelim_string_of_coord ","
                  selected
                  (",\ntiles = " ::
                     catelim_bracketedstring_of_list catelim_string_of_term ","
                       tiles
                       (", forcemap = " ::
                          catelim_string_of_Hashtbl
                            (catelim_string_of_pair catelim_string_of_coord
                               catelim_string_of_term ",")
                            catelim_string_of_forced "+:+" "|:->" forcemap
                            (", conclusive = " ::
                               catelim_string_of_bool conclusive
                                 (", countermodel = " ::
                                    catelim_string_of_bool countermodel
                                      ("}" :: ss)))))))

let rec string_of_disproofstate d = implode (catelim_string_of_disproofstate d [])

let rec disproofstate_seq = fun (Disproofstate {seq = seq}) -> seq

let rec disproofstate_selections = fun (Disproofstate {selections = selections}) -> selections

let rec disproofstate_universe =
  fun (Disproofstate {universe = universe}) -> universe

let rec disproofstate_selected =
  fun (Disproofstate {selected = selected}) -> selected

let rec disproofstate_conclusive =
  fun (Disproofstate {conclusive = conclusive}) -> conclusive

let rec disproofstate_countermodel =
  fun (Disproofstate {countermodel = countermodel}) -> countermodel

(* because of the need for facts, these don't evaluate! *)

let rec withdisproofselections (Disproofstate s) selections =
    Disproofstate {s with selections = selections}

let rec withdisproofuniverse (Disproofstate s) universe =
    Disproofstate {s with universe = universe}

let rec withdisproofselected (Disproofstate s) selected =
    Disproofstate {s with selected = selected}

let rec withdisprooftiles (Disproofstate s) tiles =
    Disproofstate {s with tiles = tiles}
    
let rec disproof_minimal =
  function
    None -> true
  | Some (Disproofstate {universe = universe}) -> issimplestuniverse universe

let newforcemap () = Hashtbl.create 17 

let newforcefun forcemap facts universe = Fix.memofix forcemap (unfixedforced facts universe)

let rec evaldisproofstate facts tree =
  fun (Disproofstate {seq = seq; selections = selections; universe = universe; selected = selected; tiles = tiles}) ->
    let (basestyle, hypsbag, basehyps, concsbag, baseconcs) = my_seqexplode (base_sequent tree) in
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
    let forcemap = newforcemap () in
    (* fun mf a v = string_of_pair string_of_coord debugstring_of_term "," a^"|->"^(stringfn_of_catelim catelim_string_of_forced) v *)
    let forced = newforcefun forcemap facts universe in
    let countermodel =
      if null selected then false else
        (* evaluate everything everywhere -- no short cuts *)
        (let results = List.map (fun root -> let (hs, cs) = seq_forced forced root seq in
                                             all fst hs && not (List.exists fst cs)
                                ) selected
         in
         all idf results)
    in
    let rec realterm t =
      match binding_of_term t with
        Some t' -> t'
      | _       -> t
    in
    let rec ivb t =
      let t = realterm t in
      (* consolereport ["ivb ", debugstring_of_term t, " => ", 
           string_of_option (stringfn_of_catelim catelim_string_of_forced) (forcemap (root <@> t))]; 
       *)
      try let results = List.map (fun root -> Hashtbl.find forcemap (root, t)) selected in
          fst (if all fst results then onbraket else offbraket) ^ (if List.exists snd results then fst lockbraket else "")
      with Not_found -> fst outbraket
    in
    let rec ivk t =
      let t = realterm t in
      (* consolereport ["ivk ", debugstring_of_term t, " => ", string_of_int (forced facts universe root t)]; *)
      try let results = List.map (fun root -> Hashtbl.find forcemap (root, t)) selected in
          (if List.exists snd results then snd lockbraket else "") ^ snd (if all fst results then onbraket else offbraket)
      with Not_found -> snd outbraket
    in
    let seqplan = makeseqplan 0 (chooseinvisbracketedstring_of_element ivb ivk) true origin seq 
    in
    Disproofstate
      {seq = seq; selections = selections; seqplan = Some seqplan;
       universe = tint_universe facts forced seqplan selections universe; 
       tiles = tiles; selected = selected;
       forcemap = forcemap; conclusive = conclusive; countermodel = countermodel}

and tint_universe facts forced (plan, _) (proofsels, textsels) =
  let selections =
     foldr (fun p ts ->
              match findfirstplanhit p plan &~~ 
                    (fun plan -> match info_of_plan plan with
                       ElementClass (e,_) -> Some e
                     | _ -> None) &~~ term_of_element
              with
                Some t -> t :: ts
              | _      -> raise (Catastrophe_ 
                                   ["Disproof.tint_universe can't find element "; string_of_pos p; " in "; 
                                    bracketedstring_of_list (debugstring_of_plan string_of_planclass) ";" plan])
           ) 
           (foldr (fun (_,ss) ts ->
                     let rec tsel sels =
                       match sels with
                         [_;t;_ ]      -> Termparse.term_of_string t :: ts
                       | _::t::s::sels -> Termparse.term_of_string t :: tsel (s::sels)
                       | []            -> ts
                       | _  -> raise (Catastrophe_ 
                                        ["Disproof.tint_universe can't understand text selection "; 
                                                    bracketedstring_of_list Stringfuns.enQuote ";" ss])
                     in tsel ss
                  ) [] textsels)
           proofsels
  in
  (* we need to recognise selections which are 'instantiable'; bindersels is supposed to
     list them all in naked form. Well, ho hum ... it works for the only example I have
     now, but obviously I can't like that.
     But there is a rationale to it: if the definition either is, or directly involves,
     a binding definition then take it. So EVERYWHERE (ALL ...) works, as it should, and
     NOWHERE(PRIM (All x ...)) doesn't, as it shouldn't.
     Hmmm.
     RB
   *)
  let bindersels =
    optionfilter (fun t -> let rec isbinder fd =
                             match fd with
                               ForceAlways         -> None
                             | ForceNever          -> None
                             | ForcePrim _         -> None
                             | ForceBoth _         -> None
                             | ForceEither _       -> None
                             | ForceIf _           -> None
                             | ForceEverywhere fd' -> isbinder fd' (* this happens to work for I2L_disproof.j All x.P(x), which is what I want *)
                             | ForceNowhere fd'    -> isbinder fd' (* this happens not to work for I2L_disproof.j not(Exists x.P(x)), which is what I want *)
                             | ForceAll tvsfd      -> Some tvsfd
                             | ForceSome tvsfd     -> Some tvsfd
                           in 
                           semantics facts t &~~ isbinder
                 ) selections
  in
  let do_world = tint_world facts forced selections bindersels in
  mkmap <.> List.map (fun (c,w) -> (c, do_world c w)) <.> aslist

and tint_world facts forced selections bindersels c (e, ls, chs) = 
  let do_label = tint_label facts forced bindersels c in
  ((if all1 (fun t -> fst (forced (c,t))) selections then Forced else Unforced),
   List.map do_label ls, chs)

and tint_label facts forced bindersels c (_,t) = 
  ((if all1 (fun bsel -> match indiv_fd facts bsel t &~~ term_of_forcedef with
                            Some t' -> fst (forced (c,t'))
                          | None    -> false
             ) bindersels 
    then Forced else Unforced),
   t) 

exception Disproof_ of string list

(* sort, eliminating duplicates.  This sort is no longer case sensitive *)
let rec alphasort render sel ts =
  List.map sel
    (sortunique (fun (s1, _) (s2, _) -> lowercase s1 < lowercase s2)
                (List.map (fun t -> render t,t) ts))

let tilesort = List.rev <.> alphasort string_of_term snd

let labelsort = alphasort (fun (_,t) ->string_of_term t) snd 

let rec tiles_of_seq facts seq =
  let (_, _, hyps, _, concs) = my_seqexplode seq in
  (* check for an absence of silliness *)
  let rec existselement f e =
    match term_of_element e with
      None -> false
    | Some t -> existsterm f t
  in
  if List.exists
       (fun e ->
          if issegvar e then
            raise
              (Disproof_
                 ["Forcing semantics can only deal with actual formulae in sequents.";
                  " Segment variables (e.g. "; string_of_element e;
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
                   ["Unknowns (e.g. "; string_of_term t; " in sequent ";
                    string_of_seq seq; ") make disproof somewhat tricky.  Try again when you have resolved the ";
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
                     ["Substitutions (e.g. "; string_of_term t;
                      " in sequent "; string_of_seq seq; ") aren't part of the forcing semantics.  Try again when you have ";
                      " simplified the substitution(s)."])
            | _ -> false))
      (concs @ hyps)
  then
    ();
  (* find the fringe *)
  let hypterms = optionfilter term_of_element hyps in
  let concterms = optionfilter term_of_element concs in
  let ts = foldl (semanticfringe facts)
             (foldl (semanticfringe facts) [] hypterms) concterms
  in
  (* add occurrence formulae if necessary *)
  let ts =
    (if List.exists (bool_of_opt <.> decodeBinding) hypterms ||
        List.exists (bool_of_opt <.> decodeBinding) concterms
     then
       List.map fst 
             ((fun (occ, vs) ->
                not (List.exists (bool_of_opt <.> (matchit occ vs)) ts)) <| !occurrences)
     else []) @ ts
  in
  (* eliminate all individuals which don't appear free in the sequent, 
   * but if there aren't any such individuals, keep just one
   *)
  let svs = seqvars seq in
  match isVariable <| nj_fold (uncurry2 tmerge) (List.map patvars ts) [] with
    []  -> ts
  | tvs ->
      let tvs' =
        match (fun i -> member (i, svs)) <| tvs with
          [] -> [List.hd tvs]
        | vs -> vs
      in
      let badvs = listsub (fun (x,y) -> x=y) tvs tvs' in
      let rec changevars i =
        if member (i, badvs) then Some (List.hd tvs') else None
      in
      List.map (mapterm changevars) ts

let rec model_of_disproofstate =
  function
    None -> None
  | Some (Disproofstate {seq = seq; universe = universe}) ->
      Some
        (seq,
         Model
           (List.map (fun (c, (_, ts, chs)) -> World (Coord c, List.map (fun v->Coord v) chs, List.map snd ts))
              (aslist universe)))

let rec disproofstate_of_model facts tree disproofopt =
  match disproofopt with
    None              -> None
  | Some (seq, model) ->
      let rec coord = fun (Coord c) -> c in
      let (Model worlds) = model in
      let ulist =
        List.map (fun (World (c, chs, ts)) -> coord c, (Unforced, List.map (fun t -> (Unforced,t)) ts, List.map coord chs))
          worlds
      in
      let utiles = nj_fold (fun (x, y) -> x @ y) (List.map (List.map snd <.> labelsofworld <.> snd) ulist) [] in
      let uvars = nj_fold (uncurry2 tmerge) (List.map variables utiles) [] in
      let tiles = List.map newoccurrence uvars @ tiles_of_seq facts seq @ utiles in
      let minc =
        foldr
          (fun ((_, cy as c), _) (_, miny as minc) ->
             if cy < miny then c else minc)
          (0, 0) ulist
      in
      let u = mkmap ulist in
      (* unused let ws = List.map fst ulist in *)
      List.iter
        (fun ((_, cy as c), (_, ts, chs)) ->
           List.iter
             (fun (_, chy as ch) ->
                if chy <= cy then
                  raise
                    (ParseError_
                       ["(disproofstate_of_model) non-ascending link ";
                        string_of_coord c; " to "; string_of_coord ch; " in ";
                        string_of_universe "" u])
                else
                  match (u <@> ch) with
                    Some (_, chts, _) ->
                      if not
                           (null (listsub (fun (x,y) -> x=y) ts chts))
                      then
                        raise
                          (ParseError_
                             ["(disproofstate_of_model) non-monotonic link ";
                              string_of_coord c; " to "; string_of_coord ch;
                              " in "; string_of_universe "" u])
                  | None ->
                      raise
                        (ParseError_
                           ["(disproofstate_of_model) broken link ";
                            string_of_coord c; " to "; string_of_coord ch; " in ";
                            string_of_universe "" u]))
             chs)
        ulist;
      Some
        (evaldisproofstate facts tree
           (Disproofstate
              {seq = seq; selections = ([],[]); seqplan = None;
               universe = mkmap ulist; tiles = tilesort tiles;
               selected = [minc]; conclusive = false; forcemap = newforcemap ();
               countermodel = false}))

let rec checkdisproof facts tree disproofopt =
  match disproofstate_of_model facts tree disproofopt with
    Some (Disproofstate {countermodel = countermodel}) -> countermodel
  | _ -> false

let rec disproof_start facts tree pathopt hyps =
  let rec replace_hyps s hyps =
    let (style, _, _, _, concs) = my_seqexplode s in
    mkSeq (style, hyps, concs)
  in
  let rec res s' =
    let (syn, _, hyps, _, concs) = my_seqexplode s' in
    let tiles = tiles_of_seq facts s' in
    let seq =
      match getsemanticturnstile syn with
        Some sem -> mkSeq (sem, hyps, concs)
      | None -> s'
    in
    evaldisproofstate facts tree
      (Disproofstate
         {seq = seq; selections = ([],[]); seqplan = None; 
          universe = simplestuniverse (); tiles = tilesort tiles;
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
    let indivs t =
      let varstrings = List.map string_of_term (variables t) in
      let longest =
        foldl (fun long v -> if String.length v > String.length long then v else long)
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
        (option_remapterm ((v |-> v')) t &~~
         (fun t' -> if member (t', tiles) then None else Some t'))
    in
    let rec newoccurrence v =
      let cs = utf8_explode (string_of_term v) in
      let ds = List.rev (takewhile isdigit (List.rev cs)) in
      let stem = utf8_implode (take (List.length cs - List.length ds) cs) in
      let stern = if null ds then 1 else atoi (utf8_implode ds) + 1 in
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
                     ["(newtile) occurrence tile "; string_of_term t;
                      " has termvars ";
                      bracketedstring_of_list string_of_term "," vs])
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
                  ["Can't make a new tile from "; string_of_term t;
                   ", because you can only use ";
                   sentencestring_of_list string_of_term ", " " and " occs];
                None
            | [p] -> Some p
            | _ -> askChoice
                       ("Choose your new tile",
                        List.map (fun t -> [t])
                          (sort (<) (List.map string_of_term possibles))) 
                     &~~ (fun i -> Some (try List.nth possibles i with
                                                                Invalid_argument "List.nth" | Failure "nth" -> 
                                                                                        raise (Catastrophe_ ["(newtile) nth [";
                                                                                                        string_of_list string_of_term ";" possibles;
                                                                                                        "] "; string_of_int i]))))
         &~~
         (fun tile ->
            Some (withdisprooftiles d (tilesort (tile :: tiles)))))

let rec addlink u (_, fromy as from) (_, to__y as to__) =
  let (_, ts, cs) = getworld u from in
  if to__y<=fromy then u else
  if member (to__, cs) then u else 
  domono ts (u ++ (from |-> (Unforced, ts, to__ :: cs))) to__

let rec deletelink u from to__ =
  let (_, ts, cs) = getworld u from in
  if member (to__, cs) then
    (u ++ (from |-> (Unforced, ts, listsub (fun (x,y) -> x=y) cs [to__])))
  else u

let spliceworldtolink u w lfrom lto__ =
  let u = deletelink u lfrom lto__ in
  let u = addlink u lfrom w in
  addlink u w lto__

let addchild u (_, py as pc) (_, cy as cc) =
  let (_, pts, pcs) = getworld u pc in
  if member (cc, pcs) then None (* already there *)
  else
    let u = if py<cy then u ++ (pc |-> (Unforced, pts, cc::pcs)) else u in
    match u <@> cc with
      None   -> Some (u ++ (cc |-> (Unforced, pts, [])))
    | Some _ -> Some (domono pts u cc)

let addchildtolink u parent child lfrom lto__ =
  addchild u parent child &~~ (fun u -> Some (spliceworldtolink u child lfrom lto__))
  
let deleteworld =
  fun (Disproofstate {universe = universe; selected = selected} as state) c ->
    (* consolereport ["deleteworld "; string_of_universe "" universe; " "; string_of_coord c]; *)
    match universe <@> c with
      None   -> None (* it wasn't there *)
    | Some _ ->
        (* a garbage-collection job! But actually we just leave the floaters floating ... *)
        let ws = listsub (fun (x,y) -> x=y) (dom universe) [c] in
        (* consolereport ["dom universe := "; bracketedstring_of_list string_of_coord ";" ws]; *)
        match ws with
          [] -> None (* we don't delete the last world *)
        | _  -> let rec doit w =
                  (* consolereport [string_of_coord w; " = "; string_of_world (getworld universe w)]; *)
                  let (_, ts, cs) = getworld universe w in
                  let cs' = listsub (fun (x,y) -> x=y) cs [c] in
                  (* consolereport [" => "; bracketedstring_of_list string_of_coord ";" cs']; *)
                  w, (Unforced, ts, cs')
                in
                let u' = mkmap (List.map doit ws) in
                Some
                  (withdisproofselected
                     (withdisproofuniverse state u')
                     (listsub (fun (x,y) -> x=y) selected [c]))

let rec worldselect =
  fun (Disproofstate {selected = selected} as d) cs ->
    if null (listsub (fun (x,y) -> x=y) selected cs) &&
       null (listsub (fun (x,y) -> x=y) cs selected)
    then
      None
    else Some (withdisproofselected d cs)

let children u = childrenofworld <.> getworld u

let rec parents u c = (fun p -> member (c, children u p)) <| dom u

let moveworld (Disproofstate {universe = u; selected = sels} as d) from (_, toy as to__) =
  if from = to__ then None else
  let (_, fromts, fromcs) = getworld u from in
  (* I used to remake the diagram in various ways; 
     now you just get what you see, and you can repair as necessary 
   *)
  (* delete all to__->child contrary links; don't point to yourself *)
  let fromcs = (fun (_, cy) -> toy<cy) <| (listsub (fun (x,y) -> x=y) fromcs [to__]) in
  (* delete parent->to__ contrary links; change parent->from links into parent->to__ links *)
  let switch w = if w=from then to__ else w in
  let swing ws = if member (to__, ws) then listsub (fun (x,y) -> x=y) ws [from] 
                 else List.map switch ws in
  let clean (_, wy as w) =
    let (_, ts, cs) = getworld u w in
    w, (Unforced, ts, (if wy<toy then swing cs else listsub (fun (x,y) -> x=y) cs [from]))
  in
  let u = mkmap (List.map clean (listsub (fun (x,y) -> x=y) (dom u) [from])) in
  let u = match u <@> to__ with
            None -> (* easy, no world at this position *) 
              u++(to__|->(Unforced, fromts, fromcs))
          | Some (_, to__ts, to__cs) -> (* harder! unite the worlds *)
              (* union of the children *)
              let to__cs = listsub (fun (x,y) -> x=y) fromcs to__cs @ to__cs in
              (* and all the labels in from *)
              domono fromts (u++(to__|->(Unforced,to__ts,to__cs))) to__
  in
  Some (withdisproofselected (withdisproofuniverse d u) (swing sels))
  
  (* this is what it used to say ...
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
            (listsub (fun (x,y) -> x=y) (dom u) [from])
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
          foldl (splitlink2 (parents u from)) ((u -- [from]), [])
            fromcs
        in
        let rec swing c = if c = from then to__ else c in
        let ulist = (fun (c, _) -> c <> from) <| aslist u in
        let u =
          match (u <@> to__) with
            None ->
              (* it's a new position: swing links from->to *)
              (mkmap
                 (List.map (fun (c, (ts, cs)) -> c, (ts, List.map swing cs))
                    ulist) ++
               (to__ |-> (fromts, fromcs)))
          | Some (tots, tocs) ->
              (* it's already there; delete unnecessary ->from links and unite the worlds *)
              let u =
                mkmap
                  (List.map
                     (fun (c, (ts, cs)) ->
                        c, (ts, (if member (to__, cs) then (fun c -> c <> from) <| cs else List.map swing cs)))
                     ulist)
              in
              domono fromts
                (u ++ (to__ |-> (tots, tocs @ listsub (fun (x,y) -> x=y) fromcs tocs)))
                to__
        in
        let sels =
          if member (to__, sels) then
            listsub (fun (x,y) -> x=y) sels [from]
          else List.map swing sels
        in
        Some (withdisproofselected (withdisproofuniverse d u) sels)
  *)
  
let moveworldtolink d from to__ lfrom lto__ =
  moveworld d from to__ &~~ 
  (fun (Disproofstate {universe=u} as d) -> 
     Some (withdisproofuniverse d (spliceworldtolink u to__ lfrom lto__)))

let showdisproof (Disproofstate {seq = seq; selections = selections; seqplan = plan;
                                 universe = universe; tiles = tiles;
                                 selected = selected; forcemap = forcemap}) =
    (*  consolereport["showdisproof: seq is "; debugstring_of_seq seq; 
                          "\nforcemap is "; string_of_mapping (string_of_pair string_of_coord debugstring_of_term ",") 
                                                          (stringfn_of_catelim catelim_string_of_forced) forcemap
                     ];
              *)

    match plan with
      Some(seqplan,seqbox) ->
        begin
          let seqsize = tbS seqbox in
          (* consolereport["seqplan is "; bracketedstring_of_list (debugstring_of_plan string_of_planclass) "; " seqplan; 
                                "; and seqbox is "; string_of_textbox seqbox]; *)
          setseqbox seqsize;
          drawindisproofpane ();
          seqdraw origin seqbox seqplan;
          (* put the emphasis in *)
          List.iter
            (fun plan ->
               match info_of_plan plan with
                 ElementClass (el, _) ->
                   let emph =
                     match term_of_element el with
                       None -> false
                     | Some t ->
                         try all (fst <.> (fun root -> Hashtbl.find forcemap (root, t))) selected 
                         with Not_found -> false
                   in
                   emphasise (seqelementpos origin seqbox plan) emph
               | _ -> ())
            seqplan;
          settiles (List.map string_of_term tiles);
          setworlds selected
            (List.map 
              (fun (c, e, labels, ws) -> 
                 c, e=Forced, List.map (fun(e,t) -> e=Forced,string_of_term t) (labelsort labels), ws)
                      (list_of_universe universe));
          forceAllDisproofSelections selections
        end
  | _ -> raise (Catastrophe_ ["Disproof.showdisproof unevaluated disproof state"])

let cleardisproof () = Japeserver.clearPane Displayfont.DisproofPane

(* this gets the colouring wrong! *)
let splitlink u from to__ w =
  if from=w || w=to__ then None else
  (*let (_, _, fromcs) = getworld u from in
  let (_, _, wcs) = getworld u w in
  if member (w, fromcs) && member(to__, wcs) then None else*)
  Some (addlink (addlink (deletelink u from to__) from w) w to__)
  
(* for export, with slightly altered semantics *)
(* this gets the colouring wrong! *)
let deletelink u from to__ =
  let (_, ts, cs) = getworld u from in
  if member (to__, cs) then
    Some (u ++ (from |-> (Unforced, ts, listsub (fun (x,y) -> x=y) cs [to__])))
  else None
