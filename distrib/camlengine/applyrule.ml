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

(* the original way of applying rules: including offerChoice (but without the Mapfix trick),
   based on sequents and biassed to natural deduction.

   Big new change: unifyterms doesn't check provisos any more (except for UnifiesProviso);
   checking is done by a new function checkprovisos: cxt -> cxt option, which we call
   when a rule is apparently successfully applied.  That function also filters out redundant
   provisos.
 *)
(* Crudely adapted to multi-conclusion sequents, Nov 94. RB *)
(* Mightily adapted to use Collections, July 96. RB *)

open Cxtfuns
open Idclass
open Listfuns
open Mappingfuns
open Name
open Optionfuns
open Provisotype (* ok. RB *)
open Proviso
open Rewrite
open Sml
open Sequent
open Seqtype
open Stringfuns
open Termfuns
open Termstore
open Termstring
open Termtype

exception Catastrophe_ = Miscellaneous.Catastrophe_
exception Tacastrophe_ = Miscellaneous.Tacastrophe_
exception Verifyproviso_ = Provisofuns.Verifyproviso_
      
type prooftree      = Prooftree.Tree.Fmttree.prooftree
 and prooftree_step = Prooftree.Tree.prooftree_step
 and rewinf         = Prooftree.Tree.rewinf
 and name           = Name.name
 and visproviso     = Proviso.visproviso
 and resnum         = Termtype.resnum
 and cxt            = Cxttype.cxt
 and element        = Termtype.element
 and seq            = Seqtype.seq
 and term           = Termtype.term

let mkJoin cxt reason how args = 
  Prooftree.Tree.mkJoin cxt reason how args Treeformat.Fmt.neutralformat

let askChoice            = Alert.askChoice
let consolereport        = Miscellaneous.consolereport
let expandFreshProviso   = Provisofuns.expandFreshProviso
let matchedtarget        = Unify.matchedtarget

let mkTip cxt seq = Prooftree.Tree.mkTip cxt seq Treeformat.Fmt.neutralformat

let prooftree_stepstring = Prooftree.Tree.prooftree_stepstring
let rewinf_uVIDs         = Rewinf.rewinf_uVIDs
let setReason            = Reason.setReason
let step_label           = Prooftree.Tree.step_label
let verifyprovisos       = Provisofuns.verifyprovisos    
  
let applydebug = ref 0
(* debug levels: 0 -- nothing
 *               1 -- surface level
 *               2 -- show the tree
 *)

let
  (beforeOfferingDo, beforeOffering, failOfferingDo, failOffering,
   succeedOfferingDo, succeedOffering)
  =
  let skip () = () in
  
  let whenoffering = ref skip
  and failoffering = ref skip
  and succeedoffering = ref skip in
  
  (*
                This is used to print the current prooftree
                if an offer has to be made during evaluation
                of a tactic.
                
                and we need to go back, especially because LAYOUT
                has the distressing effect of truncating the tree ...
        *)
  let rec beforeOfferingDo f = whenoffering := f
  and beforeOffering () = !whenoffering (); whenoffering := skip; ()
  and failOfferingDo f = failoffering := f
  and failOffering () =
    !failoffering (); failoffering := skip; succeedoffering := skip; ()
  and succeedOfferingDo f = succeedoffering := f
  and succeedOffering () =
    !succeedoffering (); failoffering := skip; succeedoffering := skip; ()
  in
  beforeOfferingDo, beforeOffering, failOfferingDo, failOffering,
  succeedOfferingDo, succeedOffering
(**************************************************************************

        Explanation stuff
        
        
***************************************************************************)


let rec failwithreason ss = setReason ss; None
let rec explain a1 a2 =
  match a1, a2 with
    why, None -> failwithreason (why ())
  | why, r -> r

(**************************************************************************
***************************************************************************)

    (* This stuff has been reorganised to recognise filters and discriminators.
     * The 'applyrule' and 'resolverule' functions give you back a possmatch, which
     * is a pair of a function and a list of possible matches.
     * The filters filter the matches.
     * The discriminators - offerChoice, takefirst, takeone, takethelot - 
     * take a possmatch, and give you back a cxt*prooftree pair,
     * using the function which is part of the possmatch value.
     *
     * It's a bit baroque, but it, or something like it, is needed so that we can 
     * filter and then build the tree of cuts which the 'resolve' functions need.
     * See remarks about 'filth' below.
     * RB March 1995
     *)
    (* Now the resolve stuff has been dropped, do we still need this? RB Nov 99 *)
    
type inforec = { reason : string; kind : string; conjecture : seq;
                 conjectureinf : rewinf; cxt : cxt; args : term list;
                 provisos : visproviso list; antecedents : seq list;
                 consequent : seq; how : prooftree_step;
                 principals : resnum list * resnum list; selhyps : element list;
                 selconcs : element list }
type info = Info of inforec

type possmatch =
  (info * element list * element list * cxt * seq list) list
(* cxt   subgoals *)

let showargs = termliststring
(* our sequents have to be pairs of collections *)
let rec breakside =
  function
    Collection cNes -> cNes
  | t ->
      raise
        (Catastrophe_
           ["breakside in applyrule given side "; smltermstring t])
(* once we have an answer, give me a proof tree *)
let rec answer =
  fun
    (Info
       {reason = reason; args = args; conjecture = conjecture; how = how},
     (thinnedL : element list), (thinnedR : element list), cxt,
     subgoals) ->
    cxt,
    mkJoin cxt reason how args conjecture
      ((mkTip cxt <.>rewriteseq cxt) <* subgoals)
      (* rewrite tips here *)
      (thinnedL, thinnedR)
(* filters *)
let nofilter r = Some r
let filter = List.filter
let rec nonempty =
  function
    [] -> None
  | xs -> Some xs
let rec runfilter e g =
  explain e <.> nonempty <.> filter g
let showel = smlelementstring termstring
let (bymatch : possmatch -> possmatch option) =
  runfilter
    (fun () ->
       ["The goal unifies with the rule, but doing so changes the goal"])
    (fun
       (Info
          {cxt = cxt;
           conjecture = conjecture;
           kind = kind;
           args = args;
           conjectureinf = conjectureinf;
           how = how}, _, _, cxt', _) ->
       let us = rewinf_uVIDs conjectureinf in
       let r = matchedtarget cxt cxt' us in
       if !applydebug > 0 then
         consolereport
           (if r then
              let rec up cxt u =
                "(" ^ string_of_vid u ^ "," ^
                      optionstring termstring ((varmap cxt <@> u)) ^
                ")"
              in
              ["bymatch passing"; step_label how; " ";
               bracketedliststring (up cxt) "," us; " and ";
               bracketedliststring (up cxt') "," us]
            else
              ["bymatch failing "; step_label how; " "; showargs args;
               " ("; seqstring conjecture; ") => (";
               seqstring (rewriteseq cxt conjecture); " ... ";
               seqstring (rewriteseq cxt' conjecture); ")"]);
       r)
let (sameprovisos : possmatch -> possmatch option) =
  runfilter
    (fun () ->
       ["The goal fits the rule, but the rule introduced some extra provisos"])
    (fun
       (Info
          {cxt = cxt;
           conjecture = conjecture;
           kind = kind;
           args = args;
           how = how}, _, _, cxt', _) ->
       let r =
         eqbags (fun (vp', vp) -> provisoactual vp' = provisoactual vp)
           (provisos cxt', provisos cxt)
       in
       if !applydebug > 0 then
         consolereport
           ["bymatch2 looking at "; step_label how; " "; showargs args;
            " ("; seqstring conjecture; ") => (";
            liststring visprovisostring " AND " (provisos cxt); " ... ";
            liststring visprovisostring " AND " (provisos cxt'); ") => ";
            string_of_bool r];
       r)
(* discriminators *)

let answerstring (Info {conjecture=Seq(st,chs,cgs)}, thinnedL, thinnedR, _, _) = 
  quadruplestring idclassstring idclassstring 
      (bracketedliststring (smlelementstring termstring) ",") 
      (bracketedliststring (smlelementstring termstring) ",") "; " 
      (snd_of_3(breakside chs),snd_of_3(breakside cgs),thinnedL,thinnedR)
          
let rec remdupposs ps =
  (* eliminate answers which _look_ the same, now that we've got through 
   * all the filters -- but only do it in bag matching
   *)
  let rec same =
    fun (Info {conjecture = Seq (st, chs, cgs)}, thinnedL, thinnedR, _, _)
      (_, thinnedL', thinnedR', _, _) ->
      (thinnedL = thinnedL' ||
       identical (snd_of_3 (breakside chs)) (thinnedL, thinnedL')) &&
      (thinnedR = thinnedR' ||
       identical (snd_of_3 (breakside cgs)) (thinnedR, thinnedR'))
  and identical a1 a2 =
    match a1, a2 with
      BagClass _, (rts, rts') -> eqlists sameresource (rts, rts')
    | _, _ -> false
  in
  nj_fold (fun (p, ps) -> if List.exists (same p) ps then ps else p :: ps) ps []
let rec takefirst =
  function
    [] -> None
  | p :: ps -> Some (answer p)
let rec takeonlyone ps =
  match remdupposs ps with
    [p] -> Some (answer p)
  | _ -> failwithreason ["the rule matched the goal in more than one way"]
let rec offerChoice ps =
  let ps = remdupposs ps in
  let rec listposs =
    function
      Info {consequent = c}, _, _, cxt, [] ->
        [implode [seqstring (rewriteseq cxt c); " solves this goal"]]
    | Info {consequent = c}, _, _, cxt, ss ->
        implode
          [seqstring (rewriteseq cxt c); " generates subgoal";
           if List.length ss = 1 then " " else "s "] ::
          ((seqstring <.> rewriteseq cxt) <* ss)
  in
  let rec numwords n =
    match n with
      1 -> "one"
    | 2 -> "two"
    | 3 -> "three"
    | 4 -> "four"
    | 5 -> "five"
    | 6 -> "six"
    | 7 -> "seven"
    | 8 -> "eight"
    | 9 -> "nine"
    | n -> string_of_int n
  in
  match ps with
    [] -> None
  | [p] -> Some (answer p)
  | (Info {kind = kind; how = how}, _, _, _, _) :: _ ->
      beforeOffering ();
      match
        askChoice
          (implode
             ["The "; kind; " "; step_label how; " matches in ";
              numwords (List.length ps); " different ways. ";
              "Select one from this list: "],
           (listposs <* ps))
      with
        None -> failOffering (); None
      | Some n -> succeedOffering (); Some (answer (List.nth (ps) (n)))
(* this isn't really discriminatory, is it? *)

let rec takethelot ps = (answer <* ps)
(**************************************************************************
***************************************************************************)

let rec fitter checker resnums =
  fun (_As, _Bs) cxt ->
    (* The rule provides the collection _As, the conjecture the collection _Bs. 
        
        Unifying _As with _Bs provides us with a list of contexts, which will include
        resource matches for elements which have matched. In order to mesh with the 
        old machinery in other parts of the engine, we need to convert those 
        resource matches into lists of formulae in _Bs that have been resource-matched.
        
        We no longer need the elements of _Bs that aren't matched as a result of 
        this function, so we return a list of pairs, each containing a unification
        context and a list of matched elements.
     *)
    (* Now that the fresh functions give a list of 'interesting resource numbers' we
     * don't return all the resnum matches.
     * RB 27/x/96
     *)
    let cxts = checker (_As, _Bs) cxt in
    let rec thinned cxt r =
      match r with
        ResUnknown i ->
          begin match (resmap cxt <@> i) with
            Some (r, t) ->
              begin match thinned cxt r with
                None -> Some (registerElement (r, t))
              | some -> some
              end
          | None -> None
          end
      | _ -> None
    in
    (fun cxt -> optionfilter (thinned cxt) resnums, cxt) <* cxts

let _BnMfilter = List.filter
(*   Match rule by hypothesis then conclusion
     and generate sets of subgoals from antecedents,
     indexed by the hypothesis match.
*)
let rec subGoalsOfRule checker (hiddenleft, hiddenright) =
  fun
    (Info
       {kind = kind;
        conjecture = Seq (_Cst, _CHs, _CGs) as conjecture;
        how = how;
        consequent = Seq (st, _Hs, _Gs) as consequent;
        antecedents = antecedents;
        cxt = cxt;
        provisos = provisos;
        principals = resnumLs, resnumRs as principals;
        selhyps = selhyps;
        selconcs = selconcs} as info) ->
    let rec expandfresh b (h, g, r, v as f) left right ps =
      let ps = expandFreshProviso b f left right ps in
      if not r && (h && hiddenleft || g && hiddenright) then
        mkvisproviso (b, FreshProviso (h, g, true, v)) :: ps
      else ps
    in
    let (impfreshprovisos, newprovisos) =
      nj_fold
        (fun (vp, (ips, nps)) ->
           match provisoactual vp with
             FreshProviso (_, _, false, _ as f) ->
               ips, expandfresh (provisovisible vp) f _CHs _CGs nps
           | FreshProviso (_, _, true, _ as i) ->
               (provisovisible vp, i) :: ips, nps
           | _ -> ips, vp :: nps)
        provisos ([], [])
    in
    let rec impprovisos thinnedL thinnedR =
      if null impfreshprovisos then []
      else
        let rec goodside ths side =
          let (_, c, els) = breakside side in
          registerCollection
            (c, (fun el -> not (member (el, ths))) <| els)
        in
        let cHs = goodside thinnedL _CHs in
        let cGs = goodside thinnedL _CGs in
        nj_fold (fun ((b, i), ps) -> expandfresh b i cHs cGs ps)
          impfreshprovisos []
    in
    (* these rewrites are probably necessary to make the tactic stuff work.  Otherwise
       I would take them out.
       RB 20/v/94
     *)
    (* We return 'thinners' as part of the result because this information is essential
       to the operation of the 'boxdraw' module.  It's not just an optimisation, either.
       Oh dear.
       RB 27/v/94
       I now realise that this is essential to the 'resource' model of how Jape
       works. It is certainly not an optimisation.
       RB 9/vii/96
     *)
    let rec result subgoals (thinnedL, thinnedR, cxt) =
      (* don't rewrite uHs, uGs, because of 'used' filters above *)
      info, thinnedL, thinnedR, cxt, (rewriteseq cxt <* subgoals)
    in
    (* get rid of alternatives which have the *same* thinners in bag matching 
    fun remdupposs (BagClass _) ps =
      let fun same (ms, _) (ms', _) = 
            eqlists (fn ((_,t1),(_,t2)) => eqterms (t1,t2)) (ms,ms') 
      in
          nj_fold (fn (p,ps) => if List.exists (same p) ps then ps else p::ps) ps []
      end
    |   remdupposs _            ps = ps
    *)
    
    let _Hkind = snd_of_3 (breakside _Hs) in
    let _Gkind = snd_of_3 (breakside _Gs) in
    let rec checkinclusive js ks =
      _All (fun j -> List.exists (fun k -> sameresource (j, k)) ks) js ||
      _All (fun k -> List.exists (fun j -> sameresource (j, k)) js) ks
    in
    let genSubGoals =
      let rec exp0 ss () =
        "In applying the " :: kind :: " " :: step_label how ::
          " to the problem sequent " :: seqstring conjecture :: ", " :: ss
      in
      let rec exp1 sing plur els () =
        let w =
          match breakside els with
            _, _, [_] -> sing
          | _ -> plur
        in
        exp0
          ["the "; w; " of the "; kind; "'s consequent don't fit the problem. The consequent of the rule is ";
           seqstring consequent; "."]
          ()
      in
      let rec unusedprincipal sing plur els () =
        let word =
          match els with
            [_] -> sing
          | _ -> plur
        in
        let ts =
          match els with
            [e] -> elementstring e
          | _ -> liststring elementstring " and " els
        in
        ["The goal fits the rule, but the rule didn't make use of the ";
         word; " "; ts; " which you selected"]
      in
        (explain (exp1 "conclusion" "conclusions" _CGs) <.> nonempty <.> 
          fitter checker resnumRs (_Gs, _CGs)) 
        &~
        (explain (unusedprincipal "conclusion" "conclusions" selconcs) <.> 
         nonempty <.> 
         _BnMfilter (fun (thRs, _) ->
                      let r = checkinclusive thRs selconcs in
                      if !applydebug > 0 then
                        consolereport
                          ["usedconc checking ";
                           bracketedliststring showel ","
                             selconcs;
                           " against ";
                           bracketedliststring showel ","
                             thRs;
                           " => "; string_of_bool r];
                      r))
        &~
        (explain (exp1 "hypothesis" "hypotheses" _CHs) <.> 
         nonempty <.> List.concat <.> 
         List.map
            (fun (thinnedR, cxt) ->
               [thinnedR] >< fitter checker resnumLs (_Hs, _CHs) cxt))
        &~
        (explain (unusedprincipal "hypothesis" "hypotheses" selhyps) <.>
         nonempty <.>
         _BnMfilter ((fun (_, (thLs, _)) ->
                       let r = checkinclusive thLs selhyps in
                       if !applydebug > 0 then
                         consolereport
                           ["usedhyp checking ";
                            bracketedliststring showel "," selhyps;
                            " against ";
                            bracketedliststring showel "," thLs;
                            " => "; string_of_bool r];
                       r)))
        &~
        (fun poss ->
           let rec doprovisos
             ((thinnedR, (thinnedL, cxt) as pos), (bads, goods)) =
             try
               bads,
               (thinnedL, thinnedR,
                verifyprovisos
                  (plusprovisos cxt
                     (impprovisos thinnedL thinnedR))) ::
                 goods
             with
               Verifyproviso_ p -> p :: bads, goods
           in
           match nj_fold doprovisos poss ([], []) with
             bads, [] ->
               begin match sortunique earlierproviso bads with
                 [p] ->
                   explain
                     (exp0
                        ["the goal fits the rule, but the proviso ";
                         provisostring p; " is violated"])
                     None
               | ps ->
                   explain
                     (exp0
                        ["the goal fits the rule, but the provisos (variously ";
                         liststring provisostring " and " ps;
                         ") are violated"])
                     None
               end
           | _, (_ :: _ as goods) -> Some goods)
        &~
        (nonempty <.> List.map (result antecedents))
    in
    if _Cst = st then genSubGoals (plusprovisos cxt newprovisos)
    else
      failwithreason
        ["The rule "; step_label how; " doesn't match the goal ";
         seqstring conjecture; " because the turnstiles are different"]
let rec showstuff stuff =
  octuplestring enQuote
    (pairstring string_of_bool string_of_bool ",")
    prooftree_stepstring showargs
    (pairstring (bracketedliststring resnumstring ",")
       (bracketedliststring resnumstring ",") ",")
    (bracketedliststring seqstring ",") seqstring
    (bracketedliststring visprovisostring " AND ") ", " stuff
let rec apply checker filter taker selhyps selconcs stuff reason cxt =
  fun (_C, _Cinf) ->
    let
      (kind, hiddencontexts, how, args, principals, antes, conseq, provs)
      =
      stuff
    in
    let _ =
      if !applydebug > 0 then
        consolereport
          ["apply "; step_label how; " "; showstuff stuff; " ";
           enQuote reason; " "; seqstring _C]
    in
    let info =
      Info { reason = reason; kind = kind; conjecture = _C; conjectureinf = _Cinf;
             cxt = cxt; args = args; provisos = provs; antecedents = antes;
             consequent = conseq; how = how; principals = principals;
             selhyps = selhyps; selconcs = selconcs }
    in
       subGoalsOfRule checker hiddencontexts info &~~
       (filter &~ taker)
