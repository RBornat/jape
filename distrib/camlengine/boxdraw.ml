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

(*

        Box-style proof rendering
        Richard Bornat & Bernard Sufrin, Oxford, 1992
        
        (and here it is May 1998 and we're still at it ...)
        
        (and now September 1999 ... how long can this go on?)

*)

(******************** Notes on the design ******************************
 *
 * The original boxdraw did a recursive scan of the tree, distinguishing between
 * nodes which are drawn as boxes -- those with excess hypotheses -- and the rest. 
 * The algorithm was pretty obvious: draw hypotheses at box nodes and conclusions
 * at line nodes; draw a line node by first drawing its antecedents, recording the
 * line numbers of the last line drawn for each antecedent as part of the
 * justification of the line drawn for the node itself; draw a box node by drawing
 * a hypothesis line containing the extra hypotheses, then draw it as a conclusion
 * line.  Because the logic we started with had left-side rules (see SCS.jt), we
 * also included in the justification a reference to any hypothesis formula
 * matched in the step, whether 'used up' or not.
 * 
 * It wasn't long before we were hiding identity lines, replacing them in the
 * justification of any node that referred to them by a reference to the relevant
 * hypothesis.
 * 
 * This was already a fairly large module, though not a particularly complicated
 * algorithm.  It got a lot worse when I modified it to treat cut nodes specially.
 * Some cut nodes couldn't be specially treated, because of deficiencies of the
 * click-to-select UI mechanism we were using; those that could be dealt with fell into
 * two categories.  I decided what to do with a cut line by drawing the
 * antecedents and looking at the result -- normally it was enough to draw the
 * left antecedent to make a decision, but not always.
 * 
 * Then I improved treatment of box nodes, splitting hypothesis lines into
 * multiple lines if they were too wide for the screen.  This was done by making
 * two passes, the first giving a measure of the size of the proof when hypothesis
 * lines aren't split, the second given a maximum width into which to fold those
 * lines.  It's done twice because you need to know the width of the longest
 * justification before you know how tightly to fold the hypothesis lines, and if
 * there is a long conclusion line -- they weren't folded -- then folding
 * hypothesis lines might make no difference.
 * 
 * At this point the code became so convoluted that Bernard protested that he
 * couldn't understand it any more (and stopped trying, IMO :-) ).
 * 
 * The whole thing was re-implemented when transitivity nodes got their own
 * treatment.  I first classified each node in the tree into box, single line, cut
 * and transitivity nodes. Then I found out the sizes of their components. Then I
 * linearised the whole (this stage was, as before, done twice).  Then I did the
 * detailed layout.
 * 
 * The latest development is to realise that cut nodes, within a transitivity
 * stack, might be treated just like antecedents of the non-transitive components of the
 * stack.  But this means treating cut nodes within a transitivity stack quite
 * differently to cut nodes outside it.  Hence another reorganisation.
 * 
 * During that reorganisation I realised that the selection mechanism, which
 * disallows arbitrary cuts, was making it almost impossible to implement a
 * readable version of the transitivity transformation.  So I decided to take the
 * plunge, and allow arbitrary cuts.  Then the cut and transitivity
 * transformations always take place -- at the expense, sometimes, of drawing lines
 * which are ambiguously hypothesis or conclusion.  The GUI has to cope with the
 * consequent selection problems, somehow.
 * 
 * RB 27/viii/99
 *)
 
open Absprooftree
open Box
open Displayclass
open Displayfont
open Draw
open Hit
open Listfuns
open Minwaste
open Mappingfuns
open Optionfuns
open Sml
open Termstring
open Text
open Thing

type box = Box.box
 and displayclass = Displayclass.displayclass
 and 'a hit = 'a Hit.hit
 and hitkind = Hit.hitkind
 and pos = Box.pos
 and size = Box.size
 and term = Termtype.term
 and textbox = Box.textbox
 and tree = Absprooftree.tree

let hasrelevanttip el t = Prooftree.Tree.Vistree.hasTip t (* for now *)
let measurestring = Japeserver.measurestring

let consolereport = Miscellaneous.consolereport
let cuthidingdebug = Prooftree.Tree.cuthidingdebug
let term_of_element = Termfuns.term_of_element
let enQuote = Stringfuns.enQuote
let explodebinapp = Termfuns.explodebinapp
let findfirst = Optionfuns.findfirst
let foldformulae = Miscellaneous.foldformulae
let isRelation = Thing.isRelation
let string_of_pair = Stringfuns.string_of_pair
let string_of_path = Listfuns.bracketedstring_of_list string_of_int ","
let string_of_quadruple = Stringfuns.string_of_quadruple
let string_of_reason = (fun s -> s)
let screenpositiondebug = Miscellaneous.screenpositiondebug
let string_of_seq = Sequent.string_of_seq
let sameresource = Termfuns.sameresource
let string_of_textbox = Box.string_of_textbox
let string_of_triple = Stringfuns.string_of_triple
let truncatereasons = Miscellaneous.truncatereasons
let turnstiles = Sequent.syntacticturnstiles
let uncurry2 = Miscellaneous.uncurry2
let _The = Optionfuns._The

exception Catastrophe_ = Miscellaneous.Catastrophe_

let textinfo_of_element = textinfo_of_element (invisbracketedstring_of_element true)
let textinfo_of_term = textinfo_of_term (invisbracketedstring_of_term true)
let outermostbox = ref true
(* set true to imitate previous boxdraw behaviour *)
let hidecut = ref true
let hidehyp = ref true
let hidetransitivity = ref true
let hidereflexivity = ref true
let showrelations = ref true
let boxseldebug = ref false
let boxfolddebug = ref false
let outerassumptionword = ref "assumption"
let outerassumptionplural = ref "assumptions"
let innerassumptionword = ref "assumption"
let innerassumptionplural = ref "assumptions"
let boxlineformat = ref "R (A) N: |"
(* "N: |R A" is going to be the default *)
let boxlinedisplay = ref "right"
let rec ttsub hs hs' = listsub sameresource hs hs'
(* This collection of classes will do for experimental purposes, but it won't last for ever *)

let stileclass = DisplayPunct
(* for now ... *)

   (* new treatment of tree transformation: multi-stage treatment of cuts, hyps, transivity
    * and reflexivity.  This treatment doesn't sit very well with the treatment of layout, but
    * it will take some time before we have a solution to that, so here you are
    * RB 1/vii/98
    *)
    
   (* extract a conclusion and its exploded form *)

let rec explodedconc cs =
  match cs with
    [c] ->
      begin match (term_of_element c &~~ explodebinapp) with
        Some bits -> Some (c, bits)
      | _ -> None
      end
  | _ -> None
(* show a hit *)

let my_hitstring = string_of_hit string_of_path
let my_selstring = string_of_sel string_of_path

(*********************** Drawing transitive stacks *****************************
 * 
 * Suppose that we have a transitivity step
 * 
 * F=G  G=_H
 * --------
 *   F=_H
 *   
 * where neither of the leaves is a further transitivity step.  Then there are
 * four ways in which it might be displayed (all now allowed because we allow
 * arbitrary cuts):
 * 
 * a.   F
 *     ...
 *     = G
 *     = _H  reason
 * 
 * b.   F
 *     = G  reason
 *     ...
 *     = _H
 *     
 * c.   F
 *     = G  reason
 *     = _H  reason
 *     
 * d.   F
 *     ...
 *     = G  
 *     ...
 *     = _H
 *     
 * And so on for deeper transitivity stacks.
 * 
 * Subproofs and cuts
 * ------------------
 * 
 * Suppose that in some transitive stack a step F=G is proved by a subproof which
 * isn't a transitivity step and isn't an immediate closing rule: that is, it has
 * a substantive subproof.  Then we can display that entire subproof before the
 * transitive stack, with the usual greying-out mechanism to stop it being
 * appealed to in the wrong places.  (If we implement the cut-splicing mechanism
 * of the FACS paper, we could actually move the subproof and make it available to
 * the whole stack, but that's not yet possible and it isn't part of the display
 * mechanism in any case.)
 * 
 * Cuts which are part of a stack are no more of a problem.  They introduce
 * hypotheses which aren't available everywhere in the transitivity stack, but if
 * we get the greying-out right that won't matter.  The remark about cut-splicing
 * from the previous paragraph applies here also.
 * 
 * So it turns out that we can treat cuts and subproofs, inside transitive stacks,
 * in exactly the same way!  Hurrah!
 *
 * BUT SERIOUS PROBLEMS REMAIN!!!!!!!!
 * -----------------------------------
 *
 * 1. How to deal with transitive steps on the left of a cut?
 * The final line of a transitive stack could be ambiguous ...
 *
 * 2. In a transitive stack, how to select intermediate results?
 * This is allied to cut-splicing, but it's a brand-new gestural problem as well.
 * Suppose we have 
 *       A
 *     = B
 *     = C
 *     = _D
 *     = E
 *
 * then we have proved A=E (root of the stack), A=B, B=C, C=_D and _D=E, each of which is 
 * the root of a subtree; but we have also proved A=C, A=_D, B=_D, B=E and C=E. How do
 * we gesture at that?  Actually the answer is easy: press on B, drag to _D, you've said
 * B=_D.  But that's some way down the line, I think.
 *
 *)
 
(*********************** Step 1: find out what to do at each node of the tree ***************)
(* We don't have a special node for identity rules, because those nodes are treated differently in 
 * 'normal' and 'transitive' dependency transformation: it's done with a boolean in LinPT
 * at this stage.  We don't any longer try to find out which kind of cut we have: that's done
 * on the fly in stage 2.  We still find out if the cut hypothesis is needed in the right Absprooftree.
 * 
 * We do still eliminate reflexive lines here, because transitive stacks get special processing in
 * stage 2, and sometimes eliminating the reflexivity can get rid of the whole stack.  Cuts in 
 * transitive stacks aren't dealt with yet, and thinking about how identity/reflexivity would interact
 * with them gives me a headache.  For the moment, therefore, it doesn't matter how we treat them.
 *)
    
type revpath = RevPath of int list
(* no more uncertainty *)

   
type normalpt =
    LinPT of
      (revpath * bool * element list * string option *
         (reason * element list * normalpt list) option)
  | BoxPT of
      (revpath * bool * (string * string) * element list * normalpt)
  | CutPT of (revpath * element * normalpt * normalpt * bool * bool)
  | TransitivityPT of (element * normalpt * normalpt)
(* no revpath needed in transitivity node, because all you ever see is the tips;
 * cuts need a revpath because of the various things that can be done by
 * pointing to subtrees
 *)

let rec pretransform prefixwithstile t =
  let innerwords = !innerassumptionword, !innerassumptionplural in
  let outerwords = !outerassumptionword, !outerassumptionplural in
  (* pt gives back isreflexive * normalpt -- isreflexive is used in transitivity. *)
  
  let rec respt (r, pt) = pt in
  let rec pt rp hyps t =
    let why = reason t in
    let (st, hs, gs) = Absprooftree.explode (sequent t) in
    let subts = subtrees t in
    let newhyps = ttsub hs hyps in
    let lprins = fst (Absprooftree.matched t) in
    let rec _T hs (n, t) = pt (n :: rp) hs t in
    let rec mkl isid subs =
      !hidereflexivity && isStructureRulenode t ReflexiveRule,
      LinPT
        (RevPath rp, isid, gs,
         (if prefixwithstile then Some st else None),
         (why &~~ (fun r -> Some (r, lprins, subs))))
    in
    let rec aslines isid =
      mkl isid ((respt <.> _T hs) <* numbered subts)
    in
    let rec boxpt lines =
      false, BoxPT (RevPath rp, true, innerwords, newhyps, lines)
    in
    let rec hyps seq = snd_of_3 (Absprooftree.explode seq) in
    let rec concs seq = thrd (Absprooftree.explode seq) in
    (* The box transformation is applied to anything which has novel lhs elements.
     * The id transformation (aslines true) is applied when it matches, except when the id
     * is the last line of a box (unless the box introduces a single hypothesis and the rest 
     * of the box is a single id line which matches that hypothesis).
     * But because it is impossible (cut transformation) to see just when a hypothesis 
     * appears at the end of a box, we do the checks about last line later.
     *)
    match
      newhyps, !hidehyp && isStructureRulenode t IdentityRule, lprins, gs
    with
      [], true, _, _ -> false, respt (aslines true)
    | _ :: _, true, _, _ -> boxpt (respt (aslines true))
    | _ :: _, false, _, _ -> boxpt (respt (pt rp hs t))
    | _ ->
        (* not an Id or a box ... *)
                      (* The cut transformation applies to cut nodes in which
                       * the first antecedent has a single rhs formula (why?).
                       * If the left antecedent is proved then the cut line is treated as a hypothesis (CutHyp); 
                       * otherwise, if the right has no open Tips which could use the cut formula then 
                       * it's treated as a conclusion (CutConc); 
                       * otherwise it could be either (CutAmbig).
                       *)
        match !hidecut && isStructureRulenode t CutRule, subts with
          true, [a1; a2] ->
            begin match
              concs (sequent a1), ttsub (hyps (sequent a2)) hs
            with
              [cc], [ch] ->
                let (_, pt1) = _T hs (0, a1) in
                let (_, pt2) = _T (ch :: hs) (1, a2) in
                false,
                CutPT
                  (RevPath rp, ch, pt1, pt2, hasrelevanttip ch a2,
                   ishiddencut t)
            | _ ->(* we don't transmit reflexivity yet ... *)
               aslines false
            end
        | _ ->
            (* multi-conc cut: useful for resolution but not for this display transform *)
            (* not an Id or a box or a cut ... *) 
                          (* The transitivity transformation applies to all transitivity nodes E=F, F=G.
                           * Reflexivity nodes, within transitivity stacks, are deleted here.
                           * I thought of having a special transitive stack datatype, but because of 
                           * reflexivity, you can't be quite sure you have a transitive stack at this point.
                           * I guess layout might make a difference too.
                           *)
            match
              !hidetransitivity && isStructureRulenode t TransitiveRule,
              gs, subts
            with
              true, [c], [a1; a2] ->
                let (a1r, a1pt as pt1) = _T hs (0, a1) in
                let (r_of_a, pt_of_a as pt2) = _T hs (1, a2) in
                if a1r then pt2
                else if r_of_a then pt1
                else false, TransitivityPT (c, a1pt, pt_of_a)
            | _ -> aslines false
  in
  match respt (pt [] [] t) with
    BoxPT (pi, _, words, hs, ptr) ->
      BoxPT (pi, !outermostbox, outerwords, hs, ptr)
  | ptr -> ptr

(******** Step 2: compute class of each element, element texts and sizes, and paths ********)
(* In order to deal with cut dependencies properly, we refine the notion of path.
 * When we point to a formula we may want to 
 *     (a) treat it as a hypothesis, conclusion or ambig (path)
 *     (b) do layout-ish things to it (layoutpath)
 *     (c) prune the tree above it (prunepath).
 * For a CutHyp transformation, pointing to the left formula (a hit on the last line of the left
 * subtree) should be reported (a) as a hypothesis hit on the right subtree; (b) or (c) as the 
 * true path to the left subtree.
 * For any Cut transformation, a hit on the last line of the right subtree should be reported
 * (a) and (b) as the true path to that line, (c) as a hit on the cut node itself.
 * Hence the pathinfo part of the dependency structure.
 *
 * The transitivity transformation is really tricky, because of the presence of Cut nodes.
 * It's incomplete (I haven't taken account of Cut nodes yet).
 *)

 (* The info which we keep in the drawing plans *)
type pathinfo = { path : int list; layoutpath : int list option; prunepath : int list option }
(* include paths in the plan information -- it makes interpreting clicks so much easier *)
type elementplankind =
    ElementPlan of elementplaninf
  | AmbigElementPlan of (elementplaninf * elementplaninf)
  | ElementPunctPlan
and elementkind = ConcPlan | HypPlan | TranPlan of side
and elementplaninf = pathinfo * element * elementkind
(* even reasons need pathinfo, these days *)
type reasonplankind = ReasonPlan of pathinfo | ReasonPunctPlan

(* this won't last long, I hope *)
let rec elementplanclass =
  function
    ElementPlan (_, _, ConcPlan  ) -> DisplayConc
  | ElementPlan (_, _, HypPlan   ) -> DisplayHyp
  | ElementPlan (_, _, TranPlan _) -> DisplayConc
  | AmbigElementPlan _             -> DisplayAmbig
  | ElementPunctPlan               -> DisplayPunct (* for now *)
     
(* for now *)
   
let rec reasonplanclass =
  function
    ReasonPlan _ -> DisplayReason
  | ReasonPunctPlan -> DisplayPunct

let rec iselementkind epk =
  match epk with
    ElementPlan _ -> true
  | AmbigElementPlan _ -> true
  | ElementPunctPlan -> false

let rec string_of_pathinfo
  {path = path; layoutpath = layoutpath; prunepath = prunepath} =
  ((((("{path=" ^ string_of_path path) ^ ",layoutpath=") ^
       string_of_option string_of_path layoutpath) ^
      ",prunepath=") ^
     string_of_option string_of_path prunepath) ^
    "}"

let rec string_of_elementplankind pk =
  (* how I HATE having to write these *)
  match pk with
    ElementPlan pi -> "ElementPlan" ^ string_of_elementplaninf pi
  | AmbigElementPlan pair ->
      "AmbigElementPlan" ^
        string_of_pair string_of_elementplaninf string_of_elementplaninf "," pair
  | ElementPunctPlan -> "ElementPunctPlan"
and string_of_elementkind ek =
  match ek with
    ConcPlan -> "ConcPlan"
  | HypPlan -> "HypPlan"
  | TranPlan side -> "TranPlan " ^ string_of_side side
and string_of_elementplaninf epi =
  string_of_triple string_of_pathinfo string_of_element string_of_elementkind "," epi
type textinfo = textsize * textlayout
type elementplaninfo = textinfo * elementplankind
type elinfo = element * elementplaninfo
type wordp = textinfo * textinfo
type dependency =
    LinDep of (elementplaninfo list * textinfo option * reasoninfo)
  | BoxDep of (bool * (textinfo * textinfo) * elinfo list * dependency)
  | IdDep of (element * dependency)
  | CutDep of (dependency * element * dependency * bool)
  | TranDep of (textinfo option * elementplaninfo * trandep list)
and reasoninfo =
  (pathinfo * textinfo * element list * dependency list) option
and trandep = textinfo * elementplaninfo * reasoninfo
(*   op         formula        *)

let string_of_elementplaninfo =
  string_of_pair string_of_textinfo string_of_elementplankind ", "

let string_of_elinfo = string_of_pair string_of_element string_of_elementplaninfo ","

let rec string_of_trandep t =
  string_of_triple string_of_textinfo string_of_elementplaninfo string_of_reasoninfo "," t
and string_of_reasoninfo r =
  string_of_option
    (string_of_quadruple string_of_pathinfo string_of_textinfo
       (bracketedstring_of_list string_of_element ",")
       (bracketedstring_of_list string_of_dependency ",") ",")
    r
and string_of_dependency d =
  match d with
    LinDep l ->
      "LinDep" ^
        string_of_triple (bracketedstring_of_list string_of_elementplaninfo ",")
          (string_of_option string_of_textinfo) string_of_reasoninfo "," l
  | BoxDep d ->
      "BoxDep" ^
        string_of_quadruple string_of_bool
          (string_of_pair string_of_textinfo string_of_textinfo ",")
          (bracketedstring_of_list string_of_elinfo ",") string_of_dependency "," d
  | IdDep i -> "IdDep" ^ string_of_pair string_of_element string_of_dependency "," i
  | CutDep c ->
      "CutDep" ^
        string_of_quadruple string_of_dependency string_of_element string_of_dependency
          string_of_bool "," c
  | TranDep t ->
      "TranDep" ^
        string_of_triple (string_of_option string_of_textinfo) string_of_elementplaninfo
          (bracketedstring_of_list string_of_trandep ",") "," t
(* A conclusion line can be dead (have a reason next to it) or live (have no
 * reason yet).  If it's dead, it may sometimes be the left-hand conclusion of a
 * cut, and therefore should be treated as a hypothesis line.  If it's live, it
 * may have to play an ambiguous role.  Similarly, lines in transitive stacks may
 * be used upwards or downwards or both, and the last line may be used upwards and
 * -- standing for the whole stack -- may be a hypothesis line at the same time.
 * 
 * In the past we used to treat this sort of thing by analysis in stage 1.  Now we
 * treat it dynamically here.  The deadf argument says what to do: with argument
 * true it deals with dead lines (at present, and probably wrongly, treats them as
 * conclusion lines).  With argument false it says what to do with live lines.  By
 * cleverness, this deals with the last lines of transitive stacks, and a very
 * similar mechanism is employed within transitive stacks.  Wheee!
 * 
 * Here goes. 
 *)

let rec ordinary _ con pi e = ElementPlan (con pi e)
let rec mkhypplan pi e = pi, e, HypPlan
let rec mkconcplan pi e = pi, e, ConcPlan

let rec dependency tranreason deadf pt =
  let rec ordinarypi =
    fun (RevPath rp) -> {path = List.rev rp; layoutpath = None; prunepath = None}
  in
  (* dealing with subtrees of normal/transformational lines *)
  let rec linsubs pi justopt =
    match justopt with
      Some (why, lprins, subpts) ->
        Some
          (pi, tranreason why, lprins,
           (dependency tranreason ordinary <* subpts))
    | None -> None
  in
  (* transforming stiles *)
  let rec dostopt stopt =
    match stopt with
      Some st -> Some (textinfo_of_string TermFont ((" " ^ st) ^ " "))
    | _ -> None
  in
  match pt with
    LinPT (rp, isid, concs, stopt, justopt) ->
      let pi = ordinarypi rp in
      let rec mkplan e =
        textinfo_of_element e, deadf (bool_of_opt justopt) mkconcplan pi e
      in
      let dep =
        LinDep ((mkplan <* concs), dostopt stopt, linsubs pi justopt)
      in
      begin match isid, justopt with
        true, Some (_, [lp], []) -> IdDep (lp, dep)
      | _ -> dep
      end
  | BoxPT (rp, boxit, (sing, plur), hs, pt') ->
      let pi = ordinarypi rp in
      let rec mkplan e =
        e, (textinfo_of_element e, ElementPlan (pi, e, HypPlan))
      in
      BoxDep
        (boxit,
         (textinfo_of_string ReasonFont sing,
          textinfo_of_string ReasonFont plur),
         (mkplan <* hs), dependency tranreason ordinary pt')
  | CutPT (RevPath rp, ch, lpt, rpt, chneeded, tobehidden) ->
      let rootp = List.rev rp in
      let leftp = List.rev (0 :: rp) in
      let rightp = List.rev (1 :: rp) in
      let rec leftdead d con (({path = p} : pathinfo) as pi) el =
        let up = {path = rightp; layoutpath = Some leftp; prunepath = Some leftp},
          ch, HypPlan
        in
        (* those leftps were ps and before that leftps ... *)
        if d then ElementPlan up else AmbigElementPlan (con pi el, up)
      in
      (* the outermost prune path dominates *)
      let rec rightdead d con ({path = p; layoutpath = l} : pathinfo) =
        deadf d con {path = p; layoutpath = l; prunepath = Some rootp}
      in
      CutDep
        (dependency tranreason (if chneeded then leftdead else ordinary)
           lpt,
         ch, dependency tranreason rightdead rpt, tobehidden)
  | TransitivityPT (el, lpt, rpt) ->
      let rec tfringe pt ts =
        (* ignoring cuttery for the moment, but not for long *)
        match pt with
          TransitivityPT (_, lpt, rpt) -> tfringe lpt (tfringe rpt ts)
        | LinPT (rp, _, concs, stopt, justopt) ->
            let pi = ordinarypi rp in
            let (c, (e, s, f)) =
              try _The (explodedconc concs) with
                _The_ ->
                  raise
                    (Catastrophe_
                       ["transitive line ";
                        bracketedstring_of_list string_of_element "," concs])
            in
            (pi, c, e, textinfo_of_string TermFont s, f, dostopt stopt,
             linsubs pi justopt) ::
              ts
        | CutPT _ ->(* cuts will eventually be part of the game, but for now ... *)
           raise (Catastrophe_ ["CutPT in tfringe"])
        | BoxPT _ -> raise (Catastrophe_ ["BoxPT in tfringe"])
      in
      let fringe = tfringe pt [] in
      (* fringe is a list of components A=B, each of which gives us a line =B (and A
       * is the rhs of the previous element, if there is one).  If this component is 
       * unjustified, A can be selected for working 'downwards', and B for working 'upwards'. 
       * If the _next_ line is unjustified, B can be selected for working 'downwards' as well.
       * So we return the downclass of A for the previous line to work with, and we use the
       * downclass of B to compute this line.
       * We pass in the downclass of the last line, because it doesn't have a successor.
       *)
      (* I think that comment is out of date ... *)
      
      let rec mktp side pi el = pi, el, TranPlan side in
      let rec tprocess ((pi, c, e, s, f, st, subs), (deadf, ts)) =
        let (d, con) =
          match subs with
            None -> false, mktp Right
          | Some _ -> true, mktp Right
        in
        let rec deadabove d' con' pi' c' =
          let down = pi, c, TranPlan Left in
          let r =
            if d' then ElementPlan down
            else AmbigElementPlan (con' pi' c', down)
          in
          (*  consolereport 
                ["deadabove ", string_of_element c, " -- making ", string_of_elementplankind r];
           *)
          r
        in
        (*  consolereport 
              ["tprocess ", string_of_element c, "; ", "; subs ", string_of_int (bool_of_opt subs),
               "; d ", string_of_int d
              ]; 
         *)
        (if d then ordinary else deadabove),
        (s, (textinfo_of_term f, deadf (bool_of_opt subs) con pi c), subs) :: ts
      in
      let (pi, c, e, s, f, st, subs) = List.hd fringe in
      (* that CANNOT go wrong, surely *)
      let (deadf', tdeps) = nj_fold tprocess fringe (deadf, []) in
      (* this certainly should _not_ be generating mktp Left ... I think it should be some kind of
       * DisplayConc ... but for now this will do.
       *)
      TranDep (st, (textinfo_of_term e, deadf' true (mktp Left) pi c), tdeps)
(* dependency *)

   (****************** Final step: put everything in place, with line numbers ******************)
   
let rec _RR id = id + 1
let rec dec id = id - 1
type lineID = int
(* justifications are given as
    single lines: line number
    hypotheses: line number . position (zero position suppressed)
    boxes: linenumber - linenumber
 *)
(* perhaps C stands for Citation? *)
type cID =
    LineID of lineID
  | BoxID of (lineID * lineID)
  | HypID of (lineID * int)
  | NoID
(* for cut lines which we wish to hide *)

   (* This function produces a single piece of text as its result, which makes it a bit
    * difficult to split it across lines.  Oh well, one day.
    *)

let _IDr = (string_of_int : lineID -> string)

let rec _Cr =
  function
    LineID l -> _IDr l
  | BoxID (s, f) -> (_IDr s ^ "-") ^ _IDr f
  | HypID (l, n) ->(* we never make a BoxID with s=f *)
     (_IDr l ^ ".") ^ string_of_int n
  | NoID ->(* we never make a HypID with l=0 *)
     ""

let rec _IDstring cids =
  string_of_list (fun x -> x) "," ((fun s -> s <> "") <| List.map _Cr cids)

let rec mapn a1 a2 a3 =
  match a1, a2, a3 with
    id, [], hn -> empty
  | id, (h, _) :: elis, hn ->
      (mapn id elis (hn + 1) ++ (h |-> HypID (id, hn)))

type reasondesc = NoReason
                | ReasonDesc of (pathinfo * textinfo * cID list)
                | ReasonWord of textinfo
              
type fitchlinerec = 
      { lineID       : lineID; 
        elementsbox  : textbox;
        idplan       : displayclass plan;
        elementsplan : elementplankind plan list;
        reasonplan   : reasonplankind plan list;
        reason       : reasondesc}
        
type fitchboxrec = { outerbox : box; 
                     boxlines : fitchstep list; 
                     boxed : bool }
                     
 and fitchstep = FitchLine of fitchlinerec
               | FitchBox of fitchboxrec

(* this datatype is included because without it, I get lost in monstrous tuples.
 * It is the type of information accumulated by (rev)folding the _L function (in linearise)
 * across a list of subtrees.
 *)
type laccrec = { id : lineID; acclines : fitchstep list; elbox : box; idW : int;
                 reasonW : int; assW : int; lastmulti : bool }
type lacc = Lacc of laccrec

(* for similar reasons, here is the type of proof layouts *)
type layoutrec =
      { lines : fitchstep list; colonplan : displayclass plan;
        idmargin : int; bodymargin : int; reasonmargin : int;
        sidescreengap : int; linethickness : int; bodybox : box }
type layout = Layout of layoutrec

(* moved outside for OCaml *)
type token =
    S of (pos -> displayclass plan)
  | Num
  | RA of (pos -> reasonplankind plan)
  | AR of (pos -> reasonplankind plan)

let rec linearise screenwidth procrustean_reasonW dp =
  let termfontleading =
    max 1 (thrd (fontinfo TermFont))
  in
  let reasonfontleading =
    max 1 (thrd (fontinfo ReasonFont))
  in
  let leading = max termfontleading (reasonfontleading) in
  let linethickness = Draw.linethickness leading in
  let _ = setproofparams Japeserver.BoxStyle linethickness in
  (* done early, so that GUIs can be ready for anything *)
           
  let textleading = (3 * leading + 1) / 2 in (* space between text lines *)
  let boxvspace = 4*linethickness in         (* space between box and the stuff inside *)
  let boxhspace = 3*linethickness in         (* ditto *)
  let boxleading = textleading in
  (* space between box and next line *)
  let reasongap = boxhspace in
  (* gap between rightmost stuff and reasons *)
  let sidescreengap = leading in
  (* pretty-space at the left of the screen *)                

  let transindent = 5 * leading in
  let (commasize, _ as comminf) = textinfo_of_text (Absprooftree.comma ()) in
  let commaW = tsW commasize in
  
  let minreasonW = 5*commaW in

  let (dotssize, _ as dotsinf) = textinfo_of_string ReasonFont ". . ." in
  
  (* In formatting a line, we recognise four elements: 
   *   line number N
   *   assertion   
   *   reason      R
   *   antecedents A
   * Other elements are inserted as written.
   * The format is a pair of strings (left, right), interpreted as stuff to appear on the left
   * and right of the assertion respectively.  To make it work in the Jape environment the pair 
   * is encoded as a single string with a newline at the breakpoint. 
   * Each element must appear exactly once in the format.
   * Numbers and assertions each appear in a column of their own; 
   * R and A must be adjacent (apart from punctuation), and make a column.
   * If an element appears on the left of the assertion, the column is right-justified;
   * if on the right, left-justified.
   *
   * We build a function which will take an assertion plan, a number plan, a reason plan 
   * and an antecedents plan (each with zero origin) and which will build a line plan. 
   * It also takes a tuple of column widths and updates it appropriately.
   *
   * To begin with this function is interpretive (oh dear). 
   *) 
  let rec formatplan
    assplan numplan reasonplan antesplan (idW, assW, reasonW) =
    let rec ispunct c = not (member (c, ["N"; "R"; "A"])) in
    let rec splitwhile f xs = takewhile f xs, dropwhile f xs in
    let rec pf cs =
      plan_of_textinfo (textinfo_of_string ReasonFont (implode cs))
    in
    let rec getelements cs =
      match splitwhile ispunct cs with
        pre, "N" :: cs' ->
          S (pf pre DisplayPunct) :: Num :: getelements cs'
      | pre, "A" :: cs' ->
          begin match splitwhile ispunct cs' with
            sep, "R" :: cs'' ->
              S (pf pre DisplayPunct) :: AR (pf sep ReasonPunctPlan) ::
                getelements cs''
          | _ -> raise (Catastrophe_ ("A not followed by R in " :: cs))
          end
      | pre, "R" :: cs' ->
          begin match splitwhile ispunct cs' with
            sep, "A" :: cs'' ->
              S (pf pre DisplayPunct) :: RA (pf sep ReasonPunctPlan) ::
                getelements cs''
          | _ -> raise (Catastrophe_ ("R not followed by A in " :: cs))
          end
      | post, [] -> [S (pf post DisplayPunct)]
      | _ -> raise (Catastrophe_ ("getelement (boxdraw) can't parse " :: cs))
    in
    let fs = explode !boxlineformat in
    let rec notsep c = c <> "\n" in
    let (ls, rs) =
      takewhile notsep fs,
      (match dropwhile notsep fs with
         [] -> raise (Catastrophe_ ["no separator in boxlineformat "; !boxlineformat])
       | _ :: rs -> rs)
    in
    let (ltokens, rtokens) = getelements ls, getelements rs in ()
  in
  let colonplan = plan_of_string ReasonFont ": " DisplayPunct origin in
  let colonsize = textsize_of_plan colonplan in
  let reasonspacef =
    plan_of_textinfo (textinfo_of_string ReasonFont " ") ReasonPunctPlan
  in
  let lantesparenf =
    plan_of_textinfo (textinfo_of_string ReasonFont "(") ReasonPunctPlan
  in
  let rantesparenf =
    plan_of_textinfo (textinfo_of_string ReasonFont ") ") ReasonPunctPlan
  in
  let rec commaf p = plan_of_textinfo comminf ElementPunctPlan p in
  let rec nullf p = [], emptytextbox in
  let rec foldformula a1 a2 =
    match a1, a2 with
      w, (_, ElementPunctPlan as el) -> el
    | w, ((size, _), epi as el) ->
        if tsW size <= w then
          begin if !boxfolddebug then consolereport ["too narrow"]; el end
        else
          let e =
            match epi with
              ElementPlan      (_, e, _)      -> e
            | AmbigElementPlan ((_, e, _), _) -> e
            | ElementPunctPlan                ->
                raise (Catastrophe_ ["foldformula ElementPunctPlan"])
          in
          Termfold.termfold TermFont textleading termfontleading textleading
                            w (stripelement e), 
          epi
  in
  (*************** the engine room ******************************: 
   * given a line number, a list of elements and reason information,
   * make the various plans required to place things on the screen
   *)
   
  (* Construct the elements plan for a single line (relative to p). 
   * _All this function does is put the dots in if there is no justification
   *)
  let rec mkelementsplan mkelps proven p =
    let (elementsplanlist, elementsbox as elementsplan) = mkelps p in
    if proven then elementsplan
    else
      let elementspos = tbPos elementsbox in
      let elementssize = textsize_of_textbox elementsbox in
      let dotspos =
        pos
          (posX elementspos,
           posY elementspos - tsA elementssize - textleading -
             tsD dotssize)
      in
      let dotsplan = plan_of_textinfo dotsinf ElementPunctPlan dotspos in
      let allsize =
        textsize_of_textbox (( +|-|+ ) (elementsbox, textbox (dotspos, dotssize)))
      in
      dotsplan :: elementsplanlist,
      textbox
        (elementspos,
         textsize (tsW allsize, tsA allsize - textleading, tsD allsize))
  in
           
  (* make the plan for a single reason, relative to p *)
  let rec mkreasonplan reason p =
    match reason with
      None                 -> [], emptytextbox, NoReason
    | Some (pi, why, cids) ->
        let rplan, rbox =
          (let reasonf = plan_of_textinfo why (ReasonPlan pi) in
           let antesf =
             plan_of_textinfo (textinfo_of_string ReasonFont (_IDstring cids))
               ReasonPunctPlan
           in
           if !boxlinedisplay = "right" then
             plancons (reasonf p)
               (fun p -> plancons (reasonspacef p) (plans_of_plan <.> antesf))
           else if null cids then plans_of_plan (reasonf p)
           else
             plancons (lantesparenf p)
               (fun p ->
                  plancons (antesf p)
                    (fun p -> plancons (rantesparenf p) (plans_of_plan <.> reasonf))))
        in
          rplan, rbox, ReasonDesc (pi, why, cids)
  in
  let rec ljreasonplan ps box =
    let shift = pos (- tsW (textsize_of_textbox box), 0) in
    List.map (fun p -> planOffset p shift) ps
  in
  let showword word p =
    let plan, box = plans_of_plan (plan_of_textinfo word ReasonPunctPlan p) in
    plan, box, ReasonWord word
  in
  (* make the data structure for a single line, positioned relative to topleftpos *)
  let rec mkLine elf reasonf id topleftpos =
    (* we construct the plans for elements, line number and reason so that their
     * baseline is originY; then we make bigelementsbox to say where the elements really are
     *)
    let (elementsplan, elementsbox) = elf origin in
    let elementssize = textsize_of_textbox elementsbox in
    let (idsize, _ as idinfo) = textinfo_of_string ReasonFont (_IDr id) in
    let idplan =
      plan_of_textinfo idinfo DisplayPunct (rightby (origin, - tsW idsize))
    in
    (* this is right justified *)
    let (reasonplan, reasonbox, reason) = reasonf origin in
    let reasonsize = textsize_of_textbox reasonbox in
    let linesize = ( +-+ ) (( +-+ ) (elementssize, reasonsize), idsize) in
    (* just to get A, _D *)
    let bigelementspos = downby (topleftpos, tsA linesize) in
    let bigsize =
      textsize
        (tsW elementssize + posX (tbPos elementsbox), tsA linesize,
         tsD linesize)
    in
    let bigelementsbox = textbox (bigelementspos, bigsize) in
    FitchLine
      {lineID = id; elementsbox = bigelementsbox; idplan = idplan; elementsplan = elementsplan; 
       reasonplan = if !boxlinedisplay = "right" then reasonplan
                    else ljreasonplan reasonplan reasonbox;
       reason = reason},
    box_of_textbox bigelementsbox, tsW idsize, tsW reasonsize
  in
  let rec startLacc id pos =
    Lacc {id = id; acclines = []; elbox = box (pos, nullsize); 
          idW = 0; reasonW = 0; assW = 0; lastmulti = false}
  in
  let rec nextpos b leading lastmulti thismulti =
    if isemptybox b then topleft b 
    else downby (botleft b, (if lastmulti || thismulti then 2*leading else leading + 1))
  in
  (* idok is what to do if you are an IdDep -- 
   *   true means disappear if you like;
   *   false means you are the last line of a box, so disappear iff you refer to the previous line.
   *)
  let rec _L wopt hypmap idok dp (Lacc accrec as acc) =
      let rec getIdDep el =
        match mapped sameresource hypmap el with
          Some cid -> cid, acc
        | None     ->
            raise (Catastrophe_ ["linearise can't find hypothesis "; string_of_element el])
      in
      let getcid = _L wopt hypmap true in
      (* convert reasoninfo -- reason and antecedent information -- 
       * to a sequence of lines and a justification (plus pathinfo for later)
       *)
      let rec dolinsubs hypmap acc justopt =
        match justopt with
          None -> acc, None
        | Some (pi, rinf, lprins, subdps) ->
            let lcids =
              (fun lp ->
                 try _The (mapped sameresource hypmap lp) with
                   _The_ -> raise (Catastrophe_
                                     ["linearise can't decode lprin "; string_of_element lp])) 
              <* lprins
            in
            let rec dosub (dp, (cids, acc)) =
              let (cid, acc') = getcid dp acc in cid :: cids, acc'
            in
            let (cids', acc') = nj_revfold dosub subdps (List.rev lcids, acc) in
            acc', Some (pi, rinf, List.rev cids')
      in
      (* plan a line: mkp does the content *)
      let rec doconcline mkp needsreason (acc, justinf) multi =
        let (Lacc {id = id; acclines = lines; elbox = elbox;
                   idW = idW; reasonW = reasonW; assW = assW; lastmulti=lastmulti}) = acc in
        let eplaninf =
          mkelementsplan mkp (not needsreason || bool_of_opt justinf)
        in
        let (line, ebox, iW, rW) =
          mkLine eplaninf (mkreasonplan justinf) id
            (nextpos elbox textleading lastmulti multi)
        in
        id,
        Lacc {id = _RR id; acclines = line :: lines; elbox = ( +||+ ) (elbox, ebox);
              idW = max iW (idW); reasonW = max rW (reasonW); assW = assW; lastmulti=multi}
      in
      (* info to prefix a line with a turnstile *)
      let rec stprefix stopt restf p =
        match stopt with
          Some st -> plancons (plan_of_textinfo st ElementPunctPlan p) restf
        | _ -> restf p
      in
      match dp with
        IdDep (el, lindep) ->
          if idok ||
             (match mapped sameresource hypmap el with
                Some (LineID id') -> accrec.id = _RR id'
              | _                 -> false)
          then
            getIdDep el
          else _L wopt hypmap false lindep acc
      | LinDep (concels, stopt, justopt) ->
          let concels' =
            match !foldformulae, wopt with
              true, Some bestW ->
                   foldformula (bestW - 2 * posX (topleft accrec.elbox) - commaW) <* concels
            | _ -> concels
          in
          let rec mkp p =
            stprefix stopt
              (plans_of_things (uncurry2 plan_of_textinfo) commaf nullf concels')
              p
          in
          let (id', acc') =
            doconcline mkp true (dolinsubs hypmap acc justopt) (List.length concels'>1)
          in
          LineID id', acc'
      | BoxDep (boxed, words, hypelis, dp) ->
          let (topleftpos, hindent, vindent, innerpos) =
            if boxed then
              let topleftpos = nextpos accrec.elbox boxleading accrec.lastmulti false in
              let hindent = linethickness + boxhspace in
              let vindent = linethickness + boxvspace in
              topleftpos, hindent, vindent,
              topleftpos +->+ pos (hindent, vindent)
            else
              let topleftpos = nextpos accrec.elbox textleading accrec.lastmulti false in
              topleftpos, 0, 0, topleftpos
          in
          let hyplines =
            match wopt with
              None -> [hypelis] (* first pass - just put them all on one line *)
            | Some bestW -> (* We make a proper 'minimum waste' split of the assumption line *)
                let rec measureplan (_, ((size, _), _)) = tsW size + commaW (* more or less *) in
                let mybestW = max (2 * tsW (fst (fst words))) (bestW - 2 * posX innerpos) in
                minwaste measureplan mybestW
                  ((fun (e, inf) -> e, if !foldformulae then foldformula mybestW inf 
                                                        else inf) <* hypelis)
          in
          let dohypline (hypelis, (hypmap, Lacc haccrec)) =
            let (word, hypmap') =
              match hypelis with
                [h] -> fst words, (hypmap ++ (fst h |-> LineID haccrec.id))
              | hs  -> snd words, (hypmap ++ mapn haccrec.id hs 1)
            in
            let (line, linebox, lineidW, linereasonW) =
              mkLine
                (plans_of_things
                   (uncurry2 plan_of_textinfo <.> snd) commaf nullf hypelis)
                (showword word) haccrec.id (nextpos haccrec.elbox textleading false false)
            in
            let lineassW =
              match hypelis with
                [_] -> 0
              | _   -> sW (bSize linebox) + 2 * posX innerpos
            in
            hypmap',
            Lacc {id = _RR haccrec.id; acclines = line :: haccrec.acclines;
                  elbox = if null haccrec.acclines then linebox else ( +||+ ) (haccrec.elbox, linebox);
                  idW = max haccrec.idW lineidW; reasonW = max haccrec.reasonW linereasonW;
                  assW = max haccrec.assW lineassW; lastmulti = false}
          in
          let (hypmap', (Lacc {id = id'} as acc')) =
            nj_revfold dohypline hyplines (hypmap, startLacc accrec.id innerpos)
          in
          let (cid, Lacc {id = id''; acclines = innerlines; elbox = innerbox;
                          idW = idW'; reasonW = reasonW'; assW = assW'})
            = _L wopt hypmap' false dp acc'
          in
          let outerbox = bOutset innerbox (size (hindent, vindent)) in
          let cid' =
            match cid with
              LineID jd     -> BoxID (accrec.id, jd)
            | BoxID (_, jd) -> BoxID (accrec.id, jd)
            | HypID _       -> if accrec.id = id' then LineID accrec.id else BoxID (accrec.id, id')
            | NoID          -> raise (Catastrophe_ ["NoID in BoxDep"])
          in
          cid',
          Lacc {id = id'';
                acclines = 
                  FitchBox {outerbox = outerbox; boxlines = innerlines; boxed = boxed} 
                    :: accrec.acclines;
                elbox = ( +||+ ) (accrec.elbox, outerbox); idW = max accrec.idW idW';
                reasonW = max accrec.reasonW (reasonW'); assW = max accrec.assW assW'; 
                lastmulti = false }
      | CutDep (ldp, cutel, rdp, tobehidden) ->
          (* this is where we hide cut hypotheses completely, if asked.  If the lhs is a single 
           * line, with some antecedents, we replace references to the cut formula by reference 
           * to those antecedents.
           * Actually, for the moment, we can only deal with lhss which are a single line with 
           * a single hypothesis antecedent.  Sorry.
           * RB 27/xii/99
           *)
          (* NoID now lets us hide lines with no antecedents *)
          let rec leftdefault () = getcid ldp acc in
          let rec noway () =
            if !cuthidingdebug then
              consolereport ["boxdraw can't hide "; string_of_dependency ldp];
            leftdefault ()
          in
          let (cutelid, acc') =
            match tobehidden, ldp with
              true, LinDep (_, _, Some just) ->
                begin match just with
                  _, _, [el], [] -> getIdDep el
                | _, _, [], [IdDep (el, _)] -> getIdDep el
                | _, _, [], [] -> NoID, acc
                | _ -> noway ()
                end
            | true, IdDep (el, _) -> getIdDep el
            | true, _ -> noway ()
            | _ -> leftdefault ()
          in
          _L wopt ((hypmap ++ (cutel |-> cutelid))) idok rdp acc'
      | TranDep (stopt, terminf, tdeps) ->
          (* In the first phase we just accumulate all the justification info, generating
           * the antecedent lines.
           * In the second phase we generate the lines
           *)
          let rec phase1 ((s, f, just), (ts, acc)) =
            let (acc', just') = dolinsubs hypmap acc just in
            (s, f, just') :: ts, acc'
          in
          let (revts, (Lacc {id = id'} as acc')) =
            nj_revfold phase1 tdeps ([], acc)
          in
          (* revts is, of course, backwards ... *)
      
      (* put in the beginning of the transitive game (forgot this for a time ...) *)
          let rec sourceline p =
            stprefix stopt
              (plans_of_plan <.> uncurry2 plan_of_textinfo terminf)
              p
          in
          let (id'', acc'') = doconcline sourceline false (acc', None) false in
          let rec phase2 ((s, f, just), (_, acc)) =
            let rec mkp p =
              let splan =
                plan_of_textinfo s ElementPunctPlan
                  (rightby (p, transindent))
              in
              plancons splan
                   (plans_of_plan <.> uncurry2 plan_of_textinfo f <.> 
                    (fun p' -> rightby (p', transindent)))
            in
            doconcline mkp true (acc, just) false
          in
          let (jd, acc''') = nj_fold phase2 revts (id'', acc'') in
          BoxID (id', jd), acc'''
  in
  (* stuff to do with computing margins and gaps *)
  (* One day boxlinedisplay will tell us in detail how to show a line.
  * At present the 'right' style is n: F R A -- n right-justified, R aligned
  * and the 'left' style is (A R) n: F -- (A R) and n right-justified
  *)
  let idmargin idW reasonW =
    (if !boxlinedisplay = "right" then 0 else reasonW + reasongap) + idW +
      tsW colonsize
  in
  let rec leftmargin idW reasonW = idmargin idW reasonW + tsW colonsize in
  let rec boxW box = sW (bSize box) in
  let rec reasonspace lines =
    match lines with
      [FitchBox {boxed = true}] -> reasongap
    | _                         -> 2 * reasongap
  in
  let rec reasonmargin lines idW reasonW elbox =
    if !boxlinedisplay = "right" then
      leftmargin idW reasonW + boxW elbox + reasonspace lines
    else reasonW
  in
  let allbutreasonW lines idW elbox =
    (* reasonably accurate for right and left reasonstyle *)
    sidescreengap + idW + tsW colonsize + boxW elbox + reasonspace lines + sidescreengap
                  + reasonspace lines (* seems to be useful but I don't know why *)
  in
  let extras lines idW reasonW =
    sidescreengap + 
      leftmargin idW reasonW +
      (if !boxlinedisplay = "right" then reasonspace lines + reasonW else 0) +
    sidescreengap
  in
  let answer (Lacc {acclines = lines; elbox = elbox; idW = idW; reasonW = reasonW; assW = assW}) =
    Layout {lines = lines; colonplan = colonplan; idmargin = idmargin idW reasonW;
            bodymargin = leftmargin idW reasonW; 
            reasonmargin = reasonmargin lines idW reasonW elbox;
            sidescreengap = sidescreengap; linethickness = linethickness;
            bodybox = elbox}
  in
  (* we do it once, then see if we might be able to make it smaller *)
  let startacc = startLacc 1 origin in
  let (_, (Lacc {elbox = elbox; idW = idW; reasonW = reasonW; assW = assW; acclines = lines} 
           as firstlayout))
    = _L None empty false dp startacc
  in
  (* body of linearise *)
  if extras lines idW reasonW + boxW elbox > screenwidth &&
     (!foldformulae || assW = boxW elbox)
  then
    (* we have a picture which is too wide, and might be made less wide *)
    let maxbestW = screenwidth - extras lines idW procrustean_reasonW in
    if !boxfolddebug then
      consolereport
        ["trying again, width "; string_of_int maxbestW; "; screenwidth ";
         string_of_int screenwidth];
    let (_, (Lacc acc as secondlayout)) = _L (Some maxbestW) empty false dp startacc in
    let Layout a = answer secondlayout in
    let availableW = screenwidth - allbutreasonW a.lines acc.idW acc.elbox in
    let rereason line =
      match line.reason with
        NoReason -> line
      | ReasonDesc (pi, why, cids) -> 
          let reasonplan = line.reasonplan in
          let w = availableW - (tsW (textsize_of_planlist reasonplan) - tsW (fst why)) in
          let why' = textinfo_procrustes (max minreasonW w) origin why in
          {line with reasonplan = fst_of_3 (mkreasonplan (Some (pi, why', cids)) origin)}
      | ReasonWord why ->
          let why' = textinfo_procrustes (max minreasonW availableW) origin why in
          {line with reasonplan = fst_of_3 (showword why' origin)}
    in
    let rec reline f =
      match f with 
        FitchLine l -> FitchLine (rereason l)
      | FitchBox  b -> FitchBox {b with boxlines = List.map reline b.boxlines}
    in
    Layout {a with lines = List.map reline a.lines}
  else answer firstlayout
  (* end linearise *)
  
(* desperation ...
    fun _IDstring id =
      case id of
       LineID l     => "LineID "^_IDr l
     | BoxID(s,f)   => "BoxID("^_IDr s^","^_IDr f^")"
     | HypID(id, n) => "HypID("^_IDr id^","^string_of_int n^")"
... end desperation *)

let rec _BoxLayout screenwidth t =
  let pt = pretransform (List.length (turnstiles ()) <> 1) t in
  let procrustean_reasonW = max 100 (screenwidth / 6) in
  let tranreason = (* we now truncate reasons later *)
    (* if !truncatereasons then
      procrustean_reason2textinfo procrustean_reasonW
    else *) textinfo_of_reason
  in
  let dp = dependency tranreason ordinary pt in
  linearise screenwidth procrustean_reasonW dp
  (* end of _BoxLayout *)

(* The emphasis (blacken/greyen) stuff is pretty confused.  
 *
 * It seems we need to do several things:
 *
 * 1. When selecting a conclusion line (including default selection, when it is taken
 * because it is on the autoselect path), greyen and deselect all other conclusions,
 * greyen all hypotheses except those available in its lhs.
 *
 * 2. When selecting a hypothesis, greyen all conclusions except those which use this
 * hypothesis, and all hypotheses which aren't available in the same lhs as this one,
 * somewhere in the Absprooftree.
 *
 * 3. When a conclusion line is selected _after_ a hypothesis, only grey, don't blacken.
 * Ditto when a hypothesis is selected after a conclusion.
 *
 * We don't yet know what to do about ambiguous selections, nor how to interact with
 * dead conclusion lines.  We are more than a little confused about how to blacken/greyen
 * commas, reasons, etc.
 *)

let rec elementsin ps =
  List.length ((iselementkind <.> planinfo) <| ps)

let rec draw goalopt p proof =
  fun (Layout {lines = lines; colonplan = colonplan; idmargin = idmargin;
               bodymargin = bodymargin; reasonmargin = reasonmargin;
               bodybox = bodybox; linethickness = linethickness}) ->
    let idx = posX p + idmargin in
    let reasonx = posX p + reasonmargin in
    let samepath path =
      function
        None          -> false
      | Some goalpath -> path = goalpath
    in
    let rec _D p line =
      match line with
        FitchLine {elementsplan = elementsplan; elementsbox = elementsbox; 
                   idplan = idplan; reasonplan = reasonplan} ->
          let pdraw = p +->+ tbPos elementsbox in
          let rec emp gpath plan =
            (* (Elementplan((el,siopt),_,class,elbox)) = *)
            let rec dohigh () = highlight (tbPos (plantextbox plan) +->+ pdraw) (Some DisplayConc) in
            let rec dogrey () = greyen (tbPos (plantextbox plan) +->+ pdraw) in
            let rec doconc path = if gpath = path then dohigh () else dogrey () in
            
            match planinfo plan with
              ElementPlan ({path = path}, el, HypPlan) ->
                if validhyp proof el gpath then () else dogrey ()
            | ElementPlan ({path = path}, el, ConcPlan) ->
                if elementsin elementsplan = 1 then doconc path
            | AmbigElementPlan
                (({path = up}, upel, ConcPlan),
                 ({path = down}, downel, HypPlan)) ->
                if gpath = up then
                  (if elementsin elementsplan = 1 then doconc up)
                else if validhyp proof downel gpath then ()
                else dogrey ()
            | ElementPlan ({path = path}, el, TranPlan Left) ->
                doconc path
            | ElementPlan ({path = path}, el, TranPlan Right) ->
                (* highlight left side of sided things for a change *)
                if path = gpath then () else dogrey ()
            | AmbigElementPlan
                (({path = up}, upel, TranPlan Right),
                 ({path = down}, downel, TranPlan Left)) ->
                if gpath = up then () else doconc down
            | AmbigElementPlan
                (({path = up}, upel, TranPlan Right),
                 ({path = down}, downel, HypPlan)) ->
                if gpath = up then ()
                else if validhyp proof downel gpath then ()
                else dogrey ()
            | ElementPunctPlan -> ()
            | _ -> raise (Catastrophe_ ["emp in _D "; debugstring_of_plan string_of_elementplankind plan])
          in
          let y = posY pdraw in
          let idpos = pos (idx, y) in
          drawplan (fun i -> i) idpos idplan;
          drawplan (fun i -> i) idpos colonplan;
          List.iter (drawplan elementplanclass pdraw) elementsplan;
          List.iter (drawplan reasonplanclass (pos (reasonx, y))) reasonplan;
          begin match goalopt with
            Some gpath -> List.iter (emp gpath) elementsplan
          | None -> ()
          end
      | FitchBox {outerbox = outerbox; boxlines = lines; boxed = boxed} ->
          if boxed then drawBox (bOffset outerbox p); List.iter (_D p) lines
    in
    drawinproofpane (); List.iter (_D (rightby (p, bodymargin))) lines

let rec print str goalopt p proof =
  fun (Layout {lines = lines; colonplan = colonplan; idmargin = idmargin;
               bodymargin = bodymargin; reasonmargin = reasonmargin}) ->
    let rec samepath a1 a2 =
      match a1, a2 with
        path, None -> false
      | path, Some goalpath -> path = goalpath
    in
    let out = output_string str in
    let outesc = out <.> String.escaped in
    let rec outplan p = out "\""; outesc (string_of_plan  p); out "\" " in
    let rec _D p line =
      match line with
        FitchLine
          {idplan = idplan;
           elementsplan = elementsplan;
           reasonplan = reasonplan} ->
          out "(LINE ";
          outplan idplan;
          out "(ASSERT ";
          List.iter outplan elementsplan;
          out ") ";
          out "(BECAUSE ";
          List.iter outplan reasonplan;
          out "))\n"
      | FitchBox {outerbox = outerbox; boxlines = lines; boxed = boxed} ->
          (* DO THIS STUFF LATER
          case goalopt of 
            Some gpath => List.iter (emp gpath) elementsplan
          | None       => ()
          *)
          if boxed then out "(BOX\n";
          revapp (_D p) lines;
          if boxed then out ")\n"
    in
    revapp (_D (rightby (p, bodymargin))) lines

let couldbe path = function
                     Some truepath -> truepath
                   | None          -> path

let answerpath hitkind {path = path; layoutpath = layoutpath; prunepath = prunepath} =
  match hitkind with
    LayoutPath -> couldbe path layoutpath
  | PrunePath  -> couldbe path prunepath
  | HitPath    -> path

let cp hitkind (pi, el, kind) =
  match kind with
    TranPlan side -> answerpath hitkind pi, (el, Some side)
  | _             -> answerpath hitkind pi, (el, None)

let hp hitkind (pi, el, kind) = answerpath hitkind pi, el

let hit_of_pos p 
               (Layout {lines = lines; bodymargin = bodymargin; reasonmargin = reasonmargin})
               hitkind =
  if !boxseldebug then
    consolereport
      ["hit_of_pos "; " "; string_of_pos p; " ... "; string_of_hitkind hitkind];
  let cp = cp hitkind in
  let hp = hp hitkind in
  let rec _H p =
    function
      FitchLine {elementsbox = elementsbox; elementsplan = elementsplan; reasonplan = reasonplan} ->
        let rec decodeplan =
          function
            ElementPlan (pi, el, kind as pl) ->
              (match kind with
                 HypPlan -> Some (FormulaHit (HypHit (hp pl)))
               | _       -> Some (FormulaHit (ConcHit (cp pl))))
          | AmbigElementPlan (up, dn) ->
              Some (FormulaHit (AmbigHit (cp up, hp dn)))
          | ElementPunctPlan -> None
        in
        if withintb (p, elementsbox) then
          findfirstplanhit (p +<-+ tbPos elementsbox) elementsplan 
          &~~ (_Some <.> planinfo) 
          &~~ decodeplan
        else
          findfirst
            (fun reason ->
               match planinfo reason with
                 ReasonPlan pi ->
                   if withintb
                        (p +<-+ pos (reasonmargin - bodymargin, posY (tbPos elementsbox)),
                         plantextbox reason)
                   then
                     Some (ReasonHit (answerpath hitkind pi))
                   else None
               | _ -> None)
            reasonplan
    | FitchBox {outerbox = outerbox; boxlines = lines} ->
        if withinY (p, outerbox) then findfirst (_H p) lines else None
  in
  findfirst (_H (rightby (p, -bodymargin))) lines

let allFormulaHits pos (Layout {lines = lines; bodymargin = bodymargin}) =
  let targetpath =
    function
      ElementPlan ({path = path}, _, ConcPlan) -> Some path
    | _ -> None
  in
  let cp = cp HitPath in
  let hp = hp HitPath in
  let rec allts pos rs ls = 
    List.fold_left (onet pos) rs ls (* efficient, but backwards -- don't think it matters *)
  and onet pos rs l =
    let oneel pos rs (Formulaplan (_, textbox, c)) =
      let tbox = tbOffset textbox pos in
      match c with 
        ElementPlan (pi, el, kind as pl) ->
         (tbox, match kind with
                  HypPlan -> HypHit (hp pl)
                | _       -> ConcHit (cp pl)) :: rs
          | AmbigElementPlan (up, dn) ->
              (tbox, AmbigHit (cp up, hp dn)) :: rs
          | ElementPunctPlan -> rs
    in
    match l with
      FitchLine {elementsplan = elementsplan; elementsbox = elementsbox} ->
        List.fold_left (oneel (pos +->+ tbPos elementsbox)) rs elementsplan
    | FitchBox {boxlines = boxlines} -> allts pos rs boxlines
  in
  allts (rightby (pos, bodymargin)) [] lines
  
let rec locateHit pos classopt hitkind (p, proof, layout) =
  hit_of_pos ((pos +<-+ p)) layout hitkind &~~
  (_Some <.> 
   (function
       FormulaHit (AmbigHit (up, dn)) as h ->
         begin match classopt with
           Some DisplayConc -> FormulaHit (ConcHit up)
         | Some DisplayHyp -> FormulaHit (HypHit dn)
         | None -> h
         | _ ->
             raise
               (Catastrophe_
                  ["locateHit (boxdraw) finds "; string_of_hit string_of_path h;
                   ", given classopt ";
                   string_of_option string_of_displayclass classopt])
         end
     | h -> h))
     
(* Greyening and blackening is now (cross fingers) simplified, at least during selection.
 * We get told when a selection is made (Some(pos, class)) or cancelled (None),
 * and at the same time we are told the current selections.
 * In either case we make up the selection that is, and greyen/blacken accordingly.
 * There is no longer any attempt to do differential greyening/blackening.
 * RB 29/viii/00
 *)
(* Here are the rules:
 *   - when you select a conclusion, all hypotheses that aren't relevant are greyened (cancelling
 *       any irrelevant hyp selections at the same time), and we blacken tip conclusions that share
 *       all the relevant selected hypotheses, greyening the rest;
 *   - when you select hypotheses but no conclusion, irrelevant tip (and all non-tip) conclusions are greyened,
 *       and we blacken hypotheses which occur at the selected position or which occur in sequents that 
 *       include all the selected hypotheses, greyening the rest;
 *   - when you select a reason, all formulae are blackened.
 *
 * The idea is to make it possible to select relevant hypotheses, and to be able to switch conclusions
 * with a single click.
 * RB 29/viii/00
 *) 

let rec notifyselect posclassopt posclasslist =
  fun
    (proofpos, proof,
     (Layout {lines = lines; bodymargin = bodymargin} as plan) as info) ->
    let rec bg emp =
      let rec reemphasise p line =
        match line with
          FitchLine
            {elementsbox = elementsbox; elementsplan = elementsplan} ->
            let p' = p +->+ tbPos elementsbox in
            List.iter
              (fun plan ->
                 if iselementkind (planinfo plan) then
                   emp plan (p' +->+ tbPos (plantextbox plan)))
              elementsplan
        | FitchBox {outerbox = outerbox; boxlines = lines} ->
            List.iter (reemphasise p) lines
      in
      List.iter (reemphasise (rightby (proofpos, bodymargin))) lines
    in
    let rec blackenthelot () = bg (fun _ -> blacken) in
    let hits =
         (fun (pos, class__) ->
            try _The (locateHit pos (Some class__) HitPath info) with
              _ ->
                raise
                  (Catastrophe_
                     ["notifyselect (boxdraw) can't identify ";
                      string_of_pair string_of_pos string_of_displayclass ","
                        (pos, class__)])) <*
         (match posclassopt with
            None -> posclasslist
          | Some hc -> hc :: posclasslist)
    in
    let rec bang s =
      raise
        (Catastrophe_
           ["notifyselect (boxdraw) sees "; s; " in ";
            bracketedstring_of_list (string_of_hit string_of_path) "," hits])
    in
    let (hyps, concs, reasons) =
      nj_fold
        (function
           FormulaHit (HypHit (hpath, hel)), (hs, cs, rs) ->
             (hpath, hel) :: hs, cs, rs
         | FormulaHit (ConcHit (cpath, (cel, _))), (hs, cs, rs) ->
             hs, (cpath, cel) :: cs, rs
         | ReasonHit rpath, (hs, cs, rs) -> hs, cs, rpath :: rs
         | _ -> bang "non Conc/Hyp/ReasonHit")
        hits ([], [], [])
    in
    let rec okhyps hyps path =
      not (List.exists (fun (_, hel) -> not (validhyp proof hel path)) hyps)
    in
    match hyps, concs, reasons with
      [], [], [] -> blackenthelot ()
    | [], [], [_] ->(* no selection *)
       blackenthelot ()
    | (hpath, _) :: hs, [], [] ->
        (* single reason selection *)
        (* hyps, but no conc *)
        let path =
          nj_fold
            (fun ((hpath, hel), path) ->
               if validhyp proof hel path then path else hpath)
            hs hpath
        in
        let rec emp plan =
          let rec blackhyp (({path = hpath} : pathinfo), el, _) =
            validhyp proof el path || okhyps hyps hpath
          in
          let rec blackconc (({path = cpath} : pathinfo), _, _) =
            stillopen proof cpath && okhyps hyps cpath
          in
          if match planinfo plan with
               ElementPlan (_, _, HypPlan as inf) -> blackhyp inf
             | ElementPlan inf -> blackconc inf
             | AmbigElementPlan (up, down) -> blackconc up || blackhyp down
             | ElementPunctPlan -> (* oh this is a can of worms! *) true
          then
            (* can't happen *) blacken
          else greyen
        in
        bg emp
    | _, [cpath, _], [] ->
        (* conc selection gives definite position *)
        let hyps =
          (fun (_, hel) -> validhyp proof hel cpath) <| hyps
        in
        let rec emp plan =
          let rec blackhyp (({path = hpath} : pathinfo), el, _) =
            validhyp proof el cpath
          in
          let rec blackconc (({path = cpath'} : pathinfo), _, _) =
            stillopen proof cpath' && okhyps hyps cpath'
          in
          if match planinfo plan with
               ElementPlan (_, _, HypPlan as inf) -> blackhyp inf
             | ElementPlan inf -> blackconc inf
             | AmbigElementPlan (up, down) ->
                 blackconc up || blackhyp down
             | ElementPunctPlan ->(* oh this is a can of worms! *)
                true
          then
            (* can't happen *)
            blacken
          else greyen
        in
        bg emp
    | _, _ :: _, _ -> bang "more than one ConcHit"
    | _, _, _ :: _ -> bang "more than one ReasonHit"

let refineSelection = true

let defaultpos screen (Layout {bodybox = bodybox; bodymargin = bodymargin;
                        sidescreengap = sidescreengap; linethickness = linethickness}) =
    let prooforigin = bPos bodybox in
    (* leftby bodymargin not needed ... *)
    (*
    consolereport ["defaultpos: screen is ", string_of_box screen, " proof is ", string_of_box bodybox, 
                   "\nbodymargin ", string_of_int bodymargin, " sidescreengap ", string_of_int sidescreengap];
    *)
    (* put the SW corner of the proof in the SW corner of the screen. Because of botleft this is
       1 pixel too high, but who cares?
     *)
    (* because there is now a single GUI implementation, and because selections are essentially
       outsets of 2*linethickness, I leave that much space below the proof
     *)
    upby (rightby (botleft screen, sidescreengap), sH (bSize bodybox)+2*linethickness)
    +<-+ prooforigin,
    screen, prooforigin

let rec rootpos viewport (Layout {lines = lines}) =
    (* position of last line in proof, first in lines *)
    let rec p =
      function
        FitchLine {elementsbox = elementsbox} -> Some (tbPos elementsbox)
      | FitchBox {boxlines = lines}           -> findfirst p lines
    in
    match findfirst p lines with
      Some p -> p
    | _      -> origin

let rec postoinclude screen box 
                     (Layout {bodymargin = bodymargin; sidescreengap = sidescreengap} as layout) =
    let (defpos, screen, prooforigin) = defaultpos screen layout in
    let otherdefpos = rightby (topleft screen, sidescreengap) +<-+ prooforigin
    in
    (*
    consolereport ["postoinclude: defpos ", string_of_pos defpos, " screen ", string_of_box screen, 
                          " prooforigin ", string_of_pos prooforigin,
                   "\nbOffset box defpos ", string_of_box (bOffset box defpos),
                       " entirelywithin screen ", string_of_int (bOffset box defpos entirelywithin screen),
                   "\notherdefpos ", string_of_pos otherdefpos, 
                       " bOffset box otherdefpos ", string_of_box (bOffset box otherdefpos),
                       " entirelywithin screen ", string_of_int (bOffset box otherdefpos entirelywithin screen)
                  ];
    *)
    (* prefer SW corner alignment *)
    if entirelywithin (bOffset box defpos, screen) then defpos
    else if(* or NW corner alignment *) 
     entirelywithin (bOffset box otherdefpos, screen) then
      otherdefpos
    else
      (* find p such that bOffset box p is in the middle of the screen:
       * that is, such that bPos box +->+ p = midp:
       * that is, choose p = midp +<-+ bPos Box.
       * I hope.
       *)
        (downby
           (rightby (bPos screen, bodymargin),
            (sH (bSize screen) - sH (bSize box)) / 2)
         +<-+ bPos box)

let rec layout viewport proof = _BoxLayout (sW (bSize viewport)) proof

(* This function is used in displaystyle.sml to position a proof.
 * I think it's best if the _conclusion_ box doesn't move.  Otherwise you get into all 
 * kinds of jumpy behaviour.
 *)
  
let targetbox pos target layout =
  match target, layout with
    None     , _                                               -> None
  | Some path, Layout {lines = lines; bodymargin = bodymargin} ->
      let ok =
        function 
          ElementPlan ({path = epath}, _, ConcPlan) as plankind ->
            if !screenpositiondebug then
              consolereport
                ["Boxdraw.targetbox.ok "; string_of_path path; "; "; 
                 string_of_elementplankind plankind; "; "; 
                 string_of_bool (path = epath)];
            path = epath
        | _ -> false
      in
      let rec search pos =
        function
          FitchLine {elementsplan = elementsplan; elementsbox = elementsbox} ->
            if List.exists (ok <.> planinfo) elementsplan then
               (if !screenpositiondebug then
                  consolereport
                    ["boxdraw targetbox gotcha "; string_of_textbox elementsbox];
                Some (tbOffset elementsbox pos))
            else None
        | FitchBox {boxlines = lines} -> findfirst (search pos) lines 
                                         (* oddly, no offset: see _D if you don't believe me *)
      in
      findfirst (search (rightby (pos, bodymargin))) lines

let rec samelayout =
  fun (Layout {lines = lines}, Layout {lines = lines'}) -> lines = lines'

let defaultpos screen = fst_of_3 <.> defaultpos screen

let highlight = highlight

(* a bit of desperate debugging ...
val _ =
let fun _IDstring id =
      case id of
       LineID l     => "LineID "^_IDr l
     | BoxID(s,f)   => "BoxID("^_IDr s^","^_IDr f^")"
     | HypID(id, n) => "HypID("^_IDr id^","^string_of_int n^")"
in
    consolereport
    ["BL ", string_of_int lastline, 
     " ", string_of_mapping string_of_element _IDstring hypmapin,
     " ", bracketedstring_of_list string_of_element "," hyps, 
     "; sequent=", string_of_seq (sequent t),
     "; hs=", bracketedstring_of_list string_of_element "," hs, 
     "; gs=", bracketedstring_of_list string_of_element "," gs, 
     "; reason=", string_of_option string_of_reason (reason t),
     "; id=", string_of_int id,
     "; cids=", bracketedstring_of_list _IDstring "," cids,
     "; rpath=", string_of_path rpath,
     "; hypmap=", string_of_mapping string_of_element _IDstring hypmap,
     "; thinnedL=", bracketedstring_of_list string_of_element "," thinnedL,
     "; prinids=", bracketedstring_of_list _IDstring "," prinids
     ]
end
*)
(* more desperate debugging
val _ = consolereport ["boxdraw begins"]

 *)

