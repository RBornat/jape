(*
        $Id$

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
 * Some cut nodes couldn't be drawn at all, because of deficiencies of the
 * click-to-select UI mechanism we were using; those that could be drawn fell into
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
 
 module M :
  sig
    include Screendraw
    val hidecut : bool ref
    val hidehyp : bool ref
    val hidetransitivity : bool ref
    val hidereflexivity : bool ref
    val outermostbox : bool ref
    val showrelations : bool ref
    val outerassumptionword : string ref
    val outerassumptionplural : string ref
    val innerassumptionword : string ref
    val innerassumptionplural : string ref
    val boxlinedisplay : string ref
    val boxseldebug : bool ref
    val boxfolddebug : bool ref
  end =
  struct
    open Listfuns
    open Box
    open Mappingfuns
    open Optionfuns
    open Tree
    open Text
    open Draw
    open Hit
    open Displayclass
    open Displayfont
    
    let element2textinfo = element2textinfo elementstring_invisbracketed
    let term2textinfo = term2textinfo termstring_invisbracketed
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
          begin match (element2term c &~~ explodebinapp) with
            Some bits -> Some (c, bits)
          | _ -> None
          end
      | _ -> None
    (* show a hit *)
    let my_hitstring = hitstring pathstring
    let my_selstring = selstring pathstring
    (*********************** Drawing transitive stacks *****************************
     * 
     * Suppose that we have a transitivity step
     * 
     * F=G  G=H
     * --------
     *   F=H
     *   
     * where neither of the leaves is a further transitivity step.  Then there are
     * four ways in which it might be displayed (all now allowed because we allow
     * arbitrary cuts):
     * 
     * a.   F
     *     ...
     *     = G
     *     = H  reason
     * 
     * b.   F
     *     = G  reason
     *     ...
     *     = H
     *     
     * c.   F
     *     = G  reason
     *     = H  reason
     *     
     * d.   F
     *     ...
     *     = G  
     *     ...
     *     = H
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
     *     = D
     *     = E
     *
     * then we have proved A=E (root of the stack), A=B, B=C, C=D and D=E, each of which is 
     * the root of a subtree; but we have also proved A=C, A=D, B=D, B=E and C=E. How do
     * we gesture at that?  Actually the answer is easy: press on B, drag to D, you've said
     * B=D.  But that's some way down the line, I think.
     *
     *)
     
    (*********************** Step 1: find out what to do at each node of the tree ***************)
    (* We don't have a special node for identity rules, because those nodes are treated differently in 
     * 'normal' and 'transitive' dependency transformation: it's done with a boolean in LinPT
     * at this stage.  We don't any longer try to find out which kind of cut we have: that's done
     * on the fly in stage 2.  We still find out if the cut hypothesis is needed in the right tree.
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
        let (st, hs, gs) = explode (sequent t) in
        let subts = subtrees t in
        let newhyps = ttsub hs hyps in
        let lprins = fst (tree.matched t) in
        let rec T hs (n, t) = pt (n :: rp) hs t in
        let rec mkl isid subs =
          !hidereflexivity && isStructureRulenode t ReflexivityRule,
          LinPT
            (RevPath rp, isid, gs,
             (if prefixwithstile then Some st else None),
             (why &~~ (fun r -> Some (r, lprins, subs))))
        in
        let rec aslines isid =
          mkl isid (respt <*> T hs) <* numbered subts)
        in
        let rec boxpt lines =
          false, BoxPT (RevPath rp, true, innerwords, newhyps, lines)
        in
        let rec hyps seq = snd (explode seq) in
        let rec concs seq = thrd (explode seq) in
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
                    let (_, pt1) = T (hs, (0, a1)) in
                    let (_, pt2) = T ((ch :: hs), (1, a2)) in
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
                  !hidetransitivity && isStructureRulenode t TransitivityRule,
                  gs, subts
                with
                  true, [c], [a1; a2] ->
                    let (a1r, a1pt as pt1) = T (hs, (0, a1)) in
                    let (a2r, a2pt as pt2) = T (hs, (1, a2)) in
                    if a1r then pt2
                    else if a2r then pt1
                    else false, TransitivityPT (c, a1pt, a2pt)
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
    type pathinfo =
      < path : int list; layoutpath : int list option;
        prunepath : int list option >
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
        ElementPlan (_, _, ConcPlan) -> DisplayConc
      | ElementPlan (_, _, HypPlan) -> DisplayHyp
      | ElementPlan (_, _, TranPlan _) -> DisplayConc
      | AmbigElementPlan _ -> DisplayAmbig
      | ElementPunctPlan ->(* for now *)
         DisplayPunct
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
    let rec pathinfostring
      {path = path; layoutpath = layoutpath; prunepath = prunepath} =
      ((((("{path=" ^ pathstring path) ^ ",layoutpath=") ^
           optionstring pathstring layoutpath) ^
          ",prunepath=") ^
         optionstring pathstring prunepath) ^
        "}"
    let rec elementplankindstring pk =
      (* how I HATE having to write these *)
      match pk with
        ElementPlan pi -> "ElementPlan" ^ elementplaninfstring pi
      | AmbigElementPlan pair ->
          "AmbigElementPlan" ^
            pairstring elementplaninfstring elementplaninfstring "," pair
      | ElementPunctPlan -> "ElementPunctPlan"
    and elementkindstring ek =
      match ek with
        ConcPlan -> "ConcPlan"
      | HypPlan -> "HypPlan"
      | TranPlan side -> "TranPlan " ^ sidestring side
    and elementplaninfstring epi =
      triplestring pathinfostring elementstring elementkindstring "," epi
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

    let elementplaninfostring =
      pairstring textinfostring elementplankindstring ", "
    let elinfostring = pairstring elementstring elementplaninfostring ","
    let rec trandepstring t =
      triplestring textinfostring elementplaninfostring reasoninfostring "," t
    and reasoninfostring r =
      optionstring
        (quadruplestring pathinfostring textinfostring
           (bracketedliststring elementstring ",")
           (bracketedliststring dependencystring ",") ",")
        r
    and dependencystring d =
      match d with
        LinDep l ->
          "LinDep" ^
            triplestring (bracketedliststring elementplaninfostring ",")
              (optionstring textinfostring) reasoninfostring "," l
      | BoxDep d ->
          "BoxDep" ^
            quadruplestring (string_of_int : bool -> string)
              (pairstring textinfostring textinfostring ",")
              (bracketedliststring elinfostring ",") dependencystring "," d
      | IdDep i -> "IdDep" ^ pairstring elementstring dependencystring "," i
      | CutDep c ->
          "CutDep" ^
            quadruplestring dependencystring elementstring dependencystring
              (string_of_int : bool -> string) "," c
      | TranDep t ->
          "TranDep" ^
            triplestring (optionstring textinfostring) elementplaninfostring
              (bracketedliststring trandepstring ",") "," t
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
        fun (RevPath rp) ->
          let module M =
            struct
              class a =
                object
                  val path = List.rev rp
                  val layoutpath = None
                  val prunepath = None
                  method path = path
                  method layoutpath = layoutpath
                  method prunepath = prunepath
                end
            end
          in
          new M.a
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
          Some st -> Some (string2textinfo TermFont ((" " ^ st) ^ " "))
        | _ -> None
      in
      match pt with
        LinPT (rp, isid, concs, stopt, justopt) ->
          let pi = ordinarypi rp in
          let rec mkplan e =
            element2textinfo e, deadf (opt2bool justopt) mkconcplan pi e
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
            e, (element2textinfo e, ElementPlan (pi, e, HypPlan))
          in
          BoxDep
            (boxit,
             (string2textinfo ReasonFont sing,
              string2textinfo ReasonFont plur),
             (mkplan <* hs), dependency tranreason ordinary pt')
      | CutPT (RevPath rp, ch, lpt, rpt, chneeded, tobehidden) ->
          let rootp = List.rev rp in
          let leftp = List.rev (0 :: rp) in
          let rightp = List.rev (1 :: rp) in
          let rec leftdead d con (({path = p} : pathinfo) as pi) el =
            let up =
              (let module M =
                 struct
                   class a =
                     object
                       val path = rightp
                       val layoutpath = Some leftp
                       val prunepath = Some leftp
                       method path = path
                       method layoutpath = layoutpath
                       method prunepath = prunepath
                     end
                 end
               in
               new M.a),
              ch, HypPlan
            in
            (* those leftps were ps and before that leftps ... *)
            if d then ElementPlan up else AmbigElementPlan (con pi el, up)
          in
          (* the outermost prune path dominates *)
          let rec rightdead d con ({path = p; layoutpath = l} : pathinfo) =
            deadf d con
              (let module M =
                 struct
                   class a =
                     object
                       val path = p
                       val layoutpath = l
                       val prunepath = Some rootp
                       method path = path
                       method layoutpath = layoutpath
                       method prunepath = prunepath
                     end
                 end
               in
               new M.a)
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
                            bracketedliststring elementstring "," concs])
                in
                (pi, c, e, string2textinfo TermFont s, f, dostopt stopt,
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
                    ["deadabove ", elementstring c, " -- making ", elementplankindstring r];
               *)
              r
            in
            (*  consolereport 
                  ["tprocess ", elementstring c, "; ", "; subs ", string_of_int (opt2bool subs),
                   "; d ", string_of_int d
                  ]; 
             *)
            (if d then ordinary else deadabove),
            (s, (term2textinfo f, deadf (opt2bool subs) con pi c), subs) :: ts
          in
          let (pi, c, e, s, f, st, subs) = List.hd fringe in
          (* that CANNOT go wrong, surely *)
          let (deadf', tdeps) = nj_fold tprocess fringe (deadf, []) in
          (* this certainly should _not_ be generating mktp Left ... I think it should be some kind of
           * DisplayConc ... but for now this will do.
           *)
          TranDep (st, (term2textinfo e, deadf' true (mktp Left) pi c), tdeps)
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
    let IDr = (string_of_int : lineID -> string)
    let rec Cr =
      function
        LineID l -> IDr l
      | BoxID (s, f) -> (IDr s ^ "-") ^ IDr f
      | HypID (l, n) ->(* we never make a BoxID with s=f *)
         (IDr l ^ ".") ^ string_of_int n
      | NoID ->(* we never make a HypID with l=0 *)
         ""
    let rec IDstring cids =
      liststring (fun x -> x) "," ((fun s -> s <> "") <| List.map Cr cids)
    let rec mapn a1 a2 a3 =
      match a1, a2, a3 with
        id, [], hn -> empty
      | id, (h, _) :: elis, hn ->
          ( ++ ) (mapn id elis (hn + 1), ( |-> ) (h, HypID (id, hn)))
    type fitchstep =
        FitchLine of
          < lineID : lineID; elementsbox : textbox;
            idplan : displayclass plan;
            elementsplan : elementplankind plan list;
            reasonplan : reasonplankind plan list >
      | FitchBox of < outerbox : box; lines : fitchstep list; boxed : bool >
    (* this datatype is included because without it, I get lost in monstrous tuples.
     * It is the type of information accumulated by (rev)folding the L function (in linearise)
     * across a list of subtrees.
     *)
    type lacc =
        Lacc of
          < id : lineID; lines : fitchstep list; elbox : box; idW : int;
            reasonW : int; assW : int >
    (* for similar reasons, here is the type of proof layouts *)
    type layout =
        Layout of
          < lines : fitchstep list; colonplan : displayclass plan;
            idmargin : int; bodymargin : int; reasonmargin : int;
            sidescreengap : int; linethickness : int; bodybox : box >
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
      let linethickness = draw.linethickness leading in
      let _ = setproofparams "box" linethickness in
      (* done early, so that GUIs can be ready for anything *)
               
      let textleading = (3 * leading + 1) / 2 in
      (* space between text lines *)
      let boxvspace = leading in
      (* space between box and the stuff inside *)
      let boxhspace = 2 * leading in
      (* ditto *)
      let boxleading = textleading in
      (* space between box and next line *)
      let reasongap = boxhspace in
      (* gap between rightmost stuff and reasons *)
      let sidescreengap = leading in
      (* pretty-space at the left of the screen *)                

      let transindent = 5 * leading in
      let (commasize, _ as comminf) = text2textinfo (tree.comma ()) in
      let commaW = tsW commasize in
      let (dotssize, _ as dotsinf) = string2textinfo ReasonFont ". . ." in
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
          textinfo2plan (string2textinfo ReasonFont (implode cs))
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
          | _ ->
              raise (Catastrophe_ ("getelement (boxdraw) can't parse " :: cs))
        in
        let fs = String.explode !boxlineformat in
        let rec notsep c = c <> "\n" in
        let (ls, rs) =
          takewhile notsep fs,
          (match dropwhile notsep fs with
             [] ->
               raise
                 (Catastrophe_
                    ["no separator in boxlineformat "; !boxlineformat])
           | _ :: rs -> rs)
        in
        let (ltokens, rtokens) = getelements ls, getelements rs in ()
      in
      let colonplan = string2plan ReasonFont ": " DisplayPunct origin in
      let colonsize = plantextsize colonplan in
      let reasonspacef =
        textinfo2plan (string2textinfo ReasonFont " ") ReasonPunctPlan
      in
      let lantesparenf =
        textinfo2plan (string2textinfo ReasonFont "(") ReasonPunctPlan
      in
      let rantesparenf =
        textinfo2plan (string2textinfo ReasonFont ") ") ReasonPunctPlan
      in
      let rec commaf p = textinfo2plan comminf ElementPunctPlan p in
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
                  ElementPlan (_, e, _) -> e
                | AmbigElementPlan ((_, e, _), _) -> e
                | ElementPunctPlan ->
                    raise (Catastrophe_ ["foldformula ElementPunctPlan"])
              in
              let estring = catelim_elementstring e [] in
              let rec measure =
                fst <*> measurestring TermFont
              in
              let _ =
                if !boxfolddebug then
                  consolereport
                    ["folding ";
                     bracketedliststring
                       (fun s ->
                          pairstring string_of_int enQuote "," (measure s, s))
                       ", " estring]
              in
              let sss = minwaste measure w estring in
              let _ =
                if !boxfolddebug then
                  consolereport
                    ["width is "; string_of_int w; ";\nformula folded to ";
                     string_of_int (List.length sss); " lines: ";
                     bracketedliststring (fun ss -> enQuote (implode ss)) ", "
                       sss]
              in
              let sys =
                (fun ss -> Syllable (TermFont, implode ss)) <* sss
              in
              let text =
                Text
                  (Linebreak textleading ::
                     (interpolate (Linebreak termfontleading) sys @
                        [Linebreak textleading]))
              in
              let _ =
                if !boxfolddebug then
                  consolereport ["text is "; textstring text]
              in
              let (size, _ as textinfo) = draw.measuretext MidBlock text in
              let _ =
                if !boxfolddebug then
                  consolereport ["textsize is "; textsizestring size]
              in
              textinfo, epi
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
          let elementssize = tbSize elementsbox in
          let dotspos =
            pos
              (posX elementspos,
               posY elementspos - tsA elementssize - textleading -
                 tsD dotssize)
          in
          let dotsplan = textinfo2plan dotsinf ElementPunctPlan dotspos in
          let allsize =
            tbSize (( +|-|+ ) (elementsbox, textbox (dotspos, dotssize)))
          in
          dotsplan :: elementsplanlist,
          textbox
            (elementspos,
             textsize (tsW allsize, tsA allsize - textleading, tsD allsize))
      in
      (* mkelementsplan *)
               
               (* make the plan for a single reason, relative to p *)
      let rec mkreasonplan a1 a2 =
        match a1, a2 with
          None, p -> [], emptytextbox
        | Some (pi, why, cids), p ->
            let reasonf = textinfo2plan why (ReasonPlan pi) in
            let antesf =
              textinfo2plan (string2textinfo ReasonFont (IDstring cids))
                ReasonPunctPlan
            in
            if !boxlinedisplay = "right" then
              plancons (reasonf p)
                (fun p ->
                   plancons (reasonspacef p)
                     (plan2plans <*> antesf))
            else if null cids then plan2plans (reasonf p)
            else
              plancons (lantesparenf p)
                (fun p ->
                   plancons (antesf p)
                     (fun p ->
                        plancons (rantesparenf p)
                          (plan2plans <*> reasonf)))
      in
      let rec ljreasonplan ps box =
        let shift = pos (- tsW (tbSize box), 0) in
        List.map (fun p -> planOffset p shift) ps
      in
      (* make the data structure for a single line, positioned relative to topleftpos *)
      let rec mkLine elf reasonf id topleftpos =
        (* we construct the plans for elements, line number and reason so that their
         * baseline is originY; then we make bigelementsbox to say where the elements really are
         *)
        let (elementsplan, elementsbox) = elf origin in
        let elementssize = tbSize elementsbox in
        let (idsize, _ as idinfo) = string2textinfo ReasonFont (IDr id) in
        let idplan =
          textinfo2plan idinfo DisplayPunct (rightby (origin, - tsW idsize))
        in
        (* this is right justified *)
        let (reasonplan, reasonbox) = reasonf origin in
        let reasonsize = tbSize reasonbox in
        let linesize = ( +-+ ) (( +-+ ) (elementssize, reasonsize), idsize) in
        (* just to get A, D *)
        let bigelementspos = downby (topleftpos, tsA linesize) in
        let bigsize =
          textsize
            (tsW elementssize + posX (tbPos elementsbox), tsA linesize,
             tsD linesize)
        in
        let bigelementsbox = textbox (bigelementspos, bigsize) in
        FitchLine
          (let module M =
             struct
               class a =
                 object
                   val lineID = id
                   val elementsbox = bigelementsbox
                   val idplan = idplan
                   val elementsplan = elementsplan
                   val reasonplan =
                     if !boxlinedisplay = "right" then reasonplan
                     else ljreasonplan reasonplan reasonbox
                   method lineID = lineID
                   method elementsbox = elementsbox
                   method idplan = idplan
                   method elementsplan = elementsplan
                   method reasonplan = reasonplan
                 end
             end
           in
           new M.a),
        textbox2box bigelementsbox, tsW idsize, tsW reasonsize
      in
      let rec startLacc id pos =
        Lacc
          (let module M =
             struct
               class a =
                 object
                   val id = id
                   val lines = []
                   val elbox = box (pos, nullsize)
                   val idW = 0
                   val reasonW = 0
                   val assW = 0
                   method id = id
                   method lines = lines
                   method elbox = elbox
                   method idW = idW
                   method reasonW = reasonW
                   method assW = assW
                 end
             end
           in
           new M.a)
      in
      let rec nextpos b leading =
        if isemptybox b then topleft b else downby (botleft b, leading + 1)
      in
      (* idok is what to do if you are an IdDep -- 
       *   true means disappear if you like;
       *   false means you are the last line of a box, so disappear iff you refer to the previous line.
       *)
      let rec L wopt hypmap idok dp =
        fun
          (Lacc
             {id = id;
              lines = lines;
              elbox = elbox;
              idW = idW;
              reasonW = reasonW;
              assW = assW} as acc) ->
          let rec getIdDep el =
            match mapped sameresource hypmap el with
              Some cid -> cid, acc
            | None ->
                raise
                  (Catastrophe_
                     ["linearise can't find hypothesis "; elementstring el])
          in
          let getcid = L (wopt, hypmap, true) in
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
                          _The_ ->
                            raise
                              (Catastrophe_
                                 ["linearise can't decode lprin ";
                                  elementstring lp])) <*
                     lprins
                in
                let rec dosub (dp, (cids, acc)) =
                  let (cid, acc') = getcid dp acc in cid :: cids, acc'
                in
                let (cids', acc') = nj_revfold dosub subdps (List.rev lcids, acc) in
                acc', Some (pi, rinf, List.rev cids')
          in
          (* plan a line: mkp does the content *)
          let rec doconcline mkp needsreason (acc, justinf) =
            let
              (Lacc
                 {id = id;
                  lines = lines;
                  elbox = elbox;
                  idW = idW;
                  reasonW = reasonW;
                  assW = assW})
              =
              acc
            in
            let eplaninf =
              mkelementsplan mkp (not needsreason || opt2bool justinf)
            in
            let (line, ebox, iW, rW) =
              mkLine eplaninf (mkreasonplan justinf) id
                (nextpos elbox textleading)
            in
            id,
            Lacc
              (let module M =
                 struct
                   class a =
                     object
                       val id = _RR id
                       val lines = line :: lines
                       val elbox = ( +||+ ) (elbox, ebox)
                       val idW = max iW (idW)
                       val reasonW = max rW (reasonW)
                       val assW = assW
                       method id = id
                       method lines = lines
                       method elbox = elbox
                       method idW = idW
                       method reasonW = reasonW
                       method assW = assW
                     end
                 end
               in
               new M.a)
          in
          (* info to prefix a line with a turnstile *)
          let rec stprefix stopt restf p =
            match stopt with
              Some st -> plancons (textinfo2plan st ElementPunctPlan p) restf
            | _ -> restf p
          in
          match dp with
            IdDep (el, lindep) ->
              if idok ||
                 (match mapped sameresource hypmap el with
                    Some (LineID id') -> id = _RR id'
                  | _ -> false)
              then
                getIdDep el
              else L (wopt, hypmap, false, lindep, acc)
          | LinDep (concels, stopt, justopt) ->
              let concels' =
                match !foldformulae, wopt with
                  true, Some bestW ->
                      (foldformula
                         (bestW - 2 * posX (topleft elbox) - commaW) <*
                       concels)
                | _ -> concels
              in
              let rec mkp p =
                stprefix stopt
                  (things2plans (uncurry2 textinfo2plan) commaf nullf
                     concels')
                  p
              in
              let (id', acc') =
                doconcline mkp true (dolinsubs hypmap acc justopt)
              in
              LineID id', acc'
          | BoxDep (boxed, words, hypelis, dp) ->
              let (topleftpos, hindent, vindent, innerpos) =
                if boxed then
                  let topleftpos = nextpos elbox boxleading in
                  let hindent = linethickness + boxhspace in
                  let vindent = linethickness + boxvspace in
                  topleftpos, hindent, vindent,
                  ( +->+ ) (topleftpos, pos (hindent, vindent))
                else
                  let topleftpos = nextpos elbox textleading in
                  topleftpos, 0, 0, topleftpos
              in
              let hyplines =
                match wopt with
                  None -> [hypelis]
                | Some bestW ->
                    (* first pass - just put them all on one line *)
                    (* We make a proper 'minimum waste' split of the assumption line *)
                    let rec measureplan (_, ((size, _), _)) =
                      tsW size + commaW
                    in
                    (* more or less *)
                    let mybestW =
                      max (2 * tsW (fst (fst words))) (bestW - 2 * posX innerpos)
                    in
                    minwaste measureplan mybestW
                      ((fun (e, inf) -> e, foldformula mybestW inf) <* hypelis)
              in
              let rec dohypline =
                fun
                  ((hypelis : elinfo list),
                   (hypmap,
                    Lacc
                      {id = id;
                       lines = lines;
                       elbox = elbox;
                       idW = idW;
                       reasonW = reasonW;
                       assW = assW})) ->
                  let (word, hypmap') =
                    match hypelis with
                      [h] ->
                        fst words,
                        ( ++ ) (hypmap, ( |-> ) (fst h, LineID id))
                    | hs -> snd words, ( ++ ) (hypmap, mapn id hs 1)
                  in
                  let rec showword p =
                    plan2plans (textinfo2plan word ReasonPunctPlan p)
                  in
                  let (line, linebox, lineidW, linereasonW) =
                    mkLine
                      (things2plans
                         (uncurry2 textinfo2plan <*> snd)
                         commaf nullf hypelis)
                      showword id (nextpos elbox textleading)
                  in
                  let lineassW =
                    match hypelis with
                      [_] -> 0
                    | _ -> sW (bSize linebox) + 2 * posX innerpos
                  in
                  hypmap',
                  Lacc
                    (let module M =
                       struct
                         class a =
                           object
                             val id = _RR id
                             val lines = line :: lines
                             val elbox =
                               if null lines then linebox
                               else ( +||+ ) (elbox, linebox)
                             val idW = max idW (lineidW)
                             val reasonW = max reasonW (linereasonW)
                             val assW = max assW (lineassW)
                             method id = id
                             method lines = lines
                             method elbox = elbox
                             method idW = idW
                             method reasonW = reasonW
                             method assW = assW
                           end
                       end
                     in
                     new M.a)
              in
              let (hypmap', (Lacc {id = id'} as acc')) =
                nj_revfold dohypline hyplines (hypmap, startLacc id innerpos)
              in
              let
                (cid,
                 Lacc
                   {id = id'';
                    lines = innerlines;
                    elbox = innerbox;
                    idW = idW';
                    reasonW = reasonW';
                    assW = assW'})
                =
                L (wopt, hypmap', false, dp, acc')
              in
              let outerbox = bOutset innerbox (size (hindent, vindent)) in
              let cid' =
                match cid with
                  LineID jd -> BoxID (id, jd)
                | BoxID (_, jd) -> BoxID (id, jd)
                | HypID _ -> if id = id' then LineID id else BoxID (id, id')
                | NoID -> raise (Catastrophe_ ["NoID in BoxDep"])
              in
              cid',
              Lacc
                (let module M =
                   struct
                     class a =
                       object
                         val id = id''
                         val lines =
                           FitchBox
                             (let module M =
                                struct
                                  class a =
                                    object
                                      val outerbox = outerbox
                                      val lines = innerlines
                                      val boxed = boxed
                                      method outerbox = outerbox
                                      method lines = lines
                                      method boxed = boxed
                                    end
                                end
                              in
                              new M.a) ::
                             lines
                         val elbox = ( +||+ ) (elbox, outerbox)
                         val idW = idW (idW')
                         val reasonW = max reasonW (reasonW')
                         val assW = max assW (assW')
                         method id = id
                         method lines = lines
                         method elbox = elbox
                         method idW = idW
                         method reasonW = reasonW
                         method assW = assW
                       end
                   end
                 in
                 new M.a)
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
                  consolereport ["boxdraw can't hide "; dependencystring ldp];
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
              L (wopt, ( ++ ) (hypmap, ( |-> ) (cutel, cutelid)), idok, rdp,
                 acc')
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
                  (plan2plans <*> uncurry2 textinfo2plan terminf)
                  p
              in
              let (id'', acc'') = doconcline sourceline false (acc', None) in
              let rec phase2 ((s, f, just), (_, acc)) =
                let rec mkp p =
                  let splan =
                    textinfo2plan s ElementPunctPlan
                      (rightby (p, transindent))
                  in
                  plancons splan
                       (plan2plans <*> uncurry2 textinfo2plan f <*> 
                        (fun p' -> rightby (p', transindent)))
                in
                doconcline mkp true (acc, just)
              in
              let (jd, acc''') = nj_fold phase2 revts (id'', acc'') in
              BoxID (id', jd), acc'''
      in
      (* stuff to do with computing margins and gaps *)
      (* One day boxlinedisplay will tell us in detail how to show a line.
      * At present the 'right' style is n: F R A -- n right-justified, R aligned
      * and the 'left' style is (A R) n: F -- (A R) and n right-justified
      *)
      let rec idmargin idW reasonW =
        (if !boxlinedisplay = "right" then 0 else reasonW + reasongap) + idW +
          tsW colonsize
      in
      let rec leftmargin idW reasonW = idmargin idW reasonW + tsW colonsize in
      let rec boxW box = sW (bSize box) in
      let rec reasonspace lines =
        match lines with
          [FitchBox {boxed = true}] -> reasongap
        | _ -> 2 * reasongap
      in
      let rec reasonmargin lines idW reasonW elbox =
        if !boxlinedisplay = "right" then
          leftmargin idW reasonW + boxW elbox + reasonspace lines
        else reasonW
      in
      let rec extras lines idW reasonW =
        sidescreengap + leftmargin idW reasonW +
          (if !boxlinedisplay = "right" then reasonspace lines + reasonW
           else 0) +
          sidescreengap
      in
      let rec answer =
        fun
          (Lacc
             {lines = lines;
              elbox = elbox;
              idW = idW;
              reasonW = reasonW;
              assW = assW}) ->
          Layout
            (let module M =
               struct
                 class a =
                   object
                     val lines = lines
                     val colonplan = colonplan
                     val idmargin = idmargin idW reasonW
                     val bodymargin = leftmargin idW reasonW
                     val reasonmargin = reasonmargin lines idW reasonW elbox
                     val sidescreengap = sidescreengap
                     val linethickness = linethickness
                     val bodybox = elbox
                     method lines = lines
                     method colonplan = colonplan
                     method idmargin = idmargin
                     method bodymargin = bodymargin
                     method reasonmargin = reasonmargin
                     method sidescreengap = sidescreengap
                     method linethickness = linethickness
                     method bodybox = bodybox
                   end
               end
             in
             new M.a)
      in
      (* we do it once, then see if we might be able to make it smaller *)
      let startacc = startLacc 1 origin in
      let
        (_,
         (Lacc
            {elbox = elbox;
             idW = idW;
             reasonW = reasonW;
             assW = assW;
             lines = lines} as firstlayout))
        =
        L (None, empty, false, dp, startacc)
      in
      (* body of linearise *)
      if extras lines idW reasonW + boxW elbox > screenwidth &&
         (!foldformulae || assW = boxW elbox)
      then
        (* we have a picture which is too wide, and might be made less wide *)
        let maxbestW = screenwidth - extras lines idW procrustean_reasonW in
        let _ =
          if !boxfolddebug then
            consolereport
              ["trying again, width "; string_of_int maxbestW; "; screenwidth ";
               string_of_int screenwidth]
        in
        answer (snd (L (Some maxbestW, empty, false, dp, startacc)))
      else answer firstlayout
    (* linearise *)(* desperation ...
    fun IDstring id =
      case id of
       LineID l     => "LineID "^IDr l
     | BoxID(s,f)   => "BoxID("^IDr s^","^IDr f^")"
     | HypID(id, n) => "HypID("^IDr id^","^string_of_int n^")"
    ... end desperation *)

    let rec BoxLayout screenwidth t =
      let pt = pretransform (List.length (turnstiles ()) <> 1) t in
      let procrustean_reasonW = max 100 (screenwidth / 10) in
      let tranreason =
        if !truncatereasons then
          procrustean_reason2textinfo procrustean_reasonW
        else reason2textinfo
      in
      let dp = dependency tranreason ordinary pt in
      linearise screenwidth procrustean_reasonW dp
    (* BoxLayout *)(* The emphasis (blacken/greyen) stuff is pretty confused.  
     *
     * It seems we need to do several things:
     *
     * 1. When selecting a conclusion line (including default selection, when it is taken
     * because it is on the autoselect path), greyen and deselect all other conclusions,
     * greyen all hypotheses except those available in its lhs.
     *
     * 2. When selecting a hypothesis, greyen all conclusions except those which use this
     * hypothesis, and all hypotheses which aren't available in the same lhs as this one,
     * somewhere in the tree.
     *
     * 3. When a conclusion line is selected _after_ a hypothesis, only grey, don't blacken.
     * Ditto when a hypothesis is selected after a conclusion.
     *
     * We don't yet know what to do about ambiguous selections, nor how to interact with
     * dead conclusion lines.  We are more than a little confused about how to blacken/greyen
     * commas, reasons, etc.
     *)
   
    let rec elementsin ps =
      List.length ((iselementkind <*> planinfo) <| ps)
    let rec draw goalopt p proof =
      fun
        (Layout
           {lines = lines;
            colonplan = colonplan;
            idmargin = idmargin;
            bodymargin = bodymargin;
            reasonmargin = reasonmargin;
            bodybox = bodybox;
            linethickness = linethickness}) ->
        let idx = posX p + idmargin in
        let reasonx = posX p + reasonmargin in
        let rec samepath a1 a2 =
          match a1, a2 with
            path, None -> false
          | path, Some goalpath -> path = goalpath
        in
        let rec D p line =
          match line with
            FitchLine
              {elementsbox = elementsbox;
               idplan = idplan;
               elementsplan = elementsplan;
               reasonplan = reasonplan} ->
              let pdraw = ( +->+ ) (p, tbPos elementsbox) in
              let rec emp gpath plan =
                (* (Elementplan((el,siopt),_,class,elbox)) = *)
                let rec dohigh () =
                  highlight (( +->+ ) (tbPos (plantextbox plan), pdraw))
                    (Some DisplayConc)
                in
                let rec dogrey () =
                  greyen (( +->+ ) (tbPos (plantextbox plan), pdraw))
                in
                let rec doconc path =
                  if gpath = path then dohigh () else dogrey ()
                in
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
                | _ ->
                    raise
                      (Catastrophe_
                         ["emp in D "; planstring elementplankindstring plan])
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
          | FitchBox {outerbox = outerbox; lines = lines; boxed = boxed} ->
              if boxed then drawBox (bOffset outerbox p); List.iter (D p) lines
        in
        drawinproofpane (); List.iter (D (rightby (p, bodymargin))) lines
    let rec print str goalopt p proof =
      fun
        (Layout
           {lines = lines;
            colonplan = colonplan;
            idmargin = idmargin;
            bodymargin = bodymargin;
            reasonmargin = reasonmargin}) ->
        let rec samepath a1 a2 =
          match a1, a2 with
            path, None -> false
          | path, Some goalpath -> path = goalpath
        in
        let rec out s = output (str, s) in
        let rec outplan p = out "\""; outesc (plan2string p); out "\" "
        and outch c =
          output
            (str,
             (match c with
                "\"" -> "\\\""
              | _ -> c))
        and outesc s = List.iter outch (String.explode s) in
        let rec D p line =
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
          | FitchBox {outerbox = outerbox; lines = lines; boxed = boxed} ->
              (* DO THIS STUFF LATER
              case goalopt of 
                Some gpath => List.iter (emp gpath) elementsplan
              | None       => ()
              *)
              if boxed then out "(BOX\n";
              revapp (D p) lines;
              if boxed then out ")\n"
        in
        revapp (D (rightby (p, bodymargin))) lines
    let rec pos2hit p =
      fun
        (Layout
           {lines = lines;
            bodymargin = bodymargin;
            reasonmargin = reasonmargin})
        hitkind ->
        let _ =
          if !boxseldebug then
            consolereport
              ["pos2hit "; " "; posstring p; " ... "; hitkindstring hitkind]
        in
        let rec H a1 a2 =
          match a1, a2 with
            p,
            FitchLine
              {elementsbox = elementsbox;
               elementsplan = elementsplan;
               reasonplan = reasonplan} ->
              let rec couldbe a1 a2 =
                match a1, a2 with
                  path, Some truepath -> truepath
                | path, None -> path
              in
              let rec answerpath
                hitkind
                  {path = path;
                   layoutpath = layoutpath;
                   prunepath = prunepath} =
                match hitkind with
                  LayoutPath -> couldbe path layoutpath
                | PrunePath -> couldbe path prunepath
                | HitPath -> path
              in
              let rec cp (pi, el, kind) =
                match kind with
                  TranPlan side -> answerpath hitkind pi, (el, Some side)
                | _ -> answerpath hitkind pi, (el, None)
              in
              let rec hp (pi, el, kind) = answerpath hitkind pi, el in
              let rec decodeplan =
                function
                  ElementPlan (pi, el, kind as pl) ->
                    begin match kind with
                      HypPlan -> Some (FormulaHit (HypHit (hp pl)))
                    | _ -> Some (FormulaHit (ConcHit (cp pl)))
                    end
                | AmbigElementPlan (up, dn) ->
                    Some (FormulaHit (AmbigHit (cp up, hp dn)))
                | ElementPunctPlan -> None
              in
              if withintb (p, elementsbox) then
                findfirstplanhit (( +<-+ ) (p, tbPos elementsbox)) elementsplan 
                &~~ (fSome <*> planinfo) 
                &~~ decodeplan
              else
                findfirst
                  (fun reason ->
                     match planinfo reason with
                       ReasonPlan pi ->
                         if withintb
                              (( +<-+ )
                                 (p,
                                  pos
                                    (reasonmargin - bodymargin,
                                     posY (tbPos elementsbox))),
                               plantextbox reason)
                         then
                           Some (ReasonHit (answerpath hitkind pi))
                         else None
                     | _ -> None)
                  reasonplan
          | p, FitchBox {outerbox = outerbox; lines = lines} ->
              if withinY (p, outerbox) then findfirst (H p) lines else None
        in
        findfirst (H (rightby (p, - bodymargin))) lines
    let rec locateHit pos classopt hitkind (p, proof, layout) =
      pos2hit (( +<-+ ) (pos, p)) layout hitkind
      &~~
      (fSome <*> 
        ((function
            FormulaHit (AmbigHit (up, dn)) as h ->
              begin match classopt with
                Some DisplayConc -> FormulaHit (ConcHit up)
              | Some DisplayHyp -> FormulaHit (HypHit dn)
              | None -> h
              | _ ->
                  raise
                    (Catastrophe_
                       ["locateHit (boxdraw) finds "; hitstring pathstring h;
                        ", given classopt ";
                        optionstring displayclassstring classopt])
              end
          | h -> h)
           ))
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
                let p' = ( +->+ ) (p, tbPos elementsbox) in
                List.iter
                  (fun plan ->
                     if iselementkind (planinfo plan) then
                       emp plan (( +->+ ) (p', tbPos (plantextbox plan))))
                  elementsplan
            | FitchBox {outerbox = outerbox; lines = lines} ->
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
                          pairstring posstring displayclassstring ","
                            (pos, class__)])) <*
             (match posclassopt with
                None -> posclasslist
              | Some hc -> hc :: posclasslist)
        in
        let rec bang s =
          raise
            (Catastrophe_
               ["notifyselect (boxdraw) sees "; s; " in ";
                bracketedliststring (hitstring pathstring) "," hits])
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
                stillopen Proof cpath && okhyps hyps cpath
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
                stillopen Proof cpath' && okhyps hyps cpath'
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
    let rec defaultpos =
      fun
        (Layout
           {bodybox = bodybox;
            bodymargin = bodymargin;
            sidescreengap = sidescreengap}) ->
        let screen = viewBox () in
        let prooforigin = bPos bodybox in
        (* leftby bodymargin not needed ... *)
        (*
        consolereport ["defaultpos: screen is ", boxstring screen, " proof is ", boxstring bodybox, 
                       "\nbodymargin ", string_of_int bodymargin, " sidescreengap ", string_of_int sidescreengap];
        *)
        (* put the SW corner of the proof in the SW corner of the screen. Because of botleft this is
           1 pixel too high, but who cares?
         *)
        ( +<-+ )
          (upby (rightby (botleft screen, sidescreengap), sH (bSize bodybox)),
           prooforigin),
        screen, prooforigin
    let rec rootpos =
      fun (Layout {lines = lines}) ->
        (* position of last line in proof, first in lines *)
        let rec p =
          function
            FitchLine {elementsbox = elementsbox} -> Some (tbPos elementsbox)
          | FitchBox {lines = lines} -> findfirst p lines
        in
        match findfirst p lines with
          Some p -> p
        | _ -> origin
    let rec postoinclude box =
      fun
        (Layout {bodymargin = bodymargin; sidescreengap = sidescreengap} as
           layout) ->
        let (defpos, screen, prooforigin) = defaultpos layout in
        let otherdefpos =
          ( +<-+ ) (rightby (topleft screen, sidescreengap), prooforigin)
        in
        (*
        consolereport ["postoinclude: defpos ", posstring defpos, " screen ", boxstring screen, 
                              " prooforigin ", posstring prooforigin,
                       "\nbOffset box defpos ", boxstring (bOffset box defpos),
                           " entirelywithin screen ", string_of_int (bOffset box defpos entirelywithin screen),
                       "\notherdefpos ", posstring otherdefpos, 
                           " bOffset box otherdefpos ", boxstring (bOffset box otherdefpos),
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
           * that is, choose p = midp +<-+ bPos box.
           * I hope.
           *)
          ( +<-+ )
            (downby
               (rightby (bPos screen, bodymargin),
                (sH (bSize screen) - sH (bSize box)) / 2),
             bPos box)
    let rec layout proof = BoxLayout (sW (bSize (viewBox ())), proof)
    (* This function is used in displaystyle.sml to position a proof.
     * I think it's best if the _conclusion_ box doesn't move.  Otherwise you get into all 
     * kinds of jumpy behaviour.
     *)
    let rec targetbox a1 a2 =
      match a1, a2 with
        None, _ -> None
      | Some path, Layout {lines = lines} ->
          let rec ok =
            function
              ElementPlan ({path = epath}, _, ConcPlan) as plankind ->
                if !screenpositiondebug then
                  consolereport
                    [elementplankindstring plankind; "; "; pathstring path;
                     "; "; string_of_int (path = epath)];
                path = epath
            | _ -> false
          in
          let rec search =
            function
              FitchLine
                {elementsplan = elementsplan; elementsbox = elementsbox} ->
                if List.exists (ok <*> planinfo) elementsplan then
                  begin
                    if !screenpositiondebug then
                      consolereport
                        ["boxdraw targetbox gotcha ";
                         textboxstring elementsbox];
                    Some elementsbox
                  end
                else None
            | FitchBox {lines = lines} -> findfirst search lines
          in
          findfirst search lines
    let rec samelayout =
      fun (Layout {lines = lines}, Layout {lines = lines'}) -> lines = lines'
    let defaultpos = fst <*> defaultpos
  end

(* a bit of desperate debugging ...
val _ =
let fun IDstring id =
      case id of
       LineID l     => "LineID "^IDr l
     | BoxID(s,f)   => "BoxID("^IDr s^","^IDr f^")"
     | HypID(id, n) => "HypID("^IDr id^","^string_of_int n^")"
in
    consolereport
    ["BL ", string_of_int lastline, 
     " ", mappingstring elementstring IDstring hypmapin,
     " ", bracketedliststring elementstring "," hyps, 
     "; sequent=", seqstring (sequent t),
     "; hs=", bracketedliststring elementstring "," hs, 
     "; gs=", bracketedliststring elementstring "," gs, 
     "; reason=", optionstring reasonstring (reason t),
     "; id=", string_of_int id,
     "; cids=", bracketedliststring IDstring "," cids,
     "; rpath=", pathstring rpath,
     "; hypmap=", mappingstring elementstring IDstring hypmap,
     "; thinnedL=", bracketedliststring elementstring "," thinnedL,
     "; prinids=", bracketedliststring IDstring "," prinids
     ]
end
*)
(* more desperate debugging
val _ = consolereport ["boxdraw begins"]

 *)

