(* $Id$ *)

module type Tactictype =
  sig
    type term and seq and name and treelayout
    type tactic =
        SkipTac
      | FailTac
      | StopTac
      | NextgoalTac
      | SetgoalTac of pathexpr
      | TheoryAltTac of name list
      | AltTac of tactic list
      | SeqTac of tactic list
      | WhenTac of tactic list
      | RepTac of tactic
      | IfTac of tactic
      | CompTac of tactic
      | CutinTac of tactic
      | UnfoldHypTac of (name * term list)
      | FoldHypTac of (name * term list)
      | UnfoldTac of (name * tactic list)
      | FoldTac of (name * tactic list)
      | AssignTac of (name * term)
      | EvalTac of term list
      | AdHocTac of term list
      | BindConcTac of (term * tactic)
      | BindHypTac of (term * tactic)
      | BindHyp2Tac of (term * term * tactic)
      | BindHypsTac of (term * tactic)
      | BindArgTac of (term * tactic)
      | BindArgTextTac of (name * tactic)
      | BindGoalPathTac of (name * tactic)
      | BindSubstTac of (term * tactic)
      | BindSubstInHypTac of (term * tactic)
      | BindSubstInConcTac of (term * tactic)
      | BindMultiArgTac of (term * tactic)
      | BindLHSTac of (term * tactic)
      | BindRHSTac of (term * tactic)
      | BindGoalTac of (term * tactic)
      | BindOpenSubGoalTac of (name * term * tactic)
      | BindOpenSubGoalsTac of (term * tactic)
      | BindFindHypTac of (term * tactic)
      | BindFindConcTac of (term * tactic)
      | BindMatchTac of (term * term * tactic)
      | BindOccursTac of (term * term * term * tactic)
      | LayoutTac of (tactic * treelayout)
      | AssocFlatTac of term
      | UnifyTac of term list
      | MapTac of (name * term list)
      | TermTac of (name * term list)
      | SubstTac of (name * (term * term) list)
      | GivenTac of term
      | WithArgSelTac of tactic
      | WithConcSelTac of tactic
      | WithFormSelTac of tactic
      | WithHypSelTac of tactic
      | WithSelectionsTac of tactic
      | WithSubstSelTac of tactic
      | MatchTac of tactic
      | SameProvisosTac of tactic
      | SimpleApplyTac of tactic
      | ApplyOrResolveTac of tactic
      | UniqueTac of tactic
      | TakeAnyTac of tactic
      | ResolveTac of tactic
      | ReplayTac of tactic
      | ContnTac of (tactic * tactic)
      | AlertTac of (term * (term * tactic) list * tactic option)
      | ExplainTac of term
      | CommentTac of term
      | BadUnifyTac of (name * name * tactic)
      | BadMatchTac of (name * name * tactic)
      | BadProvisoTac of (name * name * name * tactic)
      | UnifyArgsTac
    and pathexpr =
        Parent of pathexpr
      | LeftSibling of pathexpr
      | RightSibling of pathexpr
      | Subgoal of (pathexpr * term)
      | HypRoot of pathexpr
      | SimplePath of term
  end

module type Tactic =
  sig
    type term and tactic and name
    type ('a, 'b) mapping
    val tacname : term -> name
    (* or raise ParseError_ *)
    val transTactic : term -> tactic
    val explodeForExecute : term -> name * term list
    val tacticstring : tactic -> string
    (* the simple, unvarnished string *)
    val tacticstringwithNLs : tactic -> string
    (* guess what this one does *)
    val catelim_tacticstring : tactic -> string list -> string list
    val catelim_tacticstringwithNLs : tactic -> string list -> string list
    val remaptactic : (term, term) mapping -> tactic -> tactic
    val isguard : tactic -> bool
    val showargasint : (int -> term -> int) option ref
    val readintasarg : term array option ref
  end


(* $Id$ *)

(* some of this stuff is specific to natural deduction
  (use of 'seq' type is evidence).
  Tut tut. RB

  [[Actually it's all crap, and we need a proper MLish tactic language. BS]]
 *)

module
  Tactic
  (AAA :
    sig
      module listfuns : Listfuns
      module stringfuns : Stringfuns
      module name : sig include Nametype(* sanctioned in tactic.sml *) include Name end
      module term : sig include Termtype include Termstore include Term end
      module sequent : Sequent
      module treelayout : TreeLayout
      val andthenr : 'a option * ('a -> 'b option) -> 'b option
      val atoi : string -> int
      val consolereport : string list -> unit
      val FormulaClass : term.idclass
      val interpolate : 'a -> 'a list -> 'a list
      val remapterm :
        (term.term, term.term) sequent.mapping -> term.term -> term.term
      val try__ : ('a -> 'b) -> 'a option -> 'b option
      val unSOME : 'a option -> 'a
      exception ParseError_ of string list
      exception Catastrophe_ of string list
      exception AtoI_
      
    end)
  :
  sig include Tactictype include Tactic end =
  struct
    open AAA
    open listfuns
    open stringfuns
    open term
    open sequent
    open name
    open treelayout
    (* from listfuns *)
    
    
    (* from optionfuns *)
    
    type tactic =
        SkipTac
      | FailTac
      | StopTac
      | NextgoalTac
      | SetgoalTac of pathexpr
      | TheoryAltTac of name list
      | AltTac of tactic list
      | SeqTac of tactic list
      | WhenTac of tactic list
      | RepTac of tactic
      | IfTac of tactic
      | CompTac of tactic
      | CutinTac of tactic
      | UnfoldHypTac of (name * term list)
      | FoldHypTac of (name * term list)
      | UnfoldTac of (name * tactic list)
      | FoldTac of (name * tactic list)
      | AssignTac of (name * term)
      | EvalTac of term list
      | AdHocTac of term list
      | BindConcTac of (term * tactic)
      | BindHypTac of (term * tactic)
      | BindHyp2Tac of (term * term * tactic)
      | BindHypsTac of (term * tactic)
      | BindArgTac of (term * tactic)
      | BindArgTextTac of (name * tactic)
      | BindGoalPathTac of (name * tactic)
      | BindSubstTac of (term * tactic)
      | BindSubstInHypTac of (term * tactic)
      | BindSubstInConcTac of (term * tactic)
      | BindMultiArgTac of (term * tactic)
      | BindLHSTac of (term * tactic)
      | BindRHSTac of (term * tactic)
      | BindGoalTac of (term * tactic)
      | BindOpenSubGoalTac of (name * term * tactic)
      | BindOpenSubGoalsTac of (term * tactic)
      | BindFindHypTac of (term * tactic)
      | BindFindConcTac of (term * tactic)
      | BindMatchTac of (term * term * tactic)
      | BindOccursTac of (term * term * term * tactic)
      | LayoutTac of (tactic * treelayout)
      | AssocFlatTac of term
      | UnifyTac of term list
      | MapTac of (name * term list)
      | TermTac of (name * term list)
      | SubstTac of (name * (term * term) list)
      | GivenTac of term
      | WithArgSelTac of tactic
      | WithConcSelTac of tactic
      | WithFormSelTac of tactic
      | WithHypSelTac of tactic
      | WithSelectionsTac of tactic
      | WithSubstSelTac of tactic
      | MatchTac of tactic
      | SameProvisosTac of tactic
      | SimpleApplyTac of tactic
      | ApplyOrResolveTac of tactic
      | UniqueTac of tactic
      | TakeAnyTac of tactic
      | ResolveTac of tactic
      | ReplayTac of tactic
      | ContnTac of (tactic * tactic)
      | AlertTac of (term * (term * tactic) list * tactic option)
      | ExplainTac of term
      | CommentTac of term
      | BadUnifyTac of (name * name * tactic)
      | BadMatchTac of (name * name * tactic)
      | BadProvisoTac of (name * name * name * tactic)
      | UnifyArgsTac
    and pathexpr =
        Parent of pathexpr
      | LeftSibling of pathexpr
      | RightSibling of pathexpr
      | Subgoal of (pathexpr * term)
      | HypRoot of pathexpr
      | SimplePath of term
    (*
        TACTIC TRANSLATION
    *)

    let showargasint : (int -> term -> int) option ref = ref None
    let readintasarg : term array option ref = ref None
    let rec catelim_tacticstring sep t tail =
      let withNLs = String.sub (sep) (0) (1) = "\n" in
      let nextsep = if withNLs then sep ^ "\t" else sep in
      let rec oneline t =
        match t with
          SkipTac -> true
        | UnfoldHypTac _ -> true
        | FoldHypTac _ -> true
        | AssignTac _ -> true
        | EvalTac _ -> true
        | AdHocTac _ -> true
        | AssocFlatTac _ -> true
        | MapTac _ -> true
        | TermTac _ -> true
        | SubstTac _ -> true
        | _ -> false
      in
      let argsep = if oneline t then " " else nextsep in
      let rec tacargstring t tail =
        let rec ok =
          function
            SkipTac -> true
          | TermTac (s, []) -> true
          | _ -> false
        in
        let res = catelim_tacticstring nextsep t in
        if ok t then res tail else "(" :: res (")" :: tail)
      in
      let rec tr f = catelim_interpolate f in
      let trtacs = tr tacargstring nextsep in
      let trterms = tr catelim_argstring " " in
      let rec unSEQ =
        function
          SeqTac [t] -> unSEQ t
        | SeqTac ts -> trtacs ts
        | t -> tacargstring t
      in
      let rec trpathexpr p tail =
        let rec trns ns tail =
          match debracket ns with
            Tup _ as t -> "(" :: catelim_termstring t (")" :: tail)
          | t -> catelim_termstring t tail
        in
        match p with
          Parent p -> "(PARENT" :: argsep :: trpathexpr p (")" :: tail)
        | LeftSibling p -> "(LEFT" :: argsep :: trpathexpr p (")" :: tail)
        | RightSibling p -> "(RIGHT" :: argsep :: trpathexpr p (")" :: tail)
        | Subgoal (p, ns) ->
            "(SUBGOAL" :: argsep ::
              trpathexpr p (argsep :: trns ns (")" :: tail))
        | HypRoot p -> "(HYPROOT" :: argsep :: trpathexpr p (")" :: tail)
        | SimplePath ns -> trns ns tail
      in
      let pnstr = parseablenamestring in
      let rec termsNtac a1 a2 =
        match a1, a2 with
          [], tac -> unSEQ tac tail
        | t :: ts, tac -> catelim_argstring t (argsep :: termsNtac ts tac)
      in
      let tss =
        match t with
          SkipTac -> "SKIP" :: tail
        | FailTac -> "FAIL" :: tail
        | StopTac -> "STOP" :: tail
        | NextgoalTac -> "NEXTGOAL" :: tail
        | SetgoalTac p -> "GOALPATH" :: argsep :: trpathexpr p tail
        | TheoryAltTac ns ->
            "THEORYALT" :: " " ::
              catelim_liststring (stringfn2catelim pnstr) " " ns tail
        | AltTac tacs -> "ALT" :: nextsep :: trtacs tacs tail
        | SeqTac [t] -> catelim_tacticstring sep t tail
        | SeqTac tacs -> "SEQ" :: nextsep :: trtacs tacs tail
        | WhenTac tacs -> "WHEN" :: nextsep :: trtacs tacs tail
        | RepTac tac -> "DO" :: argsep :: unSEQ tac tail
        | IfTac tac -> "IF" :: argsep :: unSEQ tac tail
        | CompTac tac -> "PROVE" :: argsep :: unSEQ tac tail
        | CutinTac tac -> "CUTIN" :: argsep :: unSEQ tac tail
        | UnfoldHypTac (s, terms) ->
            "UNFOLDHYP " :: pnstr s :: " " :: trterms terms tail
        | FoldHypTac (s, terms) ->
            "FOLDHYP " :: pnstr s :: " " :: trterms terms tail
        | UnfoldTac (s, tacs) ->
            "UNFOLD " :: pnstr s :: nextsep :: trtacs tacs tail
        | FoldTac (s, tacs) ->
            "FOLD " :: pnstr s :: nextsep :: trtacs tacs tail
        | AssignTac (s, term) ->
            "ASSIGN " :: pnstr s :: " " :: catelim_argstring term tail
        | EvalTac terms -> "EVALUATE " :: trterms terms tail
        | AdHocTac terms -> "JAPE " :: trterms terms tail
        | BindArgTac (term, tactic) -> "LETARGSEL " :: termsNtac [term] tactic
        | BindArgTextTac (name, tactic) ->
            "LETARGTEXT " :: parseablenamestring name :: argsep ::
              unSEQ tactic tail
        | BindConcTac (term, tactic) -> "LETCONC " :: termsNtac [term] tactic
        | BindGoalPathTac (name, tactic) ->
            "LETGOALPATH " :: parseablenamestring name :: argsep ::
              unSEQ tactic tail
        | BindHypTac (term, tactic) -> "LETHYP " :: termsNtac [term] tactic
        | BindHyp2Tac (t1, t2, tactic) ->
            "LETHYP2 " :: termsNtac [t1; t2] tactic
        | BindHypsTac (term, tactic) -> "LETHYPS " :: termsNtac [term] tactic
        | BindSubstTac (term, tactic) ->
            "LETSUBSTSEL " :: termsNtac [term] tactic
        | BindSubstInHypTac (term, tactic) ->
            "LETHYPSUBSTSEL " :: termsNtac [term] tactic
        | BindSubstInConcTac (term, tactic) ->
            "LETCONCSUBSTSEL " :: termsNtac [term] tactic
        | BindMultiArgTac (term, tactic) ->
            "LETMULTIARG " :: termsNtac [term] tactic
        | BindLHSTac (term, tactic) -> "LETLHS " :: termsNtac [term] tactic
        | BindRHSTac (term, tactic) -> "LETRHS " :: termsNtac [term] tactic
        | BindGoalTac (term, tactic) -> "LETGOAL " :: termsNtac [term] tactic
        | BindOpenSubGoalTac (name, term, tactic) ->
            "LETOPENSUBGOAL " :: parseablenamestring name :: argsep ::
              termsNtac [term] tactic
        | BindOpenSubGoalsTac (term, tactic) ->
            "LETOPENSUBGOALS " :: termsNtac [term] tactic
        | BindFindHypTac (term, tactic) ->
            "LETHYPFIND " :: termsNtac [term] tactic
        | BindFindConcTac (term, tactic) ->
            "LETCONCFIND " :: termsNtac [term] tactic
        | BindMatchTac (pterm, vterm, tactic) ->
            "LETMATCH " :: termsNtac [pterm; vterm] tactic
        | BindOccursTac (pt, vt, st, tactic) ->
            "LETOCCURS " :: termsNtac [pt; vt; st] tactic
        | LayoutTac (tactic, layout) ->
            "LAYOUT " :: treelayoutstring layout :: argsep ::
              unSEQ tactic tail
        | AssocFlatTac term -> "FLATTEN " :: catelim_argstring term tail
        | MapTac (s, terms) ->
            "MAPTERMS " :: pnstr s :: " " :: trterms terms tail
        | TermTac (s, []) -> pnstr s :: tail
        | TermTac (s, terms) -> pnstr s :: " " :: trterms terms tail
        | SubstTac (s, []) -> pnstr s :: tail
        | SubstTac (s, vts) ->
            catelim_termstring
              (Subst
                 (None, true, Id (None, pnstr s, FormulaClass),
                  (match !showargasint with
                     Some lookup ->
                       let rec encodeterm t =
                         match t with
                           Collection (_, c, es) ->
                             Collection (None, c, m_a_p (encodeelement, es))
                         | _ ->
                             match hashterm t with
                               Some h -> Literal (None, Number (lookup h t))
                             | None -> t
                       and encodeelement =
                         function
                           Element (_, i, t) ->
                             Element (None, i, encodeterm t)
                         | el -> el
                       in
                       m_a_p ((fun (v, t) -> v, encodeterm t), vts)
                   | None -> vts)))
              tail
        | GivenTac i -> "GIVEN " :: catelim_argstring i tail
        | WithArgSelTac tac -> "WITHARGSEL" :: argsep :: unSEQ tac tail
        | WithConcSelTac tac -> "WITHCONCSEL" :: argsep :: unSEQ tac tail
        | WithFormSelTac tac -> "WITHFORMSEL" :: argsep :: unSEQ tac tail
        | WithHypSelTac tac -> "WITHHYPSEL" :: argsep :: unSEQ tac tail
        | WithSelectionsTac tac ->
            "WITHSELECTIONS" :: argsep :: unSEQ tac tail
        | WithSubstSelTac tac -> "WITHSUBSTSEL" :: argsep :: unSEQ tac tail
        | MatchTac tac -> "MATCH" :: argsep :: unSEQ tac tail
        | SameProvisosTac tac -> "SAMEPROVISOS" :: argsep :: unSEQ tac tail
        | SimpleApplyTac tac -> "SIMPLEAPPLY" :: argsep :: unSEQ tac tail
        | ApplyOrResolveTac tac ->
            "APPLYORRESOLVE" :: argsep :: unSEQ tac tail
        | UniqueTac tac -> "UNIQUE" :: argsep :: unSEQ tac tail
        | TakeAnyTac tac -> "ANY" :: argsep :: unSEQ tac tail
        | ResolveTac tac -> "RESOLVE" :: argsep :: unSEQ tac tail
        | ReplayTac tac -> "REPLAY" :: argsep :: unSEQ tac tail
        | ContnTac (tac1, tac2) ->
            "WITHCONTINUATION" :: argsep ::
              tacargstring tac1 (argsep :: unSEQ tac2 tail)
        | AlertTac (m, ps, copt) ->
            "ALERT" :: argsep ::
              catelim_argstring m
                (catelim_liststring
                   (catelim_pairstring catelim_termstring
                      (catelim_tacticstring " ") ", ")
                   argsep ps
                   (match copt with
                      Some t -> argsep :: tacargstring t tail
                    | None -> tail))
        | ExplainTac m -> "EXPLAIN" :: argsep :: catelim_argstring m tail
        | CommentTac m -> "COMMENT" :: argsep :: catelim_argstring m tail
        | UnifyTac terms -> "UNIFY" :: argsep :: trterms terms tail
        | BadUnifyTac (n1, n2, tac) ->
            "BADUNIFY" :: argsep :: parseablenamestring n1 :: argsep ::
              parseablenamestring n2 :: unSEQ tac tail
        | BadMatchTac (n1, n2, tac) ->
            "BADMATCH" :: argsep :: parseablenamestring n1 :: argsep ::
              parseablenamestring n2 :: unSEQ tac tail
        | BadProvisoTac (n1, n2, n3, tac) ->
            "BADPROVISO" :: argsep :: parseablenamestring n1 :: argsep ::
              parseablenamestring n2 :: parseablenamestring n3 ::
              unSEQ tac tail
        | UnifyArgsTac -> "UNIFYARGS" :: tail
      in
      tss
    let catelim_tacticstring = catelim_tacticstring " "
    and catelim_tacticstringwithNLs = catelim_tacticstring "\n"
    let tacticstring = catelim2stringfn catelim_tacticstring
    let tacticstringwithNLs = catelim2stringfn catelim_tacticstringwithNLs
    let rec remaptactic env t =
      let E = remapterm env in
      let rec Ep =
        function
          Parent p -> Parent (Ep p)
        | LeftSibling p -> LeftSibling (Ep p)
        | RightSibling p -> RightSibling (Ep p)
        | Subgoal (p, t) -> Subgoal (Ep p, E t)
        | HypRoot p -> HypRoot (Ep p)
        | SimplePath t -> SimplePath (E t)
      in
      let rec T t =
        match t with
          SkipTac -> t
        | FailTac -> t
        | StopTac -> t
        | NextgoalTac -> t
        | SetgoalTac p -> SetgoalTac (Ep p)
        | TheoryAltTac ns -> t
        | GivenTac i -> GivenTac (E i)
        | RepTac t -> RepTac (T t)
        | IfTac t -> IfTac (T t)
        | CompTac t -> CompTac (T t)
        | CutinTac t -> CutinTac (T t)
        | WithArgSelTac t -> WithArgSelTac (T t)
        | WithConcSelTac t -> WithConcSelTac (T t)
        | WithFormSelTac t -> WithFormSelTac (T t)
        | WithHypSelTac t -> WithHypSelTac (T t)
        | WithSelectionsTac t -> WithSelectionsTac (T t)
        | WithSubstSelTac t -> WithSubstSelTac (T t)
        | MatchTac t -> MatchTac (T t)
        | SameProvisosTac t -> SameProvisosTac (T t)
        | SimpleApplyTac t -> SimpleApplyTac (T t)
        | ApplyOrResolveTac t -> ApplyOrResolveTac (T t)
        | UniqueTac t -> UniqueTac (T t)
        | TakeAnyTac t -> TakeAnyTac (T t)
        | ResolveTac t -> ResolveTac (T t)
        | ReplayTac t -> ReplayTac (T t)
        | ContnTac (t1, t2) -> ContnTac (T t1, T t2)
        | AssocFlatTac tm -> AssocFlatTac (E tm)
        | UnifyTac tms -> UnifyTac (m_a_p (E, tms))
        | AltTac ts -> AltTac (m_a_p (T, ts))
        | SeqTac ts -> SeqTac (m_a_p (T, ts))
        | WhenTac ts -> WhenTac (m_a_p (T, ts))
        | UnfoldHypTac (s, tms) -> UnfoldHypTac (s, m_a_p (E, tms))
        | FoldHypTac (s, tms) -> FoldHypTac (s, m_a_p (E, tms))
        | MapTac (s, tms) -> MapTac (s, m_a_p (E, tms))
        | TermTac (s, tms) -> TermTac (s, m_a_p (E, tms))
        | SubstTac (s, vts) ->
            SubstTac (s, m_a_p ((fun (v, t) -> E v, E t), vts))
        | UnfoldTac (s, ts) -> UnfoldTac (s, m_a_p (T, ts))
        | FoldTac (s, ts) -> FoldTac (s, m_a_p (T, ts))
        | AssignTac (s, t) -> AssignTac (s, E t)
        | EvalTac tms -> EvalTac (m_a_p (E, tms))
        | AdHocTac tms -> AdHocTac (m_a_p (E, tms))
        | BindConcTac (tm, t) -> BindConcTac (E tm, T t)
        | BindHypTac (tm, t) -> BindHypTac (E tm, T t)
        | BindHyp2Tac (tm1, tm2, t) -> BindHyp2Tac (E tm1, E tm2, T t)
        | BindHypsTac (tm, t) -> BindHypsTac (E tm, T t)
        | BindArgTac (tm, t) -> BindArgTac (E tm, T t)
        | BindArgTextTac (s, t) -> BindArgTextTac (s, T t)
        | BindGoalPathTac (s, t) ->(* boy is this wrong! *)
           BindGoalPathTac (s, T t)
        | BindSubstTac (tm, t) ->(* boy is this wrong! *)
           BindSubstTac (E tm, T t)
        | BindSubstInHypTac (tm, t) -> BindSubstInHypTac (E tm, T t)
        | BindSubstInConcTac (tm, t) -> BindSubstInConcTac (E tm, T t)
        | BindMultiArgTac (tm, t) -> BindMultiArgTac (E tm, T t)
        | BindLHSTac (tm, t) -> BindLHSTac (E tm, T t)
        | BindRHSTac (tm, t) -> BindRHSTac (E tm, T t)
        | BindGoalTac (tm, t) -> BindGoalTac (E tm, T t)
        | BindOpenSubGoalTac (n, tm, t) -> BindOpenSubGoalTac (n, E tm, T t)
        | BindOpenSubGoalsTac (tm, t) ->(* boy is this wrong! *)
           BindOpenSubGoalsTac (E tm, T t)
        | BindFindHypTac (tm, t) -> BindFindHypTac (E tm, T t)
        | BindFindConcTac (tm, t) -> BindFindConcTac (E tm, T t)
        | BindMatchTac (ptm, vtm, t) -> BindMatchTac (E ptm, E vtm, T t)
        | BindOccursTac (pt, vt, st, t) ->
            BindOccursTac (E pt, E vt, E st, T t)
        | LayoutTac (t, tl) -> LayoutTac (T t, remaptreelayout env tl)
        | AlertTac (m, ps, copt) ->
            AlertTac
              (E m, m_a_p ((fun (l, t) -> E l, T t), ps),
               (fun ooo -> andthenr (copt, Some) (T ooo)))
        | ExplainTac m -> ExplainTac (E m)
        | CommentTac m -> CommentTac (E m)
        | BadUnifyTac (n1, n2, t) -> BadUnifyTac (n1, n2, T t)
        | BadMatchTac (n1, n2, t) -> BadMatchTac (n1, n2, T t)
        | BadProvisoTac (n1, n2, n3, t) -> BadProvisoTac (n1, n2, n3, T t)
        | UnifyArgsTac -> UnifyArgsTac
      in
      T t
    let rec tacticform i =
      member
        (namestring i,
         ["ANY"; "ALERT"; "ALT"; "APPLYORRESOLVE"; "ASSIGN"; "CUTIN"; "DO";
          "EVALUATE"; "EXPLAIN"; "EXPLICIT"; "FAIL"; "FLATTEN"; "FOLD";
          "FOLDHYP"; "GIVEN"; "GOALPATH"; "IF"; "IMPLICIT"; "JAPE"; "LAYOUT";
          "LETARGSEL"; "LETCONC"; "LETCONCFIND"; "LETCONCSUBSTSEL"; "LETGOAL";
          "LETGOALPATH"; "LETOPENSUBGOAL"; "LETOPENSUBGOALS"; "LETHYP";
          "LETHYP2"; "LETHYPS"; "LETHYPFIND"; "LETHYPSUBSTSEL"; "LETMATCH";
          "LETMULTIARG"; "LETOCCURS"; "LETSUBSTSEL"; "MAPTERMS"; "MATCH";
          "NEXTGOAL"; "PROVE"; "REPLAY"; "RESOLVE"; "SAMEPROVISOS"; "SEQ";
          "SIMPLEAPPLY"; "SKIP"; "STOP"; "THEORYALT"; "UNFOLD"; "UNFOLDHYP";
          "UNIFY"; "UNIQUE"; "WHEN"; "WITHARGSEL"; "WITHCONCSEL";
          "WITHCONTINUATION"; "WITHFORMSEL"; "WITHHYPSEL"; "WITHSELECTIONS";
          "WITHSUBSTSEL"; "BADUNIFY"; "BADMATCH"; "BADPROVISO"; "UNIFYARGS"])
    exception TacParseError_ of string list
    let rec mustbeSTR term =
      match debracket term with
        Literal (_, String s) -> s
      | Id (_, i, _) -> i
      | _ -> raise (TacParseError_ [argstring term; " should be a string"])
    let rec maybeSTR term =
      try Some (mustbeSTR term) with
        _ -> None
    let rec tacname term =
      try Name (mustbeSTR term) with
        TacParseError_ _ ->
          raise (TacParseError_ [argstring term; " should be a name"])
    let rec butnottacticform term =
      let name = tacname term in
      if tacticform name then
        raise (TacParseError_ [namestring name; " used as tactic name"])
      else name
    (* a_l_l tactic applications MUST be Curried.  RB & BS 13/8/93.
     * Outermost brackets are stripped from non-tuple arguments. RB
     *)
    
    let rec explodeForExecute t =
      let (f, rs) = explodeApp false t in tacname f, rs
    let rec badLayout () =
      raise
        (TacParseError_
           ["Syntax is LAYOUT fmt (subtrees) tactic; \
           \fmt can be () or s or (s,s); \
           \s can be string, id, unknown or number"])
    let rec checkNAME errf t =
      (* suitable for a place where a single name will do *)
      match debracket t with
        Id _ as t -> t
      | Unknown _ as t -> t
      | t -> raise (TacParseError_ (errf t))
    let rec checkSTR errf t =
      match debracket t with
        Id _ as t -> t
      | Unknown _ as t -> t
      | Literal _ as t -> t
      | t -> raise (TacParseError_ (errf t))
    let rec checkINT errf t =
      match debracket t with
        Id _ as t -> t
      | Unknown _ as t -> t
      | Literal (_, Number _) as t -> t
      | _ -> raise (TacParseError_ (errf t))
    let rec checkINTS errf t =
      match debracket t with
        Tup (_, ",", ts) as t' -> m_a_p (checkINT errf, ts); t'
      | t -> checkINT errf t
    let rec isguard =
      function
        BindConcTac _ -> true
      | BindHypTac _ -> true
      | BindHyp2Tac _ -> true
      | BindHypsTac _ -> true
      | BindArgTac _ -> true
      | BindArgTextTac _ -> true
      | BindGoalPathTac _ -> true
      | BindSubstTac _ -> true
      | BindSubstInHypTac _ -> true
      | BindSubstInConcTac _ -> true
      | BindMultiArgTac _ -> true
      | BindLHSTac _ -> true
      | BindRHSTac _ -> true
      | BindGoalTac _ -> true
      | BindOpenSubGoalTac _ -> true
      | BindOpenSubGoalsTac _ -> true
      | BindFindHypTac _ -> true
      | BindFindConcTac _ -> true
      | BindMatchTac _ -> true
      | BindOccursTac _ -> true
      | BadUnifyTac _ -> true
      | BadMatchTac _ -> true
      | BadProvisoTac _ -> true
      | _ -> false
    let rec mkSEQ =
      function
        [] -> SkipTac
      | [t] -> t
      | ts -> SeqTac ts
    and SEQTAC ts = mkSEQ (m_a_p (transTactic, ts))
    and SEQ1TAC a1 a2 =
      match a1, a2 with
        name, [] ->
          raise
            (TacParseError_
               [name; " tactic must include a non-empty sequence of tactics"])
      | name, ts -> SEQTAC ts
    and transTactic tacterm =
      try
        match debracket tacterm with
          Id (_, "SKIP", _) -> SkipTac
        | Id (_, "FAIL", _) -> FailTac
        | Id (_, "STOP", _) -> StopTac
        | Id (_, "NEXTGOAL", _) -> NextgoalTac
        | Id (_, "UNIFYARGS", _) -> UnifyArgsTac
        | Subst (_, _, P, vts) ->
            SubstTac
              (butnottacticform P,
               (match !readintasarg with
                  Some lookup ->
                    let rec decodeterm t =
                      match t with
                        Literal (_, Number i) ->
                          begin try Array.get (lookup) (i) with
                            _ ->
                              raise
                                (Catastrophe_
                                   ["transTactic and readintasarg see ";
                                    string_of_int i])
                          end
                      | Collection (_, c, es) ->
                          registerCollection (c, m_a_p (decodeelement, es))
                      | _ -> t
                    and decodeelement =
                      function
                        Element (_, i, t) -> registerElement (i, decodeterm t)
                      | el -> el
                    in
                    m_a_p ((fun (v, t) -> v, decodeterm t), vts)
                | None -> vts))
        | t ->
            let (n, ts as parts) = explodeForExecute t in
            let f = namestring n in
            let rec Bad why =
              raise (TacParseError_ [termstring t; " -- "; why])
            in
            let rec Assignments =
              function
                [] -> []
              | s :: t :: ts ->
                  AssignTac (tacname s, debracket t) :: Assignments ts
              | _ -> Bad "Assignment malformed"
            in
            let rec onearg =
              function
                [t] -> t
              | [] -> Bad "No argument"
              | _ -> Bad "More than one argument"
            in
            let rec atleasttwo =
              function
                _ :: _ :: _ as ts -> ts
              | ts -> Bad "should have at least two arguments"
            in
            let rec mkBind a1 a2 a3 a4 a5 =
              match a1, a2, a3, a4, a5 with
                f, bind, str, tac, x :: ts -> tac (bind x, SEQTAC ts)
              | f, bind, str, tac, [] ->
                  raise
                    (TacParseError_
                       ["Syntax is ("; f; " <"; str; "> tactic ...)"])
            in
            let rec mkBind2 a1 a2 a3 a4 a5 a6 a7 =
              match a1, a2, a3, a4, a5, a6, a7 with
                f, b1, b2, s1, s2, tac, x1 :: x2 :: ts ->
                  tac (b1 x1, b2 x2, SEQTAC ts)
              | f, b1, b2, s1, s2, tac, _ ->
                  raise
                    (TacParseError_
                       ["Syntax is ("; f; " <"; s1; "> <"; s2;
                        "> tactic ...)"])
            in
            let rec mkBind3 a1 a2 a3 a4 a5 a6 a7 a8 a9 =
              match a1, a2, a3, a4, a5, a6, a7, a8, a9 with
                f, b1, b2, b3, s1, s2, s3, tac, x1 :: x2 :: x3 :: ts ->
                  tac (b1 x1, b2 x2, b3 x3, SEQTAC ts)
              | f, b1, b2, b3, s1, s2, s3, tac, _ ->
                  raise
                    (TacParseError_
                       ["Syntax is ("; f; " <"; s1; "> <"; s2; "> <"; s3;
                        "> tactic ...)"])
            in
            let rec patbind t = t in
            let rec namebind n = tacname n in
            let rec mkMatch =
              function
                pat :: expr :: ts -> BindMatchTac (pat, expr, SEQTAC ts)
              | _ ->
                  raise
                    (TacParseError_
                       ["Syntax is (LETMATCH <pattern> <expr> tactic ...)"])
            in
            let rec mkOccurs =
              function
                pat1 :: expr :: pat2 :: ts ->
                  BindOccursTac (pat1, expr, pat2, SEQTAC ts)
              | _ ->
                  raise
                    (TacParseError_
                       ["Syntax is (LETOCCURS <pattern> <expr> <substpattern> tactic ...)"])
            in
            let rec mkLayout =
              function
                [] -> badLayout ()
              | fmt :: stuff ->
                  let rec lyt a1 a2 a3 =
                    match a1, a2, a3 with
                      con, fmt, [] -> transLayout con fmt None []
                    | con, fmt, ns :: ts -> transLayout con fmt (Some ns) ts
                  in
                  match maybeSTR fmt, stuff with
                    Some "HIDEROOT", ts ->
                      LayoutTac (SEQTAC ts, HideRootLayout)
                  | Some "HIDECUT", ts -> LayoutTac (SEQTAC ts, HideCutLayout)
                  | Some "COMPRESS", fmt :: ts -> lyt CompressedLayout fmt ts
                  | Some "COMPRESS", ts ->
                      lyt CompressedLayout (registerLiteral (String "%s")) ts
                  | _, ts -> lyt NamedLayout fmt ts
            in
            let rec mkFold a1 a2 =
              match a1, a2 with
                tac, n :: ts -> tac (tacname n, m_a_p (transTactic, ts))
              | tac, [] ->
                  raise
                    (TacParseError_
                       ["Syntax is (FOLD/UNFOLD tacticname tactic ...)"])
            in
            let rec mkHypFold a1 a2 =
              match a1, a2 with
                tac, n :: ts -> tac (tacname n, ts)
              | tac, [] ->
                  raise
                    (TacParseError_
                       ["Syntax is (FOLDHYP/UNFOLDHYP tacticname pattern ...)"])
            in
            let rec mkAlert =
              function
                m :: ps ->
                  let rec f a1 a2 =
                    match a1, a2 with
                      rs, Tup (_, ",", [l; t]) :: ps ->
                        f ((l, transTactic t) :: rs) ps
                    | rs, [t] -> AlertTac (m, rev rs, Some (transTactic t))
                    | rs, [] -> AlertTac (m, rev rs, None)
                    | rs, t :: _ ->
                        raise
                          (TacParseError_
                             ["badly formed argument "; termstring t])
                  in
                  f [] ps
              | [] -> raise (TacParseError_ ["no arguments"])
            in
            let rec parsegoalexpr t =
              let rec err _ =
                ["path expected after GOALPATH -- found "; termstring t]
              in
              match explodeApp false t with
                f, rs ->
                  match maybeSTR f, rs with
                    Some "PARENT", [r] -> Parent (parsegoalexpr r)
                  | Some "LEFT", [r] -> LeftSibling (parsegoalexpr r)
                  | Some "RIGHT", [r] -> RightSibling (parsegoalexpr r)
                  | Some "SUBGOAL", [r; ns] ->
                      Subgoal (parsegoalexpr r, checkINTS err (debracket ns))
                  | Some "HYPROOT", [r] -> HypRoot (parsegoalexpr r)
                  | _, [] -> SimplePath (checkINTS err (debracket f))
                  | _ -> raise (TacParseError_ (err t))
            in
            match f with
              "SKIP" -> Bad "SKIP mustn't be given any arguments"
            | "FAIL" -> Bad "FAIL mustn't be given any arguments"
            | "STOP" -> Bad "STOP mustn't be given any arguments"
            | "NEXTGOAL" -> Bad "NEXTGOAL mustn't be given any arguments"
            | "GOALPATH" -> SetgoalTac (parsegoalexpr (onearg ts))
            | "PROVE" -> CompTac (SEQ1TAC (f, ts))
            | "CUTIN" -> CutinTac (SEQ1TAC (f, ts))
            | "JAPE" -> AdHocTac [onearg ts]
            | "FLATTEN" -> AssocFlatTac (debracket (onearg ts))
            | "MAPTERMS" -> MapTac (explodeForExecute (onearg ts))
            | "SEQ" -> SEQTAC ts
            | "ALT" -> AltTac (m_a_p (transTactic, ts))
            | "THEORYALT" -> TheoryAltTac (m_a_p (butnottacticform, ts))
            | "IF" -> IfTac (SEQ1TAC (f, ts))
            | "DO" -> RepTac (SEQ1TAC (f, ts))
            | "FOLD" -> mkFold FoldTac ts
            | "UNFOLD" -> mkFold UnfoldTac ts
            | "FOLDHYP" -> mkHypFold FoldHypTac ts
            | "UNFOLDHYP" -> mkHypFold UnfoldHypTac ts
            | "WITHARGSEL" -> WithArgSelTac (SEQ1TAC (f, ts))
            | "WITHCONCSEL" -> WithConcSelTac (SEQ1TAC (f, ts))
            | "WITHFORMSEL" -> WithFormSelTac (SEQ1TAC (f, ts))
            | "WITHHYPSEL" -> WithHypSelTac (SEQ1TAC (f, ts))
            | "WITHSELECTIONS" -> WithSelectionsTac (SEQ1TAC (f, ts))
            | "WITHSUBSTSEL" -> WithSubstSelTac (SEQ1TAC (f, ts))
            | "EVALUATE" -> EvalTac ts
            | "ASSIGN" -> mkSEQ (Assignments ts)
            | "WHEN" ->
                let tacs = m_a_p (transTactic, ts) in
                let rec okwhen =
                  function
                    [] -> ()
                  | [t] -> ()
                  | t1 :: ts ->
                      if isguard t1 then okwhen ts
                      else Bad "WHEN must be given guarded tactics"
                in
                okwhen tacs; WhenTac tacs
            | "LETCONC" -> mkBind f patbind "pattern" BindConcTac ts
            | "LETHYP" -> mkBind f patbind "pattern" BindHypTac ts
            | "LETHYP2" ->
                mkBind2 f patbind patbind "pattern" "pattern" BindHyp2Tac ts
            | "LETHYPS" -> mkBind f patbind "pattern" BindHypsTac ts
            | "LETLHS" -> mkBind f patbind "pattern" BindLHSTac ts
            | "LETRHS" -> mkBind f patbind "pattern" BindRHSTac ts
            | "LETGOAL" -> mkBind f patbind "pattern" BindGoalTac ts
            | "LETGOALPATH" -> mkBind f namebind "name" BindGoalPathTac ts
            | "LETOPENSUBGOAL" ->
                mkBind2 f namebind patbind "name" "pattern" BindOpenSubGoalTac
                  ts
            | "LETOPENSUBGOALS" ->
                mkBind f patbind "pattern" BindOpenSubGoalsTac ts
            | "LETARGSEL" -> mkBind f patbind "pattern" BindArgTac ts
            | "LETARGTEXT" -> mkBind f namebind "name" BindArgTextTac ts
            | "LETSUBSTSEL" -> mkBind f patbind "pattern" BindSubstTac ts
            | "LETHYPSUBSTSEL" ->
                mkBind f patbind "pattern" BindSubstInHypTac ts
            | "LETCONCSUBSTSEL" ->
                mkBind f patbind "pattern" BindSubstInConcTac ts
            | "LETMULTIARG" -> mkBind f patbind "pattern" BindMultiArgTac ts
            | "LETHYPFIND" -> mkBind f patbind "pattern" BindFindHypTac ts
            | "LETCONCFIND" -> mkBind f patbind "pattern" BindFindConcTac ts
            | "LETMATCH" -> mkMatch ts
            | "LETOCCURS" -> mkOccurs ts
            | "LAYOUT" -> mkLayout ts
            | "MATCH" -> MatchTac (SEQ1TAC (f, ts))
            | "SAMEPROVISOS" -> SameProvisosTac (SEQ1TAC (f, ts))
            | "SIMPLEAPPLY" -> SimpleApplyTac (SEQ1TAC (f, ts))
            | "APPLYORRESOLVE" -> ApplyOrResolveTac (SEQ1TAC (f, ts))
            | "UNIQUE" -> UniqueTac (SEQ1TAC (f, ts))
            | "ANY" -> TakeAnyTac (SEQ1TAC (f, ts))
            | "UNIFY" -> UnifyTac (atleasttwo ts)
            | "RESOLVE" -> ResolveTac (SEQ1TAC (f, ts))
            | "REPLAY" -> ReplayTac (SEQ1TAC (f, ts))
            | "WITHCONTINUATION" ->
                begin match ts with
                  t1 :: ts -> ContnTac (transTactic t1, SEQTAC ts)
                | _ -> raise (TacParseError_ ["no argument tactic!"])
                end
            | "GIVEN" ->
                begin match ts with
                  [Literal _ as t] -> GivenTac t
                | [Id _ as t] -> GivenTac t
                | _ ->
                    raise
                      (TacParseError_
                         ["must have exactly one name/number argument!"])
                end
            | "ALERT" -> mkAlert ts
            | "EXPLAIN" -> ExplainTac (onearg ts)
            | "COMMENT" -> CommentTac (onearg ts)
            | "BADUNIFY" ->
                mkBind2 f namebind namebind "name" "name" BadUnifyTac ts
            | "BADMATCH" ->
                mkBind2 f namebind namebind "name" "name" BadMatchTac ts
            | "BADPROVISO" ->
                mkBind3 f namebind namebind namebind "name" "name" "name"
                  BadProvisoTac ts
            | _ ->
                if tacticform n then
                  raise (Catastrophe_ ["unrecognised tactic "; f])
                else TermTac parts
      with
        TacParseError_ ss ->
          raise
            (ParseError_
               (["Bad tactic: ("; termstring tacterm; ") -- "] @ ss))
      | ParseError_ ss -> raise (ParseError_ ss)
      | Catastrophe_ ss -> raise (Catastrophe_ ss)
      | exn ->
          raise
            (ParseError_
               ["Unexpected exception in transTactic: "; System.exn_name exn])
    and transLayout con fmt bopt ts =
      let rec fmterr t =
        ["format string expected in LAYOUT; found "; termstring t]
      in
      let fmt = checkSTR fmterr fmt in
      let rec nserr t =
        ["list of subtree indices expected in LAYOUT; found ";
         termstring (unSOME bopt)]
      in
      let bopt =
        match try__ (checkINTS nserr) bopt with
          Some (Id (_, "ALL", _)) -> None
        | r -> r
      in
      LayoutTac (SEQTAC ts, con (fmt, bopt))
    let tacname t =
      try tacname t with
        TacParseError_ _ ->
          raise (ParseError_ ["not a string or an identifier"])
  end






