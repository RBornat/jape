(* $Id$ *)

(* Yet another place where I can't use multiple views *)

module type T = sig
(* module type Tactictype =
  sig
 *)
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

module M : T with type ('a,'b) mapping = ('a,'b) Mappingfuns.M.mapping
              and type name = Name.M.name
              and type term = Term.Type.term
              and type treelayout = Treelayout.M.treelayout
              and type seq = Sequent.M.seq
=
  struct
    open Listfuns.M
    open Stringfuns.M
    open Term.M
    open Sequent.M
    open Name.M
    open Treelayout.M    
    open Idclass.M
    open Match.M
    open Optionfuns.M
    open Miscellaneous.M
    open SML.M
    
    type ('a,'b) mapping = ('a,'b) Mappingfuns.M.mapping
     and name = Name.M.name
     and term = Term.Type.term
     and treelayout = Treelayout.M.treelayout
     and seq = Sequent.M.seq
     
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
                             Collection (None, c, (encodeelement <* es))
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
                       ((fun (v, t) -> v, encodeterm t) <* vts)
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
      let _E = remapterm env in
      let rec _Ep =
        function
          Parent p -> Parent (_Ep p)
        | LeftSibling p -> LeftSibling (_Ep p)
        | RightSibling p -> RightSibling (_Ep p)
        | Subgoal (p, t) -> Subgoal (_Ep p, _E t)
        | HypRoot p -> HypRoot (_Ep p)
        | SimplePath t -> SimplePath (_E t)
      in
      let rec _T t =
        match t with
          SkipTac -> t
        | FailTac -> t
        | StopTac -> t
        | NextgoalTac -> t
        | SetgoalTac p -> SetgoalTac (_Ep p)
        | TheoryAltTac ns -> t
        | GivenTac i -> GivenTac (_E i)
        | RepTac t -> RepTac (_T t)
        | IfTac t -> IfTac (_T t)
        | CompTac t -> CompTac (_T t)
        | CutinTac t -> CutinTac (_T t)
        | WithArgSelTac t -> WithArgSelTac (_T t)
        | WithConcSelTac t -> WithConcSelTac (_T t)
        | WithFormSelTac t -> WithFormSelTac (_T t)
        | WithHypSelTac t -> WithHypSelTac (_T t)
        | WithSelectionsTac t -> WithSelectionsTac (_T t)
        | WithSubstSelTac t -> WithSubstSelTac (_T t)
        | MatchTac t -> MatchTac (_T t)
        | SameProvisosTac t -> SameProvisosTac (_T t)
        | SimpleApplyTac t -> SimpleApplyTac (_T t)
        | ApplyOrResolveTac t -> ApplyOrResolveTac (_T t)
        | UniqueTac t -> UniqueTac (_T t)
        | TakeAnyTac t -> TakeAnyTac (_T t)
        | ResolveTac t -> ResolveTac (_T t)
        | ReplayTac t -> ReplayTac (_T t)
        | ContnTac (t1, t2) -> ContnTac (_T t1, _T t2)
        | AssocFlatTac tm -> AssocFlatTac (_E tm)
        | UnifyTac tms -> UnifyTac ((_E <* tms))
        | AltTac ts -> AltTac ((_T <* ts))
        | SeqTac ts -> SeqTac ((_T <* ts))
        | WhenTac ts -> WhenTac ((_T <* ts))
        | UnfoldHypTac (s, tms) -> UnfoldHypTac (s, (_E <* tms))
        | FoldHypTac (s, tms) -> FoldHypTac (s, (_E <* tms))
        | MapTac (s, tms) -> MapTac (s, (_E <* tms))
        | TermTac (s, tms) -> TermTac (s, (_E <* tms))
        | SubstTac (s, vts) ->
            SubstTac (s, ((fun (v, t) -> _E v, _E t) <* vts))
        | UnfoldTac (s, ts) -> UnfoldTac (s, (_T <* ts))
        | FoldTac (s, ts) -> FoldTac (s, (_T <* ts))
        | AssignTac (s, t) -> AssignTac (s, _E t)
        | EvalTac tms -> EvalTac ((_E <* tms))
        | AdHocTac tms -> AdHocTac ((_E <* tms))
        | BindConcTac (tm, t) -> BindConcTac (_E tm, _T t)
        | BindHypTac (tm, t) -> BindHypTac (_E tm, _T t)
        | BindHyp2Tac (tm1, tm2, t) -> BindHyp2Tac (_E tm1, _E tm2, _T t)
        | BindHypsTac (tm, t) -> BindHypsTac (_E tm, _T t)
        | BindArgTac (tm, t) -> BindArgTac (_E tm, _T t)
        | BindArgTextTac (s, t) -> BindArgTextTac (s, _T t)
        | BindGoalPathTac (s, t) ->(* boy is this wrong! *)
           BindGoalPathTac (s, _T t)
        | BindSubstTac (tm, t) ->(* boy is this wrong! *)
           BindSubstTac (_E tm, _T t)
        | BindSubstInHypTac (tm, t) -> BindSubstInHypTac (_E tm, _T t)
        | BindSubstInConcTac (tm, t) -> BindSubstInConcTac (_E tm, _T t)
        | BindMultiArgTac (tm, t) -> BindMultiArgTac (_E tm, _T t)
        | BindLHSTac (tm, t) -> BindLHSTac (_E tm, _T t)
        | BindRHSTac (tm, t) -> BindRHSTac (_E tm, _T t)
        | BindGoalTac (tm, t) -> BindGoalTac (_E tm, _T t)
        | BindOpenSubGoalTac (n, tm, t) -> BindOpenSubGoalTac (n, _E tm, _T t)
        | BindOpenSubGoalsTac (tm, t) ->(* boy is this wrong! *)
           BindOpenSubGoalsTac (_E tm, _T t)
        | BindFindHypTac (tm, t) -> BindFindHypTac (_E tm, _T t)
        | BindFindConcTac (tm, t) -> BindFindConcTac (_E tm, _T t)
        | BindMatchTac (ptm, vtm, t) -> BindMatchTac (_E ptm, _E vtm, _T t)
        | BindOccursTac (pt, vt, st, t) ->
            BindOccursTac (_E pt, _E vt, _E st, _T t)
        | LayoutTac (t, tl) -> LayoutTac (_T t, remaptreelayout env tl)
        | AlertTac (m, ps, copt) ->
            AlertTac
              (_E m, ((fun (l, t) -> _E l, _T t) <* ps),
               ((copt &~~ (fSome <*> _T))))
        | ExplainTac m -> ExplainTac (_E m)
        | CommentTac m -> CommentTac (_E m)
        | BadUnifyTac (n1, n2, t) -> BadUnifyTac (n1, n2, _T t)
        | BadMatchTac (n1, n2, t) -> BadMatchTac (n1, n2, _T t)
        | BadProvisoTac (n1, n2, n3, t) -> BadProvisoTac (n1, n2, n3, _T t)
        | UnifyArgsTac -> UnifyArgsTac
      in
      _T t
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
    (* _All tactic applications MUST be Curried.  RB & BS 13/8/93.
     * Outermost brackets are stripped from non-tuple arguments. RB
     *)
    
    let rec explodeForExecute t =
      let (f, rs) = explodeApp false t in tacname f, rs
    let rec badLayout () =
      raise
        (TacParseError_
           ["Syntax is LAYOUT fmt (subtrees) tactic; \
            fmt can be () or s or (s,s); \
            s can be string, id, unknown or number"])
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
        Tup (_, ",", ts) as t' -> let _ = List.map (checkINT errf) ts in t'
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
    and _SEQTAC ts = mkSEQ ((transTactic <* ts))
    and _SEQ1TAC a1 a2 =
      match a1, a2 with
        name, [] ->
          raise
            (TacParseError_
               [name; " tactic must include a non-empty sequence of tactics"])
      | name, ts -> _SEQTAC ts
    and transTactic tacterm =
      try
        match debracket tacterm with
          Id (_, "SKIP", _) -> SkipTac
        | Id (_, "FAIL", _) -> FailTac
        | Id (_, "STOP", _) -> StopTac
        | Id (_, "NEXTGOAL", _) -> NextgoalTac
        | Id (_, "UNIFYARGS", _) -> UnifyArgsTac
        | Subst (_, _, _P, vts) ->
            SubstTac
              (butnottacticform _P,
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
                          registerCollection (c, (decodeelement <* es))
                      | _ -> t
                    and decodeelement =
                      function
                        Element (_, i, t) -> registerElement (i, decodeterm t)
                      | el -> el
                    in
                    ((fun (v, t) -> v, decodeterm t) <* vts)
                | None -> vts))
        | t ->
            let (n, ts as parts) = explodeForExecute t in
            let f = namestring n in
            let rec _Bad why =
              raise (TacParseError_ [termstring t; " -- "; why])
            in
            let rec _Assignments =
              function
                [] -> []
              | s :: t :: ts ->
                  AssignTac (tacname s, debracket t) :: _Assignments ts
              | _ -> _Bad "Assignment malformed"
            in
            let rec onearg =
              function
                [t] -> t
              | [] -> _Bad "No argument"
              | _ -> _Bad "More than one argument"
            in
            let rec atleasttwo =
              function
                _ :: _ :: _ as ts -> ts
              | ts -> _Bad "should have at least two arguments"
            in
            let rec mkBind a1 a2 a3 a4 a5 =
              match a1, a2, a3, a4, a5 with
                f, bind, str, tac, x :: ts -> tac (bind x, _SEQTAC ts)
              | f, bind, str, tac, [] ->
                  raise
                    (TacParseError_
                       ["Syntax is ("; f; " <"; str; "> tactic ...)"])
            in
            let rec mkBind2 a1 a2 a3 a4 a5 a6 a7 =
              match a1, a2, a3, a4, a5, a6, a7 with
                f, b1, b2, s1, s2, tac, x1 :: x2 :: ts ->
                  tac (b1 x1, b2 x2, _SEQTAC ts)
              | f, b1, b2, s1, s2, tac, _ ->
                  raise
                    (TacParseError_
                       ["Syntax is ("; f; " <"; s1; "> <"; s2;
                        "> tactic ...)"])
            in
            let rec mkBind3 a1 a2 a3 a4 a5 a6 a7 a8 a9 =
              match a1, a2, a3, a4, a5, a6, a7, a8, a9 with
                f, b1, b2, b3, s1, s2, s3, tac, x1 :: x2 :: x3 :: ts ->
                  tac (b1 x1, b2 x2, b3 x3, _SEQTAC ts)
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
                pat :: expr :: ts -> BindMatchTac (pat, expr, _SEQTAC ts)
              | _ ->
                  raise
                    (TacParseError_
                       ["Syntax is (LETMATCH <pattern> <expr> tactic ...)"])
            in
            let rec mkOccurs =
              function
                pat1 :: expr :: pat2 :: ts ->
                  BindOccursTac (pat1, expr, pat2, _SEQTAC ts)
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
                      LayoutTac (_SEQTAC ts, HideRootLayout)
                  | Some "HIDECUT", ts -> LayoutTac (_SEQTAC ts, HideCutLayout)
                  | Some "COMPRESS", fmt :: ts -> lyt (fun v->CompressedLayout v) fmt ts
                  | Some "COMPRESS", ts ->
                      lyt (fun v->CompressedLayout v) (registerLiteral (String "%s")) ts
                  | _, ts -> lyt (fun v->NamedLayout v) fmt ts
            in
            let rec mkFold a1 a2 =
              match a1, a2 with
                tac, n :: ts -> tac (tacname n, (transTactic <* ts))
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
                    | rs, [t] -> AlertTac (m, List.rev rs, Some (transTactic t))
                    | rs, [] -> AlertTac (m, List.rev rs, None)
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
              "SKIP" -> _Bad "SKIP mustn't be given any arguments"
            | "FAIL" -> _Bad "FAIL mustn't be given any arguments"
            | "STOP" -> _Bad "STOP mustn't be given any arguments"
            | "NEXTGOAL" -> _Bad "NEXTGOAL mustn't be given any arguments"
            | "GOALPATH" -> SetgoalTac (parsegoalexpr (onearg ts))
            | "PROVE" -> CompTac (_SEQ1TAC f ts)
            | "CUTIN" -> CutinTac (_SEQ1TAC f ts)
            | "JAPE" -> AdHocTac [onearg ts]
            | "FLATTEN" -> AssocFlatTac (debracket (onearg ts))
            | "MAPTERMS" -> MapTac (explodeForExecute (onearg ts))
            | "SEQ" -> _SEQTAC ts
            | "ALT" -> AltTac ((transTactic <* ts))
            | "THEORYALT" -> TheoryAltTac ((butnottacticform <* ts))
            | "IF" -> IfTac (_SEQ1TAC f ts)
            | "DO" -> RepTac (_SEQ1TAC f ts)
            | "FOLD" -> mkFold (fun v->FoldTac v) ts
            | "UNFOLD" -> mkFold (fun v->UnfoldTac v) ts
            | "FOLDHYP" -> mkHypFold (fun v->FoldHypTac v) ts
            | "UNFOLDHYP" -> mkHypFold (fun v->UnfoldHypTac v) ts
            | "WITHARGSEL" -> WithArgSelTac (_SEQ1TAC f ts)
            | "WITHCONCSEL" -> WithConcSelTac (_SEQ1TAC f ts)
            | "WITHFORMSEL" -> WithFormSelTac (_SEQ1TAC f ts)
            | "WITHHYPSEL" -> WithHypSelTac (_SEQ1TAC f ts)
            | "WITHSELECTIONS" -> WithSelectionsTac (_SEQ1TAC f ts)
            | "WITHSUBSTSEL" -> WithSubstSelTac (_SEQ1TAC f ts)
            | "EVALUATE" -> EvalTac ts
            | "ASSIGN" -> mkSEQ (_Assignments ts)
            | "WHEN" ->
                let tacs = (transTactic <* ts) in
                let rec okwhen =
                  function
                    [] -> ()
                  | [t] -> ()
                  | t1 :: ts ->
                      if isguard t1 then okwhen ts
                      else _Bad "WHEN must be given guarded tactics"
                in
                okwhen tacs; WhenTac tacs
            | "LETCONC" -> mkBind f patbind "pattern" (fun v->BindConcTac v) ts
            | "LETHYP" -> mkBind f patbind "pattern" (fun v->BindHypTac v) ts
            | "LETHYP2" ->
                mkBind2 f patbind patbind "pattern" "pattern" (fun v->BindHyp2Tac v) ts
            | "LETHYPS" -> mkBind f patbind "pattern" (fun v->BindHypsTac v) ts
            | "LETLHS" -> mkBind f patbind "pattern" (fun v->BindLHSTac v) ts
            | "LETRHS" -> mkBind f patbind "pattern" (fun v->BindRHSTac v) ts
            | "LETGOAL" -> mkBind f patbind "pattern" (fun v->BindGoalTac v) ts
            | "LETGOALPATH" -> mkBind f namebind "name" (fun v->BindGoalPathTac v) ts
            | "LETOPENSUBGOAL" ->
                mkBind2 f namebind patbind "name" "pattern" (fun v->BindOpenSubGoalTac v)
                  ts
            | "LETOPENSUBGOALS" ->
                mkBind f patbind "pattern" (fun v->BindOpenSubGoalsTac v) ts
            | "LETARGSEL" -> mkBind f patbind "pattern" (fun v->BindArgTac v) ts
            | "LETARGTEXT" -> mkBind f namebind "name" (fun v->BindArgTextTac v) ts
            | "LETSUBSTSEL" -> mkBind f patbind "pattern" (fun v->BindSubstTac v) ts
            | "LETHYPSUBSTSEL" ->
                mkBind f patbind "pattern" (fun v->BindSubstInHypTac v) ts
            | "LETCONCSUBSTSEL" ->
                mkBind f patbind "pattern" (fun v->BindSubstInConcTac v) ts
            | "LETMULTIARG" -> mkBind f patbind "pattern" (fun v->BindMultiArgTac v) ts
            | "LETHYPFIND" -> mkBind f patbind "pattern" (fun v->BindFindHypTac v) ts
            | "LETCONCFIND" -> mkBind f patbind "pattern" (fun v->BindFindConcTac v) ts
            | "LETMATCH" -> mkMatch ts
            | "LETOCCURS" -> mkOccurs ts
            | "LAYOUT" -> mkLayout ts
            | "MATCH" -> MatchTac (_SEQ1TAC f ts)
            | "SAMEPROVISOS" -> SameProvisosTac (_SEQ1TAC f ts)
            | "SIMPLEAPPLY" -> SimpleApplyTac (_SEQ1TAC f ts)
            | "APPLYORRESOLVE" -> ApplyOrResolveTac (_SEQ1TAC f ts)
            | "UNIQUE" -> UniqueTac (_SEQ1TAC f ts)
            | "ANY" -> TakeAnyTac (_SEQ1TAC f ts)
            | "UNIFY" -> UnifyTac (atleasttwo ts)
            | "RESOLVE" -> ResolveTac (_SEQ1TAC f ts)
            | "REPLAY" -> ReplayTac (_SEQ1TAC f ts)
            | "WITHCONTINUATION" ->
                begin match ts with
                  t1 :: ts -> ContnTac (transTactic t1, _SEQTAC ts)
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
                mkBind2 f namebind namebind "name" "name" (fun v->BadUnifyTac v) ts
            | "BADMATCH" ->
                mkBind2 f namebind namebind "name" "name" (fun v->BadMatchTac v) ts
            | "BADPROVISO" ->
                mkBind3 f namebind namebind namebind "name" "name" "name"
                  (fun v->BadProvisoTac v) ts
            | _ ->
                if tacticform n then
                  raise (Catastrophe_ ["unrecognised tactic "; f])
                else TermTac parts
      with
        TacParseError_ ss ->
          raise
            (ParseError_
               (["_Bad tactic: ("; termstring tacterm; ") -- "] @ ss))
      | ParseError_ ss -> raise (ParseError_ ss)
      | Catastrophe_ ss -> raise (Catastrophe_ ss)
      | exn ->
          raise
            (ParseError_
               ["Unexpected exception in transTactic: "; Printexc.to_string exn])
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
      LayoutTac (_SEQTAC ts, con (fmt, bopt))
    let tacname t =
      try tacname t with
        TacParseError_ _ ->
          raise (ParseError_ ["not a string or an identifier"])
  end






