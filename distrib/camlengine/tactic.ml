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

(* some of this stuff is specific to natural deduction
  (use of 'seq' type is evidence).
  Tut tut. RB

  [[Actually it's all crap, and we need a proper MLish tactic language. BS]]
 *)

open Tactictype

type tactic = Tactictype.tactic
    
open Idclass
open Listfuns
open Match
open Miscellaneous
open Nametype
open Name
open Optionfuns
open Sml
open Sequent
open Stringfuns
open Termfuns
open Termstring
open Termtype
open Termstore
open Treelayout    

(*
    TACTIC TRANSLATION
*)

let showargasint : (term -> int) option ref = ref None
let readintasarg : term array option ref = ref None
let stripextrabag = ref false

let rec catelim_string_of_tactic sep t tail =
  let withNLs = String.sub sep 0 1 = "\n" in
  let nextsep = if withNLs then sep ^ "\t" else sep in
  let oneline t =
    match t with
      SkipTac        -> true
    | UnfoldHypTac _ -> true
    | FoldHypTac   _ -> true
    | AssignTac    _ -> true
    | EvalTac      _ -> true
    | AdHocTac     _ -> true
    | AssocFlatTac _ -> true
    | MapTac       _ -> true
    | TermTac      _ -> true
    | SubstTac     _ -> true
    | _              -> false
  in
  let sepstring_of_tacarg = catelimstring_of_tacarg nextsep in
  let argsep = if oneline t then " " else nextsep in
  let tr f = catelim_interpolate f in
  let trtacs = tr sepstring_of_tacarg nextsep in
  let trterms = tr catelim_string_of_termarg " " in
  let rec unSEQ =
    function
      SeqTac [t] -> unSEQ t
    | SeqTac ts  -> trtacs ts
    | t          -> sepstring_of_tacarg t
  in
  let rec trpathexpr p tail =
    let trns ns tail =
      match debracket ns with
        Tup _ as t -> "(" :: catelim_string_of_term t (")" :: tail)
      | t -> catelim_string_of_term t tail
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
  let pnstr = parseablestring_of_name in
  let rec termsNtac ts tac =
    match ts with
      []      -> unSEQ tac tail
    | t :: ts -> catelim_string_of_termarg t (argsep :: termsNtac ts tac)
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
          catelim_string_of_list (catelim_of_stringfn pnstr) " " ns tail
    | AltTac tacs -> "ALT" :: nextsep :: trtacs tacs tail
    | SeqTac [t] -> catelim_string_of_tactic sep t tail
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
        "ASSIGN " :: pnstr s :: " " :: catelim_string_of_termarg term tail
    | EvalTac terms -> "EVALUATE " :: trterms terms tail
    | AdHocTac terms -> "JAPE " :: trterms terms tail
    | BindArgTac (term, tactic) -> "LETARGSEL " :: termsNtac [term] tactic
    | BindArgTextTac (name, tactic) ->
        "LETARGTEXT " :: parseablestring_of_name name :: argsep ::
          unSEQ tactic tail
    | BindConcTac (term, tactic) -> "LETCONC " :: termsNtac [term] tactic
    | BindGoalPathTac (name, tactic) ->
        "LETGOALPATH " :: parseablestring_of_name name :: argsep ::
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
        "LETOPENSUBGOAL " :: parseablestring_of_name name :: argsep ::
          termsNtac [term] tactic
    | BindOpenSubGoalsTac (term, tactic) ->
        "LETOPENSUBGOALS " :: termsNtac [term] tactic
    | BindFindHypTac (term, tactic) ->
        "LETHYPFIND " :: termsNtac [term] tactic
    | BindFindConcTac (term, tactic) ->
        "LETCONCFIND " :: termsNtac [term] tactic
    | BindUnifyTac (pterm, vterm, tactic) ->
        "LETUNIFY " :: termsNtac [pterm; vterm] tactic
    | BindTuplistTac (carterm, cdrterm, tupterm, tactic) ->
        "LETTUPLE " :: termsNtac [carterm; cdrterm; tupterm] tactic
    | BindOccursTac (pt, vt, st, tactic) ->
        "LETOCCURS " :: termsNtac [pt; vt; st] tactic
    | LayoutTac (tactic, layout) ->
        "LAYOUT " :: string_of_treelayout layout :: argsep ::
          unSEQ tactic tail
    | AssocFlatTac term -> "FLATTEN " :: catelim_string_of_termarg term tail
    | MapTac (s, terms) ->
        "MAPTERMS " :: pnstr s :: " " :: trterms terms tail
    | TermTac (s, []) -> pnstr s :: tail
    | TermTac (s, terms) -> pnstr s :: " " :: trterms terms tail
    | SubstTac (s, []) -> pnstr s :: tail
    | SubstTac (s, vts) ->
        catelim_string_of_term
          (Subst
             (None, true, Id (None, vid_of_string (pnstr s), FormulaClass),
              (match !showargasint with
                 Some lookup ->
                   let rec encodeterm t =
                     match t with
                       Collection (_, c, es) ->
                         Collection (None, c, (encodeelement <* es))
                     | _ ->
                         match hashterm t with
                           Some h -> Literal (None, Number (lookup t))
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
    | GivenTac i -> "GIVEN " :: catelim_string_of_termarg i tail
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
          sepstring_of_tacarg tac1 (argsep :: unSEQ tac2 tail)
    | AlertTac (m, ps) ->
        "ALERT" :: argsep ::
          catelim_string_of_termarg m
            (catelim_string_of_list
               (catelim_string_of_pair catelim_string_of_term
                  (catelim_string_of_tactic " ") ", ")
               argsep ps tail)
    | ShowHowToTac s -> "SHOWHOWTO" :: argsep :: catelim_string_of_termarg s tail
    | ExplainTac m -> "EXPLAIN" :: argsep :: catelim_string_of_termarg m tail
    | CommentTac m -> "COMMENT" :: argsep :: catelim_string_of_termarg m tail
    | UnifyTac terms -> "UNIFY" :: argsep :: trterms terms tail
    | BadUnifyTac (n1, n2, tac) ->
        "BADUNIFY" :: argsep :: parseablestring_of_name n1 :: argsep ::
          parseablestring_of_name n2 :: unSEQ tac tail
    | BadMatchTac (n1, n2, tac) ->
        "BADMATCH" :: argsep :: parseablestring_of_name n1 :: argsep ::
          parseablestring_of_name n2 :: unSEQ tac tail
    | BadProvisoTac (n1, n2, n3, tac) ->
        "BADPROVISO" :: argsep :: parseablestring_of_name n1 :: argsep ::
          parseablestring_of_name n2 :: parseablestring_of_name n3 ::
          unSEQ tac tail
    | UnifyArgsTac -> "UNIFYARGS" :: tail
  in
  tss
and catelimstring_of_tacarg sep t tail =
  let ok =
    function
      SkipTac         -> true
    | TermTac (s, []) -> true
    | _               -> false
  in
  let res = catelim_string_of_tactic sep t in
  if ok t then res tail else "(" :: res (")" :: tail)
  
let catelim_string_of_tactic = catelim_string_of_tactic " "
and catelim_stringwithNLs_of_tactic = catelim_string_of_tactic "\n"

let string_of_tactic = stringfn_of_catelim catelim_string_of_tactic
let argstring_of_tactic = stringfn_of_catelim (catelimstring_of_tacarg " ") 
let stringwithNLs_of_tactic = stringfn_of_catelim catelim_stringwithNLs_of_tactic

let remaptactic env t =
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
    | BindTuplistTac (carpat, cdrpat, expr, t) -> BindTuplistTac (_E carpat, _E cdrpat, _E expr, _T t)
    | BindUnifyTac (ptm, vtm, t) -> BindUnifyTac (_E ptm, _E vtm, _T t)
    | BindOccursTac (pt, vt, st, t) ->
        BindOccursTac (_E pt, _E vt, _E st, _T t)
    | LayoutTac (t, tl) -> LayoutTac (_T t, remaptreelayout env tl)
    | AlertTac (m, ps)  -> AlertTac (_E m, ((fun (l, t) -> _E l, _T t) <* ps))
    | ShowHowToTac s  -> ShowHowToTac (_E s)
    | ExplainTac m -> ExplainTac (_E m)
    | CommentTac m -> CommentTac (_E m)
    | BadUnifyTac (n1, n2, t) -> BadUnifyTac (n1, n2, _T t)
    | BadMatchTac (n1, n2, t) -> BadMatchTac (n1, n2, _T t)
    | BadProvisoTac (n1, n2, n3, t) -> BadProvisoTac (n1, n2, n3, _T t)
    | UnifyArgsTac -> UnifyArgsTac
  in
  _T t

let tacticform i =
  member
    (string_of_name i,
     ["ANY"; "ALERT"; "ALT"; "APPLYORRESOLVE"; "ASSIGN"; "CUTIN"; "DO";
      "EVALUATE"; "EXPLAIN"; "EXPLICIT"; "FAIL"; "FLATTEN"; "FOLD";
      "FOLDHYP"; "GIVEN"; "GOALPATH"; "IF"; "IMPLICIT"; "JAPE"; "LAYOUT";
      "LETARGSEL"; "LETCONC"; "LETCONCFIND"; "LETCONCSUBSTSEL"; "LETGOAL";
      "LETGOALPATH"; "LETOPENSUBGOAL"; "LETOPENSUBGOALS"; "LETHYP";
      "LETHYP2"; "LETHYPS"; "LETHYPFIND"; "LETHYPSUBSTSEL"; "LETTUPLE"; 
      "LETUNIFY"; "LETMULTIARG"; "LETOCCURS"; "LETSUBSTSEL"; "MAPTERMS"; 
      "MATCH"; "NEXTGOAL"; "PROVE"; "REPLAY"; "RESOLVE"; "SAMEPROVISOS"; 
      "SEQ"; "SHOWHOWTO"; "SIMPLEAPPLY"; "SKIP"; "STOP"; "THEORYALT"; "UNFOLD"; 
      "UNFOLDHYP"; "UNIFY"; "UNIQUE"; "WHEN"; "WITHARGSEL"; "WITHCONCSEL";
      "WITHCONTINUATION"; "WITHFORMSEL"; "WITHHYPSEL"; "WITHSELECTIONS";
      "WITHSUBSTSEL"; "BADUNIFY"; "BADMATCH"; "BADPROVISO"; "UNIFYARGS"])

exception TacParseError_ of string list

let mustbeSTR term =
  match debracket term with
    Literal (_, String s) -> s
  | Id (_, v, _) -> string_of_vid v
  | _ -> raise (TacParseError_ [string_of_termarg term; " should be a string"])

let maybeSTR term =
  try Some (mustbeSTR term) with
    _ -> None

let tacname term =
  try Name (mustbeSTR term) with
    TacParseError_ _ ->
      raise (TacParseError_ [string_of_termarg term; " should be a name"])

let butnottacticform term =
  let name = tacname term in
  if tacticform name then
    raise (TacParseError_ [string_of_name name; " used as tactic name"])
  else name

(* all tactic applications MUST be Curried.  RB & BS 13/8/93.
 * Outermost brackets are stripped from non-tuple arguments. RB
 *)

let explodeForExecute t =
  let (f, rs) = explodeApp false t in tacname f, rs

let badLayout () =
  raise
    (TacParseError_
       ["Syntax is LAYOUT fmt tactic; \
        fmt can be HIDEROOT, or HIDECUT, or COMPRESS label subtrees, or just label subtrees; \
        label can be an str or a non-empty bracketed tuple of strs where \
        an str can be a string, an identifier, an unknown or a number;
        subtrees must be a bracketed tuple of numbers or the word ALL. \n\n\
        It's possible to stop after label, in which case ALL is assumed; \
        it's possible to stop after COMPRESS, in which case \"%s\" ALL is assumed."])

let checkNAME errf t =
  (* suitable for a place where a single name will do *)
  match debracket t with
  | Id      _ as t -> t
  | Unknown _ as t -> t
  | t              -> raise (TacParseError_ (errf t))

let checkSTR errf t =
  match debracket t with
  | Id      _ as t -> t
  | Unknown _ as t -> t
  | Literal _ as t -> t
  | t              -> raise (TacParseError_ (errf t))

let checkINT errf t =
  match debracket t with
  | Id                  _ as t -> t
  | Unknown             _ as t -> t
  | Literal (_, Number _) as t -> t
  | _                          -> raise (TacParseError_ (errf t))

let checkINTS errf t =
  match debracket t with
  | Tup (_, ",", ts) as t' -> let _ = List.map (checkINT errf) ts in t'
  | t                      -> checkINT errf t

let isguard =
  function
  | BindConcTac         _ -> true
  | BindHypTac          _ -> true
  | BindHyp2Tac         _ -> true
  | BindHypsTac         _ -> true
  | BindArgTac          _ -> true
  | BindArgTextTac      _ -> true
  | BindGoalPathTac     _ -> true
  | BindSubstTac        _ -> true
  | BindSubstInHypTac   _ -> true
  | BindSubstInConcTac  _ -> true
  | BindMultiArgTac     _ -> true
  | BindLHSTac          _ -> true
  | BindRHSTac          _ -> true
  | BindGoalTac         _ -> true
  | BindOpenSubGoalTac  _ -> true
  | BindOpenSubGoalsTac _ -> true
  | BindFindHypTac      _ -> true
  | BindFindConcTac     _ -> true
  | BindUnifyTac        _ -> true
  | BindOccursTac       _ -> true
  | BindTuplistTac      _ -> true
  | BadUnifyTac         _ -> true
  | BadMatchTac         _ -> true
  | BadProvisoTac       _ -> true
  | _                     -> false

let rec mkSEQ =
  function
    [] -> SkipTac
  | [t] -> t
  | ts -> SeqTac ts

and _SEQTAC ts = mkSEQ (transTactic <* ts)

and _SEQ1TAC a1 a2 =
  match a1, a2 with
    name, [] ->
      raise
        (TacParseError_
           [name; " tactic must include a non-empty sequence of tactics"])
  | name, ts -> _SEQTAC ts

and transTactic tacterm =
  try match debracket tacterm with
        Subst (_, _, _P, vts) ->
          let vts =
            (match !readintasarg with
               Some lookup ->
                 let rec decodeterm t =
                   match t with
                     Literal (_, Number i) ->
                       (try Array.get lookup i with
                        _ -> raise (Catastrophe_ ["transTactic and readintasarg see "; string_of_int i]))
                  | Collection (_, c, es) -> registerCollection (c, (decodeelement <* es))
                  | _ -> t
                 and decodeelement =
                   function
                     Element (_, i, t) -> registerElement (i, decodeterm t)
                   | el                -> el
                 in
                 ((fun (v, t) -> v, decodeterm t) <* vts)
             | None -> vts)
          in
          (* this is the second part of a pair of TEMPORARY hacks.  See prooftree.sml for a more extensive
             explanation, but basically it's a good idea to strip out BAG FORMULA arguments to speed up
             saved-proof replay.
           *)
          let vts = 
            if !stripextrabag then
              match split (function (_,Collection _) -> true | _ -> false) vts with
                ([_], oks) -> (* if not (exists (existsterm isUnknown) (#2 MAP vts)) then oks else vts *)
                              oks (* it can't be slower, even if it contains unknowns *)
              | _          -> vts
            else vts
          in
          let _P = butnottacticform _P in
          (match vts with 
             [] -> TermTac (_P, [])
           | _  -> SubstTac (_P, vts))
      | t ->
              let (n, ts as parts) = explodeForExecute t in
              let f = string_of_name n in
              let _Bad why = raise (TacParseError_ [string_of_term t; " -- "; why]) in
              let rec _Assignments =
                function
                  []           -> []
                | s :: t :: ts -> AssignTac (tacname s, debracket t) :: _Assignments ts
                | _            -> _Bad "Assignment malformed"
              in
              let onearg =
                function
                  [t] -> t
                | []  -> _Bad "No argument"
                | _   -> _Bad "More than one argument"
              in
              let atleasttwo =
                function
                  _ :: _ :: _ as ts -> ts
                | ts                -> _Bad "should have at least two arguments"
              in
              let mkBind f bind str tac =
                function
                  x :: ts -> tac (bind x, _SEQTAC ts)
                | []      ->
                    raise (TacParseError_ ["Syntax is ("; f; " <"; str; "> tactics ...)"])
              in
              let mkBind2 f b1 b2 s1 s2 tac =
                function
                  x1 :: x2 :: ts -> tac (b1 x1, b2 x2, _SEQTAC ts)
                | _              ->
                    raise (TacParseError_ ["Syntax is ("; f; " <"; s1; "> <"; s2;
                                          "> tactic ...)"])
              in
              let mkBind3 f b1 b2 b3 s1 s2 s3 tac =
                function
                  x1 :: x2 :: x3 :: ts -> tac (b1 x1, b2 x2, b3 x3, _SEQTAC ts)
                | _                    ->
                    raise (TacParseError_
                             ["Syntax is ("; f; " <"; s1; "> <"; s2; "> <"; s3;
                              "> tactic ...)"])
              in
              let patbind t = t in
              let valbind t = t in
              let namebind n = tacname n in
              let mkLayout =
                function
                | []           -> badLayout ()
                | fmt :: stuff ->
                        let lyt con fmt = 
                          function []       -> transLayout con fmt None []
                          |        ns :: ts -> transLayout con fmt (Some ns) ts
                        in
                        match maybeSTR fmt, stuff with
                        | Some "HIDEROOT", ts        -> LayoutTac (_SEQTAC ts, HideRootLayout)
                        | Some "HIDECUT" , ts        -> LayoutTac (_SEQTAC ts, HideCutLayout)
                        | Some "COMPRESS", fmt :: ts -> lyt (fun v -> CompressedLayout v) fmt ts
                        | Some "COMPRESS", ts        -> lyt (fun v -> CompressedLayout v) (registerLiteral (String "%s")) ts
                        | _              , ts        -> lyt (fun v -> NamedLayout v) fmt ts
              in
              let mkFold tac =
                function
                  n :: ts -> tac (tacname n, (transTactic <* ts))
                | []      ->
                    raise (TacParseError_ ["Syntax is (FOLD/UNFOLD tacticname tactic ...)"])
              in
              let mkHypFold tac =
                function
                  n :: ts -> tac (tacname n, ts)
                | []      ->
                    raise (TacParseError_ ["Syntax is (FOLDHYP/UNFOLDHYP tacticname pattern ...)"])
              in
              let mkAlert =
                function
                  m :: ps ->
                    let rec f rs =
                      function
                        Tup (_, ",", [l; t]) :: ps -> f ((l, transTactic t) :: rs) ps
                      | []     -> AlertTac (m, List.rev rs)
                      | t :: _ ->
                          raise (TacParseError_ ["badly formed argument "; string_of_term t])
                    in
                    f [] ps
                | [] -> raise (TacParseError_ ["no arguments"])
              in
              let rec parsegoalexpr t =
                let err _ = ["path expected after GOALPATH -- found "; string_of_term t]
                in
                match explodeApp false t with
                  f, rs ->
                    (match maybeSTR f, rs with
                       Some "PARENT" , [r]     -> Parent (parsegoalexpr r)
                     | Some "LEFT"   , [r]     -> LeftSibling (parsegoalexpr r)
                     | Some "RIGHT"  , [r]     -> RightSibling (parsegoalexpr r)
                     | Some "SUBGOAL", [r; ns] -> Subgoal (parsegoalexpr r, checkINTS err (debracket ns))
                     | Some "HYPROOT", [r]     -> HypRoot (parsegoalexpr r)
                     | _             , []      -> SimplePath (checkINTS err (debracket f))
                     | _                       -> raise (TacParseError_ (err t)))
              in
              let nullArgTac t =
                match ts with
                  [] -> t
                | _  -> _Bad (f^" mustn't be given any arguments")
              in
              match f with
                "SKIP"             -> nullArgTac SkipTac
              | "FAIL"             -> nullArgTac FailTac
              | "STOP"             -> nullArgTac StopTac
              | "NEXTGOAL"         -> nullArgTac NextgoalTac
              | "UNIFYARGS"        -> nullArgTac UnifyArgsTac
              | "GOALPATH"         -> SetgoalTac (parsegoalexpr (onearg ts))
              | "PROVE"            -> CompTac (_SEQ1TAC f ts)
              | "CUTIN"            -> CutinTac (_SEQ1TAC f ts)
              | "JAPE"             -> AdHocTac [onearg ts]
              | "FLATTEN"          -> AssocFlatTac (debracket (onearg ts))
              | "MAPTERMS"         -> MapTac (explodeForExecute (onearg ts))
              | "SEQ"              -> _SEQTAC ts
              | "ALT"              -> AltTac ((transTactic <* ts))
              | "THEORYALT"        -> TheoryAltTac ((butnottacticform <* ts))
              | "IF"               -> IfTac (_SEQ1TAC f ts)
              | "DO"               -> RepTac (_SEQ1TAC f ts)
              | "FOLD"             -> mkFold (fun v->FoldTac v) ts
              | "UNFOLD"           -> mkFold (fun v->UnfoldTac v) ts
              | "FOLDHYP"          -> mkHypFold (fun v->FoldHypTac v) ts
              | "UNFOLDHYP"        -> mkHypFold (fun v->UnfoldHypTac v) ts
              | "WITHARGSEL"       -> WithArgSelTac (_SEQ1TAC f ts)
              | "WITHCONCSEL"      -> WithConcSelTac (_SEQ1TAC f ts)
              | "WITHFORMSEL"      -> WithFormSelTac (_SEQ1TAC f ts)
              | "WITHHYPSEL"       -> WithHypSelTac (_SEQ1TAC f ts)
              | "WITHSELECTIONS"   -> WithSelectionsTac (_SEQ1TAC f ts)
              | "WITHSUBSTSEL"     -> WithSubstSelTac (_SEQ1TAC f ts)
              | "EVALUATE"         -> EvalTac ts
              | "ASSIGN"           -> mkSEQ (_Assignments ts)
              | "WHEN"             ->
                      let tacs = (transTactic <* ts) in
                      let rec okwhen =
                        function
                          []       -> ()
                        | [t]      -> ()
                        | t1 :: ts ->
                                if isguard t1 then okwhen ts
                                else _Bad "WHEN must be given guarded tactics"
                      in
                      okwhen tacs; WhenTac tacs
              
              | "LETARGSEL"        -> mkBind f patbind  "pattern" (fun v->BindArgTac v) ts
              | "LETARGTEXT"       -> mkBind f namebind "name"    (fun v->BindArgTextTac v) ts
              | "LETCONC"          -> mkBind f patbind  "pattern" (fun v->BindConcTac v) ts
              | "LETCONCFIND"      -> mkBind f patbind  "pattern" (fun v->BindFindConcTac v) ts
              | "LETCONCSUBSTSEL"  ->
                      mkBind f patbind "pattern" (fun v->BindSubstInConcTac v) ts
              | "LETGOAL"          -> mkBind f patbind  "pattern" (fun v->BindGoalTac v) ts
              | "LETGOALPATH"      -> mkBind f namebind "name"    (fun v->BindGoalPathTac v) ts
              | "LETHYP"           -> mkBind f patbind  "pattern" (fun v->BindHypTac v) ts
              | "LETHYP2"          ->
                      mkBind2 f patbind patbind "pattern" "pattern" (fun v->BindHyp2Tac v) ts
              | "LETHYPS"          -> mkBind f patbind  "pattern" (fun v->BindHypsTac v) ts
              | "LETHYPFIND"       -> mkBind f patbind  "pattern" (fun v->BindFindHypTac v) ts
              | "LETHYPSUBSTSEL"   ->
                      mkBind f patbind "pattern" (fun v->BindSubstInHypTac v) ts
              | "LETLHS"           -> mkBind f patbind  "pattern"  (fun v->BindLHSTac v) ts
              | "LETTUPLE"          -> 
                   mkBind3 f patbind patbind valbind "car-pattern" "cdr-pattern" "expr" (fun v -> BindTuplistTac v) ts
              | "LETUNIFY"         -> 
                   mkBind2 f patbind valbind "pattern" "expr" (fun v -> BindUnifyTac v) ts
              | "LETMULTIARG"      -> mkBind f patbind  "pattern" (fun v->BindMultiArgTac v) ts
              | "LETRHS"           -> mkBind f patbind  "pattern" (fun v->BindRHSTac v) ts
              | "LETOCCURS"        -> 
                   mkBind3 f valbind valbind patbind "subexpr" "expr" "substitution expr" (fun v -> BindOccursTac v) ts
              | "LETOPENSUBGOAL"   ->
                   mkBind2 f namebind patbind "name" "p attern" (fun v->BindOpenSubGoalTac v) ts
              | "LETOPENSUBGOALS"  -> mkBind f patbind  "pattern" (fun v->BindOpenSubGoalsTac v) ts
              | "LETSUBSTSEL"      -> mkBind f patbind  "pattern" (fun v->BindSubstTac v) ts
              
              | "LAYOUT"           -> mkLayout ts
              | "MATCH"            -> MatchTac (_SEQ1TAC f ts)
              | "SAMEPROVISOS"     -> SameProvisosTac (_SEQ1TAC f ts)
              | "SIMPLEAPPLY"      -> SimpleApplyTac (_SEQ1TAC f ts)
              | "APPLYORRESOLVE"   -> ApplyOrResolveTac (_SEQ1TAC f ts)
              | "UNIQUE"           -> UniqueTac (_SEQ1TAC f ts)
              | "ANY"              -> TakeAnyTac (_SEQ1TAC f ts)
              | "UNIFY"            -> UnifyTac (atleasttwo ts)
              | "RESOLVE"          -> ResolveTac (_SEQ1TAC f ts)
              | "REPLAY"           -> ReplayTac (_SEQ1TAC f ts)
              | "WITHCONTINUATION" ->
                      begin match ts with
                        t1 :: ts -> ContnTac (transTactic t1, _SEQTAC ts)
                      | _ -> raise (TacParseError_ ["no argument tactic!"])
                      end
              | "GIVEN"     ->
                      begin match ts with
                        [Literal _ as t] -> GivenTac t
                      | [Id _ as t] -> GivenTac t
                      | _ ->
                              raise
                                (TacParseError_
                                       ["must have exactly one name/number argument!"])
                      end
              | "ALERT"            -> mkAlert ts
              | "SHOWHOWTO"        -> ShowHowToTac (onearg ts)
              | "EXPLAIN"          -> ExplainTac (onearg ts)
              | "COMMENT"          -> CommentTac (onearg ts)
              | "BADUNIFY"         ->
                      mkBind2 f namebind namebind "name" "name" (fun v->BadUnifyTac v) ts
              | "BADMATCH"         ->
                      mkBind2 f namebind namebind "name" "name" (fun v->BadMatchTac v) ts
              | "BADPROVISO"       ->
                      mkBind3 f namebind namebind namebind "name" "name" "name"
                        (fun v->BadProvisoTac v) ts
              | _                  ->
                      if tacticform n then raise (Catastrophe_ ["unrecognised tactic "; f])
                      else TermTac parts
  with
    TacParseError_ ss ->
      raise (ParseError_ (["Bad tactic: ("; string_of_term tacterm; ") -- "] @ ss))
  | ParseError_    ss -> raise (ParseError_ ss)
  | Catastrophe_   ss -> raise (Catastrophe_ ss)
  | exn               ->
      raise (ParseError_ ["Unexpected exception in transTactic: "; Printexc.to_string exn])

and transLayout con fmt bopt ts =
  (* let fmterr t = ["format string expected in LAYOUT; found "; string_of_term t] in
     let fmt = checkSTR fmterr fmt in *)
  let nserr t =
    ["list of subtree indices expected in LAYOUT; found "; string_of_term (_The bopt)]
  in
  let bopt =
    match optf (checkINTS nserr) bopt with
    | Some (Id (_, v, _)) as r -> (match string_of_vid v with
                                   | "ALL" -> None
                                   | _     -> r
                                  )
    | r                        -> r
  in
  LayoutTac (_SEQTAC ts, con (fmt, bopt))
  
let tacname t =
  try tacname t with
    TacParseError_ _ ->
      raise (ParseError_ ["not a string or an identifier"])






