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

(* don't open this freely.  It's meant to be a bit abstract, but it's
 * here because some people need to see it.
 * RB
 *)

type term = Termtype.term
 and seq  = Seqtype.seq
 and name = Nametype.name
 and treelayout = Treelayout.treelayout
 
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
  | Tac_of_BindHyp of (term * term * tactic)
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
