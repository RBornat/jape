/*
    Copyright (C) 2020 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the Aristotleian deductive logic encoding, distributed with jape.

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

*/

INITIALISE seektipselection false       /* yes, we are going to use CUTIN */

RULE see IS INFER A ⊢ A
RULE cut(B) IS FROM B AND B ⊢ C INFER C

STRUCTURERULE IDENTITY   see
STRUCTURERULE CUT        cut

TACTIC Fail (x) IS SEQ (ALERT x) STOP

/* single-antecedent rules with a selected conclusion must be applied backwards using
   CUTIN, so that the list-antecedents-before-conclusion trick of Aristotle still works.
   That's what furdle is all about
 */

TACTIC furdle (rule) IS
  LETGOAL _A
    (CUTIN (LETGOAL _B (UNIFY _A _B) (WITHHYPSEL rule)))
    (ANY (MATCH see))
    
TACTIC ForwardCut (n,rule) /* only applied with single hypothesis selection */
  CUTIN   
    (LETGOALPATH G
      rule
      (GOALPATH (SUBGOAL G n))
      (WITHHYPSEL see)
    )

TACTIC ForwardOrBackward (Forward, n, rule) IS 
  WHEN (LETHYP _A 
          (WHEN (LETCONC _B (furdle rule))
                (Forward n rule)))
       (furdle rule)
    

MACRO "Syllogism-step" (rule, hpat1, hpat2, mpat) IS
  SEQ (rule mpat) (WITHHYPSEL (see (hpat1))) (WITHHYPSEL (see (hpat2)))
  
MACRO "Syllogism-tac" (rule, hpat1, hpat2, mpat, cpat) IS
  WHEN (LETHYP2 hpat1 hpat2
          (WHEN (LETCONC _A ("Syllogism-step" rule hpat1 hpat2 mpat))
                (CUTIN ("Syllogism-step" rule hpat1 hpat2 mpat))
          )
       )
       (LETHYP2 _A _B
          (Fail ("%t is not applicable to the hypotheses %t and %t", rule, _A, _B))
       )
       (LETGOAL cpat
          (WHEN (LETHYP hpat1 (rule mpat) (WITHHYPSEL (see (hpat1))))
                (LETHYP hpat2 (rule mpat) SKIP (WITHHYPSEL (see (hpat2))))
                (LETHYP _A (Fail ("%t is not applicable to the conclusion %t and hypothesis %t", rule, cpat, _A)))
                rule
          )
       )
       (LETGOAL _A (Fail ("%t does not infer the conclusion %t", rule, _A)))       
