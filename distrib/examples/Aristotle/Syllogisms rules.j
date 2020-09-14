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

RULE hyp IS INFER A ⊢ A
RULE cut(B) IS FROM B AND B ⊢ C INFER C

STRUCTURERULE IDENTITY   hyp
STRUCTURERULE CUT        cut

TACTIC Fail (x) IS SEQ (ALERT x) STOP

TACTIC ForwardCut (n,rule) /* only applied with single hypothesis selection */
  CUTIN   
    (LETGOALPATH G
      rule
      (GOALPATH (SUBGOAL G n))
      (WITHHYPSEL hyp)
    )

TACTIC ForwardOrBackward (Forward, n, rule) IS 
  WHEN (LETHYP _A 
          (WHEN (LETCONC _B (WITHSELECTIONS rule) (WITHHYPSEL hyp))
                (Forward n rule)))
       (WITHSELECTIONS rule)
    

MACRO "Syllogism-step" (rule, hpat1, hpat2, mpat) IS
  SEQ (rule mpat) (WITHHYPSEL (hyp (hpat1))) (WITHHYPSEL (hyp (hpat2)))
  
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
          (WHEN (LETHYP hpat1 (rule mpat) (WITHHYPSEL (hyp (hpat1))))
                (LETHYP hpat2 (rule mpat) SKIP (WITHHYPSEL (hyp (hpat2))))
                (LETHYP _A (Fail ("%t is not applicable to the goal %t and hypothesis %t", rule, cpat, _A)))
                rule
          )
       )
       (LETGOAL _A (Fail ("%t does not infer the goal %t", rule, _A)))
       (Fail "can't happen: Syllogism-tac out of alternatives")
       
