/*
    $Id$

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
    

RULES IP(B) ARE /* WHERE the hypothesis is discharged exactly once ... */
     FROM ∏⁺(S,P) ⊢ B AND B* INFER ∑⁻(S,P)  
 AND FROM ∏⁻(S,P) ⊢ B AND B* INFER ∑⁺(S,P)  
 AND FROM ∑⁺(S,P) ⊢ B AND B* INFER ∏⁻(S,P)  
 AND FROM ∑⁻(S,P) ⊢ B AND B* INFER ∏⁺(S,P) 
END

