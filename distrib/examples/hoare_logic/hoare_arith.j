/*
    $Id$

    Copyright (C) 2004 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the jape examples distribution, which is part of jape.

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

RULE "arith_var" IS INFER A∧(x defined) simplifiesto A
RULE "arith_const" IS INFER A∧(K defined) simplifiesto A

RULES "arith_single" ARE
    FROM E∧(A defined) simplifiesto F
    INFER E∧(¬A defined) simplifiesto F
END

RULES "arith_double" ARE
    FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A → B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A ↔ B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A ∨ B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A ∧ B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A < B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A > B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A ≤ B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A ≥ B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A ≠ B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A = B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A + B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A - B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A × B defined) simplifiesto G
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G 
    INFER E∧(A ↑ B defined) simplifiesto G
END

RULES "arith_div" ARE
    FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G AND 
         G∧B≠0 equivto H
    INFER E∧(A ÷ B defined) simplifiesto H
AND FROM E∧(A defined) simplifiesto F AND 
         F∧(B defined) simplifiesto G AND 
         G∧B≠0 equivto H
    INFER E∧(A mod B defined) simplifiesto H
END

/* to do: A[B] defined */

TACTIC simpl IS
  /* we must stop ourselves matching an unknown, but still do the job */
  WHEN (LETGOAL (_E∧(_x defined) simplifiesto _F)
                /* matches variable; if it also matches constant don't do it */
                (WHEN (LETGOAL (_E∧(_K defined) simplifiesto _F) fstep)
                      (LAYOUT HIDEROOT "arith_var")))
       (ALT (LAYOUT HIDEROOT "arith_const")
            (LAYOUT HIDEROOT (MATCH "arith_single") simpl)
            (LAYOUT HIDEROOT (MATCH "arith_double") simpl simpl)
            (LAYOUT HIDEROOT (MATCH "arith_div") simpl simpl equiv))

DERIVED RULE equiv1 IS true∧A equivto A
DERIVED RULE equiv2 IS A equivto A

TACTIC equiv IS
  LAYOUT HIDEROOT
    /* again, don't let unknowns fool you */
    (WHEN (LETGOAL (true∧_A equivto _B)
                   (WHEN (LETGOAL (false∧_A equivto _B) equiv2)
                         equiv1))
          equiv2)
    
TACTIC maybetrueimpl IS
  ALT (SEQ (LAYOUT HIDEROOT (MATCH "→ intro")) (LAYOUT HIDEROOT (MATCH "faith")))
      SKIP