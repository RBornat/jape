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

RULE "arith_index" IS
     FROM E∧(F defined) simplifiesto G AND 
          G∧0≤F equivto H             AND
          H∧F<length(a) equivto I
    INFER E∧(a[F] defined) simplifiesto I

TACTIC simpl IS
  SEQ (LAYOUT HIDEROOT)
      (ALT (LETGOAL (_E∧(_x defined) simplifiesto _F)
              (UNIFY _F _E) (MATCH "arith_var"))
           (LETGOAL (_E∧(_K defined) simplifiesto _F)
              (UNIFY _F _E) (MATCH "arith_const"))
          (SEQ (MATCH "arith_index") simpl equiv equiv)
          (SEQ (MATCH "arith_single") simpl)
          (SEQ (MATCH "arith_double") simpl simpl)
          (SEQ (MATCH "arith_div") simpl simpl equiv))

TACTIC equiv IS 
  SEQ (LAYOUT HIDEROOT (ALT (SEQ "arith_dup" conjoinstac) SKIP))
      (LAYOUT HIDEROOT (ALT "true_equiv" "equiv_default"))

RULE "arith_dup" IS 
   FROM E conjoins F AND E equivto G 
  INFER E∧F equivto G

DERIVED RULE "arith_true" IS true∧A equivto A
DERIVED RULE "equiv_default" IS A equivto A

TACTIC "true_equiv" IS
  (LETGOAL (true∧_A equivto _B) (UNIFY _B _A) (MATCH "arith_true"))
    
TACTIC maybetrueimpl IS
  ALT (SEQ (LAYOUT HIDEROOT (MATCH "→ intro")) (LAYOUT HIDEROOT (MATCH "faith")))
      SKIP

/* we can simplify length assertions */

TACTIC "length_simpl"(a,E,F) IS
  LAYOUT HIDEROOT ("arith_length" a E F)

RULE "arith_length"(a,E,F,OBJECT x) IS
   FROM A«length(a)/x» simplifiesto B
  INFER A«length(a⊕E↦F)/x» simplifiesto B

/* we can get rid of duplicate obligations */

RULES "arith_conjoins0" ARE
    INFER E∧F conjoins F
AND INFER F conjoins F
END

RULE "arith_conjoinsL" IS
  FROM E conjoins G INFER E∧F conjoins G
RULE "arith_conjoinsR" IS
  FROM F conjoins G INFER E∧F conjoins G

TACTIC conjoinstac IS
  LAYOUT HIDEROOT
    (ALT (MATCH "arith_conjoins0")
         (SEQ (MATCH "arith_conjoinsL") conjoinstac)
         (SEQ (MATCH "arith_conjoinsR") conjoinstac))
