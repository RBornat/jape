/*
    Copyright (C) 2000-8 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the I2L logic encoding, distributed with jape.

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

/* these rules are stated without an explicit left-context Γ. This makes them more friendly
 * to the innocent natural-deductionist (:-)), or so we hope. The initialisation of autoAdditiveLeft
 * in I2L_syntax.j is what does the magic.
 */
 
TACTIC Fail (x) IS SEQ (ALERT x) STOP

RULE cut(B) IS FROM B AND B ⊢ C INFER C
RULE thin(A) IS FROM C INFER A ⊢ C

RULE "→ elim"       IS FROM A→B AND A INFER B
RULE "∧ elim(L)"    IS FROM A ∧ B INFER A
RULE "∧ elim(R)"    IS FROM A ∧ B INFER B
RULE "∨ elim"       IS FROM A ∨ B AND A ⊢ C AND B ⊢ C INFER C
RULE "∀ elim"       IS FROM ∀x. P(x) AND actual i INFER P(i)
RULE "∃ elim"(OBJECT i) WHERE FRESH i AND i NOTIN ∃x.P(x)
                    IS FROM ∃x.P(x) AND actual i, P(i) ⊢ C INFER C

RULE "→ intro"          IS FROM A ⊢ B INFER A→B
RULE "∧ intro"          IS FROM A AND B INFER A ∧ B
RULE "∨ intro(L)"(B)    IS FROM A INFER A ∨ B
RULE "∨ intro(R)"(B)    IS FROM A INFER B ∨ A
RULE "¬ intro"          IS FROM A ⊢ ⊥ INFER ¬A
RULE "¬ elim"(B)        IS FROM B AND ¬B INFER ⊥
RULE "∀ intro"(OBJECT i) WHERE FRESH i
                        IS FROM actual i ⊢ P(i) INFER ∀x .P(x)
RULE "∃ intro"          IS FROM P(i) AND actual i INFER ∃x.P(x)

RULE "contra (classical)"    IS FROM ¬A ⊢ ⊥ INFER A
RULE "contra (constructive)" IS FROM ⊥ INFER B
RULE "truth"                 IS INFER  ⊤

RULE hyp(A) IS INFER A ⊢ A
AUTOMATCH hyp

IDENTITY    hyp
CUT     cut
WEAKEN  thin

