/* $Id$ */

/* these rules are stated without an explicit left-context Γ. This makes them more friendly
 * to the innocent natural-deductionist (:-)), or so we hope. The initialisation of autoAdditiveLeft
 * in I2L_syntax.j is what does the magic.
 */
 
TACTIC Fail (x) IS SEQ (ALERT x) STOP

RULE cut(B) IS FROM B AND B ⊢ C INFER C
RULE thin(A) IS FROM C INFER A ⊢ C

RULE "→ elim"               IS FROM A→B AND A INFER B
RULE "∧ elim(L)"            IS FROM A ∧ B INFER A
RULE "∧ elim(R)"            IS FROM A ∧ B INFER B
RULE "∨ elim"               IS FROM A ∨ B AND A ⊢ C AND B ⊢ C INFER C
RULE "∀ elim"               IS FROM ∀x. P(x) AND actual i INFER P(i)
RULE "∃ elim"(OBJECT i) WHERE FRESH i AND i NOTIN ∃x.P(x)
                        IS FROM ∃x.P(x) AND actual i, P(i) ⊢ C INFER C

RULE "→ intro"              IS FROM A ⊢ B INFER A→B
RULE "∧ intro"              IS FROM A AND B INFER A ∧ B
RULE "∨ intro(L)"(B)        IS FROM A INFER A ∨ B
RULE "∨ intro(R)"(B)        IS FROM A INFER B ∨ A
RULE "¬ intro"              IS FROM A ⊢ ⊥ INFER ¬A
RULE "¬ elim"(B)            IS FROM B AND ¬B INFER ⊥
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

/* no longer used
RULE "inscope"(x) IS INFER actual x ⊢ x inscope
AUTOMATCH "inscope"
 */

