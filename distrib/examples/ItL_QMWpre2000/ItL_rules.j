/* $Id$ */

/* these rules are stated without an explicit left-context Γ. This makes them more friendly
 * to the innocent natural-deductionist (:-)), or so we hope. The initialisation of autoAdditiveLeft
 * in ItL_syntax.j is what does the magic.
 */
 
TACTIC Fail(x) IS (SEQ (ALERT x) FAIL)

RULE cut(B) IS FROM B AND B ⊢ C INFER C
RULE thin(A) IS FROM C INFER A ⊢ C

RULE "→-E"(A)			IS FROM A AND A→B INFER B
RULE "∧-E(L)"(B)		IS FROM A ∧ B INFER A
RULE "∧-E(R)"(A)		IS FROM A ∧ B INFER B
RULE "∨-E"(A,B)		IS FROM A ∨ B AND A ⊢ C AND B ⊢ C INFER C
RULE "¬-E"			IS FROM ¬¬A INFER A
RULE "∀-E"(c)			IS FROM ∀x. A(x) AND c inscope INFER A(c)
RULE "∃-E"(OBJECT c) WHERE FRESH c AND c NOTIN ∃x.A(x)
					IS FROM ∃x.A(x) AND var c, A(c) ⊢ C INFER C

RULE "→-I"				IS FROM A ⊢ B INFER A→B
RULE "∧-I"				IS FROM A AND B INFER A ∧ B
RULE "∨-I(L)"(B)		IS FROM A INFER A ∨ B
RULE "∨-I(R)"(A)		IS FROM B INFER A ∨ B
RULE "¬-I"(B)			IS FROM A ⊢ B ∧ ¬B INFER ¬A
RULE "∀-I"(OBJECT c) WHERE FRESH c
					IS FROM var c ⊢ A(c) INFER ∀x .A(x)
RULE "∃-I"(c)			IS FROM A(c) AND c inscope INFER ∃x.A(x)

RULE hyp(A) IS INFER A ⊢ A
AUTOMATCH hyp

STRUCTURERULE IDENTITY    hyp
STRUCTURERULE CUT            cut
STRUCTURERULE WEAKEN     thin

RULE "inscope" IS INFER var x ⊢ x inscope
AUTOMATCH "inscope"

