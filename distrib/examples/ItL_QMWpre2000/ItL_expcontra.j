/* $Id$ */

/* these rules are stated without an explicit left-context ‚. This makes them more friendly
 * to the innocent natural-deductionist (:-)), or so we hope. The initialisation of autoAdditiveLeft
 * in ItL_syntax.j is what does the magic.
 */
 
TACTIC FAIL(x) IS JAPE (fail x)

RULE cut(B) IS FROM B AND B æ C INFER C
RULE thin(A) IS FROM C INFER A æ C

RULE "ç-E"(A)			IS FROM A AND AçB INFER B
RULE "¦-E(L)"(B)		IS FROM A ¦ B INFER A
RULE "¦-E(R)"(A)		IS FROM A ¦ B INFER B
RULE "ë-E"(A,B)		IS FROM A ë B AND A æ C AND B æ C INFER C
RULE "Â-E"			IS FROM ÂÂA INFER A
RULE "è-E"(c)			IS FROM èx. A(x) AND c inscope INFER A(c)
RULE "ä-E"(OBJECT c) WHERE FRESH c AND c NOTIN äx.A(x)
					IS FROM äx.A(x) AND var c, A(c) æ C INFER C

RULE "ç-I"				IS FROM A æ B INFER AçB
RULE "¦-I"				IS FROM A AND B INFER A ¦ B
RULE "ë-I(L)"(B)		IS FROM A INFER A ë B
RULE "ë-I(R)"(A)		IS FROM B INFER A ë B
RULE "Â-I"(B)			IS FROM A æ B ¦ ÂB INFER ÂA
RULE "è-I"(OBJECT c) WHERE FRESH c
					IS FROM var c æ A(c) INFER èx .A(x)
RULE "ä-I"(c)			IS FROM A(c) AND c inscope INFER äx.A(x)

RULE hyp(A) IS INFER A æ A
AUTOMATCH hyp

STRUCTURERULE IDENTITY    hyp
STRUCTURERULE CUT            cut
STRUCTURERULE WEAKEN     thin

RULE "inscope" IS INFER var x æ x inscope
AUTOMATCH "inscope"

