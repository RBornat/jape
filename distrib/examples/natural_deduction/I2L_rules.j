/* $Id$ */

/* these rules are stated without an explicit left-context ‚. This makes them more friendly
 * to the innocent natural-deductionist (:-)), or so we hope. The initialisation of autoAdditiveLeft
 * in I2L_syntax.j is what does the magic.
 */
 
TACTIC Fail (x) IS SEQ (ALERT x) STOP

RULE cut(B) IS FROM B AND B æ C INFER C
RULE thin(A) IS FROM C INFER A æ C

RULE "ç elim"				IS FROM AçB AND A INFER B
RULE "¦ elim(L)"			IS FROM A ¦ B INFER A
RULE "¦ elim(R)"			IS FROM A ¦ B INFER B
RULE "ë elim"				IS FROM A ë B AND A æ C AND B æ C INFER C
RULE "Ù elim (classical)"	IS FROM ÂA æ Ù INFER A
RULE "Ù elim (constructive)"	
								IS FROM Ù INFER B
RULE "è elim"(c)			IS FROM èx. A(x) AND c inscope INFER A(c)
RULE "ä elim"(OBJECT c) WHERE FRESH c AND c NOTIN äx.A(x)
								IS FROM äx.A(x) AND actual c, A(c) æ C INFER C

RULE "ç intro"					IS FROM A æ B INFER AçB
RULE "¦ intro"					IS FROM A AND B INFER A ¦ B
RULE "ë intro(L)"(B)		IS FROM A INFER A ë B
RULE "ë intro(R)"(B)		IS FROM A INFER B ë A
RULE "Â intro"					IS FROM A æ Ù INFER ÂA
RULE "Ù intro"(B)			IS FROM B AND ÂB INFER Ù
RULE "è intro"(OBJECT c) WHERE FRESH c
								IS FROM actual c æ A(c) INFER èx .A(x)
RULE "ä intro"(c)				IS FROM A(c) AND c inscope INFER äx.A(x)

RULE hyp(A) IS INFER A æ A
AUTOMATCH hyp

IDENTITY	hyp
CUT			cut
WEAKEN	thin

RULE "inscope"(x) IS INFER actual x æ x inscope
AUTOMATCH "inscope"

