/* $Id$ */

/* these rules are stated without an explicit left-context ‚. This makes them more friendly
 * to the innocent natural-deductionist (:-)), or so we hope. The initialisation of autoAdditiveLeft
 * in I2L_syntax.j is what does the magic.
 */
 
TACTIC Fail (x) IS SEQ (ALERT x) STOP

RULE cut(B) IS FROM B AND B æ C INFER C
RULE thin(A) IS FROM C INFER A æ C

RULE "ç elim"			IS FROM AçB AND A INFER B
RULE "¦ elim(L)"			IS FROM A ¦ B INFER A
RULE "¦ elim(R)"			IS FROM A ¦ B INFER B
RULE "ë elim"			IS FROM A ë B AND A æ C AND B æ C INFER C
RULE "contra (classical)"	IS FROM ÂA æ Ù INFER A
RULE "contra (constructive)"	IS FROM Ù INFER B
RULE "è elim"				IS FROM èx. P(x) AND actual i INFER P(i)
RULE "ä elim"(OBJECT i) WHERE FRESH i AND i NOTIN äx.P(x)
						IS FROM äx.P(x) AND actual i, P(i) æ C INFER C

RULE "ç intro"			IS FROM A æ B INFER AçB
RULE "¦ intro"			IS FROM A AND B INFER A ¦ B
RULE "ë intro(L)"(B)		IS FROM A INFER A ë B
RULE "ë intro(R)"(B)		IS FROM A INFER B ë A
RULE "Â intro"			IS FROM A æ Ù INFER ÂA
RULE "Â elim"(B)			IS FROM B AND ÂB INFER Ù
RULE "è intro"(OBJECT i) WHERE FRESH i
					IS FROM actual i æ P(i) INFER èx .P(x)
RULE "ä intro"			IS FROM P(i) AND actual i INFER äx.P(x)

RULE hyp(A) IS INFER A æ A
AUTOMATCH hyp

IDENTITY	hyp
CUT		cut
WEAKEN	thin

/* no longer used
RULE "inscope"(x) IS INFER actual x æ x inscope
AUTOMATCH "inscope"
 */

