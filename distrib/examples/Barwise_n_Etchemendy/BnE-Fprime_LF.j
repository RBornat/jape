/* $Id$ */

/* stuff to use LF-style vars in the BnE logic */

PREFIX	10		var
POSTFIX	10		inscope

RULE "è-E"(B)						IS FROM èx. A(x) AND B inscope INFER A(B)
RULE "ä-E"(OBJECT c) WHERE FRESH c AND c NOTIN äx.A
								IS FROM äx.A(x) AND var c, A(c) æ C INFER C

RULE "è-I"(OBJECT c) WHERE FRESH c	IS FROM var c æ A(c) INFER èx .A(x)
RULE "ä-I"(B)						IS FROM A(B) AND B inscope INFER äx.A(x)
RULE "ä!-I"(OBJECT c, OBJECT d) WHERE FRESH c,d AND c,d NOTIN ä!x.A(x)
								IS FROM äx.A(x) AND var c,var d,A(c),A(d) æ c=d INFER ä!x.A(x)

RULES "inscope" ARE
							INFER var x æ x inscope
AND	FROM A inscope AND B inscope	INFER AçB inscope
AND	FROM A inscope AND B inscope	INFER A¦B inscope
AND	FROM A inscope AND B inscope	INFER AëB inscope
AND	FROM A inscope				INFER ÂA inscope
AND	FROM var x æ A inscope		INFER èx.A inscope
AND	FROM var x æ A inscope		INFER äx.A inscope
AND	FROM var x æ A inscope		INFER ä!x.A inscope
END

AUTOMATCH "inscope"

TACTIC "è-E with side condition hidden" IS LAYOUT "è-E" (0) (WITHARGSEL "è-E")
TACTIC "ä-I with side condition hidden" IS LAYOUT "ä-I" (0) (WITHARGSEL "ä-I")
	
TACTIC "è-E tac" IS FOBSS ForwardCut "è-E with side condition hidden"
TACTIC "ä-I tac" IS "ä-I with side condition hidden"

