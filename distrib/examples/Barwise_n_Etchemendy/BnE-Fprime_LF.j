/* $Id$ */

/* stuff to use LF-style vars in the BnE logic */

PREFIX	10		new
POSTFIX	10		inscope

RULES "è-E"(B) ARE 
	FROM è x .A(x) AND B inscope INFER A(B)
AND	FROM è x,y .A(x,y) AND B inscope AND C inscope INFER A(B,C)
AND	FROM è x,y,z .A(x,y,z) AND B inscope AND C inscope AND D inscope INFER A(B,C,D)
AND	FROM è w,x,y,z .A(w,x,y,z) AND B inscope AND C inscope AND D inscope AND E inscope INFER A(B,C,D,E)
END
RULES "ä-E" ARE
	(OBJECT c) WHERE FRESH c AND c NOTIN ä x .A(x) FROM ä x .A(x) AND new c, A(c) æ C INFER C
AND	(OBJECT c,OBJECT d) WHERE FRESH c,d AND c,d NOTIN ä x,y .A(x,y) 
		FROM ä x,y .A(x,y) AND new c, new d, A(c,d) æ C INFER C
AND	(OBJECT c,OBJECT d,OBJECT e) WHERE FRESH c,d,e AND c,d,e NOTIN ä x,y,z .A(x,y,z) 
		FROM ä x,y,z .A(x,y,z) AND new c, new d, new e, A(c,d,e) æ C INFER C
AND	(OBJECT c,OBJECT d,OBJECT e,OBJECT f) WHERE FRESH c,d,e,f AND c,d,e,f NOTIN ä w,x,y,z .A(w,x,y,z) 
		FROM ä w,x,y,z .A(w,x,y,z) AND new c, new d, new e, new f, A(c,d,e,f) æ C INFER C
END

RULES "è-I" ARE
	(OBJECT c) WHERE FRESH c FROM new c æ A(c) INFER è x .A(x)
AND	(OBJECT c, OBJECT d) WHERE FRESH c,d FROM new c, new d æ A(c,d) INFER è x,y .A(x,y)
AND	(OBJECT c, OBJECT d, OBJECT e) WHERE FRESH c,d,e 
		FROM new c, new d, new e æ A(c,d,e) INFER è x,y,z .A(x,y,z)
AND	(OBJECT c, OBJECT d, OBJECT e, OBJECT f) WHERE FRESH c,d,e,f 
		FROM new c, new d, new e, new f æ A(c,d,e,f) INFER è w,x,y,z .A(w,x,y,z)
END
RULES "ä-I"(B) ARE 
	FROM A(B) AND B inscope INFER ä x.A(x)
AND	FROM A(B,C) AND B inscope AND C inscope INFER ä x,y . A(x,y)
AND	FROM A(B,C,D) AND B inscope AND C inscope AND D inscope INFER ä x,y,z . A(x,y,z)
AND	FROM A(B,C,D,E) AND B inscope AND C inscope AND D inscope AND E inscope INFER ä w,x,y,z . A(w,x,y,z)
END
RULES "ä!-I" ARE
	(OBJECT c1,OBJECT c2) WHERE FRESH c1,c2 AND c1,c2 NOTIN ä! x .A(x) 
		FROM ä x .A(x) AND new c1, new c2, A(c1),A(c2) æ c1=c2 INFER ä! x .A(x)
AND	(OBJECT c1,OBJECT c2, OBJECT d1, OBJECT d2) WHERE FRESH c1,c2,d1,d2 AND c1,c2,d1,d2 NOTIN ä! x,y .A(x,y) 
		FROM ä x,y .A(x,y) AND new c1, new c2, new d1, new d2, A(c1,d1),A(c2,d2) æ c1=c2¦d1=d2 
		INFER ä! x,y .A(x,y)
AND	(OBJECT c1,OBJECT c2, OBJECT d1, OBJECT d2, OBJECT e1,OBJECT e2) 
	WHERE FRESH c1,c2,d1,d2,e1,e2 AND c1,c2,d1,d2,e1,e2 NOTIN ä! x,y,z .A(x,y,z) 
		FROM ä x,y,z .A(x,y,z) 
		AND new c1, new c2, new d1, new d2, new e1, new e2, A(c1,d1,e1),A(c2,d2,e2) æ c1=c2¦d1=d2¦e1=e2 
		INFER ä! x,y,z .A(x,y,z)
AND	(OBJECT c1,OBJECT c2, OBJECT d1, OBJECT d2, OBJECT e1,OBJECT e2, OBJECT f1,OBJECT f2) 
	WHERE FRESH c1,c2,d1,d2,e1,e2,f1,f2 AND c1,c2,d1,d2,e1,e2,f1,f2 NOTIN ä! w,x,y,z .A(w,x,y,z) 
		FROM ä w,x,y,z .A(w,x,y,z) 
		AND new c1, new c2, new d1, new d2, new e1, new e2, new f1, new f2, 
			A(c1,d1,e1,f1),A(c2,d2,e2,f2) æ c1=c2¦d1=d2¦e1=e2¦f1=f2 
		INFER ä! w,x,y,z .A(w,x,y,z)
END

RULES "inscope" ARE
							INFER new x æ x inscope
AND	FROM A inscope AND B inscope	INFER AçB inscope
AND	FROM A inscope AND B inscope	INFER A¦B inscope
AND	FROM A inscope AND B inscope	INFER AëB inscope
AND	FROM A inscope				INFER ÂA inscope
AND	FROM new x æ A inscope		INFER èx.A inscope
AND	FROM new x æ A inscope		INFER äx.A inscope
AND	FROM new x æ A inscope		INFER ä!x.A inscope
END

AUTOMATCH "inscope"

TACTIC "è-E with side condition hidden" IS LAYOUT "è-E" (0) (WITHARGSEL "è-E")
TACTIC "ä-I with side condition hidden" IS LAYOUT "ä-I" (0) (WITHARGSEL "ä-I")
	
TACTIC "è-E tac" IS FOBSS ForwardCut 0 "è-E with side condition hidden"
TACTIC "ä-I tac" IS "ä-I with side condition hidden"

