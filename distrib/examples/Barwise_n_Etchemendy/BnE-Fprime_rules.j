/* $Id$ */

RULE cut(B) IS FROM B AND B æ C INFER C
RULE thin(A) IS FROM C INFER A æ C
RULE dup(A) IS FROM A,A æ C INFER A æ C

RULE "ç-E"(A) IS FROM A AND AçB INFER B
TACTIC "ç-E forward"(Z) IS
	WHEN	(LETHYP (_Aç_B) (ForwardCut 1 "ç-E"))
	          	(LETHYP _A (ForwardCut 0 "ç-E"))
	           	(Fail (what's this in "ç-E" forward?))

TACTIC "ê-E forward"(rule) IS
	WHEN	(LETHYP (_Aê_B) (ForwardCut 1 rule))
	          	(LETHYP _A (ForwardCut 0 rule))
	           	(Fail (what's this in rule forward?))

RULE "ê-E(L)"(B) IS FROM B AND AêB INFER A
TACTIC "ê-E(L) forward"(Z) IS "ê-E forward" "ê-E(L)"

RULE "ê-E(R)"(A) IS FROM A AND AêB INFER B
TACTIC "ê-E(R) forward"(Z) IS "ê-E forward" "ê-E(R)"

RULE "¦-E(L)"(B) IS FROM A ¦ B INFER A
RULE "¦-E(R)"(A) IS FROM A ¦ B INFER B
RULE "ë-E"(A,B) IS FROM A ë B AND A æ C AND B æ C INFER C
RULE "Â-E" IS FROM ÂÂA INFER A
RULE "Ù-E" IS FROM Ù INFER A
RULES "è-E" ARE 
		(c) FROM è x .A(x) AND c inscope INFER A(c)
AND	FROM è (x,y) .A(x,y) AND c inscope AND d inscope INFER A(c,d)
AND	FROM è (x,y,z) .A(x,y,z) AND c inscope AND d inscope AND e inscope INFER A(c,d,e)
AND	FROM è (w,x,y,z) .A(w,x,y,z) AND c inscope AND d inscope AND e inscope AND f inscope INFER A(c,d,e,f)
END
RULES "ä-E" ARE
		(OBJECT c) WHERE FRESH c AND c NOTIN ä x .A(x) FROM ä x .A(x) AND var c, A(c) æ C INFER C
AND	(OBJECT c,OBJECT d) WHERE FRESH c,d AND c,d NOTIN ä (x,y) .A(x,y) 
		FROM ä (x,y) .A(x,y) AND var c, var d, A(c,d) æ C INFER C
AND	(OBJECT c,OBJECT d,OBJECT e) WHERE FRESH c,d,e AND c,d,e NOTIN ä (x,y,z) .A(x,y,z) 
		FROM ä (x,y,z) .A(x,y,z) AND var c, var d, var e, A(c,d,e) æ C INFER C
AND	(OBJECT c,OBJECT d,OBJECT e,OBJECT f) WHERE FRESH c,d,e,f AND c,d,e,f NOTIN ä (w,x,y,z) .A(w,x,y,z) 
		FROM ä (w,x,y,z) .A(w,x,y,z) AND var c, var d, var e, var f, A(c,d,e,f) æ C INFER C
END
RULES "ä!-E(ä)" ARE 
	FROM ä! x .A(x) INFER ä x .A(x)
AND	FROM ä! (x,y) .A(x,y) INFER ä (x,y) .A(x,y)
AND	FROM ä! (x,y,z) .A(x,y,z) INFER ä (x,y,z) .A(x,y,z)
AND	FROM ä! (w,x,y,z) .A(w,x,y,z) INFER ä (x,y,z) .A(w,x,y,z)
END
RULES "ä!-E(èè)" ARE
		(OBJECT x1) FROM ä! x .A(x) INFER è x . è x1 . A(x)¦A(x1)çx=x1
AND	(OBJECT x1,OBJECT y1) FROM ä! (x,y) .A(x,y) INFER è (x,y) . è (x1,y1) . A(x,y)¦A(x1,y1)çx=x1¦y=y1
AND	(OBJECT x1,OBJECT y1,OBJECT z1) FROM ä! (x,y,z) .A(x,y,z) 
		INFER è (x,y,z) . è (x1,y1,z1) . A(x,y,z)¦A(x1,y1,z1)çx=x1¦y=y1¦z=z1
AND	(OBJECT w1,OBJECT x1,OBJECT y1,OBJECT z1) FROM ä! (w,x,y,z) .A(w,x,y,z) 
		INFER è (w,x,y,z) . è (w1,x1,y1,z1) . A(w,x,y,z)¦A(w1,x1,y1,z1)çw=w1¦x=x1¦y=y1¦z=z1
END

TACTIC ForwardCut (n,Rule) IS 
	SEQ cut (ForwardUncut n rule)

TACTIC ForwardUncut (n, Rule) IS
	(LETGOALPATH G (WITHCONTINUATION (WITHARGSEL Rule) (GOALPATH (SUBGOAL G n)) (WITHHYPSEL hyp)) (GOALPATH G) NEXTGOAL)

TACTIC FOB (Forward, n, Rule) IS 
	WHEN 
		(LETHYP _P
			(ALT	(Forward n Rule)
				(WHEN	(LETARGSEL _Q 
							(Fail (Rule is not applicable to assumption ' _P ' 
															with argument ' _Q '))
						)
						(Fail (Rule is not applicable to assumption ' _P '))
				)
			)
		) 
		(ALT	(WITHSELECTIONS Rule)
			(WHEN	(LETARGSEL _P (Fail (Rule is not applicable with argument ' _P ')))
					(Fail (Rule is not applicable))
			)
		)
   
/* we really need a case statement.  This is just a version of FOB, and there are many others ... */
TACTIC FOBSS (Forward, n, Rule) IS 
	WHEN 
		(LETHYP _P
			(ALT	(Forward n Rule)
				(WHEN	(LETARGSEL _Q 
							(Fail (Rule is not applicable to assumption ' _P ' 
															with argument ' _Q '))
						)
						(Fail (Rule is not applicable to assumption ' _P '))
				)
			)
		) 
		(LETCONCSUBSTSEL _P 
			(ALT	(WITHSUBSTSEL (WITHHYPSEL Rule))
				(LETGOAL _Q
					(Fail (Rule is not applicable to conclusion ' _Q ' with substitution ' _P '))
				)
			)
		)
		(ALT	(WITHSELECTIONS Rule)
			(Fail (Rule is not applicable to that conclusion))
		)
   
TACTIC FSSOB (Forward, n, Rule) IS 
	WHEN
		(LETHYPSUBSTSEL _P (Forward n Rule)) 
		(ALT	(WITHSELECTIONS Rule)
			(WHEN	(LETARGSEL _P (Fail (Rule is not applicable with argument ' _P ')))
					(Fail (Rule is not applicable))
			)
		)
   
RULE "ç-I"  IS FROM A æ B INFER AçB
RULE "ê-I"  IS FROM AçB  AND BçA INFER AêB
RULE "¦-I"  IS FROM A AND B INFER A ¦ B
RULE "ë-I(L)"   IS FROM A INFER A ë B
RULE "ë-I(R)"   IS FROM B INFER A ë B
RULE "Â-I"   IS FROM A æ Ù INFER ÂA
RULE "Ù-I" IS FROM P AND ÂP INFER Ù
RULES "è-I" ARE
	(OBJECT c) WHERE FRESH c FROM var c æ A(c) INFER è x .A(x)
AND	(OBJECT c, OBJECT d) WHERE FRESH c,d FROM var c, var d æ A(c,d) INFER è (x,y) .A(x,y)
AND	(OBJECT c, OBJECT d, OBJECT e) WHERE FRESH c,d,e 
		FROM var c, var d, var e æ A(c,d,e) INFER è (x,y,z) .A(x,y,z)
AND	(OBJECT c, OBJECT d, OBJECT e, OBJECT f) WHERE FRESH c,d,e,f 
		FROM var c, var d, var e, var f æ A(c,d,e,f) INFER è (w,x,y,z) .A(w,x,y,z)
END
RULES "ä-I"(B) ARE 
	FROM A(B) AND B inscope INFER ä x.A(x)
AND	FROM A(B,C) AND B inscope AND C inscope INFER ä (x,y) . A(x,y)
AND	FROM A(B,C,D) AND B inscope AND C inscope AND D inscope INFER ä (x,y,z) . A(x,y,z)
AND	FROM A(B,C,D,E) AND B inscope AND C inscope AND D inscope AND E inscope INFER ä (w,x,y,z) . A(w,x,y,z)
END
RULES "ä!-I" ARE
	(OBJECT c1,OBJECT c2) WHERE FRESH c1,c2 AND c1,c2 NOTIN ä! x .A(x) 
		FROM ä x .A(x) AND var c1, var c2, A(c1),A(c2) æ c1=c2 INFER ä! x .A(x)
AND	(OBJECT c1,OBJECT c2, OBJECT d1, OBJECT d2) WHERE FRESH c1,c2,d1,d2 AND c1,c2,d1,d2 NOTIN ä! (x,y) .A(x,y) 
		FROM ä (x,y) .A(x,y) AND var c1, var c2, var d1, var d2, A(c1,d1),A(c2,d2) æ c1=c2¦d1=d2 
		INFER ä! (x,y) .A(x,y)
AND	(OBJECT c1,OBJECT c2, OBJECT d1, OBJECT d2, OBJECT e1,OBJECT e2) 
	WHERE FRESH c1,c2,d1,d2,e1,e2 AND c1,c2,d1,d2,e1,e2 NOTIN ä! (x,y,z) .A(x,y,z) 
		FROM ä (x,y,z) .A(x,y,z) 
		AND var c1, var c2, var d1, var d2, var e1, var e2, A(c1,d1,e1),A(c2,d2,e2) æ c1=c2¦d1=d2¦e1=e2 
		INFER ä! (x,y,z) .A(x,y,z)
AND	(OBJECT c1,OBJECT c2, OBJECT d1, OBJECT d2, OBJECT e1,OBJECT e2, OBJECT f1,OBJECT f2) 
	WHERE FRESH c1,c2,d1,d2,e1,e2,f1,f2 AND c1,c2,d1,d2,e1,e2,f1,f2 NOTIN ä! (w,x,y,z) .A(w,x,y,z) 
		FROM ä (w,x,y,z) .A(w,x,y,z) 
		AND var c1, var c2, var d1, var d2, var e1, var e2, var f1, var f2, 
			A(c1,d1,e1,f1),A(c2,d2,e2,f2) æ c1=c2¦d1=d2¦e1=e2¦f1=f2 
		INFER ä! (w,x,y,z) .A(w,x,y,z)
END

RULE "inscope"	INFER var x æ x inscope
AUTOMATCH "inscope"

TACTIC "è-E with side condition hidden" IS LAYOUT "è-E" (0) (WITHARGSEL "è-E")
TACTIC "ä-I with side condition hidden" IS LAYOUT "ä-I" (0) (WITHARGSEL "ä-I")

TACTIC "ä!-I tac" IS WITHARGSEL "ä!-I"
TACTIC "ç-E tac" IS FOB "ç-E forward" 0 "ç-E"
TACTIC "ë-E tac" IS FOB ForwardUncut 0 "ë-E"	
TACTIC "ä-E tac" IS FOB ForwardUncut 0 "ä-E"

TACTIC "è-E tac" IS FOBSS ForwardCut 0 "è-E with side condition hidden"
TACTIC "ä-I tac" IS "ä-I with side condition hidden"

RULE "A=A" IS INFER A=A
RULE hyp(A) IS INFER A æ A

AUTOMATCH hyp

STRUCTURERULE IDENTITY    hyp
STRUCTURERULE CUT            cut
STRUCTURERULE WEAKEN     thin

/*  Because of the ForwardSubstHiding tactic, these all have argument A and "second argument" B */
RULE "rewrite ê Ç" (A) IS FROM A ê B AND P(B) INFER P(A)
RULE "rewrite ê È" (A) IS FROM B êA AND P(B) INFER P(A)
RULE "rewrite = Ç" (A) IS FROM A=B AND P(B) INFER P(A)
RULE "rewrite = È" (A) IS FROM B=A AND P(B) INFER P(A)
RULE "rewrite ÷ Ç" (A) IS FROM A÷B AND P(B) INFER P(A)
RULE "rewrite ÷ È" (A) IS FROM B÷A AND P(B) INFER P(A)
