/* $Id$ */

RULE cut(B) IS FROM B AND B æ C INFER C
RULE thin(A) IS FROM C INFER A æ C
RULE dup(A) IS FROM A,A æ C INFER A æ C

RULE "ç-E"(A) IS FROM A AND AçB INFER B
TACTIC "ç-E forward"(Z) IS
	WHEN	(LETHYP (_Aç_B) (ForwardCut2 "ç-E"))
	          	(LETHYP _A (ForwardCut "ç-E"))
	           	(JAPE(fail(what's this in "ç-E" forward?)))

TACTIC "ê-E forward"(rule) IS
	WHEN	(LETHYP (_Aê_B) (ForwardCut2 rule))
	          	(LETHYP _A (ForwardCut rule))
	           	(JAPE(fail(what's this in rule forward?)))

RULE "ê-E(L)"(B) IS FROM B AND AêB INFER A
TACTIC "ê-E(L) forward"(Z) IS "ê-E forward" "ê-E(L)"

RULE "ê-E(R)"(A) IS FROM A AND AêB INFER B
TACTIC "ê-E(R) forward"(Z) IS "ê-E forward" "ê-E(R)"

RULE "¦-E(L)"(B) IS FROM A ¦ B INFER A

RULE "¦-E(R)"(A) IS FROM A ¦ B INFER B

RULE "ë-E"(A,B) IS FROM A ë B AND A æ C AND B æ C INFER C

RULE "Â-E" IS FROM ÂÂA INFER A

RULE "Ù-E" IS FROM Ù INFER A

RULE "è-E"(B) IS FROM èx.A(x) INFER A(B)

RULE "ä-E"(OBJECT c) WHERE FRESH c AND c NOTIN äx.A(x) IS FROM äx.A(x) AND A(c) æ C INFER C

RULE "ä!-E(1)" IS FROM ä!x.A(x) INFER äx.A(x)
RULE "ä!-E(2)"(OBJECT y) IS FROM ä!x.A(x) INFER èx.èy.A(x)¦A(y)çx=y

TACTIC ForwardCut(Rule) IS SEQ cut (WITHARGSEL Rule) (WITHHYPSEL hyp)

TACTIC ForwardCut2(Rule) IS SEQ cut (WITHARGSEL Rule) (JAPE(SUBGOAL 1)) (WITHHYPSEL hyp)

TACTIC ForwardUncut(Rule) IS SEQ (WITHARGSEL Rule) (WITHHYPSEL hyp)

TACTIC FOB (Forward, Rule) IS 
	WHEN 
		(LETHYP _P
			(ALT	(Forward Rule)
				(WHEN	(LETARGSEL _Q 
							(JAPE(failgivingreason(Rule is not applicable to assumption ' _P ' 
															with argument ' _Q ')))
						)
						(JAPE(failgivingreason(Rule is not applicable to assumption ' _P ')))
				)
			)
		) 
		(ALT	(WITHSELECTIONS Rule)
			(WHEN	(LETARGSEL _P (JAPE(failgivingreason(Rule is not applicable with argument ' _P '))))
					(JAPE(failgivingreason(Rule is not applicable)))
			)
		)
   
/* we really need a case statement.  This is just a version of FOB, and there are many others ... */
TACTIC FOBSS (Forward, Rule) IS 
	WHEN 
		(LETHYP _P
			(ALT	(Forward Rule)
				(WHEN	(LETARGSEL _Q 
							(JAPE(failgivingreason(Rule is not applicable to assumption ' _P ' 
															with argument ' _Q ')))
						)
						(JAPE(failgivingreason(Rule is not applicable to assumption ' _P ')))
				)
			)
		) 
		(LETCONCSUBSTSEL _P 
			(ALT	(WITHSUBSTSEL (WITHHYPSELRule))
				(LETGOAL _Q
					(JAPE(failgivingreason(Rule is not applicable to conclusion ' _Q ' with substitution ' _P ')))
				)
			)
		)
		(ALT	(WITHSELECTIONS Rule)
			(JAPE(failgivingreason(Rule is not applicable to that conclusion)))
		)
   
TACTIC FSSOB (Forward, Rule) IS 
	WHEN
		(LETHYPSUBSTSEL _P (Forward Rule)) 
		(ALT	(WITHSELECTIONS Rule)
			(WHEN	(LETARGSEL _P (JAPE(failgivingreason(Rule is not applicable with argument ' _P '))))
					(JAPE(failgivingreason(Rule is not applicable)))
			)
		)
   
RULE "ç-I"  IS FROM A æ B INFER AçB
RULE "ê-I"  IS FROM A æ B  AND B æ A INFER AêB
RULE "¦-I"  IS FROM A AND B INFER A ¦ B
RULE "ë-I(L)"   IS FROM A INFER A ë B
RULE "ë-I(R)"   IS FROM B INFER A ë B
RULE "Â-I"(B)   IS FROM A æ Ù INFER ÂA
RULE "Ù-I"	IS FROM P AND ÂP INFER Ù
RULE "è-I"(OBJECT c) WHERE FRESH c IS FROM A(c) INFER èx .A(x)
RULE "ä-I"(B)   IS FROM A(B) INFER äx.A(x)
RULE "ä!-I"(OBJECT c,OBJECT d) WHERE FRESH c,d AND c,d NOTIN ä!x.A(x) IS 
	FROM äx.A(x) AND A(c),A(d) æ c=d INFER ä!x.A(x)

RULE "A=A" IS INFER A=A
RULE hyp(A) IS INFER A æ A

AUTOMATCH hyp

STRUCTURERULE IDENTITY    hyp
STRUCTURERULE CUT            cut
STRUCTURERULE WEAKEN     thin

RULE "rewrite ê Ç" (A) IS FROM A ê B AND P(B) INFER P(A)
RULE "rewrite ê È" (B) IS FROM A ê B AND P(A) INFER P(B)
RULE "rewrite = Ç" (A) IS FROM A=B AND P(B) INFER P(A)
RULE "rewrite = È" (B) IS FROM A=B AND P(A) INFER P(B)
RULE "rewrite ÷ Ç" (A) IS FROM A÷B AND P(B) INFER P(A)
RULE "rewrite ÷ È" (B) IS FROM A÷B AND P(A) INFER P(B)
