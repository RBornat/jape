/* $Id$ */

TACTIC TheoremForward (thm) IS CUTIN (ALT thm (RESOLVE thm))

TACTIC TheoremForwardOrBackward(thm) IS
  WHEN	(LETHYP _P 
  				(ALT	(TheoremForward (WITHHYPSEL (WITHARGSEL thm)))
  						(Fail	("The theorem %s doesn't apply to the antecedent %t which you selected", thm, _P))
  				)
  			)
  			(LETHYPS _Ps
  				(Fail ("At present I2L Jape can't deal with multiple antecedent selections when applying theorems. Sorry.\
  						\\nCancel one of them and try again."))
  			)
  			(LETGOAL _P
				(ALT (WITHARGSEL thm) 
						(RESOLVE (WITHARGSEL thm)) 
						(TheoremForward (WITHARGSEL thm))
						(Fail	"Theorem application failed -- tell Richard")
				)
  			)
  			(LETOPENSUBGOAL G _P 
  				(Fail ("Error in I2L Jape (open subgoal in TheoremForwardOrBackward). Tell Richard."))
  			)
			(LETOPENSUBGOALS _Pgs
				(ALERT	("There is more than one unproved conclusion in the proof. Please select one – \
				 			\or select an antecedent – to show \
							\Jape where to apply the theorem.")
							("OK", STOP) 
							("Huh?", Explainantecedentandconclusionwords)
				)
			)
			(ALERT "The proof is finished -- there are no unproved conclusions left."
						("OK", STOP) ("Huh?", Explainunprovedconclusionwords)
			)
  				
/* These theorems are all stated without an explicit left context Ç. That is possible because, in I2L_rules.j,
  * we declared a WEAKEN structure rule: Jape will automatically discard any unmatched left-context
  * formulae.
  */
  
CONJECTUREPANEL Conjectures
	THEOREM IS	P, PÁQ Ê Q
	THEOREM IS	PÁQ, QÁR, P Ê R	
	THEOREM IS	PÁ(QÁR), PÁQ, P Ê R
	THEOREM IS	PÁQ, QÁR Ê PÁR
	THEOREM IS	PÁ(QÁR) Ê QÁ(PÁR)
	THEOREM IS	PÁ(QÁR) Ê (PÁQ)Á(PÁR)
	THEOREM IS	P Ê QÁP
	THEOREM IS	PÁ(QÁP)
	THEOREM IS	PÁQ Ê (QÁR)Á(PÁR)
	THEOREM IS	PÁ(QÁ(RÁS)) Ê RÁ(QÁ(PÁS))
	THEOREM IS	(PÁ(QÁR))Á((PÁQ)Á(PÁR))
	THEOREM IS	(PÁQ)ÁR Ê PÁ(QÁR)
	THEOREM "PÁ(QÁR) Ê (PÁQ)ÁR NOT" IS PÁ(QÁR) Ê (PÁQ)ÁR

	THEOREM IS	P, Q Ê P¶Q
	THEOREM IS	P¶Q Ê P
	THEOREM IS	P¶Q Ê Q
	THEOREM IS	P¶(Q¶R) Ê (P¶Q)¶R
	THEOREM IS	(P¶Q)¶R Ê P¶(Q¶R)
	
	THEOREM IS	P¶Q Ê PÁQ
	THEOREM IS	(PÁQ)¶(PÁR) Ê PÁ(Q¶R)
	THEOREM IS	PÁ(Q¶R) Ê (PÁQ)¶(PÁR)
	THEOREM IS	PÁ(QÁR) Ê (P¶Q)ÁR
	THEOREM IS	(P¶Q)ÁR Ê PÁ(QÁR)
	THEOREM IS	(PÁQ)ÁR Ê (P¶Q)ÁR
	THEOREM "(P¶Q)ÁR Ê (PÁQ)ÁR NOT" IS (P¶Q)ÁR Ê (PÁQ)ÁR
	THEOREM IS	P¶(QÁR) Ê (PÁQ)ÁR
	THEOREM "(PÁQ)ÁR Ê P¶(QÁR) NOT" IS (PÁQ)ÁR Ê P¶(QÁR)

	THEOREM IS	P Ê PÎQ	
	THEOREM IS	Q Ê PÎQ	
	THEOREM IS	PÎQ Ê QÎP
	
	THEOREM IS	QÁR Ê (PÎQ)Á(PÎR)
	THEOREM IS	PÎP Ê P
	THEOREM IS	P Ê PÎP
	THEOREM IS	PÎ(QÎR) Ê (PÎQ)ÎR
	THEOREM IS	(PÎQ)ÎR Ê PÎ(QÎR)
	
	THEOREM IS	P¶(QÎR) Ê (P¶Q)Î(P¶R)
	THEOREM IS	(P¶Q)Î(P¶R) Ê P¶(QÎR)
	THEOREM IS	PÎ(Q¶R) Ê (PÎQ)¶(PÎR)
	THEOREM IS	(PÎQ)¶(PÎR) Ê PÎ(Q¶R)
	
	THEOREM IS	(PÁR)¶(QÁR) Ê (PÎQ)ÁR
	THEOREM IS	(PÎQ)ÁR Ê (PÁR)¶(QÁR)

	THEOREM IS	¬¬PÁP
	THEOREM IS	P Ê ¬¬P
	
	THEOREM IS	PÁQ Ê ¬QÁ¬P
	THEOREM IS	¬QÁ¬P Ê PÁQ

THEOREM IS PÎQ, ¬Q Ê P
THEOREM IS PÎQ, ¬P Ê Q
	
THEOREM IS	PÎQ Ê ¬(¬P¶¬Q)
	THEOREM IS	¬(¬P¶¬Q) Ê PÎQ
	THEOREM IS	P¶Q Ê ¬(¬PÎ¬Q)
	THEOREM IS	¬(¬PÎ¬Q) Ê P¶Q
	THEOREM IS	¬(PÎQ) Ê ¬P¶¬Q
	THEOREM IS	¬P¶¬Q Ê ¬(PÎQ)
	THEOREM IS	¬(P¶Q) Ê ¬PÎ¬Q
	THEOREM IS	¬PÎ¬Q Ê ¬(P¶Q)
	THEOREM IS	 Ê ¬(P¶¬P)
	
	THEOREM IS	(PÁQ)Î(QÁP)
	THEOREM IS	P¶¬P Ê Q

	THEOREM IS	PÎ¬P
	THEOREM IS	((PÁQ)ÁP)ÁP

	THEOREM IS	actual c, P(c), Ëx.(P(x)ÁQ(x)) Ê Q(c)
	THEOREM	"P(c), Ëx.(P(x)ÁQ(x)) Ê Q(c) NOT" IS P(c), Ëx.(P(x)ÁQ(x)) Ê Q(c)
	THEOREM IS	Ëx.(P(x)ÁQ(x)) Ê Ëx.P(x)ÁËx.Q(x)
	THEOREM	"Ëx.P(x)ÁËx.Q(x) Ê Ëx.(P(x)ÁQ(x)) NOT" IS Ëx.P(x)ÁËx.Q(x) Ê Ëx.(P(x)ÁQ(x))
	THEOREM IS	Ëx.(P(x)ÁQ(x)), Ëx.(Q(x)ÁR(x)) Ê Ëx.(P(x)ÁR(x))
	THEOREM	"actual c, Q(c) Ê Ëx.(P(x)ÁQ(x)) NOT" IS actual c, Q(c) Ê Ëx.(P(x)ÁQ(x))
	THEOREM IS	Ëx.P(x)¶Ëx.Q(x) Ê Ëx.(P(x)¶Q(x))
	THEOREM IS	Ëx.(P(x)¶Q(x)) Ê Ëx.P(x)¶Ëx.Q(x)
	THEOREM IS	Ëx.(P(x)ÁQ(x)), ‰x.P(x) Ê ‰x.Q(x)
	THEOREM IS	‰x.(P(x)¶Q(x)) Ê ‰x.P(x)¶‰x.Q(x)
	THEOREM	"‰x.P(x)¶‰x.Q(x) Ê ‰x.(P(x)¶Q(x)) NOT" IS ‰x.P(x)¶‰x.Q(x) Ê ‰x.(P(x)¶Q(x))
	THEOREM IS	‰x.P(x)Î‰x.Q(x) Ê ‰x.(P(x)ÎQ(x))
	THEOREM IS	‰x.(P(x)ÎQ(x)) Ê ‰x.P(x)Î‰x.Q(x)
	THEOREM IS	actual c, Ëx.P(x) Ê ‰x.P(x)
	THEOREM	"Ëx.P(x) Ê ‰x.P(x) NOT" IS Ëx.P(x) Ê ‰x.P(x)
	THEOREM IS	Ëx.P(x) Ê ¬(‰x.¬P(x))
	THEOREM IS	¬(‰x.¬P(x)) Ê Ëx.P(x)
	THEOREM IS	‰x.P(x) Ê ¬(Ëx.¬P(x))
	THEOREM IS	¬(Ëx.¬P(x)) Ê ‰x.P(x)
	THEOREM IS	¬(Ëx.P(x)) Ê ‰x.¬P(x)
	THEOREM IS	‰x.¬P(x) Ê ¬(Ëx.P(x))
	THEOREM IS	¬(‰x.P(x)) Ê Ëx.¬P(x)
	THEOREM IS	Ëx.¬P(x) Ê ¬(‰x.P(x))

	BUTTON Apply IS apply TheoremForwardOrBackward COMMAND
END
