/* $Id$ */

TACTIC TheoremForward (thm) IS CUTIN (ALT thm (RESOLVE thm))

TACTIC TheoremForwardOrBackward(thm) IS
  WHEN	(LETHYP _A 
  				(ALT	(TheoremForward (WITHHYPSEL (WITHARGSEL thm)))
  						(Fail	("The theorem %s doesn't apply to the antecedent %t which you selected", thm, _A))
  				)
  			)
  			(LETHYPS _As
  				(Fail ("At present I2L Jape can't deal with multiple antecedent selections when applying theorems. Sorry.\
  						\\nCancel one of them and try again."))
  			)
  			(LETGOAL _A
				(ALT (WITHARGSEL thm) 
						(RESOLVE (WITHARGSEL thm)) 
						(TheoremForward (WITHARGSEL thm))
						(Fail	"Theorem application failed -- tell Richard")
				)
  			)
  			(LETOPENSUBGOAL G _A 
  				(Fail ("Error in I2L Jape (open subgoal in TheoremForwardOrBackward). Tell Richard."))
  			)
			(LETOPENSUBGOALS _As
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
  
  /* Panels are declared in reverse order because the GUIs,  quite reasonably, create panels in the order requested.
    * Thus the last you ask for is the last created, and appears at the front of the stack.
    */

CONJECTUREPANEL "Impossible conjectures"
	THEOREM IS EÁ(FÁG) Ê (EÁF)ÁG
	THEOREM IS (E¶F)ÁG Ê (EÁF)ÁG
	THEOREM IS (EÁF)ÁG Ê E¶(FÁG)
	
	THEOREM IS E Ê E¶F
	THEOREM IS EÎF Ê E¶F
	
	THEOREM IS R(j), Ëx.(R(x)ÁS(x)) Ê S(j)
	THEOREM IS Ëx.R(x)ÁËx.S(x) Ê Ëx.(R(x)ÁS(x))
	THEOREM IS actual j, S(j) Ê Ëx.(R(x)ÁS(x))
	THEOREM IS ‰x.R(x)¶‰x.S(x) Ê ‰x.(R(x)¶S(x))
	THEOREM IS Ëx.R(x) Ê ‰x.R(x)
	
	THEOREM IS actual j, actual k, ‰x.R(x) Ê R(j)

	BUTTON Apply IS apply TheoremForwardOrBackward COMMAND
END

CONJECTUREPANEL "Classical conjectures"
	THEOREM IS	¬¬E Ê E

	THEOREM IS	Ê EÎ¬E
	THEOREM IS	Ê ((EÁF)ÁE)ÁE
	
	THEOREM IS	¬FÁ¬E Ê EÁF
	THEOREM IS	¬(¬E¶¬F) Ê EÎF
	THEOREM IS	¬(¬EÎ¬F) Ê E¶F
	THEOREM IS	¬(E¶F) Ê ¬EÎ¬F
	THEOREM IS	(EÁF)Î(FÁE)
	
	THEOREM IS	¬(‰x.¬R(x)) Ê Ëx.R(x)
	THEOREM IS	¬(Ëx.¬R(x)) Ê ‰x.R(x)
	THEOREM IS	¬(Ëx.R(x)) Ê ‰x.¬R(x)
	
	THEOREM IS actual j, actual k Ê ‰x.(R(x)ÁR(j)¶R(k))

	THEOREM IS	actual i, Ëx.(R(x)Î¬R(x)), ¬(Ëy.¬R(y)) Ê ‰z.R(z)
	
	BUTTON Apply IS apply TheoremForwardOrBackward COMMAND
END
  
CONJECTUREPANEL Conjectures
	THEOREM IS	E, EÁF Ê F
	THEOREM IS	EÁF, FÁG, E Ê G	
	THEOREM IS	EÁ(FÁG), EÁF, E Ê G
	THEOREM IS	EÁF, FÁG Ê EÁG
	THEOREM IS	EÁ(FÁG) Ê FÁ(EÁG)
	THEOREM IS	EÁ(FÁG) Ê (EÁF)Á(EÁG)
	THEOREM IS	E Ê FÁE
	THEOREM IS	Ê EÁ(FÁE)
	THEOREM IS	EÁF Ê (FÁG)Á(EÁG)
	THEOREM IS	EÁ(FÁ(GÁS)) Ê GÁ(FÁ(EÁS))
	THEOREM IS	Ê (EÁ(FÁG))Á((EÁF)Á(EÁG))
	THEOREM IS	(EÁF)ÁG Ê EÁ(FÁG)

	THEOREM IS	E, F Ê E¶F
	THEOREM IS	E¶F Ê E
	THEOREM IS	E¶F Ê F
	THEOREM IS	E¶(F¶G) Ê (E¶F)¶G
	THEOREM IS	(E¶F)¶G Ê E¶(F¶G)
	
	THEOREM IS	E¶F Ê EÁF
	THEOREM IS	(EÁF)¶(EÁG) Ê EÁ(F¶G)
	THEOREM IS	EÁ(F¶G) Ê (EÁF)¶(EÁG)
	THEOREM IS	EÁ(FÁG) Ê (E¶F)ÁG
	THEOREM IS	(E¶F)ÁG Ê EÁ(FÁG)
	THEOREM IS	(EÁF)ÁG Ê (E¶F)ÁG
	THEOREM IS	E¶(FÁG) Ê (EÁF)ÁG

	THEOREM IS	E Ê EÎF	
	THEOREM IS	F Ê EÎF	
	THEOREM IS	EÎF Ê FÎE
	
	THEOREM IS	FÁG Ê (EÎF)Á(EÎG)
	THEOREM IS	EÎE Ê E
	THEOREM IS	E Ê EÎE
	THEOREM IS	EÎ(FÎG) Ê (EÎF)ÎG
	THEOREM IS	(EÎF)ÎG Ê EÎ(FÎG)
	
	THEOREM IS	E¶(FÎG) Ê (E¶F)Î(E¶G)
	THEOREM IS	(E¶F)Î(E¶G) Ê E¶(FÎG)
	THEOREM IS	EÎ(F¶G) Ê (EÎF)¶(EÎG)
	THEOREM IS	(EÎF)¶(EÎG) Ê EÎ(F¶G)
	
	THEOREM IS	(EÁG)¶(FÁG) Ê (EÎF)ÁG
	THEOREM IS	(EÎF)ÁG Ê (EÁG)¶(FÁG)

	THEOREM IS	E Ê ¬¬E
	THEOREM IS	¬E Ê EÁF
	THEOREM IS	EÁF Ê ¬FÁ¬E

	THEOREM IS EÎF, ¬F Ê E
	THEOREM IS EÎF, ¬E Ê F
		
	THEOREM IS	EÎF Ê ¬(¬E¶¬F)
	THEOREM IS	E¶F Ê ¬(¬EÎ¬F)
	THEOREM IS	¬(EÎF) Ê ¬E¶¬F
	THEOREM IS	¬E¶¬F Ê ¬(EÎF)
	THEOREM IS	¬EÎ¬F Ê ¬(E¶F)
	THEOREM IS	 Ê ¬(E¶¬E)
	
	THEOREM IS	E¶¬E Ê F

	THEOREM IS	actual j, R(j), Ëx.(R(x)ÁS(x)) Ê S(j)
	THEOREM IS	Ëx.(R(x)ÁS(x)) Ê Ëx.R(x)ÁËx.S(x)
	THEOREM IS	Ëx.(R(x)ÁS(x)), Ëx.(S(x)ÁT(x)) Ê Ëx.(R(x)ÁT(x))
	THEOREM IS	Ëx.R(x)¶Ëx.S(x) Ê Ëx.(R(x)¶S(x))
	THEOREM IS	Ëx.(R(x)¶S(x)) Ê Ëx.R(x)¶Ëx.S(x)
	THEOREM IS	Ëx.(R(x)ÁS(x)), ‰x.R(x) Ê ‰x.S(x)
	THEOREM IS	‰x.(R(x)¶S(x)) Ê ‰x.R(x)¶‰x.S(x)
	THEOREM IS	‰x.R(x)Î‰x.S(x) Ê ‰x.(R(x)ÎS(x))
	THEOREM IS	‰x.(R(x)ÎS(x)) Ê ‰x.R(x)Î‰x.S(x)
	THEOREM IS	actual j, Ëx.R(x) Ê ‰x.R(x)
	THEOREM IS	Ëx.R(x) Ê ¬(‰x.¬R(x))
	THEOREM IS	‰x.R(x) Ê ¬(Ëx.¬R(x))
	THEOREM IS	‰x.¬R(x) Ê ¬(Ëx.R(x))
	THEOREM IS	Ëx.¬R(x) Ê ¬(‰x.R(x))
	THEOREM IS	¬(‰x.R(x)) Ê Ëx.¬R(x)

	BUTTON Apply IS apply TheoremForwardOrBackward COMMAND
END
