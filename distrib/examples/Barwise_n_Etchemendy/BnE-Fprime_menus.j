/* $Id$ */

TACTIC ForwardSubst (ruleLR, ruleRL,pat) IS
	WHEN
		(LETHYPSUBSTSEL _P 
			cut
			ruleRL 
			(WHEN
				(LETHYP	_Q 
					(ALT	(WITHHYPSEL hyp) 
						(JAPE (fail(the hypothesis you formula-selected wasn't a pat formula)))
					)
				)
				(JAPE (SUBGOAL 1))
			) 
			(WITHSUBSTSEL hyp)
		)
		(LETCONCSUBSTSEL _P
			(WITHSUBSTSEL ruleLR)
			(WHEN	
				(LETHYP	_Q 
					(ALT	(WITHHYPSEL hyp) 
						(JAPE (fail(the hypothesis you formula-selected wasn't a pat formula)))
					)
				)
				SKIP
			)
		)
		(JAPE (fail(please text-select one or more instances of a sub-formula to replace)))

TACTIC ForwardSubstHiding (ruleLR, ruleRL, thm) IS
	WHEN	(LETHYPSUBSTSEL
				_P 
				cut
				 (LAYOUT () (1) ruleRL thm (WITHSUBSTSEL hyp))
			)
			(LETCONCSUBSTSEL _P (LAYOUT () (1) (WITHSUBSTSEL ruleLR) thm))
			/* the next thing does some serious trickery to make it possible to influence the second argument
			  * to a rewrite rule.  It would be superseded if there were a way to provide the second argument
			  * directly.  I would like to write ruleRL _ _P.
			  */
			(LETHYP _P cut (LAYOUT () (1) ruleRL thm (LETGOAL (_P'[_x\_Q])  (WITHHYPSEL(hyp _Q)))))
			(LETGOAL _P (LAYOUT () (1) (ruleLR _P) thm))

MENU "System F«" IS
	ENTRY "ç-I"	
	ENTRY "ê-I"
	ENTRY "¦-I"	
	ENTRY "ë-I(L)" IS FOB ForwardCut "ë-I(L)"
	ENTRY "ë-I(R)" IS FOB ForwardCut "ë-I(R)"
	ENTRY "Â-I"
	ENTRY "Ù-I"
	ENTRY "è-I"
	ENTRY "ä-I"
	
	SEPARATOR
	
	ENTRY "ç-E"		IS FOB "ç-E forward" "ç-E" 
	ENTRY "ê-E(L)"	IS FOB "ê-E(L) forward" "ê-E(L)" 
	ENTRY "ê-E(R)"	IS FOB "ê-E(R) forward" "ê-E(R)" 
	ENTRY "¦-E(L)"	IS FOB ForwardCut "¦-E(L)"
	ENTRY "¦-E(R)" 	IS FOB ForwardCut "¦-E(R)"
	ENTRY "ë-E"		IS FOB ForwardUncut "ë-E"	
	ENTRY "Â-E"		IS FOB ForwardCut "Â-E"	
	ENTRY "Ù-E"		IS FOB ForwardCut "Ù-E"	
	ENTRY "è-E"		IS FOBSS ForwardCut "è-E"	
	ENTRY "ä-E"		IS FOB ForwardUncut "ä-E"
	SEPARATOR
	ENTRY "A=A"
	ENTRY hyp		IS hyp
END

MENU "Substitution"
	ENTRY "AêÉ"		IS ForwardSubst "rewrite ê Ç" "rewrite ê È" (ê)
	ENTRY "ÉêB"		IS ForwardSubst "rewrite ê È" "rewrite ê Ç" (ê)
	ENTRY "A=É"		IS ForwardSubst "rewrite = Ç" "rewrite = È" (=)
	ENTRY "É=B"		IS ForwardSubst "rewrite = È" "rewrite = Ç" (=)
END

TACTICPANEL "Definitions" IS
	RULE "A­B ÷ Â(A=B)" IS INFER A­B ÷ Â(A=B)
	
	PREFIXBUTTON "A÷É" IS apply ForwardSubstHiding "rewrite ÷ Ç" "rewrite ÷ È"
	PREFIXBUTTON "É÷B" IS apply ForwardSubstHiding "rewrite ÷ È" "rewrite ÷ Ç"
END
