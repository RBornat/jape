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
	WHEN	
		(LETHYPSUBSTSEL _P 
				cut (LAYOUT () (1) ruleRL thm (WITHSUBSTSEL hyp))
		)
		(LETCONCSUBSTSEL _P (LAYOUT () (1) (WITHSUBSTSEL ruleLR) thm))
		/* the "second argument" of the rewrite rules has to be B */
		(LETHYP _P cut (LAYOUT () (1) (ruleRL[B\_P]) thm (WITHHYPSEL hyp)))
		(LETGOAL _P (LAYOUT () (1) (ruleLR _P) thm))

TACTIC "ä-I tac" IS WITHARGSEL "ä-I"
TACTIC "ä!-I tac" IS WITHARGSEL "ä!-I"
TACTIC "ç-E tac" IS FOB "ç-E forward" 0 "ç-E"
TACTIC "ë-E tac" IS FOB ForwardUncut 0 "ë-E"	
TACTIC "è-E tac" IS FOBSS ForwardCut 0 "è-E"
TACTIC "ä-E tac" IS FOB ForwardUncut 0 "ä-E"

MENU "System F«" IS
	ENTRY "ç-I"	
	ENTRY "ê-I"
	ENTRY "¦-I"	
	ENTRY "ë-I(L)" IS FOB ForwardCut 0 "ë-I(L)"
	ENTRY "ë-I(R)" IS FOB ForwardCut 0 "ë-I(R)"
	ENTRY "Â-I"
	ENTRY "Ù-I"
	ENTRY "è-I"
	ENTRY "ä-I" IS "ä-I tac"
	ENTRY "ä!-I" IS "ä!-I tac"
	
	SEPARATOR
	
	ENTRY "ç-E"		IS "ç-E tac" 
	ENTRY "ê-E(L)"	IS FOB "ê-E(L) forward" 0 "ê-E(L)" 
	ENTRY "ê-E(R)"	IS FOB "ê-E(R) forward" 0 "ê-E(R)" 
	ENTRY "¦-E(L)"	IS FOB ForwardCut 0 "¦-E(L)"
	ENTRY "¦-E(R)" 	IS FOB ForwardCut 0 "¦-E(R)"
	ENTRY "ë-E"		IS "ë-E tac"	
	ENTRY "Â-E"		IS FOB ForwardCut 0 "Â-E"	
	ENTRY "Ù-E"		IS FOB ForwardCut 0 "Ù-E"	
	ENTRY "è-E"		IS "è-E tac"	
	ENTRY "ä-E"		IS "ä-E tac"
	ENTRY "ä!-E(ä)"	IS FOB ForwardCut 0 "ä!-E(ä)"
	ENTRY "ä!-E(èè)"	IS FOB ForwardCut 0 "ä!-E(èè)"
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
