MACRO trueforward(tac) IS
	LETGOAL _A 
		(CUTIN (LETGOAL _B (UNIFY _A _B) tac)) 
		(ANY (MATCH hyp))
		
TACTIC fstep IS
	ALT (ANY (MATCH hyp)) (trueforward SKIP) /* avoid nasty 'hyp matches two ways', I hope */
	
TACTIC assign(assigntac) IS
	 WHEN (LETGOAL ({_A} (_x := _E) {_B})
			(ALT	assigntac 
				(SEQ	"consequence(L)" fstep fstep
					NEXTGOAL NEXTGOAL
					assigntac)))
		(LETGOAL _E (ALERT ("To make a variable-assignment step, you have to select a goal of the form \
						\{A} (x:=E) {B}. You selected %t.", _E)
				("OK", STOP)))

TACTICPANEL Instructions
	ENTRY "skip"
	ENTRY "tilt"
	ENTRY "sequence"
	ENTRY "variable-assignment" IS assign "variable-assignment"
	ENTRY "choice" IS (SEQ "choice" fstep fstep)
	ENTRY "while"
	ENTRY "consequence(L)"
	ENTRY "consequence(R)"
	ENTRY "obviously..."
	
/*	RULE "(A;B);C≜A;(B;C)" IS	A;B;C ≜ A;(B;C)
	ENTRY "flatten ;" IS 
		iterateR2L "rewrite≜"  "symmetric≜" (QUOTE (_A;(_B;_C))) "(A;B);C≜A;(B;C)" (Fail "no semicolons to flatten")
	
	BUTTON	"A≜…"	IS apply rewriteL2R "rewrite≜"  "symmetric≜"  COMMAND
	BUTTON	"…≜B"	IS apply rewriteR2L "rewrite≜"  "symmetric≜"  COMMAND */
	BUTTON	Apply	IS apply COMMAND
END
