/* $Id$ */

MACRO trueforward(tac) IS
    LETGOAL _A 
	(CUTIN (LETGOAL _B (UNIFY _A _B) tac)) 
	(ANY (MATCH hyp))
	
TACTIC fstep IS
    ALT (ANY (MATCH hyp)) (trueforward SKIP) /* avoid nasty 'hyp matches two ways', I hope */
    
