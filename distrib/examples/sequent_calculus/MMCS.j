/* $Id$ */

/*
	The multi-conclusion sequent calculus, with multiplicative versions of the branching rules (gasp!).
	This encoding is a testbed, so I have made it use the extreme form of axiom, it doesn't copy 
	Dyckhoff-style in the çæ and èæ rules.  Because of multiplicativity and un-Dyckhoffry, you have
	to use contraction sometimes.
	RB 20/ix/96
*/

USE "MCS.jt" /* the default */

/* the differences */
RULE	axiom(A)								INFER A æ A
RULE	"æ¦"		FROM ‚ æ A,Æ AND ‚' æ B,Æ' 		INFER ‚,‚' æ A¦B,Æ,Æ'
RULE	"ëæ"		FROM ‚,A æ Æ AND ‚',B æ Æ'		INFER ‚,‚',AëB æ Æ,Æ'
RULE	"çæ"		FROM ‚ æ A,Æ AND ‚',B æ Æ'		INFER ‚,‚',AçB æ Æ,Æ'
RULE	"æé"		FROM ‚ æ AçB,Æ AND ‚' æ BçA,Æ'	INFER ‚,‚' æ AéB,Æ,Æ'
RULE	cut(A)	FROM ‚ æ A,Æ AND ‚',A æ Æ'		INFER ‚,‚' æ Æ,Æ'

STRUCTURERULE CUT            		cut /* cos it's different now */
