/* $Id$ */

/*
	The multi-conclusion sequent calculus, with multiplicative versions of the branching rules (gasp!).
	This encoding is a testbed, so I have made it use the extreme form of axiom, it doesn't copy 
	Dyckhoff-style in the çæ and èæ rules.  Because of multiplicativity (and sometimes because of
	un-Dyckhoffry), you have to use contraction sometimes.
	RB 14/viii/97
	
	Now an add-on to MCS.jt
*/

/* the differences */
RULE	axiom(A)									INFER A æ A
RULE	"æ¦"		FROM ‚1 æ A,Æ1 AND ‚2 æ B,Æ2 		INFER ‚1,‚2 æ A¦B,Æ1,Æ2
RULE	"ëæ"		FROM ‚1,A æ Æ1 AND ‚2,B æ Æ2		INFER ‚1,‚2,AëB æ Æ1,Æ2
RULE	"çæ"		FROM ‚1 æ A,Æ1 AND ‚2,B æ Æ2		INFER ‚1,‚2,AçB æ Æ1,Æ2
RULE	"æé"		FROM ‚1 æ AçB,Æ1 AND ‚2 æ BçA,Æ2	INFER ‚1,‚2 æ AéB,Æ1,Æ2
RULE	cut(A)	FROM ‚1 æ A,Æ1 AND ‚2,A æ Æ2		INFER ‚1,‚2 æ Æ1,Æ2

STRUCTURERULE CUT            		cut /* cos it's different now */
