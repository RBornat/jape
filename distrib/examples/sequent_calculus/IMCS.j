/* $Id$ */

/*
	The _intuitionistic_ multiple-conclusion sequent calculus!! (add after MCS.jt)
*/

/* the differences */
RULE	"æÂ"		FROM ‚,A æ 					INFER ‚ æ ÂA,Æ
RULE	"Âæ"		FROM ‚ æ A					INFER ‚,ÂA æ Æ
RULE	"æç"		FROM ‚,A æ B	 				INFER ‚ æ AçB,Æ
RULE	"çæ"		FROM ‚ æ A AND ‚,B æ Æ			INFER ‚,AçB æ Æ
