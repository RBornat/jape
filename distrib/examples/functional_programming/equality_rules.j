/*
	Equality
		$Id$
	
	Includes syntax of arithmetic and comparison operators other than equality, but not used.
	
*/

FONTS	"Konstanz"

INFIX		2000	2000	=, ³, ², ­, <, >
INFIX		2500	2500	+, -
INFIX		2600	2600	*, /
INFIX		2700	2700	^

CLASS VARIABLE x, y
CLASS FORMULA A, B, C, F, G, X, Y, Z
CONSTANT Ù
 
 TACTIC FAIL(x)		IS JAPE(fail x)
 TACTIC FAILREASON(x)	IS JAPE (failgivingreason x)

/***************************** rules *****************************/
 
RULE hyp IS A æ A
IDENTITY hyp

RULE "= reflexive"		IS 						INFER X = X
RULE "= transitive"(Y)	IS FROM X = Y AND Y = Z 		INFER X = Z
RULE "= symmetric"		IS FROM X = Y 				INFER Y = X
RULE "(,)="			IS FROM X0=X1 AND Y0=Y1	INFER (X0, Y0) = (X1, Y1)

/* The rules of extensionality incorporate a generalization step: hence the FRESH provisos. */
RULE ext (OBJECT x) WHERE FRESH x 			IS FROM  F x = G x			INFER F = G
RULE ext2(OBJECT x, OBJECT y) WHERE FRESH x, y	IS FROM  F (x, y) = G (x,y)	INFER F = G

 /* use of AA, rather than A, is to help some other rule somewhere, which uses OBJECT A */
RULE   rewrite (X, OBJECT x)			IS FROM X=Y AND AA[x\Y] INFER AA[x\X]
RULE   rewritebackwards (Y, OBJECT x)	IS FROM X=Y AND AA[x\X] INFER AA[x\Y]

/*
	Infrastructure for rewriting in an equational theory
*/

TACTIC Flatten IS
	WHEN	(LETARGSEL _A (FLATTEN _A))
			(LETGOAL (_X = _Y) (IF(FLATTEN(_X))) (IF(FLATTEN(_Y)))) 
			(LETGOAL _X (FAIL (Cannot Flatten _X)))

TACTIC Find IS 
	WHEN	(LETARGSEL _A (ALT (FIND _A) (FAIL (Cannot find _A)))) 
			(FAIL (Please select something to find))

TACTIC Unfold(x) IS LAYOUT "Fold %s" (1) (UNFOLD rewrite x)

TACTIC UnfoldOneSel(x) IS
	WHEN	(LETSUBSTSEL _A (LAYOUT "Fold %s" (1) (WITHSUBSTSEL rewrite)) x)
			(LETARGSEL _A (FAIL (The formula you selected (_A) is not a proper subformula)))
			(FAIL (Please text-select an expression))
		
TACTIC Fold(x) IS LAYOUT "Unfold %s" (1) (FOLD rewritebackwards x)

TACTIC FoldOneSel(x) IS
	WHEN	(LETSUBSTSEL	_A (LAYOUT "Unfold %s" (1) (WITHSUBSTSEL rewritebackwards)) x)
			(LETARGSEL _A (FAIL (The formula you selected (_A) is not a proper subformula)))
			(FAIL (Please text-select an expression))

TACTIC "Unfold/Fold with hypothesis"  IS
	WHEN 
		(LETHYP _A
			(WHEN	
				(LETCONCSUBSTSEL (_B[_x\_C])
					(ALT	(SEQ (WITHSUBSTSEL rewrite) (WITHHYPSEL hyp))
						(SEQ (WITHSUBSTSEL rewritebackwards) (WITHHYPSEL hyp))
						(FAIL (hypothesis _A doesn't fit sub-formula _C))
					)
				)
				(ALT	(SEQ rewrite (WITHHYPSEL hyp))
					(SEQ rewritebackwards (WITHHYPSEL hyp))
					(LETGOAL _B (FAIL (hypothesis _A doesn't rewrite conclusion _B)))
				)
			)
		)
		(FAIL (please select a hypothesis))

TACTIC withsubstrewrite(t)
	WHEN 
		(LETCONCSUBSTSEL _A (WITHSUBSTSEL t))
		(FAILREASON (please text-select a sub-formula, or sub-formulae, in a conclusion))
		
RULE "Fold with hypothesis" (X, OBJECT x)	IS FROM X=Y æ AA[x\Y] INFER X=Y æ AA[x\X]
RULE "Unfold with hypothesis" (Y,OBJECT x)	IS FROM X=Y æ AA[x\X] INFER X=Y æ AA[x\Y]

TACTIC HypFoldUnfold(t) IS 
	WHEN 
		(LETHYP _A 
			(WHEN 
				(LETCONCSUBSTSEL (_B[_x\_C])
					(ALT	(WITHSUBSTSEL t)
						(FAIL (hypothesis _A doesn't fit sub-formula _C))
					)
				)
				(FAILREASON (please text-select a sub-formula, or sub-formulae, in a conclusion))
			)
		)
		(FAIL (please select a hypothesis))

TACTIC "UnfoldHyp"	IS HypFoldUnfold "Fold with hypothesis"
TACTIC "FoldHyp"	IS HypFoldUnfold "Unfold with hypothesis"









