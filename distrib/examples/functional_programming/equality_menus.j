/* $Id$ */

MENU	Rules IS
	ENTRY	"= reflexive (A=A)" 					IS "= reflexive"
	ENTRY	"= transitive (A=B AND B=C Û A=C)"	IS "= transitive"
	ENTRY	"= symmetric (A=B Û B=A)"			IS "= symmetric"
	ENTRY	"A=C AND B=D Û (A,B)=(C,D)"			IS "(,)="
	ENTRY	"F x = G x Û F = G" 					IS ext
	ENTRY	"F (x,y) = G (x, y) Û F = G"			IS ext2
	
	SEPARATOR
	
	ENTRY	rewrite 				IS withsubstrewrite rewrite
	ENTRY	"rewrite backwards"	IS withsubstrewrite rewritebackwards
	ENTRY	"rewrite with hypothesis"
	ENTRY	"Unfold with hypothesis"	IS UnfoldHyp
	ENTRY	"Fold with hypothesis"	IS FoldHyp
END

CONJECTUREPANEL "Conjectures" IS
		THEOREMS Theorems 
		ARE	 X=Y			æ F X = F Y
		AND	X0=X1, Y0=Y1	æ F(X0, Y0) = F(X1, Y1)
		AND	X0=X1		æ F(X0, Y)	= F(X1, Y)
		AND	Y0=Y1		æ F(X, Y0)	= F(X, Y1)
		AND	X=Y, F=G		æ F X = G Y
		END
END

