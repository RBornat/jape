/* $Id$ */

MENU	Rules IS
	ENTRY	"= reflexive (A=A)"					IS "= reflexive"
	ENTRY	"= transitive (A=B AND B=C ⇒ A=C)"	IS "= transitive"
	ENTRY	"= symmetric (A=B ⇒ B=A)"			IS "= symmetric"
	ENTRY	"A=C AND B=D ⇒ (A,B)=(C,D)"			IS "(,)="
	ENTRY	"F x = G x ⇒ F = G"					IS ext
	ENTRY	"F (x,y) = G (x, y) ⇒ F = G"			IS ext2
	
	SEPARATOR
	
	ENTRY	rewrite					IS withsubstrewrite rewrite
	ENTRY	"rewrite backwards"	IS withsubstrewrite rewritebackwards
	ENTRY	"Unfold/Fold with hypothesis"
	ENTRY	"Unfold with hypothesis"	IS UnfoldHyp
	ENTRY	"Fold with hypothesis"	IS FoldHyp
	
	SEPARATOR
	
	ENTRY	Find
	ENTRY	Flatten
END

CONJECTUREPANEL "Conjectures" IS
		THEOREMS Theorems 
		ARE	 X=Y			⊢ F X = F Y
		AND	X0=X1, Y0=Y1	⊢ F(X0, Y0) = F(X1, Y1)
		AND	X0=X1		⊢ F(X0, Y)	= F(X1, Y)
		AND	Y0=Y1		⊢ F(X, Y0)	= F(X, Y1)
		AND	X=Y, F=G		⊢ F X = G Y
		END
END

