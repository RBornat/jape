/* $Id$ */

INFIXC	  4010 4000 :
INFIXC	  3000 3010 ++
INFIXC	  2800 2800 •
INFIXC	  2900 2900 ª, Ù
OUTFIX	 › ﬁ

CLASS VARIABLE v
CLASS FORMULA C, H, J, P
CONSTANT none, one, IF, true, false, if, sel, pair,
	 fst,  snd, id, cat, rcat, rev, rev2, fold,
	 map,  filter, zip, swap,
	 monoid, length

RULE listinduction (B,	OBJECT x, OBJECT xs, OBJECT ys, OBJECT v)  WHERE FRESH x, xs, ys IS
	FROM  A[v\›ﬁ] AND A[v\›xﬁ] AND A[v\xs], A[v\ys] Ê A[v\xs++ys] 
	INFER  A[v \ B]

THEORY	Function IS
	RULES	IF
	ARE IF true X Y = X
	AND IF false X Y	= Y
	END
	RULE	if		IS	if P (F, G) X	= IF (P X) (F X) (G X)
	RULE	"•"		IS	(F • G) X		= F(G X)
	RULE	ª		IS	(FªG) X		= (F X, G X)
	RULE	Ù		IS	(FÙG)(X,Y)	= (F X, G Y)
	RULE	id		IS	id X			= X
	RULE	fst		IS	fst(X,Y)		= X
	RULE	snd		IS	snd(X,Y)		= Y
	RULE	swap		IS	swap(X,Y)		= (Y,X)
END

TACTIC "list induction tactic" IS 
	WHEN	(LETSUBSTSEL _A (WITHSUBSTSEL listinduction))
			(FAIL(Please select a sub-formula on which to perform induction))

RULE	BoolCases(B,OBJECT x) IS FROM A[x\true] AND A[x\false] INFER A[x\B]

RULE monoid(F, Z, OBJECT A, OBJECT B, OBJECT C) IS	
	FROM F A (F B C) = F (F A B) C AND F A Z = A AND F Z B = B 
	INFER monoid F Z
	
THEORY	List IS
	RULES length 
	ARE	length ›ﬁ = 0
	AND	length ›Xﬁ = 1
	AND	length (Xs++Ys) = length Xs+length Ys
	END
	
	RULE	none IS none X	= ›ﬁ
	RULE	one IS	one X	= ›Xﬁ

	RULE	cat IS	cat = fold (++) ›ﬁ

	RULES	rev
	ARE rev ›ﬁ		= ›ﬁ
	AND rev ›Xﬁ		= ›X ﬁ
	AND rev (Xs++Ys)	= rev Ys ++ rev Xs
	END

	RULES	++
	ARE ›ﬁ++Ys		= Ys
	AND Xs++›ﬁ		= Xs
	AND (Xs++Ys)++ZS	= Xs++(Ys++ZS)
	END

	RULES	map
	ARE map F ›ﬁ			= ›ﬁ
		AND map F ›Xﬁ		= ›F Xﬁ
	AND map F (Xs++Ys)	= map F Xs ++ map F Ys
	END

	RULE filter IS filter P = cat • map (if P (one, none))

	RULES zip
	ARE zip(›ﬁ, ›ﬁ)			= ›ﬁ
	AND zip(›Xﬁ, ›Yﬁ)			= ›(X,Y)ﬁ
	AND	 FROM length Xs = length Ys 
		INFER zip(Xs++Xs', Ys++Ys') = zip (Xs,Ys)++zip(Xs',Ys')
	END

	RULES fold 
	ARE fold F Z ›ﬁ		= Z
	AND	 fold F Z ›Xﬁ	= X
	AND	 FROM monoid F Z
		INFER fold F Z (Xs++Ys) = F (fold F Z Xs) (fold F Z Ys)
	END

	RULE rev2 IS rev2 = fold rcat ›ﬁ • map one
	
	RULE rcat IS rcat Xs Ys = Ys ++ Xs
	
	RULE ":" IS X:Xs = ›Xﬁ ++ Xs
END

CONSTANT ref, ins, del, move, L, R
THEORY	Reflect IS
	RULE	ref		IS	ref					= (revÙrev) • swap
	RULE	ins		IS	ins X (Xs,Ys)			= (Xs ++ ›Xﬁ, Ys)
	RULE	del		IS	del	 (Xs ++ ›Xﬁ, Ys)		= (Xs, Ys)
	RULE	move	IS	move (Xs ++ ›Xﬁ, Ys)	= (Xs, ›Xﬁ ++ Ys)
	RULE	L		IS	L F					= ref • F • ref
	RULE	R		IS	R F					= F
END

/* these three tactics are something to do with three-button mice on Suns, and disabling of same*/
TACTIC	mousebutton3(X)	IS (ALT)	
TACTIC	findbutton(X)	  	IS (ALT)	
TACTIC	cutbutton(X)	  	IS (ALT)	

TACTIC Auto(foldunfold, dohyp) IS 
ALT (dohyp foldunfoldhyp)
	(foldunfold list) 
	(foldunfold listthms) 
	(foldunfold function) 
	(foldunfold functionthms) 
	(foldunfold reflect ) 
	(foldunfold reflectthms)
	(FAIL (Cannot find anything to foldunfold) )

TACTIC UnfoldHyp(a) IS UNFOLDHYP a (_A=_B)

TACTIC UnfoldWithAnyHyp IS foldunfoldhyp "Unfold with hypothesis"
TACTIC FoldWithAnyHyp IS SEQ (foldunfoldhyp "Fold with hypothesis") hyp



