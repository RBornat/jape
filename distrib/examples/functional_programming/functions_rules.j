/* $Id$ */

INFIXC	  4010 4000 :
INFIXC	  3000 3010 ++
INFIXC	  2800 2800 Ѕ
INFIXC	  2900 2900 Л, є
OUTFIX	 н о

CLASS VARIABLE v
CLASS FORMULA C, H, J, P
CONSTANT none, one, IF, true, false, if, sel, pair,
	 fst,  snd, id, cat, rcat, rev, rev2, fold,
	 map,  filter, zip, swap,
	 monoid, length

RULE weaken(A) IS FROM B INFER A ц B
WEAKEN weaken

RULE listinduction (B,	OBJECT x, OBJECT xs, OBJECT ys, OBJECT v)  WHERE FRESH x, xs, ys IS
	FROM  A[v\но] AND A[v\нxо] AND A[v\xs], A[v\ys] ц A[v\xs++ys] 
	INFER  A[v \ B]

THEORY	Function IS
	RULES	IF
	ARE IF true X Y = X
	AND IF false X Y	= Y
	END
	RULE	if		IS	if P (F, G) X	= IF (P X) (F X) (G X)
	RULE	"Ѕ"		IS	(F Ѕ G) X		= F(G X)
	RULE	Л		IS	(FЛG) X		= (F X, G X)
	RULE	є		IS	(FєG)(X,Y)	= (F X, G Y)
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
	ARE	length но = 0
	AND	length нXо = 1
	AND	length (Xs++Ys) = length Xs+length Ys
	END
	
	RULE	none IS none X	= но
	RULE	one IS	one X	= нXо

	RULE	cat IS	cat = fold (++) но

	RULES	rev
	ARE rev но		= но
	AND rev нXо		= нX о
	AND rev (Xs++Ys)	= rev Ys ++ rev Xs
	END

	RULES	++
	ARE но++Ys		= Ys
	AND Xs++но		= Xs
	AND (Xs++Ys)++ZS	= Xs++(Ys++ZS)
	END

	RULES	map
	ARE map F но			= но
		AND map F нXо		= нF Xо
	AND map F (Xs++Ys)	= map F Xs ++ map F Ys
	END

	RULE filter IS filter P = cat Ѕ map (if P (one, none))

	RULES zip
	ARE zip(но, но)			= но
	AND zip(нXо, нYо)			= н(X,Y)о
	AND	 FROM length Xs = length Ys 
		INFER zip(Xs++Xs', Ys++Ys') = zip (Xs,Ys)++zip(Xs',Ys')
	END

	RULES fold 
	ARE fold F Z но		= Z
	AND	 fold F Z нXо	= X
	AND	 FROM monoid F Z
		INFER fold F Z (Xs++Ys) = F (fold F Z Xs) (fold F Z Ys)
	END

	RULE rev2 IS rev2 = fold rcat но Ѕ map one
	
	RULE rcat IS rcat Xs Ys = Ys ++ Xs
	
	RULE ":" IS X:Xs = нXо ++ Xs
END

CONSTANT ref, ins, del, move, L, R
THEORY	Reflect IS
	RULE	ref		IS	ref					= (revєrev) Ѕ swap
	RULE	ins		IS	ins X (Xs,Ys)			= (Xs ++ нXо, Ys)
	RULE	del		IS	del	 (Xs ++ нXо, Ys)		= (Xs, Ys)
	RULE	move	IS	move (Xs ++ нXо, Ys)	= (Xs, нXо ++ Ys)
	RULE	L		IS	L F					= ref Ѕ F Ѕ ref
	RULE	R		IS	R F					= F
END





