/* $Id$ */
/*
	Theorems on which to try your hand
*/

CONJECTUREPANEL Conjectures
	THEOREMS "Hindley_Milner" ARE
		C æ ûx.x : TçT
	AND	C æ ûf.ûg.ûx.f(g x) : (T1çT2) ç (T3çT1) ç T3 ç T2
	AND	C æ ûf.ûg.ûx.f(g x) : _T
	AND	C æ ûf.ûg.ûx.f(g x) : _T
	AND	C æ ûx.ûx.x : T1çT2çT2
	AND	C æ ûx.ûx.x : _T
	AND	C æ ûx.ûy.ûy.x : T1çT2çT3çT1
	AND	C æ ûx.ûy.ûy.x : _T
	/* these really should be derived rules - e.g. FROM C æ x:T INFER ... */
	AND	C,xÛ#T æ letrec f = ûx.x in f x end : T
	AND	C,xÛ#Tx,yÛ#Ty æ letrec f = ûx.x in (f x, f y) end : TxôTy
	AND	C,xÛ#Tx,yÛ#Ty æ let f =  ûx.x in (f x, f y) end : _T
	AND	C æ let f = ûx.ûy.(x,y) in (f 3 4, f (3,4) 1) end : _T
	AND	C æ let f = ûx.let g = ûy.(x,y) in g end in f end : _T
	AND	C æ letrec f = ûx.f x in f end: _T
	AND	C æ let f = let g = ûx.x in g end in f f end :_T
	AND	C æ letrec f = let g = ûx.f x in g end in f end : _T
	/* the next one won't typecheck ... */
	AND	C æ letrec f = let g = ûx.x f in g end in f end : _T
	AND	C æ letrec map = ûf.ûxs.if xs==nil then nil else f (hd xs)Ümap f (tl xs) fi in map end : _T
	AND	C æ letrec map = ûf.ûxs.if xs==nil then nil else f (hd xs)Ümap f (tl xs) fi , 
	                       f = ûx.x+x
	              in map f (0Ü1Ü2Ünil)
	              end : _T
	/* another that should be a derived rule */
	AND	C,xÛ#Tx æ let f = ûx.ûy.x in f 3 end : _T
	/* Two non-theorems -- they're actually derived rules because C can't map E or e */
	AND	E Û #TE æ let f = ûx.ûy.E in f 3 end : _T
	AND	e Û #Te æ let x = (e,e) in x end : _T
	/* two more that should be derived rules */
	AND	C, fÛ #numônumçnum æ let g = ûx.f(x,3) in g 4 end : _T
	AND	C,fincÛ #numçnum , fdecÛ #numçnum 
		æ letrec fadd = ûx.ûy.if x==0 then y else fadd (fdec x) (finc y) fi in fadd end : _T
	END
END


