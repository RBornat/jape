/* $Id$ */
/*
	Theorems on which to try your hand
*/

CONJECTUREPANEL Conjectures
	THEOREM INFER C æ ûx.x : TçT
	THEOREM INFER C æ ûf.ûg.ûx.f(g x) : (T1çT2) ç (T3çT1) ç T3 ç T2
	THEOREM INFER C æ ûf.ûg.ûx.f(g x) : _T
	THEOREM INFER C æ ûf.ûg.ûx.f(g x) : _T
	THEOREM INFER C æ ûx.ûx.x : T1çT2çT2
	THEOREM INFER C æ ûx.ûx.x : _T
	THEOREM INFER C æ ûx.ûy.ûy.x : T1çT2çT3çT1
	THEOREM INFER C æ ûx.ûy.ûy.x : _T
	DERIVED RULE FROM  C æ x:T INFER C æ letrec f = ûx.x in f x end : T
	DERIVED RULE FROM  C æ x:Tx AND  C æ y:Ty  INFER C æ letrec f = ûx.x in (f x, f y) end : TxôTy
	DERIVED RULE FROM  C æ x:Tx AND  C æ y:Ty  INFER C æ letrec f = ûx.x in (f x, f y) end  : _T
	THEOREM INFER C æ let f = ûx.ûy.(x,y) in (f 3 4, f (3,4) 1) end : _T
	THEOREM INFER C æ let f = ûx.let g = ûy.(x,y) in g end in f end : _T
	THEOREM INFER C æ letrec f = ûx.f x in f end: _T
	THEOREM INFER C æ let f = let g = ûx.x in g end in f f end :_T
	THEOREM INFER C æ letrec f = let g = ûx.f x in g end in f end : _T
	/* the next one won't typecheck ... */
	THEOREM INFER C æ letrec f = let g = ûx.x f in g end in f end : _T
	THEOREM INFER C æ letrec map = ûf.ûxs.if xs==nil then nil else f (hd xs)Ümap f (tl xs) fi in map end : _T
	THEOREM INFER C æ letrec map = ûf.ûxs.if xs==nil then nil else f (hd xs)Ümap f (tl xs) fi , 
	                       f = ûx.x+x
	              in map f (0Ü1Ü2Ünil)
	              end : _T
	DERIVED RULE FROM C æ x:Tx INFER C æ let f = ûx.ûy.x in f 3 end : _T
	DERIVED RULE FROM C æ E:TE INFER C æ let f = ûx.ûy.E in f 3 end : _T
	DERIVED RULE  FROM C æ e:Te INFER C æ let x = (e,e) in x end : _T
	DERIVED RULE FROM C æ fÛ #numônumçnum INFER C æ let g = ûx.f(x,3) in g 4 end : _T
	DERIVED RULE FROM C æ fincÛ #numçnum AND C æ fdecÛ #numçnum 
		INFER C æ letrec fadd = ûx.ûy.if x==0 then y else fadd (fdec x) (finc y) fi in fadd end : _T
END


