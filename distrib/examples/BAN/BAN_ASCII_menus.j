/* $Id$ */

TACTIC Fail(x) IS (SEQ (ALERT x) FAIL)

TACTIC ForwardCut (n,Rule)
	SEQ cut (ForwardUncut n Rule)

TACTIC ForwardUncut (n,Rule)
	(LETGOALPATH G 
		(WITHCONTINUATION (WITHARGSEL Rule) (GOALPATH (SUBGOAL G n)) (WITHHYPSEL hyp))
		(GOALPATH G) 
		NEXTGOAL
	)

TACTIC ForwardOrBackward (Forward, n, Rule) IS 
	WHEN	
		(LETHYP  _X 
			(ALT	(Forward n Rule)
				(WHEN	
					(LETARGSEL _Y 
							(Fail (Rule is not applicable to assumption ' _X ' with argument ' _Y ' ))
					)
					(Fail (Rule is not applicable to assumption ' _X ' ))
				)
			)
		)
		(LETGOAL  _X
			(ALT	(WITHSELECTIONS Rule)
	                   		(WHEN	
	                   			(LETARGSEL _Y
	                                                		(Fail (Rule is not applicable to conclusion ' _X ' with argument ' _Y ' ))
	                                              	)
					(Fail (Rule is not applicable to conclusion ' _X ' ))
				)
	           		)
		)
   
MENU "|È"
	SEPARATOR
	ENTRY "P|ÈX,  P|ÈY,  ... € P|È(X,Y,...)"
	ENTRY "P|È(...,X,...) € P|ÈX"				IS ForwardOrBackward ForwardCut 0 "P|È(...,X,...) € P|ÈX"
	ENTRY "P|ÈQ|È(...,X,...) € P|ÈQ|ÈX"			IS ForwardOrBackward ForwardCut 0 "P|ÈQ|È(...,X,...) € P|ÈQ|ÈX"
	SEPARATOR
	ENTRY "P|ÈËx.X(x) € P|ÈX(Y)"			IS ForwardOrBackward ForwardCut 0 "P|ÈËx.X(x) € P|ÈX(Y)"
	ENTRY "P|ÈQ|~X, [P|È#X] € P|ÈQ|ÈX"			IS ForwardOrBackward ForwardCut 1 "P|È#X, P|ÈQ|~X € P|ÈQ|ÈX"
	ENTRY "P|ÈQ|ÈX, [P|ÈQ|€X] € P|ÈX"			IS ForwardOrBackward ForwardCut 1 "P|ÈQ|€X, P|ÈQ|ÈX € P|ÈX"
	SEPARATOR
	ENTRY "P|È#X, [P|ÈQ|~X] € P|ÈQ|ÈX"			IS ForwardOrBackward ForwardCut 0 "P|È#X, P|ÈQ|~X € P|ÈQ|ÈX"
	ENTRY "P|ÈQ|€X, [P|ÈQ|ÈX] € P|ÈX"			IS ForwardOrBackward ForwardCut 0 "P|ÈQ|€X, P|ÈQ|ÈX € P|ÈX"
	ENTRY "P|È#X, [P|ÈQ|~X] € P|ÈQ|ÈX"			IS ForwardOrBackward ForwardCut 0 "P|È#X, P|ÈQ|~X € P|ÈQ|ÈX"
	ENTRY "P|ÈQ|~X, [P|È#X] € P|ÈQ|ÈX"			IS ForwardOrBackward ForwardCut 1 "P|È#X, P|ÈQ|~X € P|ÈQ|ÈX"
END

MENU "<|"
	ENTRY "P<|{X}K, [P|È(Q,P)ÍK] € P|ÈQ|~X"	IS ForwardOrBackward ForwardCut 1 "P|È(Q,P)ÍK, P<|{X}K € P|ÈQ|~X"
	ENTRY "P<|{X}K¯, [P|ÈQÿK] € P|ÈQ|~X"		IS ForwardOrBackward ForwardCut 1 "P|ÈQÿK, P<|{X}K¯ € P|ÈQ|~X"
	ENTRY " P<|<X>Y, [P|È(P,Q)üY] € P|ÈQ|~X"	IS ForwardOrBackward ForwardCut 1 "P|È(P,Q)üY, P<|<X>Y € P|ÈQ|~X"
	SEPARATOR
	ENTRY "P<|(...,X,...) € P<|X" 				IS ForwardOrBackward ForwardCut 0 "P<|(...,X,...) € P<|X"
	ENTRY "P<|<X>Y € P<|X" 				IS ForwardOrBackward ForwardCut 0 "P<|<X>Y € P<|X"
	ENTRY "P<|{X}K, [P|È(P,Q)ÍK] € P<|X"		IS ForwardOrBackward ForwardCut 1 "P|È(P,Q)ÍK, P<|{X}K € P<|X"
	ENTRY "P<|{X}K, [P|ÈPÿK] € P<|X"			IS ForwardOrBackward ForwardCut 1 "P|ÈPÿK, P<|{X}K € P<|X"
	ENTRY "P<|{X}K¯, [P|ÈQÿ K] € P<|X"		IS ForwardOrBackward ForwardCut 1 "P|ÈQÿ K, P<|{X}K¯ € P<|X"
	SEPARATOR
END

MENU "|~"
	ENTRY "P|ÈQ|~X, [P|È#X] € P|ÈQ|ÈX"			IS ForwardOrBackward ForwardCut 1 "P|È#X, P|ÈQ|~X € P|ÈQ|ÈX"
	SEPARATOR
	ENTRY "P|ÈQ|~(...,X,...) € P|ÈQ|~X"			IS ForwardOrBackward ForwardCut 0 "P|ÈQ|~(...,X,...) € P|ÈQ|~X"
	SEPARATOR
	ENTRY "P<|{X}K, [P|È(Q,P)ÍK] € P|ÈQ|~X"		IS ForwardOrBackward ForwardCut 1 "P|È(Q,P)ÍK, P<|{X}K € P|ÈQ|~X"
	ENTRY "P<|{X}K¯, [P|ÈQÿK] € P|ÈQ|~X"		IS ForwardOrBackward ForwardCut 1 "P|ÈQÿK, P<|{X}K¯ € P|ÈQ|~X"
	ENTRY "P<|<X>Y, [P|È(P,Q)üY] € P|ÈQ|~X"		IS ForwardOrBackward ForwardCut 1 "P|È(P,Q)üY, P<|<X>Y € P|ÈQ|~X"
END

MENU "|€"
	ENTRY "P|ÈQ|€X, [P|ÈQ|ÈX] € P|ÈX"			IS ForwardOrBackward ForwardCut 0 "P|ÈQ|€X, P|ÈQ|ÈX € P|ÈX"
END

MENU "ü"
	ENTRY "P|È(P,Q)üY, [P<|<X>Y] € P|ÈQ|~X"		IS ForwardOrBackward ForwardCut 0 "P|È(P,Q)üY, P<|<X>Y € P|ÈQ|~X"
	SEPARATOR
	ENTRY "P|È(R,R')üK € P|È(R',R)üK"			IS ForwardOrBackward ForwardCut 0 "P|È(R,R')üK € P|È(R',R)üK"	
	ENTRY "P|ÈQ|È(R,R')üK € P|ÈQ|È(R',R)üK"		IS ForwardOrBackward ForwardCut 0 "P|ÈQ|È(R,R')üK € P|ÈQ|È(R',R)üK"
END

MENU "ÿ"
	ENTRY "P|ÈQÿK, [P<|{X}K¯] € P|ÈQ|~X"		IS ForwardOrBackward ForwardCut  0 "P|ÈQÿK, P<|{X}K¯ € P|ÈQ|~X"
	ENTRY "P|ÈPÿK, [P<|{X}K] € P<|X"			IS ForwardOrBackward ForwardCut 0 "P|ÈPÿK, P<|{X}K € P<|X"
	ENTRY "P|ÈQÿ K, [P<|{X}K¯] € P<|X"			IS ForwardOrBackward ForwardCut 0 "P|ÈQÿ K, P<|{X}K¯ € P<|X"
END

MENU "Í"
	ENTRY "P|È(Q,P)ÍK, [P<|{X}K] € P|ÈQ|~X"		IS ForwardOrBackward ForwardCut 0 "P|È(Q,P)ÍK, P<|{X}K € P|ÈQ|~X"
	ENTRY "P|È(P,Q)ÍK, [P<|{X}K] € P<|X"		IS ForwardOrBackward ForwardCut 0 "P|È(P,Q)ÍK, P<|{X}K € P<|X"
	SEPARATOR
	ENTRY "P|È(R,R')ÍK € P|È(R',R)ÍK"		IS ForwardOrBackward ForwardCut 0 "P|È(R,R')ÍK € P|È(R',R)ÍK"
	ENTRY "P|ÈQ|È(R,R')ÍK € P|ÈQ|È(R',R)ÍK"	IS ForwardOrBackward ForwardCut 0 "P|ÈQ|È(R,R')ÍK € P|ÈQ|È(R',R)ÍK"
END

MENU "#"
	ENTRY "P|È#X, [P|ÈQ|~X] € P|ÈQ|ÈX"			IS ForwardOrBackward ForwardCut 0 "P|È#X, P|ÈQ|~X € P|ÈQ|ÈX"
	SEPARATOR
	ENTRY "P|È#X € P|È#(...,X,...)"				IS ForwardOrBackward ForwardCut 0 "P|È#X € P|È#(...,X,...)"
END

MENU Logic 
        ENTRY "P|ÈËx.X(x) € P|ÈX(Y)"				IS ForwardOrBackward ForwardCut 0 "P|ÈËx.X(x) € P|ÈX(Y)"
        SEPARATOR
	ENTRY hyp
END

AUTOMATCH hyp
