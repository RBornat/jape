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
   
MENU "|=-"
	SEPARATOR
	ENTRY "P|=-X,  P|=-Y,  ... Û P|=-(X,Y,...)"
	ENTRY "P|=-(...,X,...) Û P|=-X"				IS ForwardOrBackward ForwardCut 0 "P|=-(...,X,...) Û P|=-X"
	ENTRY "P|=-Q|=-(...,X,...) Û P|=-Q|=-X"			IS ForwardOrBackward ForwardCut 0 "P|=-Q|=-(...,X,...) Û P|=-Q|=-X"
	SEPARATOR
	ENTRY "P|=-èx.X(x) Û P|=-X(Y)"			IS ForwardOrBackward ForwardCut 0 "P|=-èx.X(x) Û P|=-X(Y)"
	ENTRY "P|=-Q|~X, [P|=-#X] Û P|=-Q|=-X"			IS ForwardOrBackward ForwardCut 1 "P|=-#X, P|=-Q|~X Û P|=-Q|=-X"
	ENTRY "P|=-Q|=-X, [P|=-Q|ÛX] Û P|=-X"			IS ForwardOrBackward ForwardCut 1 "P|=-Q|ÛX, P|=-Q|=-X Û P|=-X"
	SEPARATOR
	ENTRY "P|=-#X, [P|=-Q|~X] Û P|=-Q|=-X"			IS ForwardOrBackward ForwardCut 0 "P|=-#X, P|=-Q|~X Û P|=-Q|=-X"
	ENTRY "P|=-Q|ÛX, [P|=-Q|=-X] Û P|=-X"			IS ForwardOrBackward ForwardCut 0 "P|=-Q|ÛX, P|=-Q|=-X Û P|=-X"
	ENTRY "P|=-#X, [P|=-Q|~X] Û P|=-Q|=-X"			IS ForwardOrBackward ForwardCut 0 "P|=-#X, P|=-Q|~X Û P|=-Q|=-X"
	ENTRY "P|=-Q|~X, [P|=-#X] Û P|=-Q|=-X"			IS ForwardOrBackward ForwardCut 1 "P|=-#X, P|=-Q|~X Û P|=-Q|=-X"
END

MENU "<|"
	ENTRY "P<|{X}K, [P|=-(Q,P)êK] Û P|=-Q|~X"	IS ForwardOrBackward ForwardCut 1 "P|=-(Q,P)êK, P<|{X}K Û P|=-Q|~X"
	ENTRY "P<|{X}Kø, [P|=-QØK] Û P|=-Q|~X"		IS ForwardOrBackward ForwardCut 1 "P|=-QØK, P<|{X}Kø Û P|=-Q|~X"
	ENTRY " P<|<X>Y, [P|=-(P,Q)ŸY] Û P|=-Q|~X"	IS ForwardOrBackward ForwardCut 1 "P|=-(P,Q)ŸY, P<|<X>Y Û P|=-Q|~X"
	SEPARATOR
	ENTRY "P<|(...,X,...) Û P<|X" 				IS ForwardOrBackward ForwardCut 0 "P<|(...,X,...) Û P<|X"
	ENTRY "P<|<X>Y Û P<|X" 				IS ForwardOrBackward ForwardCut 0 "P<|<X>Y Û P<|X"
	ENTRY "P<|{X}K, [P|=-(P,Q)êK] Û P<|X"		IS ForwardOrBackward ForwardCut 1 "P|=-(P,Q)êK, P<|{X}K Û P<|X"
	ENTRY "P<|{X}K, [P|=-PØK] Û P<|X"			IS ForwardOrBackward ForwardCut 1 "P|=-PØK, P<|{X}K Û P<|X"
	ENTRY "P<|{X}Kø, [P|=-QØ K] Û P<|X"		IS ForwardOrBackward ForwardCut 1 "P|=-QØ K, P<|{X}Kø Û P<|X"
	SEPARATOR
END

MENU "|~"
	ENTRY "P|=-Q|~X, [P|=-#X] Û P|=-Q|=-X"			IS ForwardOrBackward ForwardCut 1 "P|=-#X, P|=-Q|~X Û P|=-Q|=-X"
	SEPARATOR
	ENTRY "P|=-Q|~(...,X,...) Û P|=-Q|~X"			IS ForwardOrBackward ForwardCut 0 "P|=-Q|~(...,X,...) Û P|=-Q|~X"
	SEPARATOR
	ENTRY "P<|{X}K, [P|=-(Q,P)êK] Û P|=-Q|~X"		IS ForwardOrBackward ForwardCut 1 "P|=-(Q,P)êK, P<|{X}K Û P|=-Q|~X"
	ENTRY "P<|{X}Kø, [P|=-QØK] Û P|=-Q|~X"		IS ForwardOrBackward ForwardCut 1 "P|=-QØK, P<|{X}Kø Û P|=-Q|~X"
	ENTRY "P<|<X>Y, [P|=-(P,Q)ŸY] Û P|=-Q|~X"		IS ForwardOrBackward ForwardCut 1 "P|=-(P,Q)ŸY, P<|<X>Y Û P|=-Q|~X"
END

MENU "|Û"
	ENTRY "P|=-Q|ÛX, [P|=-Q|=-X] Û P|=-X"			IS ForwardOrBackward ForwardCut 0 "P|=-Q|ÛX, P|=-Q|=-X Û P|=-X"
END

MENU "Ÿ"
	ENTRY "P|=-(P,Q)ŸY, [P<|<X>Y] Û P|=-Q|~X"		IS ForwardOrBackward ForwardCut 0 "P|=-(P,Q)ŸY, P<|<X>Y Û P|=-Q|~X"
	SEPARATOR
	ENTRY "P|=-(R,R')ŸK Û P|=-(R',R)ŸK"			IS ForwardOrBackward ForwardCut 0 "P|=-(R,R')ŸK Û P|=-(R',R)ŸK"	
	ENTRY "P|=-Q|=-(R,R')ŸK Û P|=-Q|=-(R',R)ŸK"		IS ForwardOrBackward ForwardCut 0 "P|=-Q|=-(R,R')ŸK Û P|=-Q|=-(R',R)ŸK"
END

MENU "Ø"
	ENTRY "P|=-QØK, [P<|{X}Kø] Û P|=-Q|~X"		IS ForwardOrBackward ForwardCut  0 "P|=-QØK, P<|{X}Kø Û P|=-Q|~X"
	ENTRY "P|=-PØK, [P<|{X}K] Û P<|X"			IS ForwardOrBackward ForwardCut 0 "P|=-PØK, P<|{X}K Û P<|X"
	ENTRY "P|=-QØ K, [P<|{X}Kø] Û P<|X"			IS ForwardOrBackward ForwardCut 0 "P|=-QØ K, P<|{X}Kø Û P<|X"
END

MENU "ê"
	ENTRY "P|=-(Q,P)êK, [P<|{X}K] Û P|=-Q|~X"		IS ForwardOrBackward ForwardCut 0 "P|=-(Q,P)êK, P<|{X}K Û P|=-Q|~X"
	ENTRY "P|=-(P,Q)êK, [P<|{X}K] Û P<|X"		IS ForwardOrBackward ForwardCut 0 "P|=-(P,Q)êK, P<|{X}K Û P<|X"
	SEPARATOR
	ENTRY "P|=-(R,R')êK Û P|=-(R',R)êK"		IS ForwardOrBackward ForwardCut 0 "P|=-(R,R')êK Û P|=-(R',R)êK"
	ENTRY "P|=-Q|=-(R,R')êK Û P|=-Q|=-(R,R')êK"	IS ForwardOrBackward ForwardCut 0 "P|=-Q|=-(R,R')êK Û P|=-Q|=-(R,R')êK"
END

MENU "#"
	ENTRY "P|=-#X, [P|=-Q|~X] Û P|=-Q|=-X"			IS ForwardOrBackward ForwardCut 0 "P|=-#X, P|=-Q|~X Û P|=-Q|=-X"
	SEPARATOR
	ENTRY "P|=-#X Û P|=-#(...,X,...)"				IS ForwardOrBackward ForwardCut 0 "P|=-#X Û P|=-#(...,X,...)"
END

MENU Logic 
        ENTRY "P|=-èx.X(x) Û P|=-X(Y)"				IS ForwardOrBackward ForwardCut 0 "P|=-èx.X(x) Û P|=-X(Y)"
        SEPARATOR
	ENTRY hyp
END

AUTOMATCH hyp
