/* $Id$ */

TACTIC FAIL(x) IS JAPE (fail x)

TACTIC ForwardCut (n,Rule)
  SEQ cut (WITHARGSEL Rule) (JAPE (SUBGOAL n)) (WITHHYPSEL hyp)

TACTIC ForwardUncut (n,Rule)
  SEQ (WITHARGSEL Rule) (JAPE (SUBGOAL n)) (WITHHYPSEL hyp)

TACTIC ForwardOrBackward (Forward, n, Rule)
	WHEN	(LETHYP 
				_X 
                          	(ALT	(Forward n Rule)
					(WHEN	(LETARGSEL _Y 
                                                              (FAIL (Rule is not applicable to assumption ' _X ' with argument ' _Y ' ))
							)
							(FAIL (Rule is not applicable to assumption ' _X ' ))
					)
                                )
			)
			(LETGOAL
				_X
				(ALT	(WITHSELECTIONS Rule)
	                   		(WHEN	(LETARGSEL _Y
	                                                	(FAIL (Rule is not applicable to conclusion ' _X ' with argument ' _Y ' ))
	                                              	)
							(FAIL (Rule is not applicable to conclusion ' _X ' ))
					)
	           		)
			)
   
MENU "Š"
	SEPARATOR
	ENTRY "PŠX, PŠY Û PŠ(X+Y)"
	ENTRY "PŠ(X+Y) Û PŠX"					IS ForwardOrBackward ForwardCut 0 "PŠ(X+Y) Û PŠX"
	ENTRY "PŠ(X+Y) Û PŠY"					IS ForwardOrBackward ForwardCut 0 "PŠ(X+Y) Û PŠY"
	ENTRY "PŠQŠ(X+Y) Û PŠQŠX"				IS ForwardOrBackward ForwardCut 0 "PŠQŠ(X+Y) Û PŠQŠX"
	ENTRY "PŠQŠ(X+Y) Û PŠQŠY"				IS ForwardOrBackward ForwardCut 0 "PŠQŠ(X+Y) Û PŠQŠY"
	SEPARATOR
	ENTRY "[PŠ#X], PŠQ•X Û PŠQŠX"			IS ForwardOrBackward ForwardCut 1 "PŠ#X, PŠQ•X Û PŠQŠX"
	ENTRY "[PŠQšX], PŠQŠX Û PŠX"			IS ForwardOrBackward ForwardCut 1 "PŠQšX, PŠQŠX Û PŠX"
	SEPARATOR
	ENTRY "PŠ#X, [PŠQ•X] Û PŠQŠX"			IS ForwardOrBackward ForwardCut 0 "PŠ#X, PŠQ•X Û PŠQŠX"
	ENTRY "PŠQšX, [PŠQŠX] Û PŠX"			IS ForwardOrBackward ForwardCut 0 "PŠQšX, PŠQŠX Û PŠX"
	ENTRY "PŠ#X, [PŠQ•X] Û PŠQŠX"			IS ForwardOrBackward ForwardCut 0 "PŠ#X, PŠQ•X Û PŠQŠX"
	ENTRY "[PŠ#X], PŠQ•X Û PŠQŠX"			IS ForwardOrBackward ForwardCut 1 "PŠ#X, PŠQ•X Û PŠQŠX"
END

MENU "‘"
	ENTRY "[PŠ(Q,P)êK], P‘{X}K Û PŠQ•X"		IS ForwardOrBackward ForwardCut 1 "PŠ(Q,P)êK, P‘{X}K Û PŠQ•X"
	ENTRY "[PŠQØK], P‘{X}Kø Û PŠQ•X"		IS ForwardOrBackward ForwardCut 1 "PŠQØK, P‘{X}Kø Û PŠQ•X"
	ENTRY "[PŠ(P,Q)ŸY], P‘<X>Y Û PŠQ•X"		IS ForwardOrBackward ForwardCut 1 "PŠ(P,Q)ŸY, P‘<X>Y Û PŠQ•X"
	SEPARATOR
	ENTRY "P‘(X+Y) Û P‘X" 					IS ForwardOrBackward ForwardCut 0 "P‘(X+Y) Û P‘X" 
	ENTRY "P‘(X+Y) Û P‘Y" 					IS ForwardOrBackward ForwardCut 0 "P‘(X+Y) Û P‘Y" 
	ENTRY "P‘<X>Y Û P‘X" 					IS ForwardOrBackward ForwardCut 0 "P‘<X>Y Û P‘X"
	ENTRY "[PŠ(P,Q)êK], P‘{X}K Û P‘X"		IS ForwardOrBackward ForwardCut 1 "PŠ(P,Q)êK, P‘{X}K Û P‘X"
	ENTRY "[PŠPØK], P‘{X}K Û P‘X"			IS ForwardOrBackward ForwardCut 1 "PŠPØK, P‘{X}K Û P‘X"
	ENTRY "[PŠQØ K], P‘{X}Kø Û P‘X"			IS ForwardOrBackward ForwardCut 1 "PŠQØ K, P‘{X}Kø Û P‘X"
	SEPARATOR
END

MENU "•"
	ENTRY "[PŠ#X], PŠQ•X Û PŠQŠX"			IS ForwardOrBackward ForwardCut 1 "PŠ#X, PŠQ•X Û PŠQŠX"
	SEPARATOR
	ENTRY "PŠQ•(X+Y) Û PŠQ•X"			IS ForwardOrBackward ForwardCut 0 "PŠQ•(X+Y) Û PŠQ•X"
	ENTRY "PŠQ•(X+Y) Û PŠQ•Y"			IS ForwardOrBackward ForwardCut 0 "PŠQ•(X+Y) Û PŠQ•Y"
	SEPARATOR
	ENTRY "[PŠ(Q,P)êK], P‘{X}K Û PŠQ•X"		IS ForwardOrBackward ForwardCut 1 "PŠ(Q,P)êK, P‘{X}K Û PŠQ•X"
	ENTRY "[PŠQØK], P‘{X}Kø Û PŠQ•X"		IS ForwardOrBackward ForwardCut 1 "PŠQØK, P‘{X}Kø Û PŠQ•X"
	ENTRY "[PŠ(P,Q)ŸY], P‘<X>Y Û PŠQ•X"		IS ForwardOrBackward ForwardCut 1 "PŠ(P,Q)ŸY, P‘<X>Y Û PŠQ•X"
END

MENU "š"
	ENTRY "PŠQšX, [PŠQŠX] Û PŠX"			IS ForwardOrBackward ForwardCut 0 "PŠQšX, PŠQŠX Û PŠX"
END

MENU "Ÿ"
	ENTRY "PŠ(P,Q)ŸY, [P‘<X>Y] Û PŠQ•X"		IS ForwardOrBackward ForwardCut 0 "PŠ(P,Q)ŸY, P‘<X>Y Û PŠQ•X"
	SEPARATOR
	ENTRY "PŠ(R,R')ŸK Û PŠ(R',R)ŸK"			IS ForwardOrBackward ForwardCut 0 "PŠ(R,R')ŸK Û PŠ(R',R)ŸK"	
	ENTRY "PŠQŠ(R,R')ŸK Û PŠQŠ(R',R)ŸK"		IS ForwardOrBackward ForwardCut 0 "PŠQŠ(R,R')ŸK Û PŠQŠ(R',R)ŸK"
END

MENU "Ø"
	ENTRY "PŠQØK, [P‘{X}Kø] Û PŠQ•X"		IS ForwardOrBackward ForwardCut  0 "PŠQØK, P‘{X}Kø Û PŠQ•X"
	ENTRY "PŠPØK, [P‘{X}K] Û P‘X"			IS ForwardOrBackward ForwardCut 0 "PŠPØK, P‘{X}K Û P‘X"
	ENTRY "PŠQØ K, [P‘{X}Kø] Û P‘X"			IS ForwardOrBackward ForwardCut 0 "PŠQØ K, P‘{X}Kø Û P‘X"
END

MENU "ê"
	ENTRY "PŠ(Q,P)êK, [P‘{X}K] Û PŠQ•X"		IS ForwardOrBackward ForwardCut 0 "PŠ(Q,P)êK, P‘{X}K Û PŠQ•X"
	ENTRY "PŠ(P,Q)êK, [P‘{X}K] Û P‘X"		IS ForwardOrBackward ForwardCut 0 "PŠ(P,Q)êK, P‘{X}K Û P‘X"
	SEPARATOR
	ENTRY "PŠ(R,R')êK Û PŠ(R',R)êK"		IS ForwardOrBackward ForwardCut 0 "PŠ(R,R')êK Û PŠ(R',R)êK"
	ENTRY "PŠQŠ(R,R')êK Û PŠQŠ(R,R')êK"	IS ForwardOrBackward ForwardCut 0 "PŠQŠ(R,R')êK Û PŠQŠ(R,R')êK"
END

MENU "#"
	ENTRY "PŠ#X, [PŠQ•X] Û PŠQŠX"			IS ForwardOrBackward ForwardCut 0 "PŠ#X, PŠQ•X Û PŠQŠX"
	SEPARATOR
	ENTRY "PŠ#X Û PŠ#(X+Y)"				IS ForwardOrBackward ForwardCut 0 "PŠ#X Û PŠ#(X+Y)"
	ENTRY "PŠ#Y Û PŠ#(X+Y)"				IS ForwardOrBackward ForwardCut 0 "PŠ#Y Û PŠ#(X+Y)"
END

MENU "+"
	ENTRY "PŠ(X+Y) Û PŠX"					IS ForwardOrBackward ForwardCut 0 "PŠ(X+Y) Û PŠX"
	ENTRY "PŠ(X+Y) Û PŠY"					IS ForwardOrBackward ForwardCut 0 "PŠ(X+Y) Û PŠY"
	ENTRY "PŠQŠ(X+Y) Û PŠQŠX"				IS ForwardOrBackward ForwardCut 0 "PŠQŠ(X+Y) Û PŠQŠX"
	ENTRY "PŠQŠ(X+Y) Û PŠQŠY"				IS ForwardOrBackward ForwardCut 0 "PŠQŠ(X+Y) Û PŠQŠY"
	ENTRY "P‘(X+Y) Û P‘X" 					IS ForwardOrBackward ForwardCut 0 "P‘(X+Y) Û P‘X" 
	ENTRY "P‘(X+Y) Û P‘Y" 					IS ForwardOrBackward ForwardCut 0 "P‘(X+Y) Û P‘Y" 
	SEPARATOR
	SEPARATOR
	ENTRY "PŠX, PŠY Û PŠ(X+Y)"
END

MENU Logic 
	ENTRY "èx.Y(x) Û Y(Z)"					IS ForwardOrBackward ForwardCut 0 "è-E"
	ENTRY hyp
END

AUTOMATCH hyp
