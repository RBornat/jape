/*	$Id$ */

TACTIC ForwardCutwithSubstSel(Rule) IS
	SEQ	cut 
		(WHEN	(LETSUBSTSEL _A Rule (WITHSUBSTSEL hyp))
				(JAPE (fail(please text-select one or more instances of a sub-formula)))
		)

MENU SetOps IS
	ENTRY "abstraction-I" IS FSSOB ForwardCutwithSubstSel "abstraction-I"
	ENTRY "§-I"
	ENTRY "=-I"
	ENTRY "ï-I(L)" IS FOB ForwardCut "ï-I(L)"
	ENTRY "ï-I(R)" IS FOB ForwardCut "ï-I(R)"
	ENTRY "ß-I"
	ENTRY "ø-I" IS FOB ForwardCut "ø-I"
	
	SEPARATOR
	
	ENTRY "abstraction-E" IS FOBSS ForwardCut "abstraction-E"
	ENTRY "§-E" IS FOB ForwardCut2 "§-E"
	ENTRY "=-E(L)" IS FOB ForwardCut "=-E(L)"
	ENTRY "=-E(R)" IS FOB ForwardCut "=-E(R)"
	ENTRY "ï-E" IS FOB ForwardUncut "ï-E"
	ENTRY "ß-E(L)" IS FOB ForwardCut "ß-E(L)"
	ENTRY "ß-E(R)" IS FOB ForwardCut "ß-E(R)"
	ENTRY "ø-E" IS FOB ForwardCut "ø-E"
	
	SEPARATOR
	
	ENTRY "¯-E" IS FOB ForwardCut "¯-E"
	ENTRY "AÚU"
END

TACTICPANEL "Definitions" IS
	RULE "AÂÚB ÷ Â(AÚB)" IS INFER AÂÚB ÷ Â(AÚB)
	RULE "AÚ{B} ÷ A=B" IS INFER AÚ{B} ÷ A=B
	RULE "¯ ÷ {}" IS INFER ¯ ÷ {}
	RULE "A§B ÷ (èy.yÚAçyÚB)"(OBJECT y) IS INFER A§B ÷ (èy.yÚAçyÚB)
	RULE "A=B ÷ (èy.yÚAêyÚB)"(OBJECT y) IS INFER A=B ÷ (èy.yÚAêyÚB)
	RULE "AïB ÷ { y | yÚAëyÚB }"(OBJECT y) IS INFER AïB ÷ { y | yÚAëyÚB }
	RULE "AßB ÷ { y | yÚA¦yÚB }"(OBJECT y) IS INFER AßB ÷ { y | yÚA¦yÚB }
	RULE "A-B ÷ { y | yÚA¦yÂÚB }"(OBJECT y) IS INFER A-B ÷ { y | yÚA¦yÂÚB }
	RULE "Aø ÷ {y | yÂÚA}"(OBJECT y) IS INFER Aø ÷ {y | yÂÚA}
	RULE "ïï(C) ÷ { x | äy. xÚy¦yÚC }"(OBJECT x, OBJECT y) IS INFER ïï(C) ÷ { x | äy. xÚy¦yÚC }
	RULE "ßß(C) ÷ { x | èy. yÚCçxÚy }"(OBJECT x, OBJECT y) IS INFER ßß(C) ÷ { x | èy. yÚCçxÚy }
END
