/*	$Id$ */

TACTIC ForwardCutwithSubstSel(n,Rule) IS
	SEQ	cut 
		(WHEN	(LETSUBSTSEL _A Rule (WITHSUBSTSEL hyp))
				(JAPE (fail(please text-select one or more instances of a sub-formula)))
		)

TACTIC "abstraction-I tac" IS FSSOB ForwardCutwithSubstSel 0 "abstraction-I"

TACTIC "abstraction-E tac" IS FOBSS ForwardCut 0 "abstraction-E"
TACTIC "§-E tac" IS FOB ForwardCut 1 "§-E"

MENU SetOps IS
	ENTRY "abstraction-I" IS "abstraction-I tac"
	ENTRY "=-I"
	ENTRY "§-I(c)"
	ENTRY "§-I(<c,d>)"
	ENTRY "ï-I(L)" IS FOB ForwardCut 0 "ï-I(L)"
	ENTRY "ï-I(R)" IS FOB ForwardCut 0 "ï-I(R)"
	ENTRY "ß-I"
	ENTRY "(-)-I"
	ENTRY "ø-I" IS FOB ForwardCut 0 "ø-I"
	
	SEPARATOR
	
	ENTRY "abstraction-E" IS "abstraction-E tac"
	ENTRY "§-E" IS "§-E tac"
	ENTRY "=-E(L)" IS FOB ForwardCut 0 "=-E(L)"
	ENTRY "=-E(R)" IS FOB ForwardCut 0 "=-E(R)"
	ENTRY "ï-E" IS FOB ForwardUncut 0 "ï-E"
	ENTRY "ß-E(L)" IS FOB ForwardCut 0 "ß-E(L)"
	ENTRY "ß-E(R)" IS FOB ForwardCut 0 "ß-E(R)"
	ENTRY "(-)-E(L)" IS FOB ForwardCut 0 "(-)-E(L)"
	ENTRY "(-)-E(R)" IS FOB ForwardCut 0 "(-)-E(R)"
	ENTRY "ø-E" IS FOB ForwardCut 0 "ø-E"
	
	SEPARATOR
	
	ENTRY "¯-E" IS FOB ForwardCut 0 "¯-E"
	ENTRY "AÚU"
END

TACTICPANEL "Definitions" IS
	RULE IS AÂÚB ÷ Â(AÚB)
	RULE IS ¯ ÷ {}
	RULE (OBJECT x) IS EQ ÷ {x|x=x}
	RULE (OBJECT x) IS {A} ÷ {x|x=A}
	RULE (OBJECT x) IS {A,B} ÷ {x|x=Aëx=B}
	RULE (OBJECT x) IS {A,B,C} ÷ {x|x=Aëx=Bëx=C}
	RULE (OBJECT x) IS {A,B,C,D} ÷ {x|x=Aëx=Bëx=Cëx=D}
	RULE (OBJECT y) IS A§B ÷ (èy.yÚAçyÚB)
	RULE (OBJECT y) IS A=B ÷ (èy.yÚAêyÚB)
	RULE (OBJECT y) IS AïB ÷ { y | yÚAëyÚB }
	RULE (OBJECT y) IS AßB ÷ { y | yÚA¦yÚB }
	RULE (OBJECT y) IS A-B ÷ { y | yÚA¦yÂÚB }
	RULE (OBJECT y) IS Aø ÷ {y | yÂÚA}
	RULE (OBJECT x, OBJECT y) IS ïï(C) ÷ { x | äy. xÚy¦yÚC }
	RULE (OBJECT x, OBJECT y) IS ßß(C) ÷ { x | èy. yÚCçxÚy }
	RULE (OBJECT x) IS Pow(A) ÷ { x | x§A }
	RULE (OBJECT x, OBJECT y) IS AôB ÷ { <x,y>  | xÚA¦yÚB }
	RULE (OBJECT x, OBJECT y, OBJECT z) IS A¥B ÷ { <x,z> | äy.<x,y>ÚA¦<y,z>ÚB }
END
