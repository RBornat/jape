/* $Id$ */

FONTS "Konstanz"

CLASS VARIABLE x, y, z, e, f, g, map
CLASS FORMULA E, F, G, C
CLASS CONSTANT c
CONSTANT hd, tl, nil
CLASS NUMBER n
CLASS STRING s
CONSTANT true, false

LEFTFIX ˚ .
BIND x  SCOPE E IN ˚ x . E

CLASS VARIABLE t
CLASS FORMULA S, T /* we use T for types, S for type schemes in the rules which follow */
CONSTANT bool, string, num

LEFTFIX    Ë .
BIND t SCOPE T IN Ë t . T
BIND t1,t2 SCOPE T IN Ë (t1,t2) . T
BIND t1,t2,t3 SCOPE T IN Ë (t1,t2,t3) . T
BIND t1,t2,t3,t4 SCOPE T IN Ë (t1,t2,t3,t4) . T

PREFIX ^ /* how I wish we had control over the priority of prefix operators ... */

INFIX   30 30 and
INFIX	    40  40   º
INFIX    50  50   : , €, «, », •
INFIX   101 100 Á
INFIX   102 102 Ù
/* operators for programs */
INFIXC  103 103 =
INFIXC  115 115 +, -
INFIXC   114 113 ‹

OUTFIX { }
OUTFIX  letrec in end
OUTFIX  let     in end
OUTFIX  if then else fi

BIND x  			SCOPE F	IN let x = E in F end
BIND x1, x2  		SCOPE F	IN let x1=E1 and x2=E2 in F end
BIND x1,x2,x3		SCOPE F	IN let x1=E1 and x2=E2 and x3=E3 in F end
BIND x1,x2,x3,x4	SCOPE F	IN let x1=E1 and x2=E2 and x3=E3 and x4=E4 in F end

BIND x			SCOPE E, F			IN letrec x = E in F end
BIND x1,x2		SCOPE E1, E2, F		IN letrec x1=E1 and x2=E2 in F end
BIND x1,x2,x3		SCOPE E1, E2, E3, F		IN letrec x1=E1 and x2=E2 and x3=E3 in F end
BIND x1,x2,x3,x4	SCOPE E1, E2, E3, E4, F	IN letrec x1=E1 and x2=E2 and x3=E3 and x4=E4 in F end

SEQUENT IS BAG Ê FORMULA

RULES letrules ARE
	FROM C Ê E : T1 
	AND C Ê ^T1«S1 
	AND Cºx€S1 Ê F:T	
	INFER C Ê let x=E in F end : T
AND	FROM C Ê (E1,E2) : T1ÙT2 
	AND C Ê ^T1º^T2«S1ºS2 
	AND Cºx1€S1ºx2€S2 Ê F:T
	INFER C Ê let x1=E1 and x2=E2 in F end : T
AND	FROM C Ê (E1,(E2,E3)) : T1Ù(T2ÙT3) 
	AND C Ê ^T1º^T2º^T3«S1ºS2ºS3 
	AND Cºx1€S1ºx2€S2ºx3€S3 Ê F:T
	INFER C Ê let x1=E1 and x2=E2 and x3=E3 in F end : T
AND	FROM C Ê ((E1,E2),(E3,E4)) : (T1ÙT2)Ù(T3ÙT4) 
	AND C Ê ^T1º^T2º^T3º^T4«S1ºS2ºS3ºS4
	AND Cºx1€S1ºx2€S2ºx3€S3ºx4€S4 Ê F:T
	INFER C Ê let x1=E1 and x2=E2 and x3=E3 and x4=E4 in F end : T
END

RULES letrecrules ARE
	FROM Cºx€^T1 Ê E:T1 
	AND C Ê ^T1«S1 
	AND Cºx€S1 Ê F:T	
	INFER C Ê letrec x=E in F end : T
AND	FROM Cºx1€^T1ºx2€^T2 Ê (E1,E2) : T1ÙT2 
	AND C Ê ^T1º^T2«S1ºS2 
	AND Cºx1€S1ºx2€S2 Ê F:T	
	INFER C Ê letrec x1=E1 and x2=E2 in F end : T
AND	FROM Cºx1€^T1ºx2€^T2ºx3€^T3 Ê (E1,(E2,E3)) : T1Ù(T2ÙT3) 
	AND C Ê ^T1º^T2º^T3«S1ºS2ºS3 
	AND Cºx1€S1ºx2€S2ºx3€S3 Ê F:T
	INFER C Ê letrec x1=E1 and x2=E2 and x3=E3 in F end : T
AND	FROM Cºx1€^T1ºx2€^T2ºx3€^T3ºx4€^T4 Ê ((E1,E2),(E3,E4)) : (T1ÙT2)Ù(T3ÙT4) 
	AND C Ê ^T1º^T2º^T3º^T4«S1ºS2ºS3ºS4 
	AND Cºx1€S1ºx2€S2ºx3€S3ºx4€S4 Ê F:T
	INFER C Ê letrec x1=E1 and x2=E2 and x3=E3 and x4=E4 in F end : T
END

RULES constants ARE
	C(hd)€(Ët.{t}Át)
AND	C(tl)€(Ët.{t}Á{t})
AND	C(‹)€(Ët.tÁ{t}Á{t})
AND	C(nil)€(Ët.{t})
AND	C(+)€^(numÁnumÁnum)
AND	C(-)€^(numÁnumÁnum)
AND C(=)€(Ët.tÁtÁbool)
END

RULE "(Cºx€S) (x) € S" IS INFER (Cºx€S) (x) € S
RULE "(Cºy:…) (x)€S" WHERE x NOTIN E IS FROM C(x)€S INFER (CºE€S') (x) € S 
TACTIC "C(x)€S" IS ALT "(Cºx€S) (x) € S" (SEQ "(Cºy:…) (x)€S" "C(x)€S")

RULE "(Cºc€S) (c) € S" IS INFER (Cºc€S) (c) € S
RULE "(Cºy:…) (c)€S" WHERE x NOTIN E IS FROM C(c)€S INFER (CºE€S') (c) € S 
				/* NOTIN still needed in case E is unknown */
TACTIC "C(c)€S" IS ALT "(Cºc€S) (c) € S" (SEQ "(Cºy:…) (c)€S" "C(c)€S")

RULE "C Ê x:T" IS FROM C(x)€S AND S»T INFER CÊx:T
RULE "C Ê c:T" IS FROM C(c)€S AND S»T INFER CÊc:T

RULES "S»T" ARE
	INFER ^T » T
AND	INFER (Ët.T) » T[t\T']
AND	INFER (Ë(t1,t2).T) » T[t1,t2\T1,T2]
AND	INFER (Ë(t1,t2,t3).T) » T[t1,t2,t3\T1,T2,T3]
AND	INFER (Ë(t1,t2,t3,t4).T) » T[t1,t2,t3,t4\T1,T2,T3,T4]
END

MENU Rules IS	
	RULE "F G : T"		FROM C Ê F: TÁT' AND C Ê G : T 	INFER  C Ê F G : T'
	RULE "(˚x.E) : T1ÁT2"
					FROM Cºx€^T1 Ê E:T2 			INFER C Ê (˚x.E) : T1ÁT2
	RULE "(E,F) : TÙT'"	FROM C Ê E: T AND C Ê F: T'		INFER C Ê (E,F) : TÙT'
	RULE "if E then ET else EF fi : T"
		FROM C Ê E : bool AND C Ê ET : T AND C Ê EF : T		INFER C Ê if E then ET else EF fi : T
	ENTRY "let ... : T" IS letrules
	ENTRY "letrec ... : T" IS letrecrules
	
	TACTIC "x:T" IS
		LAYOUT "C(x)€S; S»T" () 
			(ALT	(SEQ "C Ê x:T" "C(x)€S") 
				(SEQ "C Ê c:T" (ALT constants "C(c)€S"))
				(LETGOAL (_E:_T)
					(JAPE(fail(x:T can only be applied to either variables or constants: you chose _E:_T)))
				)
				(LETGOAL _E
					(JAPE(fail(conclusion _E is not a ' formula:type ' judgement)))
				)
			) 
			"S»T"
	
	SEPARATOR
	
	RULE "n:num"				INFER C Ê n:num
	RULE "s:string"				INFER C Ê s:string
	RULE "true:bool"			INFER C Ê true:bool
	RULE "false:bool"			INFER C Ê false:bool
	
	SEPARATOR
	
	ENTRY generalise
	
	SEPARATOR
	
	ENTRY Auto
	
END
    
RULE "^T«S" IS			FROM T • () • Ts AND (T,Ts) • S		INFER ^T « S
RULE "^Tº^T'«SºS'" IS	FROM ^T«S AND ^T'«S'				INFER ^Tº^T'«SºS'

RULE "t•..." (OBJECT t) WHERE HYPFRESH t AND t NOTIN Ts IS		INFER t • Ts • (t,Ts)
RULE "T1ÁT2•..."		FROM T1• Ts • Ts' AND T2 • Ts' • Ts''	INFER T1ÁT2 • Ts • Ts''
RULE "T1ÙT2•..."		FROM T1• Ts • Ts' AND T2 • Ts' • Ts''	INFER T1Ù T2 • Ts • Ts''
RULE "T•..."											INFER T • Ts • Ts

RULES"«" ARE
	INFER (T,()) • ^T
AND	INFER (T,(t,())) • (Ët.T)
AND INFER (T,(t2,(t1,()))) • (Ë(t1,t2).T)
AND INFER (T,(t3,(t2,(t1,())))) • (Ë(t1,t2,t3).T)
AND INFER (T,(t4,(t3,(t2,(t1,()))))) • (Ë(t1,t2,t3,t4).T)
END

TACTIC geninduct IS ALT "t•..." (SEQ (MATCH (ALT "T1ÁT2•..." "T1ÙT2•...")) geninduct geninduct) "T•..."

 TACTIC generalise IS
 	 LAYOUT "generalise %s" ()  
		(ALT	(SEQ "^T«S" geninduct "«")
			(SEQ "^Tº^T'«SºS'" generalise generalise)
		)

TACTIC Auto IS
	ALT	"x:T"
		"n:num"
		"s:string"
		"true:bool"
		"false:bool"
		(SEQ "F G : T" Auto Auto)
		(SEQ "(E,F) : TÙT'" Auto Auto)
		(SEQ "(˚x.E) : T1ÁT2" Auto)
		(SEQ "if E then ET else EF fi : T" Auto Auto Auto)
		(SEQ letrules Auto generalise Auto)
		(SEQ letrecrules Auto generalise Auto)

AUTOUNIFY "n:num", "s:string", "true:bool", "false:bool"
