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

SEQUENT IS FORMULA Ê FORMULA

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
	C Ê hd€(Ët.{t}Át)
AND	C Ê tl€(Ët.{t}Á{t})
AND	C Ê (‹)€(Ët.tÁ{t}Á{t})
AND	C Ê nil€(Ët.{t})
AND	C Ê (+)€^(numÁnumÁnum)
AND	C Ê (-)€^(numÁnumÁnum)
AND C Ê (=)€(Ët.tÁtÁbool)
END

RULE "Cºx€S Ê x€S" IS INFER Cºx€S Ê x€S
RULE "Cºy€... Ê  x€S" WHERE x NOTIN E IS FROM C Ê x€S INFER CºE€S1 Ê x€S 
TACTIC "C Ê x€S" IS ALT "Cºx€S Ê x€S" (SEQ "Cºy€... Ê  x€S" "C Ê x€S")

RULE "Cºc€S Ê c€S" IS INFER Cºc€S Ê c€S
RULE "Cºy€... Ê  c€S" WHERE c NOTIN E IS FROM C Ê c€S INFER CºE€S1 Ê c€S 
				/* NOTIN still needed in case E is unknown */
TACTIC "C Ê c€S" IS ALT "Cºc€S Ê c€S" (SEQ "Cºy€... Ê  c€S" "C Ê c€S")

RULE "C Ê x:T" IS FROM CÊx€S AND CÊS»T INFER CÊx:T
RULE "C Ê c:T" IS FROM CÊc€S AND CÊS»T INFER CÊc:T

RULES "S»T" ARE
	INFER C Ê ^T » T
AND	INFER C Ê (Ët.T) » T[t\T1]
AND	INFER C Ê (Ë(t1,t2).T) » T[t1,t2\T1,T2]
AND	INFER C Ê (Ë(t1,t2,t3).T) » T[t1,t2,t3\T1,T2,T3]
AND	INFER C Ê (Ë(t1,t2,t3,t4).T) » T[t1,t2,t3,t4\T1,T2,T3,T4]
END

MENU Rules IS	
	RULE "F G : T"		FROM C Ê F: T1ÁT2 AND C Ê G : T1 	INFER  C Ê F G : T2
	RULE "(˚x.E) : T1ÁT2"
					FROM Cºx€^T1 Ê E:T2 			INFER C Ê (˚x.E) : T1ÁT2
	RULE "(E,F) : T1ÙT2"	
					FROM C Ê E: T1 AND C Ê F: T2		INFER C Ê (E,F) : T1ÙT2
	RULE "if E then ET else EF fi : T"
		FROM C Ê E : bool AND C Ê ET : T AND C Ê EF : T		INFER C Ê if E then ET else EF fi : T
	ENTRY "let ... : T" IS letrules
	ENTRY "letrec ... : T" IS letrecrules
	
	TACTIC "x:T" IS
		LAYOUT "C(x)€S; S»T" () 
			(ALT	(SEQ "C Ê x:T" "C Ê x€S") 
				(SEQ "C Ê c:T" (ALT constants "C Ê c€S"))
				(WHEN
					(LETGOAL (_E:_T)
						(JAPE(fail(x:T can only be applied to either variables or constants: you chose _E)))
					)
					(LETGOAL _E (JAPE(fail(conclusion _E is not a ' formula:type ' judgement)))
					)
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
	ENTRY AutoStep
	
END
    
RULE "^T«S" IS			FROM C Ê T • () • Ts AND C Ê (T,Ts) • S		INFER C Ê ^T « S
RULE "T1ºT2«S1ºS2" IS	FROM C Ê T1«S1 AND C Ê T2«S2		INFER C Ê T1ºT2«S1ºS2

RULE "t•..." (OBJECT t) WHERE t NOTIN C AND t NOTIN Ts IS			INFER C Ê t • Ts • (t,Ts)
RULE "T1ÁT2•..."		FROM C Ê T1• Tsin • Tsmid AND C Ê T2 • Tsmid • Tsout	
														INFER C Ê T1ÁT2 • Tsin • Tsout
RULE "T1ÙT2•..."		FROM C Ê C Ê T1• Tsin • Tsmid AND C Ê T2 • Tsmid • Tsout	
														INFER C Ê T1Ù T2 • Tsin • Tsout
RULE "T•..."												INFER C Ê T • Ts • Ts

RULES"«" ARE
	INFER C Ê (T,()) • ^T
AND	INFER C Ê (T,(t,())) • (Ët.T)
AND INFER C Ê (T,(t2,(t1,()))) • (Ë(t1,t2).T)
AND INFER C Ê (T,(t3,(t2,(t1,())))) • (Ë(t1,t2,t3).T)
AND INFER C Ê (T,(t4,(t3,(t2,(t1,()))))) • (Ë(t1,t2,t3,t4).T)
END

TACTIC geninduct IS ALT "t•..." (SEQ (MATCH (ALT "T1ÁT2•..." "T1ÙT2•...")) geninduct geninduct) "T•..."

 TACTIC generalise IS
 	 LAYOUT "generalise %s" ()  
		(ALT	(SEQ "^T«S" geninduct "«")
			(SEQ "T1ºT2«S1ºS2" generalise generalise)
		)

TACTIC Auto IS
	WHEN	(LETGOAL (_x:_T) "x:T")
			(LETGOAL (_c:_T) 
				(ALT "x:T" "n:num" "s:string" "true:bool" "false:bool"
					(JAPE (fail (_c isn't a constant from the context, or one of the fixed constants))) 
				)
			)
			(LETGOAL (_F _G:_T) "F G : T" Auto Auto)
			(LETGOAL ((_E,_F):_T) "(E,F) : T1ÙT2" Auto Auto)
			(LETGOAL ((˚_x._E):_T) "(˚x.E) : T1ÁT2" Auto)
			(LETGOAL (if _E then _ET else _EF fi:_T) "if E then ET else EF fi : T" Auto Auto Auto)
			(LETGOAL (let _x=_E in _F end:_T) letrules Auto generalise Auto)
			(LETGOAL (let _x1=_E1 and _x2=_E2 in _F end:_T) letrules Auto generalise Auto)
			(LETGOAL (let _x1=_E1 and _x2=_E2 and _x3=E3 in _F end:_T) letrules Auto generalise Auto)
			(LETGOAL (let _x1=_E1 and _x2=_E2 and _x3=E3 and _x4=_E4 in _F end:_T) letrules Auto generalise Auto)
			(LETGOAL (letrec _x=_E in _F end:_T) letrecrules Auto generalise Auto)
			(LETGOAL (letrec _x1=_E1 and _x2=_E2 in _F end:_T) letrecrules Auto generalise Auto)
			(LETGOAL (letrec _x1=_E1 and _x2=_E2 and _x3=E3 in _F end:_T) letrecrules Auto generalise Auto)
			(LETGOAL (letrec _x1=_E1 and _x2=_E2 and _x3=E3 and _x4=_E4 in _F end:_T) letrecrules Auto generalise Auto)
			(LETGOAL (_E:_T) (JAPE (fail (_E is not a recognisable program formula (Auto)))))
			(LETGOAL _E (JAPE (fail (_E is not a recognisable judgement (Auto)))))
			
TACTIC AutoStep IS
	WHEN	(LETGOAL (_x:_T) "x:T")
			(LETGOAL (_c:_T) 
				(ALT "x:T" "n:num" "s:string" "true:bool" "false:bool"
					(JAPE (fail (_c isn't a constant from the context, or one of the fixed constants))) 
				)
			)
			(LETGOAL (_F _G:_T) "F G : T")
			(LETGOAL ((_E,_F):_T) "(E,F) : T1ÙT2")
			(LETGOAL ((˚_x._E):_T) "(˚x.E) : T1ÁT2")
			(LETGOAL (if _E then _ET else _EF fi:_T) "if E then ET else EF fi : T")
			(LETGOAL (let _x=_E in _F end:_T) letrules)
			(LETGOAL (let _x1=_E1 and _x2=_E2 in _F end:_T) letrules)
			(LETGOAL (let _x1=_E1 and _x2=_E2 and _x3=E3 in _F end:_T) letrules)
			(LETGOAL (let _x1=_E1 and _x2=_E2 and _x3=E3 and _x4=_E4 in _F end:_T) letrules)
			(LETGOAL (letrec _x=_E in _F end:_T) letrecrules)
			(LETGOAL (letrec _x1=_E1 and _x2=_E2 in _F end:_T) letrecrules)
			(LETGOAL (letrec _x1=_E1 and _x2=_E2 and _x3=E3 in _F end:_T) letrecrules)
			(LETGOAL (letrec _x1=_E1 and _x2=_E2 and _x3=E3 and _x4=_E4 in _F end:_T) letrecrules)
			(LETGOAL (_E:_T) (JAPE (fail (_E is not a recognisable program formula (AutoStep)))))
			(LETGOAL _E (JAPE (fail (_E is not a recognisable judgement (AutoStep)))))
			
AUTOUNIFY "n:num", "s:string", "true:bool", "false:bool"
