/* $Id$ */

FONTS "Konstanz"

CLASS VARIABLE x y z e f g map
CLASS FORMULA E F G
CLASS CONSTANT c
CONSTANT hd tl nil
CLASS NUMBER n
CLASS STRING s
CONSTANT true false

CLASS VARIABLE t
CLASS FORMULA S T /* we use T for types, S for type schemes in the rules which follow */
CONSTANT bool string num

/* operators for programs */

SUBSTFIX	500		{ E / x }
JUXTFIX	400
INFIX		150T 	Ù
INFIX		100R 	Á
LEFTFIX	75		Ë .
PREFIX	75		# /* now we have control over the priority of prefix operators ... */
INFIX		55L 		• ë
INFIX		50L   	:  € « »

INFIXC	140L		+ -
INFIXC	120R		‹
INFIXC	100L		== /* we need to use this because we also have let f = ... */
LEFTFIX	75		˚ .
INFIX		50L		=


BIND x  SCOPE E IN ˚ x . E

BIND t SCOPE T IN Ë t . T
BIND t1 t2 SCOPE T IN Ë (t1, t2) . T
BIND t1 t2 t3 SCOPE T IN Ë (t1, t2, t3 ). T
BIND t1 t2 t3 t4 SCOPE T IN Ë (t1, t2, t3, t4) . T

OUTFIX [ ]
OUTFIX  letrec in end
OUTFIX  let     in end
OUTFIX  if then else fi

BIND x  			SCOPE F	IN let x = E in F end
BIND x1 x2  		SCOPE F	IN let x1=E1 , x2=E2 in F end
BIND x1 x2 x3		SCOPE F	IN let x1=E1 , x2=E2 , x3=E3 in F end
BIND x1 x2 x3 x4	SCOPE F	IN let x1=E1 , x2=E2 , x3=E3 , x4=E4 in F end

BIND x			SCOPE E F			IN letrec x = E in F end
BIND x1 x2		SCOPE E1 E2  F		IN letrec x1=E1 , x2=E2 in F end
BIND x1 x2 x3		SCOPE E1 E2  E3 F	IN letrec x1=E1 , x2=E2 , x3=E3 in F end
BIND x1 x2 x3 x4	SCOPE E1 E2 E3 E4 F	IN letrec x1=E1 , x2=E2 , x3=E3 , x4=E4 in F end

CLASS LIST C
SEQUENT IS LIST Ê FORMULA

RULES letrules ARE
	FROM C Ê E : T1 
	AND C Ê T1«S1 
	AND C,x€S1 Ê F:T	
	INFER C Ê let x=E in F end : T
AND	FROM C Ê E1 : T1 AND C Ê E2 : T2 
	AND C Ê T1«S1 AND C Ê T2«S2 
	AND C,x1€S1,x2€S2 Ê F:T
	INFER C Ê let x1=E1 , x2=E2 in F end : T
AND	FROM C Ê E1 : T1 AND C Ê E2 : T2 AND C Ê E3 : T3
	AND C Ê T1«S1 AND C Ê T2«S2 AND C Ê T3«S3 
	AND C,x1€S1,x2€S2,x3€S3 Ê F:T
	INFER C Ê let x1=E1 , x2=E2 , x3=E3 in F end : T
AND	FROM C Ê E1 : T1 AND C Ê E2 : T2 AND C Ê E3 : T3 AND C Ê E4 : T4
	AND C Ê T1«S1 AND C Ê T2«S2 AND C Ê T3«S3 AND C Ê T4«S4
	AND C,x1€S1,x2€S2,x3€S3,x4€S4 Ê F:T
	INFER C Ê let x1=E1 , x2=E2 , x3=E3 , x4=E4 in F end : T
END

RULES letrecrules ARE
	FROM C,x€#T1 Ê E:T1 
	AND C Ê T1«S1 
	AND C,x€S1 Ê F:T	
	INFER C Ê letrec x=E in F end : T
AND	FROM C,x1€#T1,x2€#T2 Ê E1 : T1 AND C,x1€#T1,x2€#T2 Ê E2 : T2 
	AND C Ê T1«S1 AND C Ê T2«S2 
	AND C,x1€S1,x2€S2 Ê F:T	
	INFER C Ê letrec x1=E1 , x2=E2 in F end : T
AND	FROM C,x1€#T1,x2€#T2,x3€#T3 Ê E1 : T1 AND C,x1€#T1,x2€#T2,x3€#T3 Ê E2 : T2
	AND C,x1€#T1,x2€#T2,x3€#T3 Ê E3 : T3
	AND C Ê T1«S1 AND C Ê T2«S2 AND C Ê T3«S3 
	AND C,x1€S1,x2€S2,x3€S3 Ê F:T
	INFER C Ê letrec x1=E1 , x2=E2 , x3=E3 in F end : T
AND	FROM C,x1€#T1,x2€#T2,x3€#T3,x4€#T4 Ê E1 : T1 
	AND C,x1€#T1,x2€#T2,x3€#T3,x4€#T4 Ê E2 : T2
	AND C,x1€#T1,x2€#T2,x3€#T3,x4€#T4 Ê E3 : T3 
	AND C,x1€#T1,x2€#T2,x3€#T3,x4€#T4 Ê E4 : T4
	AND C Ê T1«S1 AND C Ê T2«S2 AND C Ê T3«S3 AND C Ê T4«S4
	AND C,x1€S1,x2€S2,x3€S3,x4€S4 Ê F:T
	INFER C Ê letrec x1=E1 , x2=E2 , x3=E3 , x4=E4 in F end : T
END

RULES constants ARE
	C Ê hd€Ëtt.[tt]Átt
AND	C Ê tl€Ëtt.[tt]Á[tt]
AND	C Ê (‹)€Ëtt.ttÁ[tt]Á[tt]
AND	C Ê nil€Ëtt.[tt]
AND	C Ê (+)€#numÁnumÁnum
AND	C Ê (-)€#numÁnumÁnum
AND C Ê (==)€Ëtt.ttÁttÁbool
END

RULE "C Ê x€S" WHERE x IN x€S' NOTONEOF C' IS INFER C,x€S,C' Ê x€S
RULE "C Ê c€S" WHERE c IN c€S' NOTONEOF C' IS INFER C,c€S,C' Ê c€S

IDENTITY "C Ê x€S"
IDENTITY "C Ê c€S"

RULE "C Ê x:T" IS FROM CÊx€S AND S»T INFER CÊx:T
RULE "C Ê c:T" IS FROM CÊc€S AND S»T INFER CÊc:T

RULES "S»T" ARE
	INFER #T » T
AND	INFER Ëtt.TT » TT{T1/tt}
AND	INFER Ë(tt1,tt2).TT » TT{T1,T2/tt1,tt2}
AND	INFER Ë(tt1,tt2,tt3).TT » TT{T1,T2,T3/tt1,tt2,tt3}
AND	INFER Ë(tt1,tt2,tt3,tt4).TT » TT{T1,T2,T3,T4/tt1,tt2,tt3,tt4}
END

/* a sort of weakening ... */
RULE weaken WHERE y NOTIN E IS FROM C Ê E:T INFER C,y€S Ê E:T

MENU Rules IS	
	RULE "F G : T"			FROM C Ê F: T1ÁT2 AND C Ê G : T1 	INFER  C Ê F G : T2
	RULE "˚x.E : T1ÁT2"		FROM C,x€#T1 Ê E:T2 			INFER C Ê ˚x.E : T1ÁT2
	RULE "(E,F) : T1ÙT2"		FROM C Ê E: T1 AND C Ê F: T2		INFER C Ê (E,F) : T1ÙT2
	RULE "if E then ET else EF fi : T"
		FROM C Ê E : bool AND C Ê ET : T AND C Ê EF : T			INFER C Ê if E then ET else EF fi : T
	ENTRY "let ... : T" IS letrules
	ENTRY "letrec ... : T" IS letrecrules
	
	TACTIC "x:T" IS
		SEQ
			(ALT	(LAYOUT "C(x)€S; S»T" () "C Ê x:T" "C Ê x€S") 
				(LAYOUT "C(c)€S; S»T" ()  "C Ê c:T" "C Ê c€S")
				(LAYOUT "constant" () "C Ê c:T" constants)
				(WHEN
					(LETGOAL (_E:_T)
						(Fail (x:T can only be applied to either variables or constants: you chose _E))
					)
					(LETGOAL _E (Fail (conclusion _E is not a ' formula:type ' judgement))
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
	ENTRY genstep
	
	SEPARATOR
	
	ENTRY Auto
	ENTRY AutoStep
	
	SEPARATOR
	
	ENTRY weaken
	
END
    
RULE "T«S" IS		FROM C Ê T • #T ë S 	INFER C Ê T « S

RULES "new t•..." (OBJECT t1) WHERE t1 NOTIN C ARE
	CÊ t1 • #Të Ët1.T 
AND	CÊ t1 • Ëtt1.T ë Ë(tt1,t1).T 
AND	CÊ t1 • Ë(tt1,tt2).T ë Ë(tt1,tt2,t1).T 
AND	CÊ t1 • Ë(tt1,tt2,tt3).T ë Ë(tt1,tt2,tt3,t1).T 
END

RULE "T1ÁT2•..."	FROM C Ê T1• Sin ë Smid AND C Ê T2 • Smid ë Sout	INFER C Ê T1ÁT2 • Sin ë Sout
RULE "T1ÙT2•..."	FROM C Ê T1• Sin ë Smid AND C Ê T2 • Smid ë Sout	INFER C Ê T1ÙT2 • Sin ë Sout
RULE "[T]•..."		FROM C Ê T • Sin ë Sout						INFER C Ê [T] • Sin ë Sout
RULE "same T•..."											INFER C Ê T • S ë S


TACTIC geninduct IS 
	ALT	(SEQ (MATCH (ALT "T1ÁT2•..." "T1ÙT2•...")) geninduct geninduct) 
		(SEQ (MATCH "[T]•...") geninduct)
		"new t•..."
		"same T•..."

TACTIC generalise IS LAYOUT "generalise" ()  "T«S" geninduct
TACTIC genstep IS 
	ALT "T«S" 
		(MATCH "T1ÁT2•...") 
		(MATCH "T1ÙT2•...") 
		(MATCH "[T]•...") 
		"new t•..."
		"same T•..."

TACTIC Auto IS
	WHEN	(LETGOAL (_x:_T) "x:T")
			(LETGOAL (_c:_T) 
				(ALT "x:T" "n:num" "s:string" "true:bool" "false:bool"
					(Fail (_c isn't a constant from the context, or one of the fixed constants))
				)
			)
			(LETGOAL (_F _G:_T) "F G : T" Auto Auto)
			(LETGOAL ((_E,_F):_T) "(E,F) : T1ÙT2" Auto Auto)
			(LETGOAL ((˚_x._E):_T) "˚x.E : T1ÁT2" Auto)
			(LETGOAL (if _E then _ET else _EF fi:_T) "if E then ET else EF fi : T" Auto Auto Auto)
			(LETGOAL (let _x=_E in _F end:_T) 
				letrules Auto generalise Auto)
			(LETGOAL (let _x1=_E1 , _x2=_E2 in _F end:_T) 
				letrules Auto Auto generalise generalise Auto)
			(LETGOAL (let _x1=_E1 , _x2=_E2 , _x3=E3 in _F end:_T) 
				letrules Auto Auto Auto generalise generalise generalise Auto)
			(LETGOAL (let _x1=_E1 , _x2=_E2 , _x3=E3 , _x4=_E4 in _F end:_T) 
				letrules Auto Auto Auto Auto generalise generalise generalise generalise Auto)
			(LETGOAL (letrec _x=_E in _F end:_T) 
				letrecrules Auto generalise Auto)
			(LETGOAL (letrec _x1=_E1 , _x2=_E2 in _F end:_T) 
				letrecrules Auto Auto generalise generalise Auto)
			(LETGOAL (letrec _x1=_E1 , _x2=_E2 , _x3=E3 in _F end:_T) 
				letrecrules Auto Auto Auto generalise generalise generalise Auto)
			(LETGOAL (letrec _x1=_E1 , _x2=_E2 , _x3=E3 , _x4=_E4 in _F end:_T) 
				letrecrules Auto Auto Auto Auto generalise generalise generalise generalise Auto)
			(LETGOAL (_E:_T) (Fail (_E is not a recognisable program formula (Auto))))
			(LETGOAL (_T « _S) generalise)
			(LETGOAL _E (Fail (_E is not a recognisable judgement (Auto))))
			
TACTIC AutoStep IS
	WHEN	(LETGOAL (_x:_T) "x:T")
			(LETGOAL (_c:_T) 
				(ALT "x:T" "n:num" "s:string" "true:bool" "false:bool"
					(Fail (_c isn't a constant from the context, or one of the fixed constants))
				)
			)
			(LETGOAL (_F _G:_T) "F G : T")
			(LETGOAL ((_E,_F):_T) "(E,F) : T1ÙT2")
			(LETGOAL ((˚_x._E):_T) "˚x.E : T1ÁT2")
			(LETGOAL (if _E then _ET else _EF fi:_T) "if E then ET else EF fi : T")
			(LETGOAL (let _x=_E in _F end:_T) letrules)
			(LETGOAL (let _x1=_E1 , _x2=_E2 in _F end:_T) letrules)
			(LETGOAL (let _x1=_E1 , _x2=_E2 , _x3=E3 in _F end:_T) letrules)
			(LETGOAL (let _x1=_E1 , _x2=_E2 , _x3=E3 , _x4=_E4 in _F end:_T) letrules)
			(LETGOAL (letrec _x=_E in _F end:_T) letrecrules)
			(LETGOAL (letrec _x1=_E1 , _x2=_E2 in _F end:_T) letrecrules)
			(LETGOAL (letrec _x1=_E1 , _x2=_E2 , _x3=E3 in _F end:_T) letrecrules)
			(LETGOAL (letrec _x1=_E1 , _x2=_E2 , _x3=E3 , _x4=_E4 in _F end:_T) letrecrules)
			(LETGOAL (_E:_T) (Fail (_E is not a recognisable program formula (AutoStep))))
			(LETGOAL (_T « _S) generalise)
			(LETGOAL _E (Fail (_E is not a recognisable judgement (AutoStep))))
			
AUTOUNIFY "n:num", "s:string", "true:bool", "false:bool"
