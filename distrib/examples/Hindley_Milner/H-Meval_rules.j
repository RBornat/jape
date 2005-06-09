/* $Id$ */

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

SUBSTFIX    500     { E / x }
JUXTFIX 400
INFIX       150T    ×
INFIX       100R    →
LEFTFIX 75      ∀ .
PREFIX  75      # /* now we have control over the priority of prefix operators ... */
INFIX       55L         • ◁
INFIX       50L     :  ⇒ ≺ ≻

INFIXC  140L        + -
INFIXC  120R        ::
INFIXC  100L        == /* we need to use this because we also have let f = ... */
LEFTFIX 75      λ .
INFIX       50L     =


BIND x  SCOPE E IN λ x . E

BIND t SCOPE T IN ∀ t . T
BIND t1 t2 SCOPE T IN ∀ (t1, t2) . T
BIND t1 t2 t3 SCOPE T IN ∀ (t1, t2, t3 ). T
BIND t1 t2 t3 t4 SCOPE T IN ∀ (t1, t2, t3, t4) . T

OUTFIX [ ]
OUTFIX  letrec in end
OUTFIX  let     in end
OUTFIX  if then else fi

BIND x              SCOPE F IN let x = E in F end
BIND x1 x2          SCOPE F IN let x1=E1 , x2=E2 in F end
BIND x1 x2 x3       SCOPE F IN let x1=E1 , x2=E2 , x3=E3 in F end
BIND x1 x2 x3 x4    SCOPE F IN let x1=E1 , x2=E2 , x3=E3 , x4=E4 in F end

BIND x          SCOPE E F           IN letrec x = E in F end
BIND x1 x2      SCOPE E1 E2  F      IN letrec x1=E1 , x2=E2 in F end
BIND x1 x2 x3       SCOPE E1 E2  E3 F   IN letrec x1=E1 , x2=E2 , x3=E3 in F end
BIND x1 x2 x3 x4    SCOPE E1 E2 E3 E4 F IN letrec x1=E1 , x2=E2 , x3=E3 , x4=E4 in F end

CLASS LIST C
SEQUENT IS LIST ⊢ FORMULA

RULES letrules ARE
    FROM C ⊢ E : T1 
    AND C ⊢ T1≺S1 
    AND C,x⇒S1 ⊢ F:T    
    INFER C ⊢ let x=E in F end : T
AND FROM C ⊢ E1 : T1 AND C ⊢ E2 : T2 
    AND C ⊢ T1≺S1 AND C ⊢ T2≺S2 
    AND C,x1⇒S1,x2⇒S2 ⊢ F:T
    INFER C ⊢ let x1=E1 , x2=E2 in F end : T
AND FROM C ⊢ E1 : T1 AND C ⊢ E2 : T2 AND C ⊢ E3 : T3
    AND C ⊢ T1≺S1 AND C ⊢ T2≺S2 AND C ⊢ T3≺S3 
    AND C,x1⇒S1,x2⇒S2,x3⇒S3 ⊢ F:T
    INFER C ⊢ let x1=E1 , x2=E2 , x3=E3 in F end : T
AND FROM C ⊢ E1 : T1 AND C ⊢ E2 : T2 AND C ⊢ E3 : T3 AND C ⊢ E4 : T4
    AND C ⊢ T1≺S1 AND C ⊢ T2≺S2 AND C ⊢ T3≺S3 AND C ⊢ T4≺S4
    AND C,x1⇒S1,x2⇒S2,x3⇒S3,x4⇒S4 ⊢ F:T
    INFER C ⊢ let x1=E1 , x2=E2 , x3=E3 , x4=E4 in F end : T
END

RULES letrecrules ARE
    FROM C,x⇒#T1 ⊢ E:T1 
    AND C ⊢ T1≺S1 
    AND C,x⇒S1 ⊢ F:T    
    INFER C ⊢ letrec x=E in F end : T
AND FROM C,x1⇒#T1,x2⇒#T2 ⊢ E1 : T1 AND C,x1⇒#T1,x2⇒#T2 ⊢ E2 : T2 
    AND C ⊢ T1≺S1 AND C ⊢ T2≺S2 
    AND C,x1⇒S1,x2⇒S2 ⊢ F:T 
    INFER C ⊢ letrec x1=E1 , x2=E2 in F end : T
AND FROM C,x1⇒#T1,x2⇒#T2,x3⇒#T3 ⊢ E1 : T1 AND C,x1⇒#T1,x2⇒#T2,x3⇒#T3 ⊢ E2 : T2
    AND C,x1⇒#T1,x2⇒#T2,x3⇒#T3 ⊢ E3 : T3
    AND C ⊢ T1≺S1 AND C ⊢ T2≺S2 AND C ⊢ T3≺S3 
    AND C,x1⇒S1,x2⇒S2,x3⇒S3 ⊢ F:T
    INFER C ⊢ letrec x1=E1 , x2=E2 , x3=E3 in F end : T
AND FROM C,x1⇒#T1,x2⇒#T2,x3⇒#T3,x4⇒#T4 ⊢ E1 : T1 
    AND C,x1⇒#T1,x2⇒#T2,x3⇒#T3,x4⇒#T4 ⊢ E2 : T2
    AND C,x1⇒#T1,x2⇒#T2,x3⇒#T3,x4⇒#T4 ⊢ E3 : T3 
    AND C,x1⇒#T1,x2⇒#T2,x3⇒#T3,x4⇒#T4 ⊢ E4 : T4
    AND C ⊢ T1≺S1 AND C ⊢ T2≺S2 AND C ⊢ T3≺S3 AND C ⊢ T4≺S4
    AND C,x1⇒S1,x2⇒S2,x3⇒S3,x4⇒S4 ⊢ F:T
    INFER C ⊢ letrec x1=E1 , x2=E2 , x3=E3 , x4=E4 in F end : T
END

RULES constants ARE
    C ⊢ hd⇒∀tt.[tt]→tt
AND C ⊢ tl⇒∀tt.[tt]→[tt]
AND C ⊢ (::)⇒∀tt.tt→[tt]→[tt]
AND C ⊢ nil⇒∀tt.[tt]
AND C ⊢ (+)⇒#num→num→num
AND C ⊢ (-)⇒#num→num→num
AND C ⊢ (==)⇒∀tt.tt→tt→bool
END

RULE "C ⊢ x⇒S" WHERE x IN x⇒S' NOTONEOF C' IS INFER C,x⇒S,C' ⊢ x⇒S
RULE "C ⊢ c⇒S" WHERE c IN c⇒S' NOTONEOF C' IS INFER C,c⇒S,C' ⊢ c⇒S

IDENTITY "C ⊢ x⇒S"
IDENTITY "C ⊢ c⇒S"

RULE "C ⊢ x:T" IS FROM C⊢x⇒S AND S≻T INFER C⊢x:T
RULE "C ⊢ c:T" IS FROM C⊢c⇒S AND S≻T INFER C⊢c:T

RULES "S≻T" ARE
    INFER #T ≻ T
AND INFER ∀tt.TT ≻ TT{T1/tt}
AND INFER ∀(tt1,tt2).TT ≻ TT{T1,T2/tt1,tt2}
AND INFER ∀(tt1,tt2,tt3).TT ≻ TT{T1,T2,T3/tt1,tt2,tt3}
AND INFER ∀(tt1,tt2,tt3,tt4).TT ≻ TT{T1,T2,T3,T4/tt1,tt2,tt3,tt4}
END

/* a sort of weakening ... */
RULE weaken WHERE y NOTIN E IS FROM C ⊢ E:T INFER C,y⇒S ⊢ E:T

MENU Rules IS   
    RULE "F G : T"          FROM C ⊢ F: T1→T2 AND C ⊢ G : T1    INFER  C ⊢ F G : T2
    RULE "λx.E : T1→T2"     FROM C,x⇒#T1 ⊢ E:T2             INFER C ⊢ λx.E : T1→T2
    RULE "(E,F) : T1×T2"        FROM C ⊢ E: T1 AND C ⊢ F: T2        INFER C ⊢ (E,F) : T1×T2
    RULE "if E then ET else EF fi : T"
        FROM C ⊢ E : bool AND C ⊢ ET : T AND C ⊢ EF : T         INFER C ⊢ if E then ET else EF fi : T
    ENTRY "let ... : T" IS letrules
    ENTRY "letrec ... : T" IS letrecrules
    
    TACTIC "x:T" IS
        SEQ
            (ALT (LAYOUT "C(x)⇒S; S≻T" () "C ⊢ x:T" "C ⊢ x⇒S") 
                 (LAYOUT "C(c)⇒S; S≻T" ()  "C ⊢ c:T" "C ⊢ c⇒S")
                 (LAYOUT "constant" () "C ⊢ c:T" constants)
                 (WHEN
                    (LETGOAL (_E:_T)
                        (Fail (x:T can only be applied to either variables or constants: you chose _E)))
                    (LETGOAL _E (Fail (conclusion _E is not a ' formula:type ' judgement))))) 
            "S≻T"
    
    SEPARATOR
    
    RULE "n:num"                INFER C ⊢ n:num
    RULE "s:string"             INFER C ⊢ s:string
    RULE "true:bool"            INFER C ⊢ true:bool
    RULE "false:bool"           INFER C ⊢ false:bool
    
    SEPARATOR
    
    ENTRY generalise
    ENTRY genstep
    
    SEPARATOR
    
    ENTRY Auto
    ENTRY AutoStep
    
    SEPARATOR
    
    ENTRY weaken
    
END
    
RULE "T≺S" IS       FROM C ⊢ T • #T ◁ S     INFER C ⊢ T ≺ S

RULES "new t•..." (OBJECT t1) WHERE t1 NOTIN C ARE
    C⊢ t1 • #T                ◁ ∀t1.T 
AND C⊢ t1 • ∀tt1.T           ◁ ∀(tt1,t1).T 
AND C⊢ t1 • ∀(tt1,tt2).T     ◁ ∀(tt1,tt2,t1).T 
AND C⊢ t1 • ∀(tt1,tt2,tt3).T ◁ ∀(tt1,tt2,tt3,t1).T 
END

RULE "T1→T2•..."    FROM C ⊢ T1• Sin ◁ Smid AND C ⊢ T2 • Smid ◁ Sout    INFER C ⊢ T1→T2 • Sin ◁ Sout
RULE "T1×T2•..."    FROM C ⊢ T1• Sin ◁ Smid AND C ⊢ T2 • Smid ◁ Sout    INFER C ⊢ T1×T2 • Sin ◁ Sout
RULE "[T]•..."       FROM C ⊢ T • Sin ◁ Sout                               INFER C ⊢ [T] • Sin ◁ Sout
RULE "same T•..."                                                            INFER C ⊢ T • S ◁ S


TACTIC geninduct IS 
    ALT (SEQ (MATCH (ALT "T1→T2•..." "T1×T2•...")) geninduct geninduct) 
        (SEQ (MATCH "[T]•...") geninduct)
        "new t•..."
        "same T•..."

TACTIC generalise IS LAYOUT "generalise" ()  "T≺S" geninduct

TACTIC genstep IS 
    ALT "T≺S" 
        (MATCH "T1→T2•...") 
        (MATCH "T1×T2•...") 
        (MATCH "[T]•...") 
        "new t•..."
        "same T•..."

TACTIC Auto IS
    WHEN    (LETGOAL (_x:_T) "x:T")
            (LETGOAL (_c:_T) 
                (ALT "x:T" "n:num" "s:string" "true:bool" "false:bool"
                    (Fail (_c isn't a constant from the context, or one of the fixed constants))
                )
            )
            (LETGOAL (_F _G:_T) "F G : T" Auto Auto)
            (LETGOAL ((_E,_F):_T) "(E,F) : T1×T2" Auto Auto)
            (LETGOAL ((λ_x._E):_T) "λx.E : T1→T2" Auto)
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
            (LETGOAL (_T ≺ _S) generalise)
            (LETGOAL _E (Fail (_E is not a recognisable judgement (Auto))))
            
TACTIC AutoStep IS
    WHEN    (LETGOAL (_x:_T) "x:T")
            (LETGOAL (_c:_T) 
                (ALT "x:T" "n:num" "s:string" "true:bool" "false:bool"
                    (Fail (_c isn't a constant from the context, or one of the fixed constants))
                )
            )
            (LETGOAL (_F _G:_T) "F G : T")
            (LETGOAL ((_E,_F):_T) "(E,F) : T1×T2")
            (LETGOAL ((λ_x._E):_T) "λx.E : T1→T2")
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
            (LETGOAL (_T ≺ _S) generalise)
            (LETGOAL _E (Fail (_E is not a recognisable judgement (AutoStep))))
            
AUTOUNIFY "n:num", "s:string", "true:bool", "false:bool"
