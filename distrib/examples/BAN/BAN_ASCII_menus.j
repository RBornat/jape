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
            (ALT    (Forward n Rule)
                (WHEN   
                    (LETARGSEL _Y 
                            (Fail (Rule is not applicable to assumption ' _X ' with argument ' _Y ' ))
                    )
                    (Fail (Rule is not applicable to assumption ' _X ' ))
                )
            )
        )
        (LETGOAL  _X
            (ALT    (WITHSELECTIONS Rule)
                            (WHEN   
                                (LETARGSEL _Y
                                                            (Fail (Rule is not applicable to conclusion ' _X ' with argument ' _Y ' ))
                                                    )
                    (Fail (Rule is not applicable to conclusion ' _X ' ))
                )
                    )
        )
   
MENU "|≡"
    SEPARATOR
    ENTRY "P|≡X,  P|≡Y,  ... ⇒ P|≡(X,Y,...)"
    ENTRY "P|≡(...,X,...) ⇒ P|≡X"             IS ForwardOrBackward ForwardCut 0 "P|≡(...,X,...) ⇒ P|≡X"
    ENTRY "P|≡Q|≡(...,X,...) ⇒ P|≡Q|≡X"           IS ForwardOrBackward ForwardCut 0 "P|≡Q|≡(...,X,...) ⇒ P|≡Q|≡X"
    SEPARATOR
    ENTRY "P|≡∀x.X(x) ⇒ P|≡X(Y)"            IS ForwardOrBackward ForwardCut 0 "P|≡∀x.X(x) ⇒ P|≡X(Y)"
    ENTRY "P|≡Q|~X, [P|≡#X] ⇒ P|≡Q|≡X"            IS ForwardOrBackward ForwardCut 1 "P|≡#X, P|≡Q|~X ⇒ P|≡Q|≡X"
    ENTRY "P|≡Q|≡X, [P|≡Q|⇒X] ⇒ P|≡X"           IS ForwardOrBackward ForwardCut 1 "P|≡Q|⇒X, P|≡Q|≡X ⇒ P|≡X"
    SEPARATOR
    ENTRY "P|≡#X, [P|≡Q|~X] ⇒ P|≡Q|≡X"            IS ForwardOrBackward ForwardCut 0 "P|≡#X, P|≡Q|~X ⇒ P|≡Q|≡X"
    ENTRY "P|≡Q|⇒X, [P|≡Q|≡X] ⇒ P|≡X"           IS ForwardOrBackward ForwardCut 0 "P|≡Q|⇒X, P|≡Q|≡X ⇒ P|≡X"
    ENTRY "P|≡#X, [P|≡Q|~X] ⇒ P|≡Q|≡X"            IS ForwardOrBackward ForwardCut 0 "P|≡#X, P|≡Q|~X ⇒ P|≡Q|≡X"
    ENTRY "P|≡Q|~X, [P|≡#X] ⇒ P|≡Q|≡X"            IS ForwardOrBackward ForwardCut 1 "P|≡#X, P|≡Q|~X ⇒ P|≡Q|≡X"
END

MENU "<|"
    ENTRY "P<|{X}K, [P|≡(Q,P)↔K] ⇒ P|≡Q|~X" IS ForwardOrBackward ForwardCut 1 "P|≡(Q,P)↔K, P<|{X}K ⇒ P|≡Q|~X"
    ENTRY "P<|{X}K⁻¹, [P|≡Q↦K] ⇒ P|≡Q|~X"        IS ForwardOrBackward ForwardCut 1 "P|≡Q↦K, P<|{X}K⁻¹ ⇒ P|≡Q|~X"
    ENTRY " P<|<X>Y, [P|≡(P,Q)⇌Y] ⇒ P|≡Q|~X"    IS ForwardOrBackward ForwardCut 1 "P|≡(P,Q)⇌Y, P<|<X>Y ⇒ P|≡Q|~X"
    SEPARATOR
    ENTRY "P<|(...,X,...) ⇒ P<|X"                 IS ForwardOrBackward ForwardCut 0 "P<|(...,X,...) ⇒ P<|X"
    ENTRY "P<|<X>Y ⇒ P<|X"                IS ForwardOrBackward ForwardCut 0 "P<|<X>Y ⇒ P<|X"
    ENTRY "P<|{X}K, [P|≡(P,Q)↔K] ⇒ P<|X"      IS ForwardOrBackward ForwardCut 1 "P|≡(P,Q)↔K, P<|{X}K ⇒ P<|X"
    ENTRY "P<|{X}K, [P|≡P↦K] ⇒ P<|X"          IS ForwardOrBackward ForwardCut 1 "P|≡P↦K, P<|{X}K ⇒ P<|X"
    ENTRY "P<|{X}K⁻¹, [P|≡Q↦ K] ⇒ P<|X"        IS ForwardOrBackward ForwardCut 1 "P|≡Q↦ K, P<|{X}K⁻¹ ⇒ P<|X"
    SEPARATOR
END

MENU "|~"
    ENTRY "P|≡Q|~X, [P|≡#X] ⇒ P|≡Q|≡X"            IS ForwardOrBackward ForwardCut 1 "P|≡#X, P|≡Q|~X ⇒ P|≡Q|≡X"
    SEPARATOR
    ENTRY "P|≡Q|~(...,X,...) ⇒ P|≡Q|~X"           IS ForwardOrBackward ForwardCut 0 "P|≡Q|~(...,X,...) ⇒ P|≡Q|~X"
    SEPARATOR
    ENTRY "P<|{X}K, [P|≡(Q,P)↔K] ⇒ P|≡Q|~X"     IS ForwardOrBackward ForwardCut 1 "P|≡(Q,P)↔K, P<|{X}K ⇒ P|≡Q|~X"
    ENTRY "P<|{X}K⁻¹, [P|≡Q↦K] ⇒ P|≡Q|~X"        IS ForwardOrBackward ForwardCut 1 "P|≡Q↦K, P<|{X}K⁻¹ ⇒ P|≡Q|~X"
    ENTRY "P<|<X>Y, [P|≡(P,Q)⇌Y] ⇒ P|≡Q|~X"     IS ForwardOrBackward ForwardCut 1 "P|≡(P,Q)⇌Y, P<|<X>Y ⇒ P|≡Q|~X"
END

MENU "|⇒"
    ENTRY "P|≡Q|⇒X, [P|≡Q|≡X] ⇒ P|≡X"           IS ForwardOrBackward ForwardCut 0 "P|≡Q|⇒X, P|≡Q|≡X ⇒ P|≡X"
END

MENU "⇌"
    ENTRY "P|≡(P,Q)⇌Y, [P<|<X>Y] ⇒ P|≡Q|~X"     IS ForwardOrBackward ForwardCut 0 "P|≡(P,Q)⇌Y, P<|<X>Y ⇒ P|≡Q|~X"
    SEPARATOR
    ENTRY "P|≡(R,R')⇌K ⇒ P|≡(R',R)⇌K"         IS ForwardOrBackward ForwardCut 0 "P|≡(R,R')⇌K ⇒ P|≡(R',R)⇌K" 
    ENTRY "P|≡Q|≡(R,R')⇌K ⇒ P|≡Q|≡(R',R)⇌K"       IS ForwardOrBackward ForwardCut 0 "P|≡Q|≡(R,R')⇌K ⇒ P|≡Q|≡(R',R)⇌K"
END

MENU "↦"
    ENTRY "P|≡Q↦K, [P<|{X}K⁻¹] ⇒ P|≡Q|~X"        IS ForwardOrBackward ForwardCut  0 "P|≡Q↦K, P<|{X}K⁻¹ ⇒ P|≡Q|~X"
    ENTRY "P|≡P↦K, [P<|{X}K] ⇒ P<|X"          IS ForwardOrBackward ForwardCut 0 "P|≡P↦K, P<|{X}K ⇒ P<|X"
    ENTRY "P|≡Q↦ K, [P<|{X}K⁻¹] ⇒ P<|X"            IS ForwardOrBackward ForwardCut 0 "P|≡Q↦ K, P<|{X}K⁻¹ ⇒ P<|X"
END

MENU "↔"
    ENTRY "P|≡(Q,P)↔K, [P<|{X}K] ⇒ P|≡Q|~X"     IS ForwardOrBackward ForwardCut 0 "P|≡(Q,P)↔K, P<|{X}K ⇒ P|≡Q|~X"
    ENTRY "P|≡(P,Q)↔K, [P<|{X}K] ⇒ P<|X"      IS ForwardOrBackward ForwardCut 0 "P|≡(P,Q)↔K, P<|{X}K ⇒ P<|X"
    SEPARATOR
    ENTRY "P|≡(R,R')↔K ⇒ P|≡(R',R)↔K"     IS ForwardOrBackward ForwardCut 0 "P|≡(R,R')↔K ⇒ P|≡(R',R)↔K"
    ENTRY "P|≡Q|≡(R,R')↔K ⇒ P|≡Q|≡(R',R)↔K"   IS ForwardOrBackward ForwardCut 0 "P|≡Q|≡(R,R')↔K ⇒ P|≡Q|≡(R',R)↔K"
END

MENU "#"
    ENTRY "P|≡#X, [P|≡Q|~X] ⇒ P|≡Q|≡X"            IS ForwardOrBackward ForwardCut 0 "P|≡#X, P|≡Q|~X ⇒ P|≡Q|≡X"
    SEPARATOR
    ENTRY "P|≡#X ⇒ P|≡#(...,X,...)"               IS ForwardOrBackward ForwardCut 0 "P|≡#X ⇒ P|≡#(...,X,...)"
END

MENU Logic 
        ENTRY "P|≡∀x.X(x) ⇒ P|≡X(Y)"                IS ForwardOrBackward ForwardCut 0 "P|≡∀x.X(x) ⇒ P|≡X(Y)"
        SEPARATOR
    ENTRY hyp
END

AUTOMATCH hyp
