/* $Id$ */

TACTIC Fail (x) IS SEQ (ALERT x) STOP

RULE cut(B) IS FROM B AND B ⊢ C INFER C
RULE thin(A) IS FROM C INFER A ⊢ C
RULE dup(A) IS FROM A,A ⊢ C INFER A ⊢ C

RULE "→-E"(A) IS FROM A AND A→B INFER B
TACTIC "→-E forward"(Z) IS
    WHEN (LETHYP (_A→_B) (ForwardCut 1 "→-E"))
         (LETHYP _A (ForwardCut 0 "→-E"))
         (Fail (what's this in "→-E" forward?))

TACTIC "↔-E forward"(rule) IS
    WHEN (LETHYP (_A↔_B) (ForwardCut 1 rule))
         (LETHYP _A (ForwardCut 0 rule))
         (Fail (what's this in rule forward?))

RULE "↔-E(L)"(B) IS FROM B AND A↔B INFER A
TACTIC "↔-E(L) forward"(Z) IS "↔-E forward" "↔-E(L)"

RULE "↔-E(R)"(A) IS FROM A AND A↔B INFER B
TACTIC "↔-E(R) forward"(Z) IS "↔-E forward" "↔-E(R)"

RULE "∧-E(L)"(B) IS FROM A ∧ B INFER A
RULE "∧-E(R)"(A) IS FROM A ∧ B INFER B
RULE "∨-E"(A,B) IS FROM A ∨ B AND A ⊢ C AND B ⊢ C INFER C
RULE "¬-E" IS FROM ¬¬A INFER A
RULE "⊥-E" IS FROM ⊥ INFER A

RULES "∀-E" ARE 
(c) FROM ∀x.A(x) AND c inscope INFER A(c)
AND FROM ∀(x,y).A(x,y) AND c inscope AND d inscope INFER A(c,d)
AND FROM ∀(x,y,z).A(x,y,z) AND c inscope AND d inscope AND e inscope INFER A(c,d,e)
AND FROM ∀(w,x,y,z).A(w,x,y,z) AND c inscope AND d inscope AND e inscope AND f inscope INFER A(c,d,e,f)
END

RULES "∃-E" ARE
    (OBJECT c) WHERE FRESH c AND c NOTIN ∃x.A(x) FROM ∃x.A(x) AND var c, A(c) ⊢ C INFER C
AND (OBJECT c,OBJECT d) WHERE FRESH c,d AND c,d NOTIN ∃(x,y).A(x,y) 
        FROM ∃(x,y).A(x,y) AND var c, var d, A(c,d) ⊢ C INFER C
AND (OBJECT c,OBJECT d,OBJECT e) WHERE FRESH c,d,e AND c,d,e NOTIN ∃(x,y,z).A(x,y,z) 
        FROM ∃(x,y,z).A(x,y,z) AND var c, var d, var e, A(c,d,e) ⊢ C INFER C
AND (OBJECT c,OBJECT d,OBJECT e,OBJECT f) WHERE FRESH c,d,e,f AND c,d,e,f NOTIN ∃(w,x,y,z).A(w,x,y,z) 
        FROM ∃(w,x,y,z).A(w,x,y,z) AND var c, var d, var e, var f, A(c,d,e,f) ⊢ C INFER C
END

RULES "∃!-E(∃)" ARE 
    FROM ∃!x.A(x)               INFER ∃x.A(x)
AND FROM ∃!(x,y).A(x,y)         INFER ∃(x,y).A(x,y)
AND FROM ∃!(x,y,z).A(x,y,z)     INFER ∃(x,y,z).A(x,y,z)
AND FROM ∃!(w,x,y,z).A(w,x,y,z)  INFER ∃(x,y,z).A(w,x,y,z)
END

RULES "∃!-E(∀∀)" ARE
    (OBJECT x1) FROM ∃!x.A(x) INFER ∀x.∀x1.A(x)∧A(x1)→x=x1
AND (OBJECT x1,OBJECT y1) FROM ∃!(x,y).A(x,y) INFER ∀(x,y).∀(x1,y1).A(x,y)∧A(x1,y1)→x=x1∧y=y1
AND (OBJECT x1,OBJECT y1,OBJECT z1) FROM ∃!(x,y,z).A(x,y,z) 
        INFER ∀(x,y,z).∀(x1,y1,z1).A(x,y,z)∧A(x1,y1,z1)→x=x1∧y=y1∧z=z1
AND (OBJECT w1,OBJECT x1,OBJECT y1,OBJECT z1) FROM ∃!(w,x,y,z).A(w,x,y,z) 
        INFER ∀(w,x,y,z).∀(w1,x1,y1,z1).A(w,x,y,z)∧A(w1,x1,y1,z1)→w=w1∧x=x1∧y=y1∧z=z1
END

TACTIC ForwardCut (n,Rule) IS 
    SEQ cut (ForwardUncut n Rule)

TACTIC ForwardUncut (n, Rule) IS
    (LETGOALPATH G (WITHCONTINUATION (WITHARGSEL Rule) (GOALPATH (SUBGOAL G n)) (WITHHYPSEL hyp)) (GOALPATH G) NEXTGOAL)

TACTIC FOB (Forward, n, Rule) IS 
    WHEN (LETHYP _P
             (ALT (Forward n Rule)
                  (WHEN (LETARGSEL _Q 
                             (Fail (Rule is not applicable to assumption ' _P ' 
                                                           with argument ' _Q ')))
                        (Fail (Rule is not applicable to assumption ' _P '))))) 
         (ALT (WITHSELECTIONS Rule)
              (WHEN (LETARGSEL _P (Fail (Rule is not applicable with argument ' _P ')))
                    (Fail (Rule is not applicable))))
   
/* we really need a case statement.  This is just a version of FOB, and there are many others ... */
TACTIC FOBSS (Forward, n, Rule) IS 
    WHEN (LETHYP _P
             (ALT (Forward n Rule)
                  (WHEN (LETARGSEL _Q 
                             (Fail (Rule is not applicable to assumption ' _P ' 
                                                            with argument ' _Q ')))
                         (Fail (Rule is not applicable to assumption ' _P '))))) 
         (LETCONCSUBSTSEL _P 
             (ALT (WITHSUBSTSEL (WITHHYPSEL Rule))
                  (LETGOAL _Q
                     (Fail (Rule is not applicable to conclusion ' _Q ' with substitution ' _P ')))))
         (ALT (WITHSELECTIONS Rule)
              (Fail (Rule is not applicable to that conclusion)))
   
TACTIC FSSOB (Forward, n, Rule) IS 
    WHEN (LETHYPSUBSTSEL _P (Forward n Rule)) 
         (ALT (WITHSELECTIONS Rule)
              (WHEN (LETARGSEL _P (Fail (Rule is not applicable with argument ' _P ')))
                    (Fail (Rule is not applicable))))
   
RULE "→-I"    IS FROM A ⊢ B INFER A→B
RULE "↔-I"    IS FROM A→B  AND B→A INFER A↔B
RULE "∧-I"    IS FROM A AND B INFER A ∧ B
RULE "∨-I(L)" IS FROM A INFER A ∨ B
RULE "∨-I(R)" IS FROM B INFER A ∨ B
RULE "¬-I"    IS FROM A ⊢ ⊥ INFER ¬A
RULE "⊥-I"    IS FROM P AND ¬P INFER ⊥
RULES "∀-I" ARE
    (OBJECT c) WHERE FRESH c FROM var c ⊢ A(c) INFER ∀x.A(x)
AND (OBJECT c, OBJECT d) WHERE FRESH c,d FROM var c, var d ⊢ A(c,d) INFER ∀(x,y).A(x,y)
AND (OBJECT c, OBJECT d, OBJECT e) WHERE FRESH c,d,e 
        FROM var c, var d, var e ⊢ A(c,d,e) INFER ∀(x,y,z).A(x,y,z)
AND (OBJECT c, OBJECT d, OBJECT e, OBJECT f) WHERE FRESH c,d,e,f 
        FROM var c, var d, var e, var f ⊢ A(c,d,e,f) INFER ∀(w,x,y,z).A(w,x,y,z)
END
RULES "∃-I"(B) ARE 
    FROM A(B) AND B inscope INFER ∃x.A(x)
AND FROM A(B,C) AND B inscope AND C inscope INFER ∃(x,y).A(x,y)
AND FROM A(B,C,D) AND B inscope AND C inscope AND D inscope INFER ∃(x,y,z).A(x,y,z)
AND FROM A(B,C,D,E) AND B inscope AND C inscope AND D inscope AND E inscope INFER ∃(w,x,y,z).A(w,x,y,z)
END
RULES "∃!-I" ARE
    (OBJECT c1,OBJECT c2) WHERE FRESH c1,c2 AND c1,c2 NOTIN ∃!x.A(x) 
        FROM ∃x.A(x) AND var c1, var c2, A(c1),A(c2) ⊢ c1=c2 INFER ∃!x.A(x)
AND (OBJECT c1,OBJECT c2, OBJECT d1,OBJECT d2) 
    WHERE FRESH c1,c2,d1,d2 AND c1,c2,d1,d2 NOTIN ∃!(x,y).A(x,y) 
        FROM ∃(x,y).A(x,y) AND var c1, var c2, var d1, var d2, A(c1,d1),A(c2,d2) ⊢ c1=c2∧d1=d2 
        INFER ∃!(x,y).A(x,y)
AND (OBJECT c1,OBJECT c2, OBJECT d1, OBJECT d2, OBJECT e1,OBJECT e2) 
    WHERE FRESH c1,c2,d1,d2,e1,e2 AND c1,c2,d1,d2,e1,e2 NOTIN ∃!(x,y,z).A(x,y,z) 
        FROM ∃(x,y,z).A(x,y,z) 
        AND var c1, var c2, var d1, var d2, var e1, var e2, A(c1,d1,e1),A(c2,d2,e2) ⊢ c1=c2∧d1=d2∧e1=e2 
        INFER ∃!(x,y,z).A(x,y,z)
AND (OBJECT c1,OBJECT c2, OBJECT d1, OBJECT d2, OBJECT e1,OBJECT e2, OBJECT f1,OBJECT f2) 
    WHERE FRESH c1,c2,d1,d2,e1,e2,f1,f2 AND c1,c2,d1,d2,e1,e2,f1,f2 NOTIN ∃!(w,x,y,z).A(w,x,y,z) 
        FROM ∃(w,x,y,z).A(w,x,y,z) 
        AND var c1, var c2, var d1, var d2, var e1, var e2, var f1, var f2, 
            A(c1,d1,e1,f1),A(c2,d2,e2,f2) ⊢ c1=c2∧d1=d2∧e1=e2∧f1=f2 
        INFER ∃!(w,x,y,z).A(w,x,y,z)
END

RULE "inscope"  INFER var x ⊢ x inscope
AUTOMATCH "inscope"

TACTIC "∀-E with side condition hidden" IS LAYOUT "∀-E" (0) (WITHARGSEL "∀-E")
TACTIC "∃-I with side condition hidden" IS LAYOUT "∃-I" (0) (WITHARGSEL "∃-I")

TACTIC "∃!-I tac" IS WITHARGSEL "∃!-I"
TACTIC "→-E tac"  IS FOB "→-E forward" 0 "→-E"
TACTIC "∨-E tac"  IS FOB ForwardUncut 0 "∨-E"    
TACTIC "∃-E tac"  IS FOB ForwardUncut 0 "∃-E"

TACTIC "∀-E tac"  IS FOBSS ForwardCut 0 "∀-E with side condition hidden"
TACTIC "∃-I tac"  IS "∃-I with side condition hidden"

RULE "A=A" IS INFER A=A
RULE hyp(A) IS INFER A ⊢ A

AUTOMATCH hyp

STRUCTURERULE IDENTITY    hyp
STRUCTURERULE CUT            cut
STRUCTURERULE WEAKEN     thin

/*  Because of the ForwardSubstHiding tactic, these all have argument A and "second argument" B */
RULE "rewrite ↔ «" (A) IS FROM A ↔ B AND P(B) INFER P(A)
RULE "rewrite ↔ »" (A) IS FROM B ↔A AND P(B) INFER P(A)
RULE "rewrite = «" (A) IS FROM A=B AND P(B) INFER P(A)
RULE "rewrite = »" (A) IS FROM B=A AND P(B) INFER P(A)
RULE "rewrite ≜ «" (A) IS FROM A≜B AND P(B) INFER P(A)
RULE "rewrite ≜ »" (A) IS FROM B≜A AND P(B) INFER P(A)
