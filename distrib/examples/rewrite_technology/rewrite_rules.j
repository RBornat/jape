/* $Id$ */

/* I'd really like to do everything transitively ... */

INITIALISE hidetransitivity true
INITIALISE hidereflexivity true

RULE "transitive=" IS FROM A=B AND B=C INFER A=C
RULE "reflexive=" IS INFER A=A
RULE "symmetric=" IS FROM A=B INFER B=A

TRANSITIVE "transitive="
REFLEXIVE "reflexive="

RULE "symmetric≠" IS FROM A≠B INFER B≠A

RULE "symmetric≜" IS FROM A≜B INFER B≜A

/* in order to work, the rules used with this tactic have to be FROM .. something(B) INFER something (A) */
TACTIC ForwardSubstHiding (ruleL2R, ruleR2L, thm) IS
    WHEN    
        (LETHYPSUBSTSEL _E 
                (CUTIN (LAYOUT "" (1) ruleR2L thm (WITHSUBSTSEL hyp))))
        (LETCONCSUBSTSEL _E (LAYOUT "" (1) (WITHSUBSTSEL ruleL2R) thm))
        /* the "second argument" of the rewrite rules has to be B */
        (LETHYP _E (CUTIN (LAYOUT "" (1) (ruleR2L«_E/B») thm (WITHHYPSEL hyp))))
        (LETGOAL _E (LAYOUT "" (1) (ruleL2R«_E/A») thm))

RULE "rewrite="(A, OBJECT xx) IS FROM A=B AND E«B/xx» INFER E«A/xx»
RULE "rewrite1="(A, OBJECT xx) IS FROM A=B INFER E«A/xx»=E«B/xx»
RULE "rewrite≜"(A, OBJECT xx) IS FROM A≜B AND E«B/xx» INFER E«A/xx»

TACTIC rewriteByhypL2R (rewrite, label) IS
    WHEN 
        (LETCONCSUBSTSEL _E
            (WHEN
                (LETHYP (_A=_B)
                    (LAYOUT label)
                    (WITHSUBSTSEL rewrite)
                    (WITHHYPSEL hyp)
                    fstep)
                (LETHYP _A
                    (Fail ("hypothesis must be of the form A=B .. you selected %t", _A)))
                (Fail "no hypothesis selected")))
        (LETGOAL _E
            (WHEN
                (LETHYP (_xx=_B)
                    (LAYOUT label)
                    rewrite«_E,_xx,_B,_xx/E,A,B,xx»
                    (WITHHYPSEL hyp)
                    fstep)
                (LETHYP _A
                    (Fail ("If you don't make a text-selection in the conclusion, the \
                           \hypothesis must be of the form x=A .. you selected %t", _A)))
                (Fail "no hypothesis selected")))
        (Fail "Please select a conclusion to be rewritten")

TACTIC rewriteByhypR2L (rewrite, reverse, label) IS
    WHEN 
        (LETCONCSUBSTSEL _E
            (WHEN
                (LETHYP (_A=_B)
                    (LAYOUT label)
                    (WITHSUBSTSEL rewrite)
                    (LAYOUT HIDEROOT reverse)
                    (WITHHYPSEL hyp)
                    fstep)
                (LETHYP _A
                    (Fail ("hypothesis must be of the form A=B .. you selected %t", _A)))
                (Fail "no hypothesis selected")))
        (LETGOAL _E
            (WHEN
                (LETHYP (_B=_xx)
                    (LAYOUT label)
                    rewrite«_E,_xx,_B,_xx/E,A,B,xx»
                    (LAYOUT HIDEROOT reverse)
                    (WITHHYPSEL hyp)
                    fstep)
                (LETHYP _A
                    (Fail ("If you don't make a text-selection in the conclusion, the \
                           \hypothesis must be of the form A=x .. you selected %t", _A)))
                (Fail "no hypothesis selected")))
        (Fail "Please select a conclusion to be rewritten")

TACTIC byrewrite (rew) IS
    WHEN    
        (LETCONCSUBSTSEL (_E«_A/_xx») rew«_E,_A,_xx/E,A,xx»)
        (LETHYPSUBSTSEL (_E«_A/_xx») 
            (CUTIN 
                (LETGOALPATH G 
                    rew 
                    (GOALPATH (SUBGOAL G 1)) 
                    (WITHSUBSTSEL hyp) (GOALPATH G) NEXTGOAL)))
        (Fail "no selection")

TACTIC "byrewrite*" (rew) IS
    WHEN    
        (LETCONCSUBSTSEL (_E«_A/_xx») rew«_A/A»)
        (LETHYPSUBSTSEL (_E«_A/_xx») 
            (CUTIN 
                (LETGOALPATH G
                    rew«_A/B» 
                    (GOALPATH (SUBGOAL G 1)) (WITHHYPSEL hyp) 
                    (GOALPATH G) NEXTGOAL)))
        (LETARGSEL _A
            (WHEN   
                (LETHYP _E 
                    (CUTIN 
                        (LETGOALPATH G 
                            rew«_A/B» 
                            (GOALPATH (SUBGOAL G 1)) (WITHSUBSTSEL hyp) 
                            (GOALPATH G) NEXTGOAL)))
                rew«_A/A»))
        (Fail "no selection")

TACTIC rewritehypformula(stepname,rewrite,action,close) IS
    CUTIN 
        (LAYOUT stepname)
        rewrite 
        (LETGOALPATH G 
            action
            (GOALPATH (RIGHT G)) 
            close)

TACTIC rewriteL2R (rewrite, rev, tac) IS
    WHEN    
        (LETHYPSUBSTSEL _E 
            (rewritehypformula tac rewrite 
                (LAYOUT HIDEROOT rev (LAYOUT HIDEROOT tac)) 
                (WITHSUBSTSEL hyp)))
        (LETCONCSUBSTSEL _E (LAYOUT tac) (WITHSUBSTSEL rewrite) (LAYOUT HIDEROOT tac))
        (LETHYP _E 
            (rewritehypformula tac rewrite 
                (LAYOUT HIDEROOT rev«_E/A» (LAYOUT HIDEROOT tac))
                (WITHHYPSEL hyp)))
        (LETGOAL _E (LAYOUT tac) rewrite«_E/A» (LAYOUT HIDEROOT tac))

TACTIC rewriteR2L (rewrite, rev, tac) IS
    WHEN    
        (LETHYPSUBSTSEL _E 
            (rewritehypformula tac rewrite (LAYOUT HIDEROOT tac) (WITHSUBSTSEL hyp)))
        (LETCONCSUBSTSEL _E 
            (LAYOUT tac) (WITHSUBSTSEL rewrite) (LAYOUT HIDEROOT rev) (LAYOUT HIDEROOT tac))
        (LETHYP _E 
            (rewritehypformula tac (rewrite«_E/B») (LAYOUT HIDEROOT tac) (WITHHYPSEL hyp)))
        (LETGOAL _E 
            (LAYOUT tac) rewrite (LAYOUT HIDEROOT rev«_E/B») (LAYOUT HIDEROOT tac))

TACTIC symmetry(rule) IS
    WHEN 
        (LETHYP _E (CUTIN rule (WITHHYPSEL hyp)))
        (LAYOUT HIDEROOT rule)
        
