/* $Id$ */

TACTIC ForwardSubst (ruleLR, ruleRL, pat) IS
    WHEN
        (LETHYPSUBSTSEL _P 
            cut
            (LETGOALPATH G
                ruleRL 
                (WHEN
                    (LETHYP _Q 
                        (ALT (WITHHYPSEL hyp) 
                             (Fail (the hypothesis you formula-selected wasn't a pat formula))))
                    (GOALPATH (SUBGOAL G 1))) 
                (WITHSUBSTSEL hyp)
                (GOALPATH G)
                NEXTGOAL))
        (LETCONCSUBSTSEL _P
            (WITHSUBSTSEL ruleLR)
            (WHEN   
                (LETHYP _Q 
                    (ALT (WITHHYPSEL hyp) 
                         (Fail (the hypothesis you formula-selected wasn't a pat formula))))
                SKIP))
        (Fail (please text-select one or more instances of a sub-formula to replace))

TACTIC ForwardSubstHiding (ruleLR, ruleRL, thm) IS
    WHEN    
        (LETHYPSUBSTSEL _P  cut (LAYOUT thm (1) ruleRL thm (WITHSUBSTSEL hyp)))
        (LETCONCSUBSTSEL _P (LAYOUT thm (1) (WITHSUBSTSEL ruleLR) thm))
        /* the "second argument" of the rewrite rules has to be B */
        (LETHYP _P cut (LAYOUT thm (1) (ruleRL[B\_P]) thm (WITHHYPSEL hyp)))
        (LETGOAL _P (LAYOUT thm (1) (ruleLR _P) thm))

MENU "System F´" IS
    ENTRY "→-I" 
    ENTRY "↔-I"
    ENTRY "∧-I" 
    ENTRY "∨-I(L)" IS FOB ForwardCut 0 "∨-I(L)"
    ENTRY "∨-I(R)" IS FOB ForwardCut 0 "∨-I(R)"
    ENTRY "¬-I"
    ENTRY "⊥-I"
    ENTRY "∀-I"
    ENTRY "∃-I" IS "∃-I tac"
    ENTRY "∃!-I" IS "∃!-I tac"
    
    SEPARATOR
    
    ENTRY "→-E"     IS "→-E tac" 
    ENTRY "↔-E(L)"  IS FOB "↔-E(L) forward" 0 "↔-E(L)" 
    ENTRY "↔-E(R)"  IS FOB "↔-E(R) forward" 0 "↔-E(R)" 
    ENTRY "∧-E(L)"  IS FOB ForwardCut 0 "∧-E(L)"
    ENTRY "∧-E(R)"  IS FOB ForwardCut 0 "∧-E(R)"
    ENTRY "∨-E"     IS "∨-E tac"    
    ENTRY "¬-E"     IS FOB ForwardCut 0 "¬-E"   
    ENTRY "⊥-E"     IS FOB ForwardCut 0 "⊥-E"   
    ENTRY "∀-E"     IS "∀-E tac"    
    ENTRY "∃-E"     IS "∃-E tac"
    ENTRY "∃!-E(∃)" IS FOB ForwardCut 0 "∃!-E(∃)"
    ENTRY "∃!-E(∀∀)"    IS FOB ForwardCut 0 "∃!-E(∀∀)"
    SEPARATOR
    ENTRY "A=A"
    ENTRY hyp       IS hyp
END

MENU "Substitution"
    ENTRY "A↔…"     IS ForwardSubst "rewrite ↔ «" "rewrite ↔ »" (↔)
    ENTRY "…↔B"     IS ForwardSubst "rewrite ↔ »" "rewrite ↔ «" (↔)
    ENTRY "A=…"     IS ForwardSubst "rewrite = «" "rewrite = »" (=)
    ENTRY "…=B"     IS ForwardSubst "rewrite = »" "rewrite = «" (=)
END

TACTICPANEL "Definitions" IS
    RULE INFER A≠B ≜ ¬(A=B)
    
    BUTTON "A≜…" IS apply ForwardSubstHiding "rewrite ≜ «" "rewrite ≜ »"  COMMAND
    BUTTON "…≜B" IS apply ForwardSubstHiding "rewrite ≜ »" "rewrite ≜ «"  COMMAND
END
