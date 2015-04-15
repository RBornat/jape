/*
        Tactics that support ''do what I probably mean'' interaction
        for the IFP presentation of logic.
*/

TACTIC Fail (x) IS SEQ (ALERT x) STOP

TACTIC ForwardCut (n,rule)
    CUTIN (ForwardUncut n rule)

TACTIC ForwardUncut (n,Rule)
    WHEN    
        (LETHYP _Ah 
            (LETGOALPATH G (WITHARGSEL Rule) (GOALPATH (SUBGOAL G n)) (WITHHYPSEL hyp) (GOALPATH G) NEXTGOAL))
        /* If LETHYP fails at this point, we had better have a singleton LHS.*/
        (LETLHS _Ah
            (LETGOALPATH G 
                (WITHARGSEL Rule) 
                (GOALPATH (SUBGOAL G n)) 
                (LETGOAL _Ag 
                    (ALT (UNIFY _Ag _Ah) 
                         (Fail ("Error in IFP Jape (can't unify lhs %t with rhs %t in ForwardUncut).", 
                                _Ah, _Ag)))
                    (ANY hyp)
                ) 
                (GOALPATH G) 
                NEXTGOAL))
        (Fail "Error in IFP Jape (ForwardUncut falls through).")

TACTIC TheoremForward (thm) IS CUTIN (ALT thm (RESOLVE thm))

TACTIC TheoremForwardOrBackward(thm) IS
    WHEN    
        (LETHYP _A 
            (ALT    (TheoremForward (WITHHYPSEL (WITHARGSEL thm)))))
        (LETHYPS _As
            (Fail ("At present IFP Jape can't deal with multiple hypothesis selections when applying theorems. Sorry.\
                    \\nCancel all but one of them and try again.")))
        (LETGOAL _A
            (ALT (WITHARGSEL thm) 
                (RESOLVE (WITHARGSEL thm)) 
                (TheoremForward (WITHARGSEL thm))
                (Fail   "Theorem application failed -- tell Bernard Sufrin")))
        (LETOPENSUBGOAL G _A 
            (Fail ("Error in IFP Jape (open subgoal in TheoremForwardOrBackward). Tell Bernard Sufrin.")))
        (LETOPENSUBGOALS _As
            (ALERT  ("There is more than one unproved conclusion in the proof. Please select one – \
                        \or select a hypothesis – to show \
                        \Jape where to apply the theorem.")
                    ("OK", STOP) 
                    ))
        (ALERT  "The proof is finished -- there are no unproved conclusions left."
                ("OK", STOP) 
                )

