/* $Id$ */

TACTIC ForwardCut (n,rule)
    SEQ cut (ForwardUncut n rule)

TACTIC ForwardUncut (n,rule)
    (LETGOALPATH G (WITHARGSEL rule) (GOALPATH (SUBGOAL G n)) (WITHHYPSEL hyp) (GOALPATH G) NEXTGOAL)

TACTIC ForwardOrBackward (Forward, n, rule) IS 
    WHEN    
        (LETHYP _P 
            (ALT    
                (Forward n rule)
                (WHEN   
                    (LETARGSEL _Q 
                        (Fail (rule is not applicable to assumption ' _P ' with argument ' _Q ')))
                    (Fail (rule is not applicable to assumption ' _P ')))))
        (ALT    
            (WITHSELECTIONS rule)
            (WHEN   
                (LETARGSEL _P
                    (Fail (rule is not applicable with argument ' _P ')))
                (Fail (rule is not applicable))))
    
MENU Rules IS
    ENTRY "→-I"
    ENTRY "∧-I"    
    ENTRY "∨-I(L)"  IS ForwardOrBackward ForwardCut 0 "∨-I(L)"
    ENTRY "∨-I(R)"  IS ForwardOrBackward ForwardCut 0 "∨-I(R)"
    ENTRY "¬-I"
    ENTRY "∀-I"
    ENTRY "∃-I"     IS "∃-I with side condition hidden"
    SEPARATOR
    ENTRY "→-E"     IS ForwardOrBackward ForwardCut 1 "→-E" 
    ENTRY "∧-E(L)"  IS ForwardOrBackward ForwardCut 0 "∧-E(L)"
    ENTRY "∧-E(R)"  IS ForwardOrBackward ForwardCut 0 "∧-E(R)"
    ENTRY "∨-E"     IS ForwardOrBackward ForwardUncut 0 "∨-E"   
    ENTRY "¬-E"     IS ForwardOrBackward ForwardCut 0 "¬-E" 
    ENTRY "∀-E"     IS ForwardOrBackward ForwardCut 0 "∀-E with side condition hidden"  
    ENTRY "∃-E"     IS ForwardOrBackward ForwardUncut 0 "∃-E"
    SEPARATOR
    ENTRY hyp
END
   
/* The next tactic is needed because we can't give an application of a tactical as an argument.
   * But we'd probably include it anyway, for clarity.
   */
TACTIC "∀-E with side condition hidden" IS LAYOUT "∀-E" (0) (WITHARGSEL "∀-E")
TACTIC "∃-I with side condition hidden" IS  LAYOUT "∃-I" (0) (WITHARGSEL "∃-I")

