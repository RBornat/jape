INFIX 100R ↔ /* iff, same prio as → */

RULE "↔ elim"            IS FROM A ↔ B INFER (A→B)∧(B→A)
RULE "↔ intro"           IS FROM (A→B)∧(B→A) INFER A ↔ B

MENU "Backward"
    BEFOREENTRY "∧ intro"
    ENTRY   "↔ intro" IS BackwardOnlyC (QUOTE (_A↔_B)) 
                                        (Noarg (SEQ "↔ intro" fstep fstep) "↔ intro") "↔ intro" "A↔B"
END

