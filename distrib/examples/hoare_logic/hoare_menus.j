TACTIC assign(assigntac) IS
    WHEN   
        (LETGOAL ({_A} (_x := _E) {_B})
            (ALT    assigntac 
                    (SEQ    "consequence(L)" fstep (trueforward assigntac))))
        (LETGOAL _E (ALERT ("To make a variable-assignment step, you have to select a goal of the form \
			    \{A} (x:=E) {B}. You selected %t.", _E)
			    ("OK", STOP)))

MENU Instructions
    ENTRY "skip"
    ENTRY "tilt"
    ENTRY "sequence"
    ENTRY "variable-assignment" IS assign "variable-assignment"
    ENTRY "array-element-assignment" IS assign "array-element-assignment"
    ENTRY "choice" IS (SEQ "choice" fstep fstep)
    ENTRY "while"
    ENTRY "consequence(L)"
    ENTRY "consequence(R)"
    ENTRY "obviously..."
END

TACTICPANEL Arithmetic
    RULE IS A=A
    RULE IS A<B ≜ B>A
    RULE IS A≤B ≜ B≥A
    RULE IS A≤B ≜ ¬(A>B)
    RULE IS A≥B ≜ ¬(A<B)
/*  RULE "(A;B);C≜A;(B;C)" IS   A;B;C ≜ A;(B;C)
    ENTRY "flatten ;" IS 
        iterateR2L "rewrite≜"  "symmetric≜" (QUOTE (_A;(_B;_C))) "(A;B);C≜A;(B;C)" (Fail "no semicolons to flatten")
 */
    BUTTON  "A≜…"   IS apply rewriteL2R "rewrite≜"  "symmetric≜"  COMMAND
    BUTTON  "…≜B"   IS apply rewriteR2L "rewrite≜"  "symmetric≜"  COMMAND
    BUTTON  Apply   IS apply COMMAND
END
