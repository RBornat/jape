TACTIC assign(assigntac) IS
    WHEN   
        (LETGOAL ({_A} (_x := _E) {_B})
            (ALT    assigntac 
                    (SEQ    "consequence(L)" fstep (trueforward assigntac))))
        (LETGOAL _E (ALERT ("To make a variable-assignment step, you have to select a goal of the form \
                            \{A} (x:=E) {B}. You selected %t.", _E)
                            ("OK", STOP)))

TACTIC "A = .." IS
    WHEN
        (LETCONCSUBSTSEL (_E«_A/_xx»)
            (WHEN
                (LETHYP (_B=_C)
                    (WHEN
                        (LETMATCH (ANTIQUOTE _A) _B
                            (LAYOUT "equality-substitution")
                            (WITHSUBSTSEL "rewrite=")
                            (WITHHYPSEL hyp)
                            fstep)
                        (ALERT ("To use left-to-right equality substitution on a text selection, \
                                \you must text-select instances of A and a hypothesis \
                                \of the form A=B.\n\n\
                                \You text-selected %t and a hypothesis %t \
                                \(%t isn't %t ...).", _A, _B=_C, _A, _B)
                               ("OK", STOP))))
                (LETHYPS _Bs
                    (ALERT ("To use equality substitution with a text selection \
                            \you must select a single hypothesis of the form A=B.\n\n\
                            \You selected %l.", (_Bs, ", ", " and "))
                           ("OK", STOP)))
                (ALERT ("To use equality substitution you must select a single \
                        \hypothesis of the form A=B.\n\n\
                        \You didn't select a hypothesis at all.")
                       ("OK", STOP))))
        (LETGOAL _A
            (WHEN
                (LETHYP (_xx=_B)
                    (WHEN
                        (LETOCCURS (ANTIQUOTE _xx) _A _F
                            (LAYOUT "equality-substitution")
                            "rewrite="«_E,_xx,_B,_xx/E,A,B,xx»
                            (WITHHYPSEL hyp)
                            fstep)
                        (ALERT ("You are asking to rewrite %t using equality %t, left-to-right.\n\n\
                                \But since %t doesn't occur in %t, there doesn't seem \
                                \much point ...", _A, _xx=_B, _xx, _A)
                               ("OK", STOP))))
                (LETHYP (_B=_xx)
                    (ALERT ("To use left-to-right equality substitution without a text selection \
                            \you must select a single hypothesis of the form x=A.\n\n\
                            \You selected %t, which might work right-to-left.\n\n\
                            \Do you want to try right-to-left?", _B=_xx)
                           ("Right to Left", "... = B") ("Cancel", STOP)))
                (LETHYP (_B=_C)
                    (ALERT ("To use left-to-right equality substitution without a text selection \
                            \you must select a single hypothesis of the form x=A.\n\n\
                            \You selected %t (%t isn't a variable; neither is %t ...).", _B=_C, _B, _C)
                           ("OK", STOP)))
                (LETHYPS _Bs
                    (ALERT ("To use left-to-right equality substitution without a text selection \
                            \you must select a single hypothesis of the form x=A.\n\n\
                            \You selected %l.", (_Bs, ", ", " and "))
                           ("OK", STOP)))
                (ALERT ("To use equality substitution you must select a single \
                        \hypothesis of the form A=B.\n\n\
                        \You didn't select a hypothesis")
                       ("OK", STOP))))
        (ALERT ("Please select a conclusion and an equality hypothesis")
               ("OK", STOP))

TACTIC ".. = B" IS
    WHEN
        (LETCONCSUBSTSEL (_E«_A/_xx»)
            (WHEN
                (LETHYP (_B=_C)
                    (WHEN
                        (LETMATCH (ANTIQUOTE _A) _C
                            (LAYOUT "equality-substitution")
                            (WITHSUBSTSEL "rewrite=")
                            (LAYOUT HIDEROOT "symmetric=")
                            (WITHHYPSEL hyp)
                            fstep)
                        (ALERT ("To use right-to-left equality substitution on a text selection, \
                                \you must text-select instances of A and a hypothesis \
                                \of the form A=B.\n\n\
                                \You text-selected %t and a hypothesis %t \
                                \(%t isn't %t ...).", _A, _B=_C, _A, _C)
                               ("OK", STOP))))
                (LETHYPS _Bs
                    (ALERT ("To use equality substitution with a text selection \
                            \you must select a single hypothesis of the form A=B.\n\n\
                            \You selected %l.", (_Bs, ", ", " and "))
                           ("OK", STOP)))
                (ALERT ("To use equality substitution with a text selection \
                        \you must select a single hypothesis of the form A=B.\n\n\
                        \You didn't select a hypothesis at all.")
                       ("OK", STOP))))
        (LETGOAL _A
            (WHEN
                (LETHYP (_B=_xx)
                    (WHEN
                        (LETOCCURS (ANTIQUOTE _xx) _A _F
                            (LAYOUT "equality-substitution")
                            "rewrite="«_E,_xx,_B,_xx/E,A,B,xx»
                            (LAYOUT HIDEROOT "symmetric=")
                            (WITHHYPSEL hyp)
                            fstep)
                        (ALERT ("You are asking to rewrite %t using equality %t, right-to-left.\n\n\
                                \But since %t doesn't occur in %t, there doesn't seem \
                                \much point ...", _A, _B=_xx, _xx, _A)
                               ("OK", STOP))))
                (LETHYP (_xx=_B)
                    (ALERT ("To use right-to-left equality substitution without a text selection \
                            \you must select a single hypothesis of the form A=x.\n\n\
                            \You selected %t, which might work left-to-right.\n\n\
                            \Do you want to try left-to-right?", _xx=_B)
                           ("Left to right", "A = ..") ("Cancel", STOP)))
                (LETHYP (_B=_C)
                    (ALERT ("To use right-to-leftequality-substitution without a text selection \
                            \you must select a single hypothesis of the form A=x.\n\n\
                            \You selected %t (%t isn't a variable; neither is %t ...).", _B=_C, _C, _B)
                           ("OK", STOP)))
                (LETHYPS _Bs
                    (ALERT ("To use right-to-left equality substitution without a text selection \
                            \you must select a single hypothesis of the form A=x.\n\n\
                            \You selected %l.", (_Bs, ", ", " and "))
                           ("OK", STOP)))
                (ALERT ("To use equality substitution you must select a single \
                        \hypothesis of the form A=B.\n\n\
                        \You didn't select a hypothesis")
                       ("OK", STOP))))
        (ALERT ("Please select a conclusion and an equality hypothesis")
               ("OK", STOP))

MENU Programs
    ENTRY "skip"
    ENTRY "tilt"
    ENTRY "sequence"
    ENTRY "variable-assignment" IS assign "variable-assignment"
    ENTRY "array-element-assignment" IS assign "array-element-assignment"
    ENTRY "choice" IS (SEQ "choice" fstep fstep)
    ENTRY "while"
    ENTRY "consequence(L)"
    ENTRY "consequence(R)"
END

MENU Extras
    ENTRY "A=A" IS (SEQ (LAYOUT "A=A") "reflexive=")
    
    SEPARATOR
    
    ENTRY "A = .."
    ENTRY ".. = B"
    
    SEPARATOR
    
    ENTRY "obviously"
END

TACTICPANEL Comparison
    RULE IS A<B ≜ B>A
    RULE IS A≤B ≜ A<B ∨ A=B
    RULE IS A≤B ≜ B≥A
    RULE IS A≤B ≜ ¬(A>B)
    RULE IS A≥B ≜ ¬(A<B)
/*  RULE "(A;B);C≜A;(B;C)" IS   A;B;C ≜ A;(B;C)
    ENTRY "flatten ;" IS 
        iterateR2L "rewrite≜"  "symmetric≜" (QUOTE (_A;(_B;_C))) "(A;B);C≜A;(B;C)" (Fail "no semicolons to flatten")
 */
    BUTTON  "A≜…"   IS apply rewriteL2R "rewrite≜"  "symmetric≜"  COMMAND
    BUTTON  "…≜B"   IS apply rewriteR2L "rewrite≜"  "symmetric≜"  COMMAND
END
