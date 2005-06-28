/*
    $Id$

    Copyright (C) 2004-5 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the Hoare logic example distribution, which is part of jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*/

TACTIC GivenTac(i) IS
    ALT (GIVEN i)
        (CUTIN (GIVEN i))

INITIALISE givenMenuTactic GivenTac

TACTIC perhapsconsequenceL (tac) IS
    ALT    tac 
           (SEQ "consequence(L)" fstep (trueforward tac))

TACTIC perhapsconsequenceLR (tac) IS
    ALT    tac 
           (SEQ "consequence(L)" fstep (trueforward tac))
           (SEQ "consequence(R)" (trueforward tac) fstep)
           (SEQ "consequence(L)" fstep
                "consequence(R)" (trueforward tac) fstep)

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
                                \you must text-select instances of B and a hypothesis \
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

/* At present I can't see how to write a general AssocBackwards tactic, cos the environments
   keep getting muddled up. Passing (QUOTE (_A∧_B)) as an argument doesn't hack it: I'd need to
   pass a function which had its own environment. And that's a bridge too far, this week.
 */
 
TACTIC "∧ intro*"  IS
    WHEN    
        (LETGOAL (_P∧_Q)  
            (LAYOUT COMPRESS "∧ intro") 
            "∧ intro" "∧ intro*" 
            (LETMATCH _G tacticresult "∧ intro*" (ASSIGN tacticresult _G)) /* take leftmost GOALPATH */
        )
        (trueforward (QUOTE (LETGOALPATH G (ASSIGN tacticresult G))))

TACTIC "sequence*"  IS
    WHEN    
        (LETGOAL ({_A} (_C1; _C2) {_B})  
            (LAYOUT COMPRESS "sequence") 
            "sequence" "sequence*" 
            (LETMATCH _G tacticresult "sequence*" (ASSIGN tacticresult _G)) /* take leftmost GOALPATH */
        )
        (trueforward (QUOTE (LETGOALPATH G (ASSIGN tacticresult G))))

TACTIC "Ntuple*"  IS
    ALT   
        (LAYOUT COMPRESS "Ntuple" ALL
            "Ntuple" "Ntuple*" 
            (LETMATCH _G tacticresult "Ntuple*" (ASSIGN tacticresult _G)))
        (trueforward (QUOTE (LETGOALPATH G (ASSIGN tacticresult G))))

TACTIC "NtupleInner*"  IS
    WHEN    
        (LETGOAL ({_A}(_B{_C}_D){_E})  
            (LAYOUT COMPRESS "NtupleInner") 
            "NtupleInner" "NtupleInner*" 
            (LETMATCH _G tacticresult "NtupleInner*" (ASSIGN tacticresult _G)) /* take leftmost GOALPATH */
        )
        (trueforward (QUOTE (LETGOALPATH G (ASSIGN tacticresult G))))

/* multiple compressed forward steps are harder still */

TACTIC "∧ elim* step"(P, rule, H) IS
    WHEN    
        (LETMATCH (_P∧_Q)  P  
            (CUTIN (LAYOUT HIDEROOT) rule (LETGOAL _A (UNIFY _A H) hyp)))
        (CUTIN (LAYOUT "∧ elim") rule (LETGOAL _A (UNIFY _A H) hyp))

TACTIC "∧ elim*"(P)  IS
    WHEN    
        (LETMATCH (_P∧_Q)  P    
            ("∧ elim* step" _P  "∧ elim(L)" P) 
            ("∧ elim*" _P) 
            ("∧ elim* step" _Q  "∧ elim(R)" P) 
            ("∧ elim*" _Q))
        SKIP

TACTIC obviouslytac IS
  WHEN
    (LETHYPS _A 
      (LETLISTMATCH _A1 _B _A /* must work ... */
        (WHEN   
          (LETLISTMATCH _B1 _C _B 
            (WHEN
              (LETLISTMATCH _C1 _D _C 
                (WHEN 
                  (LETLISTMATCH _D1 _E _D 
                    (WHEN
                      (LETLISTMATCH _E1 _F _E 
                        (ALERT 
                            ("Unfortunately, Jape is set up to recognise only up to \
                             \four ‘obvious’ links. You selected too many (%l).\n\n\
                             \Please moderate your enthusiasm and try again."
                             (_A, ", ", " and "))
                             ("OK", STOP)))
                        (LAYOUT "obviously, from" ALL 
                            "obviously4" (WITHHYPSEL (hyp _A1)) (WITHHYPSEL (hyp _B1)) (WITHHYPSEL (hyp _C1)) 
                            (WITHHYPSEL (hyp _D1)))))
                  (LAYOUT "obviously, from" ALL 
                      "obviously3" (WITHHYPSEL (hyp _A1)) (WITHHYPSEL (hyp _B1)) (WITHHYPSEL (hyp _C1)))))
              (LAYOUT "obviously, from" ALL 
                  "obviously2" (WITHHYPSEL (hyp _A1)) (WITHHYPSEL (hyp _B1)))))
          (LAYOUT "obviously, from" ALL 
              "obviously1" (WITHHYPSEL (hyp _A1))))))
    (LAYOUT "obviously" ALL "obviously0")

MENU Backward IS
    BEFOREENTRY "∧ intro"
    ENTRY "∧ intro (all at once)" 
                    IS BackwardOnlyC (QUOTE (_A∧_B)) 
                                     (Noarg "∧ intro*" "∧ intro") 
                                     "∧ intro" "A∧B"
    RENAMEENTRY "∧ intro" "∧ intro (one step)"
END

MENU Forward IS
    BEFOREENTRY "∧ elim (preserving left)"
    ENTRY   "∧ elim (all at once)"      
                    IS Forward (QUOTE (_A∧_B)) 
                               (Noarg (LETHYP _P  (MATCH ("∧ elim*" _P))) "∧ elim")  
                               "∧ elim" "∧ intro" "A∧B"
END

MENU Programs
    ENTRY "skip"    IS BackwardOnlyA (QUOTE ({_A} skip {_B}))
                                    (perhapsconsequenceL "skip")
                                    "skip"
                                    "{A}skip{B}"
    ENTRY "tilt"    IS BackwardOnlyA (QUOTE ({_A} tilt {_B}))
                                    (perhapsconsequenceL "tilt")
                                    "tilt"
                                    "{A}tilt{B}"
    ENTRY "sequence"    
                    IS BackwardOnlyA (QUOTE ({_A} (_C1; _C2) {_B}))
                                    (Noarg "sequence*" "sequence")
                                    "sequence"
                                    "{A}(C1;C2){B}"
    ENTRY "Ntuple"  IS BackwardOnlyDouble (QUOTE (_A{_B}_C{_D})) (QUOTE ({_A}(_B{_C}_D){_E}))
                                    (Noarg "Ntuple*" "Ntuple")
                                    "Ntuple"
                                    "A{B}C{D} or {A}(B{C}D){E}"
                                    "only makes sense backwards."
    ENTRY "variable-assignment" 
                    IS BackwardOnlyA (QUOTE ({_A} (_x := _E) {_B}))
                                    (Noarg (perhapsconsequenceL (SEQ "variable-assignment" simpl)))
                                    "variable-assignment"
                                    "{A}(x:=E){B}"
    ENTRY "array-element-assignment"
                    IS BackwardOnlyA (QUOTE ({_A} (_a[_E] := _F) {_B}))
                                    (perhapsconsequenceL 
                                      (QUOTE (LETGOAL ({_A} (_a[_E] := _F) {_B}) 
                                                "array-element-assignment" ("length_simpl" _a _E _F) simpl simpl)))
                                    "array-element-assignment"
                                    "{A}(a[E]:=F){B}"
    ENTRY "choice"  IS BackwardOnlyA (QUOTE ({_A} if _E then _C1 else _C2 fi {_B}))
                                    (perhapsconsequenceL (SEQ "choice" simpl fstep fstep))
                                    "choice"
                                    "{A} if E then C1 else C2 fi {B}"
    ENTRY "while"  IS BackwardOnlyA (QUOTE ({_A} while _E do _C od {_B}))
                                    (perhapsconsequenceLR (SEQ "while" simpl maybetrueimpl fstep fstep /* fstep */))
                                    "while"
                                    "{A} while E do C od {B}"
    
    SEPARATOR
    
    ENTRY "consequence(L)"
    ENTRY "consequence(R)"
END

RULE "identity" IS
  FROM ⊤∧(A defined) simplifiesto B 
  AND  B
  INFER A=A
  
TACTIC "identitytac" IS
  SEQ "identity" 
      defin 
      (ALT (LAYOUT HIDEROOT "truth") SKIP)

MENU Extras
    ENTRY "A=A" IS (LAYOUT "A=A" ALL "identitytac")
    
    SEPARATOR
    
    ENTRY "A = .."
    ENTRY ".. = B"
    
    SEPARATOR
    
    ENTRY "obviously" IS obviouslytac
    
    SEPARATOR
    
    ENTRY "boundedness from (in)equality" IS boundednesstac
END

TACTIC boundednesstac IS
  WHEN 
    (LETHYPSUBSTSEL (_P«_a[_E]/_x») 
        (WHEN 
            (LETHYPSUBSTSEL (_A<_B) (checkdependencytac "bounded(<)(L)" "bounded(<)(R)" (_A<_B) _a _E))
            (LETHYPSUBSTSEL (_A≤_B) (checkdependencytac "bounded(≤)(L)" "bounded(≤)(R)" (_A≤_B) _a _E))
            (LETHYPSUBSTSEL (_A=_B) (checkdependencytac "bounded(=)(L)" "bounded(=)(R)" (_A=_B) _a _E))
            (LETHYPSUBSTSEL (_A≥_B) (checkdependencytac "bounded(≥)(L)" "bounded(≥)(R)" (_A≥_B) _a _E))
            (LETHYPSUBSTSEL (_A>_B) (checkdependencytac "bounded(>)(L)" "bounded(>)(R)" (_A>_B) _a _E))
            (LETHYPSUBSTSEL (_A≠_B)
                (ALERT ("To deduce array-index boundedness, you must text-select \
                        \an array-indexing formula inside an equality/inequality hypothesis. \
                        \\n\nYou chose hypothesis %t: it's hard to be sure if that guarantees boundedness, \
                        \and Jape is playing safe. Sorry! Try again?", _E) 
                       ("OK", STOP)))
            (LETHYPSUBSTSEL _E
                (ALERT ("To deduce array-index boundedness, you must text-select \
                        \an array-indexing formula inside an equality/inequality hypothesis. \
                        \\n\nYou chose hyphothesis %t, which isn't an equality or an inequality.", _E) 
                       ("OK", STOP)))))
    (LETHYPSUBSTSEL (_P«_E/_x»)
        (ALERT ("To deduce array-index boundedness, you must text-select \
                \an array-indexing formula inside an equality/inequality hypothesis. \
                \\n\nYou text-selected %t, which isn't an array-indexing formula.", _E) 
               ("OK", STOP)))
    (ALERT "To deduce array-index boundedness, you must text-select \
           \an array-indexing formula inside an equality/inequality hypothesis. \
           \\n\nYou don't appear to have done that ..." 
           ("OK", STOP))

TACTICPANEL Comparison
    RULE IS A=B ≜ B=A
    RULE IS A=B ≜ ¬(A≠B)
    RULE IS A≠B ≜ B≠A
    RULE IS A≠B ≜ ¬(A=B)
    RULE IS A<B ≜ B>A
    RULE IS A≤B ≜ A<B ∨ A=B
    RULE IS A≤B ≜ B≥A
    RULE IS A≤B ≜ ¬(A>B)
    /* RULE IS A≤B ≜ A<B+1
    RULE IS A+1≤B ≜ A<B */
    RULE IS A≥B ≜ ¬(A<B)
    /*RULE IS A≥B ≜ A>B-1
    RULE IS A-1≥B ≜ A>B */
/*  RULE "(A;B);C≜A;(B;C)" IS   A;B;C ≜ A;(B;C)
    ENTRY "flatten ;" IS 
        iterateR2L "rewrite≜"  "symmetric≜" (QUOTE (_A;(_B;_C))) "(A;B);C≜A;(B;C)" (Fail "no semicolons to flatten")
 */
    BUTTON  "A≜…"   IS apply rewriteL2R "rewrite≜"  "symmetric≜"  COMMAND COMMAND fstep
    BUTTON  "…≜B"   IS apply rewriteR2L "rewrite≜"  "symmetric≜"  COMMAND COMMAND fstep
END

RULE indexR IS FROM E=G INFER (A⊕E↦F)[G]=F
RULE indexL IS FROM E≠G INFER (A⊕E↦F)[G]=A[G]

TACTIC "index(=)" IS 
    SEQ indexR (ALT (LAYOUT HIDEROOT "identitytac") fstep) fstep

TACTIC "index(≠)" IS
    SEQ indexL fstep

TACTICPANEL Indexing
    ENTRY "FROM E=G INFER (A⊕E↦F)[G]=F" IS "index(=)"
    ENTRY "FROM E≠G INFER (A⊕E↦F)[G]=A[G]" IS "index(≠)"

    BUTTON  "A≜…"   IS apply rewriteL2R "rewrite="  "symmetric="  COMMAND COMMAND fstep
    BUTTON  "…≜B"   IS apply rewriteR2L "rewrite="  "symmetric="  COMMAND COMMAND fstep
END

MENU "Edit" IS
  SEPARATOR
  CHECKBOX boxlinedressright "Justifications on right"
END