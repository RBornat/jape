/*
    Copyright (C) 2004-8 Richard Bornat
     
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

/* left-to-right equality substitution -- i.e. given A=B, and instances of A, change them to B */

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
                               ("OK", STOP) ("Huh?", Explainlefttorighttextselection))))
                (LETHYPS _Bs
                    (ALERT ("To use equality substitution with a text selection \
                            \you must select a single hypothesis of the form A=B.\n\n\
                            \You selected %l.", (_Bs, ", ", " and "))
                           ("OK", STOP) ("Huh?", Explainlefttorighttextselection)))
                (ALERT ("To use equality substitution you must select a single \
                        \hypothesis of the form A=B.\n\n\
                        \You didn't select a hypothesis at all.")
                       ("OK", STOP) ("Huh?", Explainlefttorighttextselection))))
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
                               ("OK", STOP) ("Huh?", Explainlefttorighttextselection))))
                (LETHYP (_B=_xx)
                    (ALERT ("To use left-to-right equality substitution without a text selection \
                            \you must select a single hypothesis of the form x=A.\n\n\
                            \You selected %t, which might work right-to-left.\n\n\
                            \Do you want to try right-to-left?", _B=_xx)
                           ("Right to Left", ".. = B") ("Cancel", STOP) ("Huh?", Explainlefttorighttextselection)))
                (LETHYP (_B=_C)
                    (ALERT ("To use left-to-right equality substitution without a text selection \
                            \you must select a single hypothesis of the form x=A.\n\n\
                            \You selected %t (%t isn't a variable; neither is %t ...).", _B=_C, _B, _C)
                           ("OK", STOP) ("Huh?", Explainlefttorighttextselection)))
                (LETHYPS _Bs
                    (ALERT ("To use left-to-right equality substitution without a text selection \
                            \you must select a single hypothesis of the form x=A.\n\n\
                            \You selected %l.", (_Bs, ", ", " and "))
                           ("OK", STOP) ("Huh?", Explainlefttorighttextselection)))
                (ALERT ("To use equality substitution you must select a single \
                        \hypothesis of the form A=B.\n\n\
                        \You didn't select a hypothesis")
                       ("OK", STOP) ("Huh?", Explainlefttorighttextselection))))
        (ALERT ("Please select a conclusion and an equality hypothesis")
               ("OK", STOP) ("Huh?", Explainhypothesisandconclusionwords))

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
                               ("OK", STOP) ("Huh?", Explainrighttolefttextselection))))
                (LETHYPS _Bs
                    (ALERT ("To use equality substitution with a text selection \
                            \you must select a single hypothesis of the form A=B.\n\n\
                            \You selected %l.", (_Bs, ", ", " and "))
                           ("OK", STOP) ("Huh?", Explainrighttolefttextselection)))
                (ALERT ("To use equality substitution with a text selection \
                        \you must select a single hypothesis of the form A=B.\n\n\
                        \You didn't select a hypothesis at all.")
                       ("OK", STOP) ("Huh?", Explainrighttolefttextselection))))
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
                               ("OK", STOP) ("Huh?", Explainrighttolefttextselection))))
                (LETHYP (_xx=_B)
                    (ALERT ("To use right-to-left equality substitution without a text selection \
                            \you must select a single hypothesis of the form A=x.\n\n\
                            \You selected %t, which might work left-to-right.\n\n\
                            \Do you want to try left-to-right?", _xx=_B)
                           ("Left to right", "A = ..") ("Cancel", STOP) ("Huh?", Explainrighttolefttextselection)))
                (LETHYP (_B=_C)
                    (ALERT ("To use right-to-leftequality-substitution without a text selection \
                            \you must select a single hypothesis of the form A=x.\n\n\
                            \You selected %t (%t isn't a variable; neither is %t ...).", _B=_C, _C, _B)
                           ("OK", STOP) ("Huh?", Explainrighttolefttextselection)))
                (LETHYPS _Bs
                    (ALERT ("To use right-to-left equality substitution without a text selection \
                            \you must select a single hypothesis of the form A=x.\n\n\
                            \You selected %l.", (_Bs, ", ", " and "))
                           ("OK", STOP) ("Huh?", Explainrighttolefttextselection)))
                (ALERT ("To use equality substitution you must select a single \
                        \hypothesis of the form A=B.\n\n\
                        \You didn't select a hypothesis")
                       ("OK", STOP) ("Huh?", Explainrighttolefttextselection))))
        (ALERT ("Please select a conclusion and an equality hypothesis")
               ("OK", STOP) ("Huh?", Explainhypothesisandconclusionwords))

/* one day this will be in explain_technology */

TACTIC Explainlefttorighttextselection IS
    ExplainThenStop(
        "1. If you have a hypothesis of the form x=something and a conclusion with xs in it, \
        \you can select (single-click) the hypothesis and the conclusion and use A=.. to \
        \replace all the xs with somethings.\n\
        \1a. If you text-select some of the xs then only they will be replaced by somethings.\n\n\
        \2. If you have a hypothesis of the form something=stuff and a conclusion with somethings in it, \
        \you can select (single-click) the hypothesis and the conclusion, and text-select the \
        \somethings you want to replace, and then A=.. will replace the somethings by stuffs.\n\n\
        \3. If you want to do it the other way around, try ..=B."
    )
    
TACTIC Explainrighttolefttextselection IS
    ExplainThenStop(
        "1. If you have a hypothesis of the form something=x and a conclusion with xs in it, \
        \you can select (single-click) the hypothesis and the conclusion and use ..=B to \
        \replace all the xs with somethings.\n\
        \1a. If you text-select some of the xs then only they will be replaced by somethings.\n\n\
        \2. If you have a hypothesis of the form stuff=something and a conclusion with somethings in it, \
        \you can select (single-click) the hypothesis and the conclusion, and text-select the \
        \somethings you want to replace, and then ..=B will replace the somethings by stuffs.\n\n\
        \3. If you want to do it the other way around, try A=..."
    )
    
/* At present I can't see how to write a general AssocBackwards tactic, cos the environments
   keep getting muddled up. Passing (QUOTE (_A∧_B)) as an argument doesn't hack it: I'd need to
   pass a function which had its own environment. And that's a bridge too far, this week.
 */
 
TACTIC "∧ intro*"  IS
    WHEN    
        (LETGOAL (_P∧_Q)  
            (LAYOUT COMPRESS "∧ intro") 
            "∧ intro" "∧ intro*" 
            (LETMATCH _G tacticresult "∧ intro*" (ASSIGN tacticresult _G))) /* take leftmost GOALPATH */
        (trueforward (QUOTE (LETGOALPATH G (ASSIGN tacticresult G))))

TACTIC "sequence*"  IS
    WHEN    
        (LETGOAL ({_A} (_C1; _C2) {_B})  
            (LAYOUT COMPRESS "sequence") 
            "sequence" "sequence*" 
            (LETMATCH _G tacticresult "sequence*" (ASSIGN tacticresult _G))) /* take leftmost GOALPATH */
        (trueforward (QUOTE (LETGOALPATH G (ASSIGN tacticresult G))))

TACTIC "Ntuple*"  IS
    ALT   
        (LAYOUT COMPRESS "Ntuple" ALL
            "Ntuple" "Ntuple*" 
            (LETMATCH _G tacticresult "Ntuple*" (ASSIGN tacticresult _G))) /* take leftmost GOALPATH */
        (trueforward (QUOTE (LETGOALPATH G (ASSIGN tacticresult G))))

TACTIC "NtupleInner*"  IS
    WHEN    
        (LETGOAL ({_A}(_B{_C}_D){_E})  
            (LAYOUT COMPRESS "NtupleInner") 
            "NtupleInner" "NtupleInner*" 
            (LETMATCH _G tacticresult "NtupleInner*" (ASSIGN tacticresult _G))) /* take leftmost GOALPATH */
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
                    IS SEQ (BackwardOnlyC (QUOTE (_A∧_B)) 
                                          (Noarg "∧ intro*" "∧ intro") 
                                          "∧ intro" "A∧B")
                           (GOALPATH tacticresult)
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
                    IS SEQ (BackwardOnlyA (QUOTE ({_A} (_C1; _C2) {_B}))
                                          (Noarg "sequence*" "sequence")
                                          "sequence"
                                          "{A}(C1;C2){B}")
                           (GOALPATH tacticresult)
    ENTRY "Ntuple"  IS SEQ (BackwardOnlyDouble (QUOTE (_A{_B}_C{_D})) (QUOTE ({_A}(_B{_C}_D){_E}))
                                               (Noarg "Ntuple*" "Ntuple")
                                               "Ntuple"
                                               "A{B}C{D} or {A}(B{C}D){E}"
                                               "only makes sense backwards.")
                           (GOALPATH tacticresult)
    ENTRY "variable-assignment" 
                    IS BackwardOnlyA (QUOTE ({_A} (_x := _E) {_B}))
                                    (Noarg (perhapsconsequenceL (SEQ "variable-assignment" simpl)))
                                    "variable-assignment"
                                    "{A}(x:=E){B}"
    ENTRY "array-element-assignment"
        IS BackwardOnlyA 
            (QUOTE ({_A} (_a[_E] := _F) {_B}))
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

RULE IS A=B≜B=A
RULE IS A=B≜¬(A≠B)
RULE IS A≠B≜B≠A
RULE IS A≠B≜¬(A=B)
RULE IS A<B≜B>A
RULE IS A≤B≜A<B ∨ A=B
RULE IS A≤B≜B≥A
RULE IS A≤B≜¬(A>B)
RULE IS A≤B≜A<B+1
RULE IS A+1≤B≜A<B
RULE IS A≥B≜¬(A<B)
RULE IS A≥B≜A>B-1
RULE IS A-1≥B≜A>B

/* I've found a way to run multi-arg tactics from a panel ... */
TACTICPANEL Comparison
    ENTRY "A=B≜B=A"     IS  rwComparison "A=B≜B=A"      "A=B"   (QUOTE (_C«_A=_B/_xx»))  "B=A"      (QUOTE (_C«_B=_A/_xx»)) 
    ENTRY "A=B≜¬(A≠B)"  IS  rwComparison "A=B≜¬(A≠B)" "A=B"   (QUOTE (_C«_A=_B/_xx»))  "¬(A≠B)" (QUOTE (_C«¬(_A≠_B)/_xx»)) 
    ENTRY "A≠B≜B≠A"     IS  rwComparison "A≠B≜B≠A"    "A≠B"  (QUOTE (_C«_A≠_B/_xx»)) "B≠A"     (QUOTE (_C«_B≠_A/_xx»)) 
    ENTRY "A≠B≜¬(A=B)"  IS  rwComparison "A≠B≜¬(A=B)" "A≠B"  (QUOTE (_C«_A≠_B/_xx»))  "¬(A=B)" (QUOTE (_C«¬(_A=_B)/_xx»)) 
    ENTRY "A<B≜B>A"     IS  rwComparison "A<B≜B>A"      "A<B"   (QUOTE (_C«_A<_B/_xx»))  "B>A"      (QUOTE (_C«_B>_A/_xx»)) 
    ENTRY "A≤B≜A<B∨A=B" IS  rwComparison "A≤B≜A<B∨A=B" "A≤B" (QUOTE (_C«_A≤_B/_xx»)) "A<B∨A=B" (QUOTE (_C«_A<_B∨_A=_B/_xx»)) 
    ENTRY "A≤B≜B≥A"     IS  rwComparison "A≤B≜B≥A"     "A≤B" (QUOTE (_C«_A≤_B/_xx»))  "B≥A"    (QUOTE (_C«_B≥_A/_xx»)) 
    ENTRY "A≤B≜¬(A>B)"  IS  rwComparison "A≤B≜¬(A>B)" "A≤B"  (QUOTE (_C«_A≤_B/_xx»))  "¬(A>B)" (QUOTE (_C«¬(_A>_B)/_xx»)) 
    ENTRY "A≤B≜A<B+1"   IS  rwComparison "A≤B≜A<B+1" "A≤B"  (QUOTE (_C«_A≤_B/_xx»))  "A<B+1" (QUOTE (_C«_A<_B+1/_xx»))
    ENTRY "A+1≤B≜A<B"   IS  rwComparison "A+1≤B≜A<B" "A+1≤B"  (QUOTE (_C«_A+1≤_B/_xx»))  "A<B" (QUOTE (_C«_A<_B/_xx»))
    ENTRY "A≥B≜¬(A<B)"  IS  rwComparison "A≥B≜¬(A<B)" "A≥B"  (QUOTE (_C«_A≥_B/_xx»))  "¬(A<B)" (QUOTE (_C«¬(_A<_B)/_xx»)) 
    ENTRY "A≥B≜A>B-1"   IS  rwComparison "A≥B≜A>B-1" "A≥B"  (QUOTE (_C«_A≥_B/_xx»))  "A>B-1" (QUOTE (_C«_A>_B-1/_xx»)) 
    ENTRY "A-1≥B≜A>B"   IS  rwComparison "A-1≥B≜A>B" "A-1≥B"  (QUOTE (_C«_A-1≥_B/_xx»))  "A>B" (QUOTE (_C«_A>_B/_xx»)) 
/*  RULE "(A;B);C≜A;(B;C)" IS   A;B;C≜A;(B;C)
    ENTRY "flatten ;" IS 
        iterateR2L "rewrite≜"  "symmetric≜" (QUOTE (_A;(_B;_C))) "(A;B);C≜A;(B;C)" (Fail "no semicolons to flatten")
 */
    BUTTON  "A  ≜ …"   IS apply COMMAND "rwLeft≜"
    BUTTON  "… ≜ B"   IS apply COMMAND "rwRight≜"
END

TACTIC rwComparison (rule, lshape, lpat, rshape, rpat, macro) IS
  macro rule rule lshape lpat rshape rpat
  
MACRO "rwLeft≜" (rule, label, lshape, lpat, rshape, rpat) IS
  rwMenu (rewriteL2R "rewrite≜"  "symmetric≜" rule label fstep) label "left to right" lshape lpat
  
MACRO "rwRight≜" (rule, label, lshape, lpat, rshape, rpat) IS
  rwMenu (rewriteR2L "rewrite≜"  "symmetric≜" rule label fstep) label "right to left" rshape rpat
  
MACRO rwMenu (tac, label, dir, shape, pat) IS
  WHEN
    (LETHYPSUBSTSEL pat tac)
    (LETHYPSUBSTSEL (_B«_A/_xx»)
        (ALERT ("To use %s %s, you have to select (or subformula-select) something of the form %s. \
                \You subformula-selected %t, which isn't of the right form.", 
                label, dir, shape, _A)
               ("OK", STOP) ("Huh?", SHOWHOWTO "TextSelect")))
    (LETCONCSUBSTSEL pat tac)
    (LETCONCSUBSTSEL (_B«_A/_xx»)
        (ALERT ("To use %s %s, you have to select (or subformula-select) something of the form %s. \
                \You subformula-selected %t, which isn't of the right form.", 
                label, dir, shape, _A)
               ("OK", STOP) ("Huh?", SHOWHOWTO "TextSelect")))
    (LETARGSEL _A
        (ALERT ("To use %s %s, you have to select (or subformula-select) something of the form %s. \
                \You managed (using token selection, perhaps?) to select %t, which isn't a subformula.", 
                label, dir, shape, _A)
               ("OK", STOP) ("Huh?", SHOWHOWTO "TextSelect")))
    (LETMULTIARG _A
        (ALERT ("To use %s %s, you have to select (or subformula-select) something of the form %s. \
                \You subformula-selected %l.\
                \\n\n\
                \Multiple identical subformula selections are allowed, but your \
                \selections weren't identical (or, if they are identical, they aren't subformulae!).", 
                label, dir, shape, (_A, ", ", " and "))
               ("OK", STOP)))
    (LETHYP pat tac)
    (LETHYP _A
        (ALERT ("To use %s %s, you have to select (or subformula-select) something of the form %s. \
                \You selected the hypothesis %t, which is of the wrong form.", label, dir, shape, _A)
               ("OK", STOP) ("Huh?", Explainhypothesisandconclusionwords)))
    (LETCONC pat tac)
    (LETCONC _A
        (ALERT ("To use %s %s, you have to select (or subformula-select) something of the form %s. \
                \You selected the conclusion %t, which is of the wrong form.", label, dir, shape, _A)
               ("OK", STOP) ("Huh?", Explainhypothesisandconclusionwords)))
    (LETGOAL pat tac)
    (ALERT ("To use %s %s, you have to select (or subformula-select) something of the form %s. \
            \You didn't select or subformula-select anything", label, dir, shape)
           ("OK", STOP) ("Huh?", SEQ (SHOWHOWTO "FormulaSelect") (SHOWHOWTO "TextSelect")))
           
RULE indexR IS FROM E=G INFER (A⊕E↦F)[G]=F
RULE indexL IS FROM E≠G INFER (A⊕E↦F)[G]=A[G]

TACTIC "index(=)" IS 
    SEQ indexR (ALT (LAYOUT HIDEROOT "identitytac") fstep) fstep

TACTIC "index(≠)" IS
    SEQ indexL fstep

TACTICPANEL Indexing
    ENTRY "FROM E=G INFER (A⊕E↦F)[G]=F" IS 
        rwIndex "FROM E=G INFER (A⊕E↦F)[G]=F" "(A⊕E↦F)[G]" (QUOTE (_B«(_A⊕_E↦_F)[_G]/_xx»)) "F" (QUOTE (_B«_F/_xx»)) "index(=)"
    ENTRY "FROM E≠G INFER (A⊕E↦F)[G]=A[G]" IS 
        rwIndex "FROM E≠G INFER (A⊕E↦F)[G]=A[G]" "(A⊕E↦F)[G]" (QUOTE (_B«(_A⊕_E↦_F)[_G]/_xx»)) "A[G]" (QUOTE (_B«_A[_G]/_xx»)) "index(≠)"

    BUTTON  "A = …"   IS apply COMMAND "rwLeft="
    BUTTON  "… = B"   IS apply COMMAND "rwRight="
END

TACTIC rwIndex (label, lshape, lpat, rshape, rpat, tac, macro) IS
  macro tac label lshape lpat rshape rpat
  
MACRO "rwLeft=" (tac, label, lshape, lpat, rshape, rpat) IS
  rwMenu (rewriteL2R "rewrite="  "symmetric=" tac label fstep) label "left to right" lshape lpat
  
MACRO "rwRight=" (tac, label, lshape, lpat, rshape, rpat) IS
  rwMenu (rewriteR2L "rewrite="  "symmetric=" tac label fstep) label "right to left" rshape rpat
  
MENU "Edit" IS
  SEPARATOR
  CHECKBOX boxlinedressright "Justifications on right"
END