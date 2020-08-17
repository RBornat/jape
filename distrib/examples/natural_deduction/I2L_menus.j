/*
    Copyright (C) 2000-20 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the I2L logic encoding, distributed with jape.

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

/* ******************** remains of the old 'choose by selection' mechanism, which is dead but may fly again ******************** */

TACTIC ForwardOrBackward (Forward, n, Rule) IS 
    WHEN    (LETHYP _Ah 
                (ALT    (Forward n Rule)
                    (WHEN   (LETARGSEL _B  (Fail (Rule is not applicable to hypothesis ' _Ah ' with argument ' _B ')))
                                (Fail (Rule is not applicable to hypothesis ' _Ah ')))))
            (ALT    (WITHSELECTIONS Rule)
                (WHEN   (LETARGSEL _Ah (Fail (Rule is not applicable with argument ' _Ah ')))
                    (Fail (Rule is not applicable))))

/* ******************** tactics to apply rules pseudo-forwards ******************** */

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
                         (Fail ("Error in I2L Jape (can't unify lhs %t with rhs %t in ForwardUncut). Tell Richard Bornat.", 
                                _Ah, _Ag)))
                    (ANY hyp)
                ) 
                (GOALPATH G) 
                NEXTGOAL))
        (Fail "Error in I2L Jape (ForwardUncut falls through). Tell Richard Bornat.")

/* ******************** tactics to deal with variables in quantification rules ******************** */

TACTIC "∀ elim forward" IS
    WHEN    
            (LETHYP2 (actual _i) (∀_x._A) ("∀ elim forward step" (∀_x._A) _i))
            (LETHYP (∀_x._A) ("∀ elim forward moan" ("You only selected %t.", ∀_x._A)))
            (LETHYP (actual _i) ("∀ elim forward moan" ("You only selected %t.", actual _i)))
            (LETHYPS _As ("∀ elim forward moan" ("You selected %l.", (_As, ", ", " and "))))
            (LETGOAL (∀_x._A)
                (ALERT  ("To make a ∀ elim step forward, you must select a hypothesis of the form ∀x.A, and \
                        \also a pseudo-assumption of the form actual i. You didn't select any hypotheses, but the \
                        \current conclusion is %t, which could be used to make a ∀ intro step backwards.\
                        \\nDid you perhaps mean to make a backward step with ∀ intro?", ∀_x._A)
                        ("OK",STOP) ("Huh?",SEQ Explainhypothesisandconclusionwords STOP)))
            ("∀ elim forward moan" "You didn't select any hypotheses")
            
TACTIC "∀ elim forward step" (P, i) IS
    Noarg   (CUTIN 
                "∀ elim"
                (WITHHYPSEL (hyp P))
                (WITHHYPSEL (hyp (actual i))))
            "∀ elim"

TACTIC "∀ elim forward moan" (extra) IS
    ALERT   ("To make a ∀ elim step forward, you must select a hypothesis of the form ∀x.A, and \
            \also a pseudo-assumption of the form actual i. %s",extra)
            ("OK", STOP) ("Huh?", SEQ Explainvariables STOP)

TACTIC "∃ elim forward" IS
    ForwardUncut 0 "∃ elim"
    
TACTIC "∃ intro backward" IS  
    WHEN    
        (LETCONC (∃_x._A)
            ("∃ intro backward hypcheck" "∃ intro backward step" ("You selected the conclusion %t (which is ok), but ", ∃_x._A)))
        (LETCONC _A
            ("∃ intro backward selection moan" (" You selected the conclusion %t.", _A)))
        (LETGOAL (∃_x._A)
            ("∃ intro backward hypcheck" "∃ intro backward step" ("The conclusion is %t (which is ok), but", ∃_x._A)))
        (LETGOAL _A /* the wrong goal, sob */
            ("∃ intro backward selection moan" (" The conclusion you are working on is %t.", _A)))
        (LETOPENSUBGOAL G (∃_x._A) /* there is something they might have chosen */
            ("∃ intro backward hypcheck" ("∃ intro backward unselected goal" (∃_x._A))
                                          ("You didn't select the unproved conclusion %t, and", ∃_x._A)))
        ("∃ intro backward hypcheck" ("∃ intro backward selection moan" " You didn't make any conclusion selection.") 
            ("You didn't make any conclusion selection, and "))

TACTIC "∃ intro backward unselected goal" (A) IS
    ALERT ("To make an ∃ intro step backwards, you have to use a conclusion of the form \
           \∃x.A, and select a pseudo-assumption of the form actual i. You selected the \
           \pseudo-assumption, but you didn't click on the conclusion %t. Would you like to proceed \
           \with the step using %t?\
           \\n\n(You can avoid this message in the future if you always click on the relevant \
           \conclusion before you make the step.)", A, A)
           ("OK", SEQ (GOALPATH G) "∃ intro backward step")  ("Huh?", SEQ Explainvariables STOP) ("Cancel", STOP)
                            
TACTIC "∃ intro backward step" (i) IS
    Noarg (SEQ "∃ intro" fstep (WITHHYPSEL (hyp (actual i)))) "∃ intro"

TACTIC "∃ intro backward selection moan" (stuff) IS
    ALERT   ("To make an ∃ intro step backwards, you have to use a conclusion of the form \
            \∃x.A, and select a pseudo-assumption of the form actual i. %s", stuff)
            ("OK", STOP) ("Huh?", SEQ Explainvariables STOP)

TACTIC "∃ intro backward hypcheck" (action, gmess) IS
    WHEN    
        (LETHYP (actual _i) (action _i)) /* the right hypothesis - hoorah! */
        (LETHYP (∃_x._A) /* looks like a forward step */
            (ALERT  ("To make an ∃ intro step backwards, you have to select a conclusion of the form \
                    \∃x.A, and a pseudo-assumption of the form actual i. You selected \
                    \the hypothesis %t, which would fit ∃ elim going forward.\
                    \\nWould you like to make the forward step instead?",∃_x._A)
                    ("Forward", "∃ elim forward") ("Huh?", SEQ Explainvariables STOP) ("Cancel", STOP)))
        (LETHYP _A /* the wrong hypothesis */
            ("∃ intro backward selection moan" ("%s you selected the hypothesis %t instead.", gmess, _A)))
        (LETHYPS _As /* more than one */
            ("∃ intro backward selection moan" ("%s you selected more than one hypothesis – %l.", gmess, (_As, ", ", " and "))))
        ("∃ intro backward selection moan" ("%s you didn't select a hypothesis.", gmess))

/* ******************** most rules don't want arguments ******************** */

TACTIC Noarg (rule, stepname) IS
    WHEN    
        (LETARGTEXT arg 
            (ALERT  
                ("The %s rule doesn't need an argument, but you subformula-selected %s. \
                 \Do you want to go on with the step, ignoring the subformula selection?", 
                 stepname, arg)
                ("OK", rule) ("Cancel", STOP)))
        (LETMULTIARG args 
            (ALERT  
                ("The %s rule doesn't need an argument, but you subformula-selected %l. \
                 \Do you want to go on with the step, ignoring all the subformula selections?", 
                 stepname, (args, ", ", " and "))
                ("OK", rule) ("Cancel", STOP)))
        rule

TACTIC SingleorNoarg (rule, stepname) IS 
    WHEN    (LETARGSEL _A (WITHARGSEL rule))
            (LETMULTIARG _As 
                (ALERT  ("The %s rule can accept a single argument. You subformula-selected %l, which is more than \
                         \it can deal with. Cancel some or all of your subformula selections and try again.", 
                         stepname, _As)
                        ("OK", STOP) ("Huh?", (SHOWHOWTO "TextSelect"))))
            rule

/* ******************** tactics for rules that only work backwards ******************** */

TACTIC BackwardOnlyA (pattern, action, stepname, shape) 
            IS BackwardOnly pattern action stepname shape "only makes sense working backwards"

TACTIC BackwardOnlyB (pattern, action, stepname, shape) 
            IS BackwardOnly pattern action stepname shape "doesn't work backwards in I2L Jape yet"

TACTIC BackwardOnlyC (pattern, action, stepname, shape) 
            IS BackwardOnly pattern action stepname shape "can also be used forward -- see the Forward menu"

/* ******************** tactic for rules that allow a hypothesis selection, but must have the right goal ******************** */

TACTIC BackwardWithHyp (gpattern, action, stepname, shape) IS
    WHEN    (LETGOAL gpattern (WITHSELECTIONS action))
                (LETGOAL _A (ComplainBackwardWrongGoal stepname shape))
                (ALT 
                    (ComplainBackwardConc gpattern stepname shape)
                    (ComplainBackwardNoGoal gpattern stepname shape)
                    (Fail ("Error in I2L Jape (no error message in BackwardWithHyp [%t] %s [%t]). Tell Richard Bornat.",
                           gpattern,stepname,shape)))

/* ******************** special one for hyp ******************** */

TACTIC "try hyp" (Ph, Pg, mess) IS
    SEQ (ALT (UNIFY Ph Pg)
            (BADUNIFY X Y (ALERT ("%s, because the formula structures don't match.", mess) ("OK", STOP)))
            (BADMATCH X Y (ALERT ("%s, because to do so would change the proof (you shouldn't see this message).", mess) ("OK", STOP)))
            (BADPROVISO X Y P 
                (ALERT ("%s, because to do so would smuggle a variable out of its scope \
                            \(see the proviso %s in the proviso pane).", mess, P)
                            ("OK", STOP)
                            ("Huh?", (ALERT "The proviso i NOTIN _B (for example) appears when the unknown _B occurs both inside \
                                                    \AND outside the scope box for i. It means that nothing unified with _B can contain \
                                                    \the variable i, because that would produce an occurrence of i outside its scope box."
                                                    ("OK", STOP))))))
        (ALT (WITHHYPSEL hyp)   
            (Fail "I2L Jape error (hyp failed in hyptac). Tell Richard Bornat"))

TACTIC hyptac IS
    WHEN    (LETHYP _Ah /* demand hypothesis selection to avoid multi-hyp messages ... */
                (WHEN   (LETCONC _Ag ("try hyp" _Ah _Ag ("%t and %t don't unify", _Ah, _Ag)))
                        (LETOPENSUBGOAL G _Ag 
                            /* shall we just do it if we can? */
                            (GOALPATH G) 
                            ("try hyp" _Ah _Ag  ("%t and %t (the only relevant unproved conclusion) don't unify", _Ah, _Ag)))
                        (LETOPENSUBGOALS _Ags
                            (ALERT  ("Please select one of the unproved conclusions %l to unify with the hypothesis %t.", 
                                    (_Ags, ", ", " or "), _Ah) ("OK", STOP) ("Huh?", SEQ Explainhypothesisandconclusionwords STOP )))
                        (ALERT  ("There aren't any unproved conclusions relevant to the hypothesis %t.", _Ah)
                                ("OK", STOP) ("Huh?", SEQ Explainhypothesisandconclusionwords STOP ))))
            (LETGOAL _Ac 
                (WHEN   (LETLHS ()
                            (ALERT  ("You can't use hyp to prove %t, because there aren't any relevant hypotheses.", _Ac)
                                    ("OK", STOP) ("Huh?", SEQ Explainhypothesisandconclusionwords STOP )))
                        (ALERT  ("Please select a hypothesis to unify with the conclusion %s.", _Ac)
                                ("OK", STOP) ("Huh?", SEQ Explainhypothesisandconclusionwords STOP ))))
            (ALERT  "To use hyp, you have to select a conclusion and a hypothesis. You didn't select anything"
                    ("OK", STOP) ("Huh?", SEQ Explainhypothesisandconclusionwords STOP ))
                
/* ******************** the Backward menu ******************** */

MENU Backward IS
    ENTRY   "∧ intro"                       IS BackwardOnlyC (QUOTE (_A∧_B)) 
                                                    (Noarg "∧ intro backward" "∧ intro") "∧ intro" "A∧B"
    ENTRY   "→ intro (makes assumption)"    IS BackwardOnlyA (QUOTE (_A→_B)) 
                                                    (Noarg "→ intro" "→ intro") "→ intro" "A→B"
    ENTRY   "∨ intro (preserving left)"     IS BackwardOnlyC (QUOTE (_A∨_B)) 
                                                    (Noarg (LAYOUT "∨ intro" (0) "∨ intro(L)" fstep) "∨ intro") "∨ intro" "A∨B"
    ENTRY   "∨ intro (preserving right)"    IS BackwardOnlyC (QUOTE (_A∨_B)) 
                                                    (Noarg (LAYOUT "∨ intro" (0) "∨ intro(R)" fstep) "∨ intro") "∨ intro" "A∨B"
    ENTRY   "¬ intro (makes assumption A)"  IS BackwardOnlyA (QUOTE (¬_A)) 
                                                    (WITHARGSEL "¬ intro") "¬ intro" "¬A" 
    ENTRY   "∀ intro (introduces variable)" IS BackwardOnlyA (QUOTE (∀_x._A)) 
                                                    (Noarg "∀ intro" "∀ intro") "∀ intro" "∀x.A" 
    ENTRY   "∃ intro (needs variable)"      "∃ intro backward"
    
    ENTRY "truth" IS BackwardOnlyA ⊤(Noarg "truth" "truth") "truth" ⊤
    
    SEPARATOR
    
    ENTRY   "contra (classical; makes assumption ¬A)"   
                                             IS BackwardOnlyA (QUOTE _A) 
                                                    (Noarg "contra (classical)" "contra (classical)") "contra (classical)" "A"
    
    ENTRY   "contra (constructive)"         IS BackwardOnlyA (QUOTE _A) 
                                                    (Noarg "contra (constructive)" "contra (constructive)") "contra (constructive)" "A"
    
    ENTRY   "¬ elim (invents formulae)" IS 
        WHEN 
            (LETHYP2 _A (¬_A) 
                (ALERT  ("To make a backward step with ¬ elim you don't have to select any hypotheses. \
                        \You selected %t and %t, which would fit a forward step. Since it works better \
                        \that way, would you like to use ¬ elim going forward?", _A, ¬_A)
                        ("Forward", "¬ elim forward") ("Cancel", STOP)))
            (LETHYP (¬_A) 
                (BackwardWithHyp ⊥ (Noarg (SEQ ("¬ elim") fstep (WITHHYPSEL (hyp (¬_A)))) "¬ elim") "¬ elim" "⊥"))
            (BackwardOnlyC ⊥ (SEQ (SingleorNoarg "¬ elim" "¬ elim") fstep fstep) "¬ elim" "⊥")
    
    SEPARATOR
    
    ENTRY   hyp     IS Noarg hyptac hyp
END
    
TACTIC "∧ intro backward" IS
    WHEN    (LETGOAL (_A∧_B) /* bound to work, surely? */
                "∧ intro" fstep fstep   )
            (Fail "∧ intro backward tactic failed. Tell Richard Bornat.")
                
/* ******************** special one for rules which don't care about hypothesis shape ******************** */

TACTIC "∨ introforward" (rule) IS
    WHEN    (LETHYP _A (ForwardCut 0 (LAYOUT "∨ intro" (0) (WITHARGSEL rule))))
            (ComplainForwardNoHyp "∨ intro" "" "")

TACTIC "∧ intro forward" IS
    WHEN    
        (LETHYP2 _A _B
            (ALERT
                ("∧ intro going forward is visually ambiguous – that is, it can be carried out either way round. \
                 \\nDo you want to\n\
                 \\n(a)    Make %t∧%t, or \
                 \\n(b)    Make %t∧%t.", _A, _B, _B, _A)
                 ("(a)",    
                    (CUTIN  
                        "∧ intro" [A,B \ _A,_B]  
                        (ANY (WITHHYPSEL (MATCH (hyp _A)))) 
                        (ANY (WITHHYPSEL (MATCH (hyp _B))))))
                 ("(b)",    
                    (CUTIN 
                        "∧ intro" [ A,B \ _B,_A ] 
                        (ANY (WITHHYPSEL (MATCH (hyp _B))))
                        (ANY (WITHHYPSEL (MATCH (hyp _A))))))
                 ("Cancel", STOP)))
        (LETHYP _A 
            (ALERT  ("To make a forward step with ∧ intro, it's necessary to select TWO hypothesis formulae to combine. \
                    \You only selected %t.", _A)
                    ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP))) 
        (LETHYPS _As 
            (ALERT  ("To make a forward step with ∧ intro, it's necessary to select only two hypothesis formulae to combine. \
                    \You selected %l.", (_As, ", ", " and "))
                    ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP))) 
        (LETGOAL (_A∧_B)
            (ALERT  ("To make a forward step with ∧ intro, it's necessary to select TWO hypothesis formulae to combine. \
                    \You didn’t select any at all.\
                    \\n\nHowever, the current conclusion %t would fit ∧ intro going backwards. Did you perhaps mean to \
                    \make a backward step?", _A∧_B)
                    ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP)))
        (ALERT  ("To make a forward step with ∧ intro, it's necessary to select TWO hypothesis formulae to combine. \
                \You didn’t select any at all.")
                ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP))

/* this tactic is so horrible gesturally that even though it works I've taken it out of the Forward menu, and commented it out.

TACTIC "∃ intro forward" IS
    WHEN    
        (LETHYP (actual _i)
            (WHEN
                (LETHYPSUBSTSEL (_P [ _x \ _i1 ] ) 
                    ("∃ intro forward checkvar" _i _i1) 
                    ("∃ intro forward complain" 
                        (" (you only selected actual %t)", _i) 
                        (" (as you did). Sorry to be so fussy, but please click on %t and try again", _P [ _x \ _i1 ] )))
                ("∃ intro forward checktextsel" (" (you only selected actual %t)", _i))))
        (LETHYP _A
            ("∃ intro forward checktextsel" (" (you selected %t)", _A)))
        (LETHYP2 (actual _i) _A
            (WHEN
                (LETHYPSUBSTSEL (_P [ _x \ _i1 ] )
                    ("∃ intro forward checksamehyp" _A (_P [ _x \ _i1 ] ) _i1)
                    ("∃ intro forward checkvar" _i _i1) 
                    ("∃ intro forward doit" _A _x _i1 )
                )
                ("∃ intro forward checktextsel" " (as you did)")))
        (LETHYPS _As
            ("∃ intro forward checktextsel" (" (you selected %l)", _As)))
        ("∃ intro forward checktextsel" " (you didn't select any hypotheses)")

TACTIC "∃ intro forward checktextsel" (varstuff) IS
    WHEN    (LETHYPSUBSTSEL (_P [ _x \ _i1 ] ) 
                ("∃ intro forward complain" varstuff " (as you did)"))
            (LETHYPSUBSTSEL (_P [ _x \ _B ] ) 
                ("∃ intro forward complain" varstuff (" (your subformula-selection %t isn't a variable)", _B)))
            (LETSUBSTSEL (_P [ _x \ _B ] ) 
                ("∃ intro forward complain" varstuff (" (your subformula-selection %t isn't in a hypothesis)", _B)))
            (LETARGTEXT i
                ("∃ intro forward complain" varstuff (" (your subformula-selection %s isn't a subformula)", i)))
            (SEQ
                ("∃ intro forward complain" varstuff (" (you didn't subformula-select anything, or you subformula-selected several different things)")))

MACRO "∃ intro forward checksamehyp" (h, sel, i) IS
    WHEN    (LETUNIFY h sel SKIP)
            ("∃ intro forward complain" " (as you did)" (" (your subformula selection %t isn't inside the hypothesis %t)", i, h))

MACRO "∃ intro forward checkvar"(i,j) IS
    WHEN    (LETUNIFY i j SKIP)
            ("∃ intro forward complain" (" (you selected actual %t)",i) (" (you subformula-selected instance(s) of %t, which doesn't match %t)", j, i))

MACRO "∃ intro forward doit" (P1,x1,i1) IS
    (CUTIN "∃ intro" (WITHSUBSTSEL hyp) (WITHHYPSEL (hyp (actual i1))))

TACTIC "∃ intro forward complain"(varstuff, selstuff) IS
    SEQ
        (ALERT  ("To make an ∃ intro step forward you have to select an assumption like actual i and also \
                \a hypothesis formula in its scope%s,\
                \and subformula-select instances of the variable in that formula%s.",
                varstuff, selstuff)
                ("OK", SKIP) ("Huh?" (SHOWHOWTO "TextSelect")))
        STOP
                
 */
 
/* ******************** messages about bad forward selections ******************** */

TACTIC BadForward (pattern, stepname, shape) IS 
    BadForward2 pattern stepname shape (" of the form %s", shape)

MACRO BadForward2(pattern, stepname, shape, stuff) IS
    WHEN    (LETHYP pattern /* right hypothesis, other things wrong */ 
                (BadForward3 pattern stepname shape stuff)) /* this looks wrong. RB 20.vi.2005 */
            (LETHYP _Ah /* wrong hypothesis, other things wrong as well */
                (ComplainForwardWrongHyp stepname shape _Ah))
            /* no hypothesis at all -- just complain about that */
            (ComplainForwardNoHyp stepname stuff "")

TACTIC BadForward3(sel, stepname) IS
    WHEN    (LETOPENSUBGOALS _Ags
                (ALERT  ("There is more than one unproved conclusion (formula below a line of dots) \
                        \associated with the hypothesis %t. \
                        \\nSelect one of them (%l) before you make a forward step.", sel, (_Ags, ", ", " or "))
                        ("OK", SKIP) ("Huh?", ExplainHypMulti stepname sel _Ags)))
            (LETOPENSUBGOAL G _Ag
                (Fail ("Single subgoal in BadForward3. Error in I2LJape -- tell Richard Bornat.")))
            (ALERT  ("There are no unproved conclusions associated with the hypothesis %t.", sel)
                        ("OK", SKIP) ("Huh?",  ExplainDeadHyp stepname sel))

/* ******************** the Forward menu ******************** */

TACTIC "→ elim forward" IS
    WHEN    
        (LETHYP2 _A (_A→_B) 
            (Noarg (CUTIN "→ elim" (WITHHYPSEL (hyp (_A→_B))) (WITHHYPSEL (hyp _A))) "→ elim"))
        (LETHYP2 _C (_A→_B)
            ("→ elim forward fail"
                (" You selected %t, which is of the form A→B, but your other selection %t doesn't match %t.", _A→_B, _C, _A)))
        (LETHYP2 _A _B
            ("→ elim forward fail" (" Neither of your selections (%t and %t) is of the form A→B.", _A, _B)))
        (LETHYP (_A→_B) 
            (WHEN (LETCONC _C 
                    (Noarg (CUTIN "→ elim" (WITHHYPSEL hyp) fstep) "→ elim"))
                    ("→ elim forward fail" (" You only selected %t.", _A→_B))))
        (LETHYP _A
            ("→ elim forward fail" (" You selected %t, which isn't of the form A→B.", _A)))
        (LETHYPS _A
            ("→ elim forward fail" (" You selected too many hypotheses – %l.", (_A, ", ", " and "))))
        ("→ elim forward fail" (" You didn't select anything."))
                
TACTIC "→ elim forward fail" (extra) IS
        Fail    ("To make a forward step with → elim you must select something of the form A→B, \
            \and something which matches A (or a target conclusion).%s", extra)

MENU Forward IS
    ENTRY   "∧ elim (preserving left)"      IS Forward (QUOTE (_A∧_B)) (ForwardCut 0 (Noarg (LAYOUT "∧ elim" (0) "∧ elim(L)") "∧ elim"))  "∧ elim" "∧ intro" "A∧B"
    ENTRY   "∧ elim (preserving right)"     IS Forward (QUOTE (_A∧_B)) (ForwardCut 0 (Noarg (LAYOUT "∧ elim" (0) "∧ elim(R)") "∧ elim")) "∧ elim" "∧ intro" "A∧B"
    ENTRY   "→ elim"                        IS "→ elim forward"
    ENTRY   "∨ elim (makes assumptions)"    IS Forward (QUOTE (_A∨_B)) 
                                                        (Noarg ("targeted forward" (ForwardUncut 0 "∨ elim") "∨ elim") "∨ elim") 
                                                        "∨ elim" "∨ intro" "A∨B"
    ENTRY   "¬ elim"                        IS "¬ elim forward"
    ENTRY   "∀ elim (needs variable)"       IS "∀ elim forward"
    ENTRY   "∃ elim (assumption & variable)"    IS Forward (QUOTE (∃_x._A)) 
                                                            (Noarg ("targeted forward" (ForwardUncut 0 "∃ elim") "∃ elim") "∃ elim") 
                                                            "∃ elim" "∃ intro" "∃x.A"
    ENTRY   "contra (constructive)"         IS Forward ⊥  (Noarg ("targeted forward" "contra (constructive) forward" "contra (constructive)") 
                                                                  "contra (constructive)")
                                                           "contra (constructive)" "¬ elim" ⊥

    SEPARATOR
    ENTRY   "∧ intro"                      IS "∧ intro forward"

    ENTRY   "∨ intro (invents right)"      IS "∨ introforward" "∨ intro(L)"
    ENTRY   "∨ intro (invents left)"       IS "∨ introforward" "∨ intro(R)"
    

    /* removed -- see comment on the tactic below
    ENTRY   "∃ intro (needs variable)"      IS "∃ intro forward"
    */

    SEPARATOR
    ENTRY   hyp IS Noarg hyptac hyp
END

TACTIC "contra (constructive) forward" IS
    WHEN    (LETGOAL _A
            "contra (constructive)"
            (WHEN
                (LETHYP ⊥ (WITHHYPSEL (hyp ⊥)))
                (hyp ⊥)))
        (ForwardCut 0 (Noarg "contra (constructive)" "contra (constructive)"))

TACTIC "¬ elim forward" IS
    WHEN    
        (LETHYP2 _A (¬_A) 
            (Noarg (CUTIN "¬ elim" (WITHHYPSEL (hyp _A)) (WITHHYPSEL (hyp (¬_A)))) "¬ elim"))
        (LETHYP (¬_A)
            (Noarg (CUTIN "¬ elim" fstep (WITHHYPSEL (hyp (¬_A)))) "¬ elim"))
        (LETHYP2 _A _B
            (ALERT  ("To make a forward step with ¬ elim, it's necessary to select two hypothesis formulae -- one to match \
                        \A, the other to match ¬A. \
                        \You selected %t and %t.", _A, _B)
                    ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP)))
        (LETHYP _A 
            (ALERT  ("To make a forward step with ¬ elim, it's necessary to select two hypothesis formulae -- one to match \
                        \A, the other to match ¬A. \
                        \You only selected %t.", _A)
                    ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP))) 
        (LETHYPS _As 
            (ALERT  ("To make a forward step with ¬ elim, it's necessary to select only two hypothesis formulae to combine. \
                        \You selected %l.", (_As, ", ", " and "))
                    ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP))) 
        (LETGOAL ⊥
            (ALERT  ("To make a forward step with ¬ elim, it's necessary to select two hypothesis formulae -- one to match \
                        \A, the other to match ¬A. \
                        \You didn’t select any at all.\
                        \\n\nHowever, the current conclusion fits ¬ elim going backwards. Did you perhaps mean to \
                        \make a backward step?", _A∧_B)
                    ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP)))
        (ALERT  ("To make a forward step with ¬ elim, it's necessary to select two hypothesis formulae -- one to match \
                    \A, the other to match ¬A. \
                    \You didn’t select any at all.")
                ("OK", STOP) ("Huh?", SEQ ExplainClicks STOP))


TACTIC "targeted forward" (action, stepname) IS
    WHEN        
        (LETHYP _Ah ("targeted forward 2" action stepname _Ah))
        (LETLHS _Ah ("targeted forward 2" action stepname _Ah))
        (Fail ("Error in I2LJape (targeted forward falls through). Tell Richard Bornat."))

TACTIC "targeted forward 2"(action, stepname, hyp) IS
    WHEN    
        (LETCONC _A action) 
        (LETOPENSUBGOAL G _Ag ("targeted forward single" action stepname G hyp _Ag))
        (LETOPENSUBGOALS _As
            (ALERT  ("The %s step needs a target conclusion to work towards. \
                        \Please click on one of the unproved conclusions – %l – and try the \
                        \%s step again.", stepname, (_As, ", ", " or "), stepname)
                    ("OK", STOP) ("Huh?", Explainunprovedconclusionwords)))
        (BadForward3 hyp stepname) 

MACRO "targeted forward single"(action, stepname, path, selhyp, ogoal) IS
    LETGOALPATH G1
        (WHEN 
                (LETUNIFY path G1 action) /* just do it, it's the next line */
                (ALERT  ("The %s step needs a target conclusion to work towards. \
                            \There's only one unproved conclusion (%t) which is relevant to the hypothesis %t \
                            \which you selected.\
                            \\nWould you like to proceed, using %t as the target?\
                            \\n(You can avoid this message in the future if you always select the target conclusion \
                            \before making the %s step.)", stepname, ogoal, selhyp, ogoal, stepname)
                        ("OK", SEQ (GOALPATH path) action) 
                        ("Huh?", SEQ Explainhypothesisandconclusionwords STOP)
                        ("Cancel", STOP)))

/* ******************** explanations ******************** */

TACTIC Explainvariables IS
ALERT   "Variables are introduced into a proof by ∀ intro and/or ∃ elim. They appear at the head of a box \
        \as a special pseudo-assumption actual i (this is Jape's version of scope-boxing; it uses variable \
        \names i, i1, and so on as appropriate). \
        \\n\nYou have to select one of these pseudo-assumptions each time you make a ∀ elim or ∃ intro step.\
        \\n\n\
        \The box enclosing actual ... is the scope of the variable. If there are unknowns in your proof, \
        \Jape will protect the scope box \
        \by putting a proviso such as i NOTIN _B in the Provisos pane .  If you try to \
        \break the scope using hyp or Unify then Jape will stop you, quoting the proviso."

TACTIC Explainprovisos IS
    ALERT   "The proviso i NOTIN _B (for example) appears when the unknown _B occurs both inside \
            \AND outside variable i's scope box. It means that nothing unified with _B can contain \
            \the variable i, because that would produce an occurrence of i outside its scope box."

/* ******************** we don't like some of the automatic alerts ******************** */

PATCHALERT "double-click is not defined" 
    "Double-clicking a formula doesn't mean anything in I2L Jape: you have to select a formula \
    \(single-click) and then choose a step from the Backward or Forward menu."
    ("OK") ("Huh?", HowToFormulaSelect)

/* I can no longer find the code that generated this alert, but I've left the patch just in case */

PATCHALERT "You double-clicked on a used hypothesis or a proved conclusion"
    "Double-clicking a formula doesn't mean anything in I2L Jape: you have to select a formula \
    \(single-click) and then choose a step from the Backward or Forward menu."
    ("OK") ("Huh?", HowToFormulaSelect)
    
/* ******************** and we do our own (careful) unification ******************** */

MENU Edit
    ENTRY Unify IS I2Lunify
END

/* really, this is a product of the stupid notion that LETMULTIARGS doesn't match one ... oh well, fix it another day */
TACTIC DOunify IS
  ALT UNIFYARGS
      (BADUNIFY X Y 
        (ALERT  ("%t and %t don't unify, because the formula structures don't match.", X, Y) 
                ("OK", STOP)))
      (BADMATCH X Y 
        (ALERT  ("%t and %t don't unify, because to do so would change the proof \
                 \(if you see this message, tell Richard Bornat).", X, Y) 
                ("OK", STOP)))
      (BADPROVISO X Y P 
        (ALERT  ("%t and %t don't unify, because to do so would smuggle a variable out of its scope \
                   \(see the proviso %s in the proviso pane).", X, Y, P)
                ("OK", STOP) ("Huh?", SEQ Explainprovisos STOP)))

TACTIC I2Lunify IS
    WHEN    
        (LETMULTIARG _As DOunify)
        (LETARGSEL _A DOunify)    
        (SEQ (ALERT "To use Unify, you must subformula-select at least one formula."
                    ("OK", SKIP) ("Huh?", (SHOWHOWTO "TextSelect")))
             STOP)
