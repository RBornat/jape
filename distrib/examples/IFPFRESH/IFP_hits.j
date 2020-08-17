/*      
        Mouse-clicks on formulae
*/

HYPHIT  P   ⊢ P IS hyp   
HYPHIT  P∧Q ⊢ R IS ALT  (SEQ "∧ elim(L)" (WITHHYPSEL hyp))
                        (SEQ "∧ elim(R)" (WITHHYPSEL hyp))
                        (ForwardAndElimBoth)
                        
HYPHIT  P∧Q ⊢ P IS (SEQ "∧ elim(L)" (WITHHYPSEL hyp))
HYPHIT  P∧Q ⊢ Q IS (SEQ "∧ elim(R)" (WITHHYPSEL hyp))
HYPHIT  P∧Q ⊢ R IS (SEQ ForwardAndElimL ForwardAndElimR)
HYPHIT  P↔Q ⊢ P IS ForwardIffElimL
HYPHIT  P↔Q ⊢ Q IS ForwardIffElimR
HYPHIT  P↔Q ⊢ R IS ForwardIffElimBoth

HYPHIT  P→Q  ⊢ R    IS ForwardImpElim
HYPHIT  P, P→Q  ⊢ R    IS ForwardImpElim

HYPHIT  P∨Q  ⊢ R    IS ForwardUncut 0 "∨ elim"
HYPHIT  ¬¬P  ⊢ Q    IS NotNotElimHit
HYPHIT  ¬P   ⊢ ⊥    IS NotElim
HYPHIT  ¬P,R ⊢ Q    IS ForwardNotElim
HYPHIT  ∀x.P ⊢ Q           IS ForwardAllElim
HYPHIT  fresh i, ∀x.P ⊢ Q    IS ForwardAllElim
HYPHIT  ∃x.P ⊢ Q    IS (WITHARGSEL "∃ elim")  
HYPHIT  ⊥   ⊢ Q    IS "⊥ elim" 

/************************** = **********************************/

HYPHIT T1=T2 ⊢ Q IS ForwardEqElim

TACTIC ForwardEqElim IS
       WHEN (LETHYPSUBSTSEL (_P[_x\_S]) 
                    (LETHYP (_T1=_T2) 
                            (ALT (SEQ (UNIFY _S _T1) (cut (_P[_x\_T2])) (SEQ ("= elim") hyp) )
                                 (ForwardEqElimExplain (_T1=_T2))
                            )))
            (LETHYP (_T1=_T2) (ALT (LETCONC _P (SEQ ("= elim") hyp)) (ForwardEqElimExplain (_T1=_T2))))
            
TACTIC ForwardEqElimExplain(T) IS
       ALERT ("You selected the equation %s; but you either need to select a conclusion\n\
              \or to select an instance of the left-hand of the equation within a formula.", T)
              ("OK", STOP)
              ("Explain = elim", "Explain = elim")

MENU "= Rules" IS
   ENTRY "= intro"
   ENTRY "= elim"
   SEPARATOR
   ENTRY "= elim (select t1=t2) (text-select t1 in Φ(t1) to cut with Φ(t2))" IS 
         ForwardEqElim
   SEPARATOR
   RULE "= sym"    IS FROM T1=T2 INFER T2=T1   
   RULE "= trans"  IS FROM T1=T2 AND T2=T3 INFER T1=T3
   SEPARATOR
   TACTIC "= sym (forward)" IS 
          WHEN (LETHYP  (_T1=_T2) (cut (_T2=_T1)) ("= sym"))
               (Fail "'= sym (forward)' requires you to select an equation")
END

/***************************************************************/

CONCHIT Q∧R     IS "∧ intro"
CONCHIT Q∨R     IS (ALT 
                    (SEQ "∨ intro(L)" hyp) (SEQ "∨ intro(R)" hyp) 
                    (Fail "Neither of the ∨ intro rules is immediately applicable"))
CONCHIT Q→R     IS "→ intro"      
CONCHIT ¬Q      IS "¬ intro"       
CONCHIT ∀x.Q    IS "∀ intro"  

CONCHIT fresh i ⊢ ∃x.Q IS VarExIntro
CONCHIT H ⊢ ∃x.Q IS (SEQ "∃ intro" (ALT (WITHHYPSEL hyp)))
CONCHIT ⊢ ∃x.Q   IS (SEQ "∃ intro" )

MENU "Rules" IS
  ENTRY hyp
  ENTRY "¬ elim" IS NotElim
  ENTRY "⊥ elim"  
  SEPARATOR
  SEPARATOR
  ENTRY "→ elim (forward)"    IS ForwardImpElim
  ENTRY "¬ elim (forward)"    IS ForwardNotElim
  ENTRY "¬¬ elim (forward)"   IS ForwardNotNotElim
  SEPARATOR
  ENTRY "∧ elim(L) (forward)" IS ForwardAndElimL
  ENTRY "∧ elim(R) (forward)" IS ForwardAndElimR
  ENTRY "∧ intro (forward)"    IS ForwardAndIntro("∧ intro")
  ENTRY "∨ intro(R) (forward)" IS ForwardOrIntro("∨ intro(R)")
  ENTRY "∨ intro(L) (forward)" IS ForwardOrIntro("∨ intro(L)")
  ENTRY "↔ elim(L) (forward)" IS ForwardIffElimL
  ENTRY "↔ elim(R) (forward)" IS ForwardIffElimR
END


/* Entries test individual tactics 
MENU "[Test Tactics]" IS
  ENTRY TwoHypsNotElim
  ENTRY ForwardNotElim
END
*/


TACTIC SelectFail(x) IS (ALERT x ("OK", STOP) ("Show the current selections", SHOWSELECTIONS))

/************************** Negation *************************/
TACTIC ForwardNotNotElim
 WHEN (LETHYP (¬¬_B) (SEQ (cut(_B)) ("¬¬ elim"(_B))))
      (Fail "¬¬ elim (forward) cannot be used here")
 
TACTIC NotNotElimHit
 WHEN (LETHYP (¬¬_B) 
         (ALT (LETGOAL (_G) 
                 /* We cannot simply say: (LETGOAL (_B) ...) here.
                    The LET forms introduce NEW pattern variables,
                    so we need to check that the goal is the hyp
                    with a (LETUNIFY _B _G ...)
                 */
                 /* (ALERT ("Goal is %s |- %s", _B, _G)) */
                 /* Goal same as hyp: just apply the rule */ 
                     (LETUNIFY _G _B ("¬¬ elim"[B\_B])))
                 /* Goal distinct from hyp: cut with _B, then apply the rule */
                     (SEQ (cut[B\_B]) ("¬¬ elim"[B\_B]) (WITHHYPSEL (hyp)))))
      (Fail "¬¬ elim cannot be used forwards or backwards here")
 
TACTIC NotElim IS
 WHEN 
  (LETGOAL ⊥
   (WHEN
    (LETHYP2 (B) (¬ _B)  (TwoHypsNotElim (⊥)))      
    (LETHYP (¬ _B)    (SEQ ("¬ elim" _B) (WITHHYPSEL (hyp (¬ _B)))))      
    (Fail "¬ elim needs a goal of ⊥ and a selected hypothesis of the form ¬P")
    ))
  (Fail "Your goal should be ⊥: try ⊥-elim or RAA first")

TACTIC TwoHypsNotElim(G) IS
 WHEN 
  (LETGOAL ⊥
   (WHEN
    (LETHYP2 _B (¬ _B)  
        (ALT (SEQ  ("¬ elim" _B) (WITHHYPSEL (hyp (¬ _B))) (WITHHYPSEL (hyp _B)))
             (ALERT ("¬ elim is not applicable with selected hyps %s and %s and goal %s.\n\
                    \(this may have happened because you refused to select a rule-match)", _B, ¬_B, G)
                    ("OK", STOP)
                    ("Explain 'rule-match'", "The rule ... matches in N different ways ...")
                    ))
        )      
     (TwoHypsNotElimError(G))))
  (Fail "Your goal should be ⊥: try ⊥-elim or RAA first")
  
  
TACTIC TwoHypsNotElimError(G) IS
  WHEN (LETHYP2 _A (¬ _B) 
             (ALT (SEQ (UNIFY _A _B)
                       (Fail ("You selected goal %s and hypotheses %s and %s\n\
                              \but ¬-elim cannot be applied", G, _A, (¬ _B))))
                  (ALERT ("You selected goal %s and hypotheses %s and %s\n\
                              \but ¬-elim cannot be applied, because the hypotheses cannot be made to match\n\
                              \(if it is not obvious why, then perhaps a proviso was violated)?", G, _A, (¬ _B))
                              ("OK", STOP)
                              ("Explain provisos", "What are provisos?"))))
       (SelectFail "¬ elim needs a goal of ⊥ and selected hypotheses of the form Φ and ¬Φ")

TACTIC ForwardNotElim IS
 WHEN 
  (LETGOAL ⊥ (TwoHypsNotElim(⊥)))
  (LETHYP2 _B (¬ _A) (LETGOAL _G (SEQ (cut ⊥) (TwoHypsNotElim(_G)))))
  (SelectFail "Using ¬ elim forwards this way needs hypotheses of the form Φ and ¬Φ to be selected")

  
/************************** Disjunction *************************/
TACTIC ForwardOrIntro(introtac) IS
 WHEN 
   (LETHYP _A 
     (WHEN (LETARGSEL (_B) 
            (ALT (SEQ (cut (_A∨_B)) (introtac) (WITHHYPSEL (hyp)))
             (SelectFail ("You text-selected %s and formula selected the hypothesis %s.\
                    \\nBut %s (forward) failed: because %s couldn't be used immediately to prove conclusion %s",
                    _A, _B,  introtac, introtac, (_A ∨ _B))) 
            ))
           (SelectFail ("You formula-selected the hypothesis %s.\nYou also need to text-select Φ in order to use %s forward to establish %s∨Φ", 
                   _A, introtac, _A))))
   (LETARGSEL _A (Fail ("You text-selected %s.\nYou also need to formula-select a hypothesis Φ in order to use %s forward to establish %s∨Φ",
                      _A, introtac, _A)))
   (SelectFail ("To use %s forwards to establish Ψ∨Φ you need to formula-select a hypothesis Ψ and text-select Φ", introtac))

/************************** Conjunction *************************/
TACTIC ForwardAndIntro(introtac) IS
 WHEN 
   (LETHYP _A 
     (WHEN (LETARGSEL (_B) 
            (ALT (SEQ (cut (_A∧_B)) (introtac) (WITHHYPSEL (hyp)))
             (SelectFail ("You text-selected %s and formula selected the hypothesis %s.\
                    \\nBut %s (forward) failed: because %s couldn't be used immediately to prove conclusion %s",
                    _A, _B,  introtac, introtac, (_A ∧ _B))) 
            ))
           (SelectFail ("You formula-selected the hypothesis %s.\nYou also need to text-select Φ in order to use %s forward to establish %s∧Φ", 
                   _A, introtac, _A))))
   (LETARGSEL _A (Fail ("You text-selected %s.\nYou also need to formula-select a hypothesis Φ in order to use %s forward to establish %s∧Φ",
                      _A, introtac, _A)))
   (SelectFail ("To use %s forwards to establish Ψ∧Φ you need to formula-select a hypothesis Ψ and text-select Φ", introtac))
  
TACTIC ForwardAndElimL IS
 WHEN 
  (LETHYP (_A ∧ _B) (SEQ (cut (_A)) "∧ elim(L)") (WITHHYPSEL (hyp(_A ∧ _B)))) 
  (SelectFail "You need to select a conjunctive hypothesis for forward ∧-elim(L) to work")

TACTIC ForwardAndElimR IS
 WHEN 
  (LETHYP (_A ∧ _B) (SEQ (cut (_B)) "∧ elim(R)")(WITHHYPSEL (hyp(_A ∧ _B)))) 
  (SelectFail "You need to select a conjunctive hypothesis for forward ∧-elim(R) to work")
  
TACTIC ForwardAndElimBoth IS 
  WHEN
   (LETHYP (_A ∧ B) (SEQ ForwardAndElimL ForwardAndElimR))
   (SelectFail "You need to select a conjunctive hypothesis for forward ∧-elim to work")
   
/************************************** iff *********************************************/
TACTIC ForwardIffElimL IS
 WHEN 
  (LETHYP (_A ↔ _B) (SEQ (CUTIN "↔ elim(L)" (WITHHYPSEL (hyp))))) 
  (SelectFail "You need to select an ↔ hypothesis for forward ↔-elim(L) to work")

TACTIC ForwardIffElimR IS
 WHEN 
  (LETHYP (_A ↔ _B) (SEQ (CUTIN "↔ elim(R)" (WITHHYPSEL (hyp))))) 
  (SelectFail "You need to select an ↔ hypothesis for forward ↔-elim(R) to work")
  
TACTIC ForwardIffElimBoth IS 
  WHEN
   (LETHYP (_A ↔ B) (SEQ ForwardIffElimL ForwardIffElimR))
   (SelectFail "You need to select an ↔ hypothesis for forward ↔-elim to work")

/************************** Implication *************************/    
TACTIC ForwardImpElim IS
  WHEN   (LETHYP2 _A (_A → _B)
             (ALT
               (SEQ (cut _B) 
                    (WITHHYPSEL ("→ elim"[A,B\_A,_B])))
                    (Fail ("You selected %s and %s→%s, but → elim (forward) failed: Tell Bernard", _A, _A, _B))))
         (LETHYP (_A→_B)  (SEQ (cut _B) (WITHHYPSEL ("→ elim"[A,B\_A,_B]))))
       (SelectFail "You need to select a hypothesis of the form Φ→Ψ for → elim (forward) (you can also select the Φ)")
       
/************************** ∃ ****************************************/

TACTIC VarExIntro IS
  WHEN (LETHYP (fresh _i) (SEQ ("∃ intro"[T\_i]) (ALT hyp (SEQ))))
       (Fail "VarExIntro invoked without a fresh declaration selected.")

TACTIC ForwardExIntro IS 
  WHEN (LETHYPSUBSTSEL (_P[_x\_S]) 
                       /* (ALERT ("%s %s %s", _P, _x, _S)) */
                       (CUTIN ("∃ intro"[x,P\_x,_P]) (hyp (_P[_x\_S]))))
       (Fail ("You have invoked ∃ intro (forward).\n\
               \The ∃ intro rule is: FROM Φ(ζ) INFER ∃ x∙Φ(x).\n\
               \Before doing this you should have text-selected a term ζ from within a hypothesis formula Φ(ζ) \
               \so as to make clear what you meant."))
              

/************************** ∀ ****************************************/

CONSTANT ζ

TACTIC ForwardAllElim IS 
    WHEN (LETARGSEL (_T)
         (ALT
          (LETHYP (∀ _x . _P) 
                  (CUTIN "∀ elim" [T\_T] (WITHHYPSEL (hyp))))
                   /* Didn't seem to be able to do this with cut/∀ elim/hyp
                      Maybe I didn't specify them enough and the following might have worked:
                        (SEQ (cut[B\(_P[_x\_T])] ("∀ elim" [T\_T]) (WITHHYPSEL (hyp)))) 
                      but, as they say, IIABDFI!
                   */
          (ForwardAllElimSelectFail)))
       (LETHYP2 (fresh _T) (∀ _x . _P) (CUTIN "∀ elim" [T\_T] (WITHHYPSEL (hyp))))
       (LETHYP (∀ _x . _P)
        (ALERT 
             ("You have invoked ∀ elim (forward) by double-clicking or from the Quantifier Rules menu, \
              \with selected hypothesis\n   %s\n\n\
              \For this to work, you need to text-select a term, ζ (say), to substitute for %s in\n   %s\n\n\
              \The result will be\n   %s\n\n\
              \If you select an assumption of the form (fresh ν) then Jape will behave as if you had text-selected ν\n\n\
              \If you really want Jape to invent a term proof variable for you, then you can \
              \invoke '∀ elim (forward, inventing a proof variable)' below, or from a menu.", (∀ _x . _P), _x, _P, (_P[_x\ζ]))
              ("OK", STOP)
              ("∀ elim (forward, inventing a proof variable)", ForwardAllElimInvent)))
       (ForwardAllElimSelectFail)
       

TACTIC ForwardAllElimSelectFail IS 
       (SelectFail "If you want to use ∀ elim (forward) at this point, then \
              \you need to select a conclusion, \n\
              \and a formula of the form ∀ x . ϕ(x), \n\
              \and text select a term T, or a 'fresh' declaration.")
              
     
TACTIC ForwardAllElimInvent IS
   WHEN (LETHYP (∀ _x . _P) (CUTIN "∀ elim"  (WITHHYPSEL (hyp))))
        (SelectFail "If you want to use ∀ elim and have it invent a term at this point, then \
              \you need to select a formula of the form ∀ x . ϕ(x).")

MENU "Quantifier Rules" IS

SEPARATOR
   
   ENTRY "∃ intro (forward) (needs a text-selected term in a hypothesis; invents a variable)" IS ForwardExIntro

SEPARATOR
   
   ENTRY "∀ elim (forward)" IS ForwardAllElim
   ENTRY "∀ elim (forward, inventing a term proof-variable)" IS ForwardAllElimInvent
   ENTRY "∀⊢ (derived)" IS "∀⊢"
END
              
/************************* Helpistry *********************************/

TACTIC EXPLAINTHENSTOP(x) IS
    SEQ (ALERT x) STOP
    
 PATCHALERT "double-click is not defined"
            "Double-clicking has no effect with the current selection(s).\n\
            \You can find out what (IFP Jape) thinks the current selection is from the Help menu.\n\
            \You will also find explanations there of how to make selections." 
            ("OK") 
 
 TACTIC SHOWSELECTIONSWITHTEXT(caption, value) IS
             WHEN (LETCONC _G (ALT (LETHYPS _H 
                                     (ALERT ("Selected hypothes(is/es)\n   %s\n\n\
                                             \Selected conclusion\n   %s\n\n%s %s", _H, _G, caption, value)
                                            ("OK", STOP)))
                                     (ALERT ("No Selected hypothesis\n\n\
                                             \Selected conclusion\n   %s\n\n%s %s", _G, caption, value)
                                            ("OK", STOP))
                              ))
                  (LETHYPS _H (ALERT ("Selected hypothes(is/es) \n   %s\n\n\
                                      \(no selected conclusion)\n\n%s %s", _H, caption, value)
                                     ("OK", STOP))
                              )
                  (LETCONC _G (ALERT ("Selected conclusion\n   %s\n\n\
                                      \(no selected hypothesis)\n\n%s %s", _G, caption, value)
                                     ("OK", STOP))
                              )
                  (ALERT ("No selected hypothesis or conclusion.\n\n%s %s", caption, value) 
                         ("OK", STOP))
 
 TACTIC SHOWSELECTIONS IS
        WHEN     (LETMULTIARG _T (SHOWSELECTIONSWITHTEXT "Selected texts:" _T))
                 (LETARGSEL _T   (SHOWSELECTIONSWITHTEXT "Selected text:" _T))
                 (SHOWSELECTIONSWITHTEXT "No selected" text)
                 
MENU Help IS
 TACTIC "Show the current formula(e) selection(s)" IS SHOWSELECTIONS
 TACTIC "Explain help" IS 
        EXPLAINTHENSTOP "General help for Jape can be found on the Help menu.\n\
                        \The IFP logic is explained in the notes for \
                        \the Oxford University course \"Introduction to Formal Proof\"."
 SEPARATOR
 SEPARATOR
 TACTIC "Explain Text selection (also called subformula selection)"  IS (SHOWHOWTO TextSelect)
 TACTIC "Explain Formula selection" IS (SHOWHOWTO FormulaSelect) 
 SEPARATOR
 SEPARATOR
 TACTIC "Explain = elim" IS 
   ALERT "To make a forward inference of the form Φ(τ2) from an hypothesis of the form τ1=τ2 \
         \(when the goal conclusion is already of the form Φ(τ2)) just double-click \
         \on the hypothesis τ1=τ2.\n\nWhen the goal conclusion is not already of the form Φ(τ2) \
         \then text-select the τ1 in an hypothesis of the form Φ(τ1) first."
         ("OK", STOP)
 SEPARATOR
 TACTIC "Hypotheses and Conclusions" IS 
   ALERT 
        "When you select a hypothesis, you get a downward-pointing selection (a box round the \
        \formula, open at the bottom). You work forward from a hypothesis selection.\n\n\
        \Unproved conclusion formulae are written with three dots above them. \
        \When you select an unproved conclusion, you get \
        \an upward-pointing selection (a box round the formula, open at the top). You can work \
        \backwards from a conclusion selection, or it can be a target for a forward step.\n\n\
        \Some formulae introduced by buttons labelled (forward) can be used as a hypothesis or as an \
        \unproved conclusion. The selection box for these has a dotted horizontal line. \
        \Click in the bottom half of the formula to make an \
        \hypothesis selection, in the top half for a conclusion selection.\n\n\
        \Any formula can be used as a hypothesis if there are relevant unproved conclusions below it \
        \in the proof."
        ("OK", STOP)
        ("Explain hypotheses in scope", "Hypotheses in scope")
 
 TACTIC "Meaning of ..." IS
   EXPLAINTHENSTOP  
        "An ... (ellipsis) in a proof-in-progress marks a place (an open goal) where there is still work to be done. \
        \The formula just below the ... is an unproved conclusion (notice that it doesn't have a reason beside it). \
        \Your job is to show that each unproved conclusion follows \
        \from the hypotheses in scope for it.\n\n\
        \When making a proof step you nearly always need have an open goal selected: \
        \IFP Jape is set up to do this automatically after each move; but you can \
        \change the goal by selecting another; and you can change Jape's policy from the Proof Style menu.\n\n\
        \When there are no unproved conclusions left, the proof is finished."
        
 TACTIC "Hypotheses in scope" IS
   EXPLAINTHENSTOP
        "The hypotheses in scope at a formula are the ones that are not greyed-out when the conclusion \
        \is selected.\n\n\
        \You can also find out what they are by changing the view to Tree display \
        \(though I don't recommend doing this in the middle of a large proof.)"
 
 TACTIC "The Rules menu and the [Rules] menu"
    ALERT 
        "The Rules menu contains frequently-used, and some derived rules. \n\
        \It also has buttons that invoke proof-tactics that appear to have a 'forward' effect: \
        \these operate by using 'cut' to establish a new formula in the context.\n\n\
        \The [Rules] menu has /all/ the rules on it: Jape invokes them without trying to be clever."
        ("OK", STOP)
        ("Explain the cut rule", "The cut rule")
       
 TACTIC "The cut rule" IS
     EXPLAINTHENSTOP
        "The rule cut(Φ) transforms a proof goal of the form Γ ⊢ κ into the two proof sub-goals: \n\n\
        \       Γ ⊢ Φ\n\
        \       Γ, Φ ⊢ κ\n\n\
        \In the box form display IFP Jape normally shows only the second subgoal, \
        \thereby giving the appearance that Ψ has been added to the context.\n\n\
        \Tactics labelled (forward) on the Rules menu generally operate by inserting a cut, \
        \then trying to use the rule named on their button to close the Γ ⊢ Φ goal. \
        \If that rule fails, then the tactic itself fails, and IFP Jape will try to explain why."

 TACTIC "The rule ... matches in N different ways ..." IS
     EXPLAINTHENSTOP
        "Occasionally a 'forward' tactic will put up a dialogue box that asks you to \
        \choose between different ways of applying a rule. Sometimes it shows \
        \ways that are not actually different[*], but in any case any of them can be safely selected.\n\n\
        \[*]What is happening here is that Jape is mis-reporting the fact that it has found two distinct lines with the same \
        \hypothesis on."
        
 TACTIC "What are provisos?" IS
    EXPLAINTHENSTOP
        "A proviso is Jape's representation of one of the side-conditions of a quantifier rule.\n\
        \Provisos usually constrain the way that proof-variables invented by Jape may later be resolved / bound.\n\
        \The provisos, if any, on proof-variables present in the current context are always visible on a panel \
        \below the proof display, that starts with the line 'Provided:'.\n\
        \It is possible that the invocation of a forward tactic, or of a quantifier rule with an inappropriate \
        \text-selection, would violate a proviso that you cannot yet see; in that case you will usually receive an explanation of what went wrong." 

END
















