/*
        $Id$
        
        Provides transitive reasoning style for the functional programming theories
        
*/

REFLEXIVE  RULE "= reflexive"
INITIALISE hidereflexivity true

TACTICPANEL Definitions 

	BUTTON	"Flatten LHS"	IS apply FlattenLHS
	BUTTON	"Flatten RHS"	IS apply FlattenRHS
	BUTTON	"Unfold LHS"	IS apply UnfoldL COMMAND
	BUTTON	"Unfold RHS"	IS apply UnfoldR COMMAND
	BUTTON	"Fold LHS"	IS apply FoldL   COMMAND
	BUTTON	"Fold RHS"	IS apply FoldR   COMMAND

END

CONJECTUREPANEL Conjectures 

	BUTTON	"Flatten LHS"	IS apply FlattenLHS
	BUTTON	"Flatten RHS"	IS apply FlattenRHS
	BUTTON	"Unfold LHS"	IS apply UnfoldL COMMAND
	BUTTON	"Unfold RHS"	IS apply UnfoldR COMMAND
	BUTTON	"Fold LHS"	IS apply FoldL   COMMAND
	BUTTON	"Fold RHS"	IS apply FoldR   COMMAND
	
	/* The Jape engine gets the feedback wrong for CHECKBOXES in panels
	CHECKBOX  hidetransitivity
	        "Transitive Style" 
	INITIALLY true
	*/
	

END

MENU Transitivity IS

	CHECKBOX  hidetransitivity
	        "Transitive Presentation Style" 
	INITIALLY true
	
END

TACTIC FlattenLHS IS 
       (LETGOALPATH G
         "= transitive"
         (GOALPATH (SUBGOAL G 0))
         (LAYOUT "Associativity" (1)
          (LETGOAL (_X = _Y) (FLATTEN(_X)) "= reflexive")))
       
TACTIC FlattenRHS IS 
     (LETGOALPATH G
         "= transitive" 
         (GOALPATH (SUBGOAL G 1))
          (LAYOUT "Associativity" (1)
            (LETGOAL (_X = _Y) 
                  (FLATTEN(_Y)) 
                  "= reflexive")))
       

MENU Unfolding IS
 ENTRY "Unfold LHS"     IS (UnfoldL SearchTactic)
 ENTRY "Unfold RHS"     IS (UnfoldR SearchTactic)
 ENTRY "Fold LHS"       IS (FoldL SearchTactic)
 ENTRY "Fold RHS"       IS (FoldR SearchTactic)
 ENTRY "Flatten LHS"	IS FlattenLHS
 ENTRY "Flatten RHS"	IS FlattenRHS
 /* 
 CHECKBOX tactictracing "Trace" 
 */
END

TACTIC UNFOLDWITH1(rule1)        IS (LAYOUT "Unfold" () rule1) 
TACTIC UNFOLDWITH2(rule1, arg)   IS (LAYOUT "Unfold" () (rule1 arg)) 
TACTIC FOLDWITH1(rule1)          IS (LAYOUT "Fold" () "= symmetric" rule1) 
TACTIC FOLDWITH2(rule1, arg)     IS (LAYOUT "Fold" () "= symmetric" (rule1 arg))
TACTIC USEHYPORRULE(rule)        IS (WHEN (LETHYP _H (WITHHYPSEL hyp)) (ALT hyp rule))

/* 
        Search the expression tree structure
        
        We could do with a ``MAPTERM'' that applies
        a rule to successive subterms of a particular term.
        The following search is horribly inefficient.
*/
            
TACTIC FunApply(rule) IS (rule)

TACTIC FunRecurse(continue, rule) IS 
       (ALT (continue rule)
            (SEQ FunctionOperator (continue rule))
            (SEQ FunctionOperand  (continue rule))
            (SEQ LeftPair         (continue rule))
            (SEQ RightPair        (continue rule))
            (SEQ Singleton        (continue rule)))
            
TACTIC FunSearch1(rule) IS (FunRecurse FunApply rule)
TACTIC FunSearch2(rule) IS (FunRecurse FunSearch1 rule)
TACTIC FunSearch3(rule) IS (FunRecurse FunSearch2 rule)
TACTIC FunSearch4(rule) IS (FunRecurse FunSearch3 rule)

/********************************************
        
        Supplementary (derivable) inference rules
        here only for reasons of efficiency.
        
*/

RULE FunctionOperand  IS FROM X=Y  INFER F X = F Y
RULE FunctionOperator IS FROM F=G  INFER F X = G X
RULE LeftPair         IS FROM X=X' INFER (X, Y)=(X', Y)
RULE RightPair        IS FROM Y=Y' INFER (X, Y)=(X, Y')
RULE Singleton        IS FROM Y=Y' INFER [Y]=[Y']

/*******************************************/

       
/*******************************************/

HYPHIT  H æ C IS  UnfoldHypWithOptionalSelectionLHS

TACTIC UnfoldHypWithOptionalSelectionLHS() IS
(LETHYP (_A=_B)  /* Use selected hypothesis to rewrite everywhere */
        (LETCONC (_X=_Y)
           "= transitive" 
            (ALT
               (LAYOUT "Unfold with hyp" (1) (rewriteLR{X\_A}) (WITHHYPSEL hyp))
               (Fail "Cannot use the selected hypothesis to unfold on the LHS"))))

TACTIC UnfoldL(rule) IS
(WHEN 
      (LETSUBSTSEL (_A{_x\_F}=_B)       /* Rewrite the selection */
        "= transitive" 
        (LAYOUT "Unfold %s" () 
                (rewriteLR{X,AA,x\_F,_A,_x}) 
                (USEHYPORRULE rule)))
      
     (LETCONCFIND (_XOLD=_YOLD, _XNEW=_YNEW)
       (LETGOALPATH G
         "= transitive" 
         (ALT 
          (LAYOUT "Associativity" (2)
             (associativity _XNEW _XOLD) EVALUATE)
          (LETARGSEL _XSEL (Fail ("%s isn't a subterm", _XSEL))))))
     
     (LETHYP (_A=_B)  /* Use selected hypothesis to rewrite everywhere */
              (LETCONC (_X=_Y)
                       "= transitive" 
                       (ALT
                         (LAYOUT "Unfold using hyp" (1) (rewriteLR{X\_A}) (WITHHYPSEL hyp))
                         (Fail "Cannot use the selected hypothesis to unfold on the LHS"))))
                       
      (LETGOAL (_A=_B)                      /* No selection: a little automation */
       (LETGOALPATH G
        "= transitive"
        (ALT (UNFOLDWITH1 rule)             /* Close with the rule */
             (UNFOLDWITH2 FunSearch4 rule)  /* Close by rewriting inside the formula */
             (Fail "Nothing obvious to unfold on the LHS"))
        (GOALPATH (SUBGOAL G 1))))
      
      (Fail (Cannot unfold because there is no equation)))

RULE    associativity(Y, Y') IS
FROM    ASSOCEQ (Y, Y') 
INFER   Y'=Y


TACTIC UnfoldR(rule) IS
(WHEN 
      (LETSUBSTSEL (_A=_B{_x\_F}) 
          (LETGOALPATH G
            "= transitive" 
            (GOALPATH (SUBGOAL G 1)) 
            (LAYOUT "Fold" () 
                    (rewriteRL{X,AA,x\_F,_B,_x}) 
                    (USEHYPORRULE rule))))
                    
      (LETCONCFIND (_XOLD=_YOLD, _XNEW=_YNEW)
       (LETGOALPATH G
         "= transitive" (GOALPATH (SUBGOAL G 1))
         (ALT 
          (LAYOUT "Associativity" (2)
             (associativity _YOLD _YNEW) EVALUATE)
          (LETARGSEL _XSEL (Fail ("%s isn't a subterm", _XSEL))))))

      (LETHYP (_A=_B)  /* Use selected hypothesis to rewrite everywhere */
              (LETCONC (_X=_Y)
                 (LETGOALPATH G
                       "= transitive" 
                       (GOALPATH (SUBGOAL G 1))
                       (ALT (LAYOUT "Fold using hyp" (1) (rewriteRL{X\_A}) (WITHHYPSEL hyp))
                            (Fail "Cannot use the selected hypothesis to unfold on the RHS")))))
                       
      (LETGOAL (_A=_B)                         /* No selection */
        (LETGOALPATH G
          "= transitive"
          (GOALPATH (SUBGOAL G 1)) 
          (ALT (FOLDWITH1 rule)            /* Close with the rule */
               (FOLDWITH2 FunSearch4 rule) /* Close by rewriting inside operand */
               (Fail "Nothing obvious to unfold on the RHS"))))
      
      (Fail (Cannot unfold because there is no equation)))

TACTIC FoldL(rule) IS
(WHEN 
        (LETSUBSTSEL (_A{_x\_F}=_B) 
                "= transitive" 
	        (LAYOUT "Fold" ()
	                (rewriteRL{Y,AA,x\_F,_A,_x})
	                (USEHYPORRULE rule)))
	                
        (Fail "To Fold LHS you need to text-select a subterm of the LHS, with or without a hypothesis"))
	                

TACTIC FoldR(rule) IS
(WHEN 
        (LETSUBSTSEL (_A=_B{_x\_F}) 
          (LETGOALPATH G
                "= transitive" 
		(GOALPATH (SUBGOAL G 1)) 
		(LAYOUT "Unfold" ()
		        (rewriteLR{Y,AA,x\_F,_B,_x}) 
		        (USEHYPORRULE rule))))
		        
        (Fail "To Fold RHS you need to text-select a subterm of the RHS, with or without a hypothesis"))































