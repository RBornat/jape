/* $Id$ */

MENU    Rules IS
    ENTRY   "Find"  IS FindSelection
    
    SEPARATOR
        
    ENTRY   "List Induction"    IS "list induction tactic"
    ENTRY   "Boolean cases" IS "Boolean cases tactic"
    ENTRY   "Monoid Definition" IS monoid
END

TACTIC JUSTFAIL       IS (ALT)
TACTIC JUSTAPPLY(tac) IS tac

MENU    "Searching" IS
/*  
    This menu has radio buttons which set tactic-valued variables.
    The tactic SearchTactic is an alternation of the current settings
    of these values: since JUSTFAIL is a unit of ALT, SearchTactic
    is effectively an alternation of the enabled groups of laws.
    
    The variable SearchHypotheses is slightly different: when set to JUSTAPPLY,
    it just applies its argument tactic;  otherwise it fails. It's
    called from 
*/
            
        RADIOBUTTON SearchHypotheses IS 
                "Search hypotheses" IS JUSTAPPLY
        AND     "Ignore hypotheses" IS JUSTFAIL
        INITIALLY   JUSTAPPLY
        END

        RADIOBUTTON list IS 
                "List rules enabled"  IS List
        AND     "List rules disabled" IS JUSTFAIL
        INITIALLY   List
        END
        
        RADIOBUTTON listthms IS 
                "List theorems enabled"  IS ListThms
        AND     "List theorems disabled" IS JUSTFAIL
        INITIALLY   JUSTFAIL
        END

        RADIOBUTTON function IS 
                "Function rules enabled"  IS Function
        AND     "Function rules disabled" IS JUSTFAIL
        INITIALLY   Function
        END

        RADIOBUTTON functionthms IS 
                "Function theorems enabled"  IS FunctionThms
        AND     "Function theorems disabled" IS JUSTFAIL
        INITIALLY   JUSTFAIL
        END
        
        RADIOBUTTON reflect IS 
                "Reflect rules enabled"  IS Reflect
        AND     "Reflect rules disabled" IS JUSTFAIL
        INITIALLY   JUSTFAIL
        END

        RADIOBUTTON reflectthms IS 
                "Reflect theorems enabled"  IS ReflectThms
        AND     "Reflect theorems disabled" IS JUSTFAIL
        INITIALLY   JUSTFAIL
        END
        
END

TACTIC  SearchTactic IS 
    ALT list listthms function functionthms reflect reflectthms

TACTICPANEL "Definitions" 
    TACTIC "Use any rule enabled by Searching" IS SearchTactic
    ENTRY   ":"
    ENTRY   "•"   
    ENTRY   "×"   
    ENTRY   "⊗"   
    ENTRY   "++"
    ENTRY   "cat"   
    ENTRY   "del"
    ENTRY   "filter"
    ENTRY   "fold"
    ENTRY   "fst" 
    ENTRY   "if"      
    ENTRY   "id"   
    ENTRY   "ins"
    ENTRY   "length"
    ENTRY   "map"
    ENTRY   "move"
    ENTRY   "none" 
    ENTRY   "one"  
    ENTRY   "rcat"
    ENTRY   "ref"
    ENTRY   "rev"  
    ENTRY   "rev2"
    ENTRY   "snd"  
    ENTRY   "swap" 
    ENTRY   "zip"
    ENTRY   L
    ENTRY   R

    BUTTON  "Unfold *"  IS apply RepeatedlyUnfold
    BUTTON  "Unfold"    IS apply UnfoldObvious COMMAND
    BUTTON  "Fold"  IS apply FoldObvious COMMAND
    BUTTON  "Apply" IS apply COMMAND
    BUTTON  "Flatten"   IS apply Flatten
    BUTTON  "Find"  IS apply FindSelection

    BUTTON  "UnfoldL"   IS apply UnfoldL COMMAND
    BUTTON  "UnfoldR"   IS apply UnfoldR COMMAND
    BUTTON  "FoldL" IS apply FoldL COMMAND
    BUTTON  "FoldR" IS apply FoldR COMMAND
END

TACTIC UnfoldL(rule) IS
    WHEN 
        (LETSUBSTSEL (_A{_x\_F}=_B) 
            "= transitive" 
            (LAYOUT "Unfold %s" () (rewriteLR{X,AA,x\_F,_A,_x})) 
            rule)
        (Fail "UnfoldL didn't find a substitution")

TACTIC UnfoldR(rule) IS
    WHEN 
        (LETSUBSTSEL (_A=_B{_x\_F}) 
            (LETGOALPATH G
                "= transitive" 
                (GOALPATH (SUBGOAL G 1)) 
                (LAYOUT "Fold %s" () (rewriteRL{X,AA,x\_F,_B,_x})) 
                rule
            )
        )
        (Fail "UnfoldR didn't find a substitution")

TACTIC FoldL(rule) IS
    WHEN 
        (LETSUBSTSEL (_A{_x\_F}=_B) 
            "= transitive" 
            (LAYOUT "Fold %s" () (rewriteRL{Y,AA,x\_F,_A,_x})) 
            rule)
        (Fail "FoldL didn't find a substitution")

TACTIC FoldR(rule) IS
    WHEN 
        (LETSUBSTSEL (_A=_B{_x\_F}) 
            (LETGOALPATH G
                "= transitive" 
                (GOALPATH (SUBGOAL G 1)) 
                (LAYOUT "Unfold %s" () (rewritesmalLR{Y,AA,x\_F,_B,_x}))
                rule
            )
        )
        (Fail "FoldR didn't find a substitution")

MENU Edit
    RADIOBUTTON textselectionmode IS
        "Subformula selection"  IS subformula
    AND "Token selection"       IS token
    INITIALLY subformula
    END
        
    RADIOBUTTON displaystyle IS
        "Box display style"     IS box
    AND "Tree display style"    IS tree
    INITIALLY box
    END
        
    RADIOBUTTON autoselect IS
        "Automatic goal selection"  IS true
    AND "Manual goal selection"     IS false
    INITIALLY true
    END

    RADIOBUTTON applyconjectures IS
        "Apply both conjectures and theorems"   IS true
    AND "Apply only theorems "                  IS false
    INITIALLY false
    END

    CHECKBOX hidetransitivity "transformational style" INITIALLY false
END

CONJECTUREPANEL "Conjectures" IS
        ENTRY "Use any rule enabled by Searching" IS SearchTactic

        THEOREMS    syntactic
        ARE monoid (++) []
        AND monoid rcat []
        END
        
        THEOREMS ListThms
        ARE rev • rev   = id
        AND rev2        = rev
        AND map F • map G   = map (F • G)
        AND cat (XS++YS)    = cat XS ++ cat YS
        AND map F • cat = cat • (map (map F))
        AND none • F    = none
        AND map F • none    = none
        AND map F • one = one • F
        AND map F • rev = rev • map F
        AND map id      = id
        AND length•map F    = length
        AND zip • (map F ⊗ map G) = map (F⊗G)
        AND map F • if P (G,G')   = if P (map F • G, map F • G')
        AND filter P    = map fst • filter snd • zip • (id ⊗ map P)
        END

        THEOREMS FunctionThms
        ARE swap • swap   = id
        AND swap • (F×G) • swap     = G×F
        AND (F×G) • (H⊗J)   = (F • H)⊗(G • J)
        AND id×id           = id
        AND (F×G) • (H×J)   = (F • H)× (G • J)
        AND F  • id       = F
        AND if P (F, G) • H = if (P•H) (F•H, G•H)
        AND H • if P (F,G) = if P (H•F, H•G)
        AND if P • (F⊗G) • H = if (P • H) • ((F•H) ⊗ (G•H))
        AND fst • (F⊗G) = F
        AND snd • (F⊗G) = G
        /* This is last because it unfolds trivially */
        AND F = id • F
        END

        THEOREMS Associativity
        ARE (F • G) • H = F • (G • H)
        END


        THEOREMS Composition(J,H,F,G)
        ARE
         J • H = id,     H • F = H • G ⊢ F=G
        AND
         J • H = id,     F • J = G • J ⊢ F=G
        AND
         J • J = id, J • F • J = J • G • J ⊢ F=G
        END

        THEOREMS ReflectThms
        ARE ref • ref = id
        AND del • ins X = id
        AND R F • R G = R (F • G)
        AND L F • L G = L (F • G)
        AND G • L F = L G ⊢ L G • F = G
        END
        
        

    BUTTON  "Unfold *"  IS apply RepeatedlyUnfold
    BUTTON  "Unfold"    IS apply UnfoldObvious COMMAND
    BUTTON  "Fold"      IS apply FoldObvious COMMAND
    BUTTON  "Apply"     IS apply COMMAND
    BUTTON  "Flatten"   IS apply Flatten
    BUTTON  "Find"      IS apply FindSelection
END

TACTIC  RepeatedlyUnfold IS 
    (SEQ 
      (ALT UnfoldUsingSearch 
           (Fail ("Search yields nothing to Unfold (check the Searching menu)")))
      (DO UnfoldUsingSearch)
    )
    
TACTIC  UnfoldUsingSearch IS 
ALT (SearchHypotheses UnfoldWithAnyHyp)
    (Unfold SearchTactic)
    FAIL
    /*(Fail ("Search yields nothing to Unfold (check the Searching menu)"))*/

TACTIC UnfoldWithAnyHyp IS UNFOLDHYP "Fold with hypothesis" (_A=_B)

/*
    This is here for completeness: searching for something which
    folds should be done rather gingerly in most theories -- right
    hand sides tend to be rather less structured than left hand sides,
    so too many things match.
*/
TACTIC  FoldUsingSearch IS 
ALT (SearchHypotheses FoldWithAnyHyp)
    (Fold SearchTactic)
    FAIL
    /*(Fail ("Search yields nothing to Fold (check the Searching menu)"))*/

TACTIC FoldWithAnyHyp   IS FOLDHYP   "Unfold with hypothesis" (_A=_B)

TACTIC FindSelection IS
/*
    Use associative rewrite laws to make the current
    text selection a proper subterm of the term it's
    in.
*/
(WHEN 
 (LETHYPFIND (_XOLD=_YOLD, _XNEW=_YNEW)
   (ALT 
     (LAYOUT "Associativity" (2)
       (rewriteHypotheticalEquation _XOLD _XNEW _YOLD _YNEW) EVALUATE EVALUATE)
     (LETARGSEL _XSEL (Fail ("%s isn't a subterm", _XSEL)))))
 (LETCONCFIND (_XOLD=_YOLD, _XNEW=_YNEW)
   (ALT 
    (LAYOUT "Associativity" (2)
      (rewriteEquation _XOLD _XNEW _YOLD _YNEW) EVALUATE EVALUATE)
    (LETARGSEL _XSEL (Fail ("%s isn't a subterm", _XSEL)))))
)


CONSTANT ASSOCEQ

RULE    rewriteEquation(X, X', Y, Y') IS
FROM    ASSOCEQ (X, X')
AND     ASSOCEQ (Y, Y') 
AND X'=Y'
INFER   X=Y

RULE    rewriteHypotheticalEquation(X, X', Y, Y') IS
FROM    ASSOCEQ (X, X')
AND     ASSOCEQ (Y, Y') 
AND X'=Y'⊢ P
INFER   X=Y ⊢ P


CONCHIT monoid X Y IS monoid
CONCHIT C      IS  (ALT UnfoldUsingSearch (Fail ("Search yields nothing to Unfold (check the Searching menu)")))
HYPHIT  H ⊢ C IS  UnfoldHypWithOptionalSelection 

/*
    Two semi-intelligent tactics for use from panels. If
    there's a selection, then it will be made a proper
    subterm (by associative rewriting) then (fold/un)folded
    using the given tactic. If there's no selection, then
    the first place at which the given tactic is applicable will be
    the source of the rewrite.

*/
TACTIC UnfoldObvious(tac) IS
WHEN (LETARGSEL _A (UnfoldTheSel _A tac))
     (ALT (Unfold tac) 
      (Fail("%s can't be used rightwards anywhere", tac)))
          

TACTIC FoldObvious(tac) IS
WHEN (LETARGSEL _A (FoldTheSel _A tac))
     (ALT (Fold tac) 
      (Fail("%s can't be used leftwards anywhere", tac)))

    
TACTIC UnfoldTheSel(sel, tac) IS
ALT (SEQ  (FindSelection sel) 
      (LAYOUT "Fold using %s" (1) 
          /* Try to rewrite the selected term if it's possible.
             If the tree has been *changed* by FindSelection,
             then it may not be possible, so just rewrite
             all instances of the selection. If this
             behaviour isn't satisfactory, then the user
             should do the Find.. by hand, then do the Unfold.
          */
          (ALT (WITHSUBSTSEL rewrite) (rewrite sel))  
          tac))
    (Fail ("Cannot Unfold  %s with %s", sel, tac))

TACTIC FoldTheSel(sel, tac) IS
ALT (SEQ (FindSelection sel) 
     (LAYOUT "Unfold using %s" (1) 
          /* See the Unfold ... tactic for an explanation */
          (ALT (WITHSUBSTSEL rewritebackwards) (rewritebackwards sel))
          tac))
    (Fail ("Cannot Fold %s with %s", sel, tac))

/*
    USed from HYPHIT -- 
*/
TACTIC UnfoldHypWithOptionalSelection IS
WHEN (LETSUBSTSEL _X{_x\_XX} 
       (ALT (WITHHYPSEL (WITHSUBSTSEL "Fold with hypothesis")) 
        (Fail (Couldn't Unfold selected term _XX))))
     "Unfold/Fold with hypothesis"
