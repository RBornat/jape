/* $Id$ */

MENU	Rules IS
	SEPARATOR
		
	ENTRY	"List Induction"		IS "list induction tactic"
	ENTRY	"Boolean cases"	IS BoolCases
	ENTRY	"Monoid Definition" IS monoid
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
		AND             "Ignore hypotheses" IS JUSTFAIL
		INITIALLY       JUSTAPPLY
		END

		RADIOBUTTON list IS 
			        "List rules enabled"  IS List
		AND	        "List rules disabled" IS JUSTFAIL
		INITIALLY       List
		END
		
		RADIOBUTTON listthms IS 
			        "List theorems enabled"  IS ListThms
		AND	        "List theorems disabled" IS JUSTFAIL
		INITIALLY       JUSTFAIL
		END

		RADIOBUTTON function IS 
			        "Function rules enabled"  IS Function
		AND	        "Function rules disabled" IS JUSTFAIL
		INITIALLY       Function
		END

		RADIOBUTTON functionthms IS 
			        "Function theorems enabled"  IS FunctionThms
		AND	        "Function theorems disabled" IS JUSTFAIL
		INITIALLY       JUSTFAIL
		END
		
		RADIOBUTTON reflect IS 
			        "Reflect rules enabled"  IS Reflect
		AND	        "Reflect rules disabled" IS JUSTFAIL
		INITIALLY       JUSTFAIL
		END

		RADIOBUTTON reflectthms IS 
			        "Reflect theorems enabled"  IS ReflectThms
		AND	        "Reflect theorems disabled" IS JUSTFAIL
		INITIALLY       JUSTFAIL
		END
		
END

TACTIC  SearchTactic IS 
        ALT list listthms function functionthms reflect reflectthms

TACTICPANEL "Definitions" 
        TACTIC "Use any rule enabled by Searching" IS SearchTactic
	ENTRY	":"
	ENTRY	"•"	  
	ENTRY	"Ù"	  
	ENTRY	"ª"	  
	ENTRY	"++"
	ENTRY	"cat"	
	ENTRY	"del"
	ENTRY	"filter"
	ENTRY	"fold"
	ENTRY	"fst" 
	ENTRY	"if"	  
	ENTRY	"id"   
	ENTRY	"ins"
	ENTRY	"length"
	ENTRY	"map"
	ENTRY	"move"
	ENTRY	"none" 
	ENTRY	"one"  
	ENTRY	"rcat"
	ENTRY	"ref"
	ENTRY	"rev"  
	ENTRY	"rev2"
	ENTRY	"snd"  
	ENTRY	"swap" 
	ENTRY	"zip"
	ENTRY	L
	ENTRY	R

	BUTTON          "Unfold *"      IS apply RepeatedlyUnfold
	PREFIXBUTTON	"Unfold"        IS apply UnfoldObvious
	PREFIXBUTTON	"Fold"		IS apply FoldObvious
	PREFIXBUTTON	"Apply"		IS apply
	BUTTON          "Flatten"       IS apply Flatten
	BUTTON          "Find"          IS apply FindSelection
END

MENU Edit
	RADIOBUTTON displaystyle IS
	        "Box display style"	IS box
	AND     "Tree display style"	IS tree
	INITIALLY box
	END
		
	RADIOBUTTON autoselect IS
		"Automatic goal selection" IS true
	AND	"Manual goal selection" IS false
	INITIALLY true
	END

	RADIOBUTTON applyconjectures IS
		"Apply both conjectures and theorems" IS true
	AND	"Apply only theorems " IS false
	INITIALLY false
	END
END

CONJECTUREPANEL "Conjectures" IS
                ENTRY "Use any rule enabled by Searching" IS SearchTactic

		THEOREMS	syntactic
		ARE monoid (++) ›ﬁ
		AND monoid rcat ›ﬁ
		END
		
		THEOREMS ListThms
		ARE     rev • rev	= id
		AND     rev2	        = rev
		AND	map F • map G	= map (F • G)
		AND	map F • cat	= cat • (map (map F))
		AND	none • F	= none
		AND	map F • none	= none
		AND	map F • one	= one • F
		AND	map F • rev	= rev • map F
		AND	map id		= id
		AND	length•map F	= length
		AND	zip • (map F ª map G) = map (FªG)
		AND	map F • if P (G,G')   = if P (map F • G, map F • G')
		AND	filter P	= map fst • filter snd • zip • (id ª map P)
		END

		THEOREMS FunctionThms
		ARE	swap • swap	  = id
		AND	swap • (FÙG) • swap		= GÙF
		AND	(FÙG) • (HªJ)	= (F • H)ª(G • J)
		AND	idÙid			= id
		AND	(FÙG) • (HÙJ)	= (F • H)Ù (G • J)
		AND	F  • id		  = F
		AND     if P (F, G) • H = if (P•H) (F•H, G•H)
		AND     H • if P (F,G) = if P (H•F, H•G)
		AND     if P • (FªG) • H = if (P • H) • ((F•H) ª (G•H))
		AND     fst • (FªG) = F
		AND     snd • (FªG) = G
		/* This is last because it unfolds trivially */
		AND     F = id • F
		END

		THEOREMS Associativity
		ARE	(F • G) • H = F • (G • H)
		END


		THEOREMS Composition(J,H,F,G)
		ARE
		 J • H = id,	 H • F = H • G Ê F=G
		AND
		 J • H = id,	 F • J = G • J Ê F=G
		AND
		 J • J = id, J • F • J = J • G • J Ê F=G
		END

		THEOREMS ReflectThms
		ARE	ref • ref = id
		AND	del • ins X = id
		AND	R F • R G = R (F • G)
		AND	L F • L G = L (F • G)
		AND	G • L F = L G Ê L G • F = G
		END
		
		

	        BUTTON          "Unfold *"      IS apply RepeatedlyUnfold
		PREFIXBUTTON	"Unfold"	IS apply UnfoldObvious
		PREFIXBUTTON	"Fold"	        IS apply FoldObvious
		PREFIXBUTTON	"Apply"         IS apply
		BUTTON          "Flatten"       IS apply Flatten
		BUTTON          "Find"          IS apply FindSelection
		
END

TACTIC  RepeatedlyUnfold IS SEQ UnfoldUsingSearch (DO UnfoldUsingSearch)

TACTIC  UnfoldUsingSearch IS 
ALT     (SearchHypotheses UnfoldWithAnyHyp)
	(Unfold SearchTactic)
	(FAIL ("Search yields nothing to Unfold (check the Searching menu)"))

TACTIC UnfoldWithAnyHyp IS UNFOLDHYP "Fold with hypothesis" (_A=_B)

/*
        This is here for completeness: searching for something which
        folds should be done rather gingerly in most theories -- right
        hand sides tend to be rather less structured than left hand sides,
        so too many things match.
*/
TACTIC  FoldUsingSearch IS 
ALT     (SearchHypotheses FoldWithAnyHyp)
	(Fold SearchTactic)
	(FAIL ("Search yields nothing to Fold (check the Searching menu)"))

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
     (LETARGSEL _XSEL (FAIL ("%s isn't a subterm", _XSEL)))))
 (LETCONCFIND (_XOLD=_YOLD, _XNEW=_YNEW)
   (ALT 
    (LAYOUT "Associativity" (2)
      (rewriteEquation _XOLD _XNEW _YOLD _YNEW) EVALUATE EVALUATE)
    (LETARGSEL _XSEL (FAIL ("%s isn't a subterm", _XSEL)))))
)


CONSTANT ASSOCEQ

RULE    rewriteEquation(X, X', Y, Y', OBJECT x) IS
FROM    ASSOCEQ (X, X')
AND     ASSOCEQ (Y, Y') 
AND     X'=Y'
INFER   X=Y

RULE    rewriteHypotheticalEquation(X, X', Y, Y', OBJECT x) IS
FROM    ASSOCEQ (X, X')
AND     ASSOCEQ (Y, Y') 
AND     X'=Y'Ê P
INFER   X=Y Ê P



CONCHIT C      IS  UnfoldUsingSearch
HYPHIT  H |- C IS  UnfoldHypWithOptionalSelection 

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
          (FAIL("%s can't be used rightwards anywhere", tac)))
                  

TACTIC FoldObvious(tac) IS
WHEN (LETARGSEL _A (FoldTheSel _A tac))
     (ALT (Fold tac) 
          (FAIL("%s can't be used leftwards anywhere", tac)))

    
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
    (FAIL ("Cannot Unfold  %s with %s", sel, tac))

TACTIC FoldTheSel(sel, tac) IS
ALT (SEQ (FindSelection sel) 
         (LAYOUT "Unfold using %s" (1) 
                  /* See the Unfold ... tactic for an explanation */
                  (ALT (WITHSUBSTSEL rewritebackwards) (rewritebackwards sel))
                  tac))
    (FAIL ("Cannot Fold %s with %s", sel, tac))

/*
        USed from HYPHIT -- 
*/
TACTIC UnfoldHypWithOptionalSelection IS
WHEN (LETSUBSTSEL _X[_x\_XX] 
       (ALT (WITHHYPSEL (WITHSUBSTSEL "Fold with hypothesis")) 
            (FAIL (Couldn't Unfold selected term _XX))))
     "Unfold/Fold with hypothesis"














