/* $Id$ */

MENU	Rules IS
	SEPARATOR
		
	ENTRY	"List induction"		IS "list induction tactic"
	ENTRY	"Boolean cases"	IS BoolCases
	ENTRY	monoid
END

TACTIC JUSTFAIL IS (ALT)

MENU "Searching" IS
		RADIOBUTTON foldunfoldhyp IS 
			"Search hypotheses" IS UnfoldHyp
		AND "Ignore hypotheses" IS JUSTFAIL
		INITIALLY UnfoldHyp
		END

		RADIOBUTTON list IS 
			"List rules enabled"  IS List
		AND	 "List rules disabled" IS JUSTFAIL
		INITIALLY  List
		END
		
		RADIOBUTTON listthms IS 
			 "List theorems enabled"  IS ListThms
		AND	 "List theorems disabled" IS JUSTFAIL
		INITIALLY JUSTFAIL
		END

		RADIOBUTTON function IS 
			 "Function rules enabled"  IS Function
		AND	 "Function rules disabled" IS JUSTFAIL
		INITIALLY  Function
		END

		RADIOBUTTON functionthms IS 
			 "Function theorems enabled"  IS FunctionThms
		AND	 "Function theorems disabled" IS JUSTFAIL
		INITIALLY JUSTFAIL
		END
		
		RADIOBUTTON reflect IS 
			 "Reflect rules enabled"  IS Reflect
		AND	 "Reflect rules disabled" IS JUSTFAIL
		INITIALLY JUSTFAIL
		END

		RADIOBUTTON reflectthms IS 
			 "Reflect theorems enabled"	 IS ReflectThms
		AND	 "Reflect theorems disabled" IS JUSTFAIL
		INITIALLY JUSTFAIL
		END		   
END

TACTICPANEL "Definitions" 
		PREFIXBUTTON	"Unfold"		IS apply UnfoldOneSel
		PREFIXBUTTON	"Fold"		IS apply FoldOneSel
		BUTTON		"Unfold *"	IS apply DO (Auto Unfold UnfoldWithAnyHyp)
		PREFIXBUTTON	"Apply"		IS apply
END

MENU Edit
		RADIOBUTTON displaystyle IS
			  "Box display style"	IS box
		AND	  "Tree display style"	IS tree
		INITIALLY box
		END
		
		RADIOBUTTON autoselect IS
				"Automatic goal selection" IS true
		AND		"Manual goal selection" IS false
		END

		RADIOBUTTON applyconjectures IS
				"Apply both conjectures and theorems" IS true
		AND		"Apply only theorems " IS false
		INITIALLY false
		END
END

TACTICPANEL "Definitions" 

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
END

CONJECTUREPANEL "Conjectures" IS

		THEOREMS	syntactic
		ARE monoid (++) ›ﬁ
		AND monoid rcat ›ﬁ
		END
		
		THEOREMS	ListThms
		ARE		rev • rev		= id
		AND		rev2	 = rev
		AND		map F • map G	= map (F • G)
		AND		map F • cat		= cat • (map (map F))
		AND		none • F		= none
		AND		map F • none	= none
		AND		map F • one		= one • F
		AND		map F • rev		= rev • map F
		AND		map id			= id
		AND		length•map F		= length
		AND		zip • (map F ª map G) = map (FªG)
		AND		map F • if P (G,G') = if P (map F • G, map F • G')
		AND		filter P		= map fst • filter snd • zip • (id ª map P)
		END

		THEOREMS	FunctionThms
		ARE		swap • swap	  = id
		AND		swap • (FÙG) • swap		= GÙF
		AND		(FÙG) • (HªJ)	= (F • H)ª(G • J)
		AND		idÙid			= id
		AND		(FÙG) • (HÙJ)	= (F • H)Ù (G • J)
		AND		F  • id		  = F
		AND		F		 = id • F
		END

		THEOREMS	Associativity
		ARE			(F • G) • H = F • (G • H)
		END


		THEOREMS  Composition(J,H,F,G)
		ARE
		 J • H = id,	 H • F = H • G Ê F=G
		AND
		 J • H = id,	 F • J = G • J Ê F=G
		AND
		 J • J = id, J • F • J = J • G • J Ê F=G
		END

		THEOREMS	ReflectThms
		ARE			ref • ref = id
		AND			del • ins X = id
		AND			R F • R G = R (F • G)
		AND			L F • L G = L (F • G)
		AND			G • L F = L G Ê L G • F = G
		END

		PREFIXBUTTON	"Apply"		IS apply
		PREFIXBUTTON	"Unfold"		IS apply UnfoldOneSel
		PREFIXBUTTON	"Fold"		IS apply FoldOneSel
END

CONCHIT C IS Auto Unfold UnfoldWithAnyHyp
HYPHIT  H |- C IS  UnfoldHypWithOptionalSelection 

TACTIC UnfoldHypWithOptionalSelection IS
	WHEN	
		(LETSUBSTSEL _X[_x\_XX] 
           		(ALT	(WITHHYPSEL (WITHSUBSTSEL "Unfold with hypothesis")) 
                		(FAIL ("Couldn't Unfold selected term " _XX))
                	)
		)
      		"rewrite with hypothesis"




