/* $Id$ */

/*
	An all-introduction variant of intuitionistic predicate calculus
	Bernard Sufrin & Richard Bornat, Oxford 1991
	updated Richard Bornat, Hornsey April 1994 (how time flies!)
	updated again July 1996, with proper sequent syntax
*/

USE "sequent_syntax.j"
CONSTANT ⊥

SEQUENT IS BAG ⊢ FORMULA

RULE	hyp(A)				INFER Γ,A ⊢ A
RULE	"⊢∧"	FROM Γ ⊢ A AND Γ ⊢ B	INFER Γ ⊢ A∧B
RULE	"∧⊢"	FROM Γ, A, B ⊢ C		INFER Γ, A∧B ⊢ C
RULE	"⊢∨(L)"	    FROM  Γ ⊢ A		INFER Γ ⊢ A∨B
RULE	"⊢∨(R)"	    FROM  Γ ⊢ B		INFER Γ ⊢ A∨B
RULE	"∨⊢"	FROM Γ, A ⊢ C AND Γ, B ⊢ C	INFER Γ, A∨B ⊢ C
RULE	"⊢¬"	FROM Γ ⊢ A→ ⊥		INFER Γ ⊢ ¬A
RULE	"¬⊢"	FROM Γ, A→ ⊥ ⊢ B		INFER Γ, ¬A ⊢ B
RULE	"⊢→"	FROM Γ, A ⊢ B		INFER Γ ⊢ A→B
RULE	"→⊢"	FROM Γ ⊢ A AND Γ, B ⊢ C INFER Γ, A→B ⊢ C
RULE	"⊢≡"	FROM Γ ⊢ A→B AND Γ ⊢ B→A	INFER Γ ⊢ A≡B
RULE	"≡⊢"	FROM Γ, A→B,  B→A ⊢ C	INFER Γ, A≡B ⊢ C
RULE	"⊥⊢"				INFER Γ, ⊥ ⊢ A
RULE	"⊢∀"(OBJECT m) WHERE FRESH m
		FROM Γ ⊢ P(m)		INFER Γ ⊢ ∀x.P(x)
RULE	"∀⊢"(B)	    FROM Γ, P(B) ⊢ C		INFER Γ, ∀x.P(x) ⊢ C
RULE	"⊢∃"(B)	    FROM Γ ⊢ P(B)		INFER Γ ⊢ ∃x.P(x)
RULE	"∃⊢"(OBJECT m) WHERE FRESH m
		FROM  Γ, P(m) ⊢ C		INFER Γ, ∃x.P(x) ⊢ C
RULE	cut(A)	FROM Γ ⊢ A AND Γ, A ⊢ C INFER Γ ⊢ C
RULE	thin(A)	    FROM Γ ⊢ B		INFER Γ, A ⊢ B
RULE	dup(A)	FROM Γ, A, A ⊢ B		INFER Γ, A ⊢ B

UMENU Tracing IS
     CHECKBOX tactictracing "Trace" 
END

MENU Rules IS
	ENTRY hyp
	ENTRY cut
	SEPARATOR
	ENTRY "∧⊢"
	ENTRY "∨⊢"
	ENTRY "→⊢"
	ENTRY "¬⊢"
	ENTRY "≡⊢"
	ENTRY "⊥⊢"
	ENTRY "∀⊢"
	ENTRY "∃⊢"
	SEPARATOR
	ENTRY "⊢∧"
	ENTRY "⊢∨(L)"
	ENTRY "⊢∨(R)"
	ENTRY "⊢→"
	ENTRY "⊢¬"
	ENTRY "⊢≡"
	ENTRY "⊢∀"
	ENTRY "⊢∃"
END

TACTIC	"⊢∨"	IS 
	(ALT	(PROVE	"⊢∨(L)"	 ( hyp))
		(PROVE	"⊢∨(R)"	 ( hyp))
		(SEQ (EXPLAIN ("⊢∨" does not lead to an immediate conclusion)) (FAIL)))
MENU Auto
	TACTIC "Prove this propositional goal"		IS (PROVE Propositional)
	TACTIC "Prove remaining propositional goals"	IS Propositional
	TACTIC Propositional IS
		DO	(ALT	(PROVE hyp)
				"⊢∨"
				"⊢→" "⊢¬" "⊢∧" "∧⊢" "¬⊢" 
				"∨⊢"
				"→⊢"
				(SEQ "⊢∨(L)" (PROVE Propositional) "⊢∨(R)" (PROVE Propositional))
				"⊢∨(L)"
				"⊢∨(R)"
				"⊥⊢"
			)
END
	
HYPHIT	A ⊢ A	IS hyp	     
HYPHIT	A∧B ⊢ C IS "∧⊢"	   
HYPHIT	A∨B ⊢ C IS "∨⊢"	   
HYPHIT	A→B ⊢ C IS "→⊢"	   
HYPHIT	¬A ⊢ B	IS "¬⊢"	    
HYPHIT	A≡B ⊢ C IS "≡⊢"	  
HYPHIT	⊥ ⊢ A	IS	"⊥⊢" 
HYPHIT	∀x.A ⊢ B	IS "∀⊢"
HYPHIT	∃x.A ⊢ B	IS "∃⊢"

CONCHIT B∧C	IS "⊢∧"
CONCHIT B∨C	IS "⊢∨"	     
CONCHIT B→C	IS "⊢→"	     
CONCHIT ¬B	IS "⊢¬"	      
CONCHIT B≡C	IS "⊢≡"	    
CONCHIT ∀x.B	IS "⊢∀"	 
CONCHIT ∃x.B	IS "⊢∃"	 

/*AUTOMATCH hyp*/

STRUCTURERULE IDENTITY	  hyp
STRUCTURERULE CUT	     cut
STRUCTURERULE WEAKEN	 thin
