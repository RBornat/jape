/* $Id$ */

/* single-conclusion sequent calculus with LF-like treatment of variables */

USE "sequent_scoping.j"

RULE	"⊢∀"(OBJECT m) WHERE FRESH m
			FROM Γ , var m ⊢ P(m) 				INFER Γ ⊢ ∀x.P(x)
RULE	"∀⊢"(B)	FROM Γ, P(B) ⊢ C AND Γ ⊢ B inscope			INFER Γ, ∀x.P(x) ⊢ C
RULE	"⊢∃"(B)	FROM Γ ⊢ P(B) AND Γ ⊢ B inscope			INFER Γ ⊢ ∃x.P(x)
RULE	"∃⊢"(OBJECT m) WHERE FRESH m
			FROM  Γ, var m, P(m) ⊢ C 				INFER Γ, ∃x.P(x) ⊢ C

TACTIC "∀⊢ with side condition hidden" IS LAYOUT "∀⊢" (0) (WITHSELECTIONS "∀⊢")
TACTIC "⊢∃ with side condition hidden" IS LAYOUT "⊢∃" (0) (WITHSELECTIONS "⊢∃")

MENU Rules IS
	ENTRY "∀⊢" IS "∀⊢ with side condition hidden"
	ENTRY "⊢∃" IS "⊢∃ with side condition hidden"
END

HYPHIT	∀x.A ⊢ B	IS "∀⊢ with side condition hidden"
CONCHIT	∃x.B		IS "⊢∃ with side condition hidden"
