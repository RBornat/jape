/* $Id$ */

USE "sequent_scoping.j"

RULE	"⊢∀"(OBJECT m) WHERE FRESH m
			FROM Γ, var m ⊢ A(m),∆				INFER Γ ⊢ ∀x.A(x),∆
RULE	"∀⊢"(B)	FROM Γ, A(B) ⊢ ∆ AND Γ ⊢ B inscope		INFER Γ,∀x.A(x) ⊢ ∆
RULE	"⊢∃"(B)	FROM Γ ⊢ A(B),∆ AND Γ ⊢ B inscope		INFER Γ ⊢ ∃x.A(x),∆
RULE	"∃⊢"(OBJECT m) WHERE FRESH m
			FROM  Γ, var m, A(m) ⊢ ∆				INFER Γ, ∃x.A(x) ⊢ ∆

TACTIC "∀⊢ with side condition hidden" IS LAYOUT "∀⊢" (0) (WITHSELECTIONS "∀⊢")
TACTIC "⊢∃ with side condition hidden" IS LAYOUT "⊢∃" (0) (WITHSELECTIONS "⊢∃")

MENU Rules IS
	ENTRY "∀⊢" IS "∀⊢ with side condition hidden"
	ENTRY "⊢∃" IS "⊢∃ with side condition hidden"
END

HYPHIT	∀x.A ⊢	IS "∀⊢ with side condition hidden"
CONCHIT	⊢ ∃x.B	IS "⊢∃ with side condition hidden"
