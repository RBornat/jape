/* $Id$ */

/* LF-style variables in the sequent calculus (multiple and single conclusion) */

PREFIX	10		var
POSTFIX	10		inscope

RULES "inscope" ARE
	Γ, var x ⊢ x inscope
AND	FROM Γ ⊢ A inscope AND Γ ⊢ B inscope INFER Γ ⊢ A→B inscope
AND	FROM Γ ⊢ A inscope AND Γ ⊢ B inscope INFER Γ ⊢ A∧B inscope
AND	FROM Γ ⊢ A inscope AND Γ ⊢ B inscope INFER Γ ⊢ A∨B inscope
AND	FROM Γ ⊢ A inscope INFER Γ ⊢ ¬A inscope
AND	FROM Γ, var x ⊢ A inscope INFER Γ ⊢ ∀x.A inscope
AND	FROM Γ, var x ⊢ A inscope INFER Γ ⊢ ∃x.A inscope 
END

AUTOMATCH "inscope"

