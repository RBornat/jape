/* $Id$ */

/*
    The _intuitionistic_ multiple-conclusion sequent calculus!! (add after MCS.jt)
*/

/* the differences */
RULE	"⊢¬"	    FROM Γ,A ⊢			INFER Γ ⊢ ¬A,∆
RULE	"¬⊢"	    FROM Γ ⊢ A			INFER Γ,¬A ⊢ ∆
RULE	"⊢→"	    FROM Γ,A ⊢ B		    INFER Γ ⊢ A→B,∆
RULE	"→⊢"	    FROM Γ ⊢ A AND Γ,B ⊢ ∆	    INFER Γ,A→B ⊢ ∆
