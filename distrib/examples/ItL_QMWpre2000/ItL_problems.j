/* $Id$ */

TACTIC TheoremForwardOrBackward(thm) IS
  WHEN	(LETHYP _P cut (ALT (WITHSELECTIONS thm) (RESOLVE (WITHSELECTIONS thm))))
	(ALT (WITHSELECTIONS thm) (RESOLVE (WITHSELECTIONS thm)) 
	    (SEQ cut (ALT (WITHSELECTIONS thm) (RESOLVE (WITHSELECTIONS thm))))
	)
  
/* These theorems are all stated without an explicit left context Γ. That is possible because, in ItL_rules.j,
  * we declared a WEAKEN structure rule: Jape will automatically discard any unmatched left-context
  * formulae.
  */
  
CONJECTUREPANEL Conjectures
    THEOREM INFER   P, P → Q ⊢ Q
    THEOREM INFER   P → Q, Q → R , P ⊢ R    
    THEOREM INFER   P → (Q → R), P → Q, P ⊢ R
    THEOREM INFER   P → Q, Q → R ⊢ P → R
    THEOREM INFER   P → (Q → R) ⊢ Q → (P → R)
    THEOREM INFER   P → (Q → R) ⊢ (P → Q) → (P → R)
    THEOREM INFER   P ⊢ Q → P
    THEOREM INFER   P → (Q → P)
    THEOREM INFER   P → Q ⊢ (Q → R) → (P → R)
    THEOREM INFER   P → (Q → (R → S)) ⊢ R → (Q → (P → S ))
    THEOREM INFER   (P → (Q → R)) → ((P → Q) → (P → R))
  
    THEOREM INFER   P, Q ⊢ P ∧ Q
    THEOREM INFER   P ∧ Q ⊢ P
    THEOREM INFER   P ∧ Q ⊢ Q
    THEOREM INFER   (P ∧ Q) → R ⊢ P → (Q → R)
    THEOREM INFER   P → (Q → R) ⊢ (P ∧ Q) → R
  
    THEOREM INFER   P ⊢ P ∨ Q	
    THEOREM INFER   Q ⊢ P ∨ Q	
  
    THEOREM INFER   P ∨ Q ⊢ Q ∨ P
    THEOREM INFER   Q → R ⊢ (P ∨ Q) → (P ∨ R)
    THEOREM INFER   P ∨ P ⊢ P
    THEOREM INFER   P ⊢ P ∨ P
    THEOREM INFER   P ∨ (Q ∨ R) ⊢ (P ∨ Q) ∨ R
    THEOREM INFER   (P ∨ Q) ∨ R ⊢ P ∨ (Q ∨ R)
    THEOREM INFER   P ∧ (Q ∨ R) ⊢ (P ∧ Q) ∨ (P ∧ R)
    THEOREM INFER   (P ∧ Q) ∨ (P ∧ R) ⊢ P ∧ (Q ∨ R)
    THEOREM INFER   P ∨ (Q ∧ R) ⊢ (P ∨ Q) ∧ (P ∨ R)
    THEOREM INFER   (P ∨ Q) ∧ (P ∨ R) ⊢ P ∨ (Q ∧ R)
    THEOREM INFER   P → R, Q → R ⊢ (P ∨ Q) → R 
  
    THEOREM INFER   ¬¬P → P
    THEOREM INFER   P ⊢ ¬ ¬P
    THEOREM INFER   P → Q ⊢ ¬Q → ¬P
    THEOREM INFER   ¬Q → ¬P ⊢ P → Q
    THEOREM INFER   P ∨ ¬P
    THEOREM INFER   P ∨ Q ⊢ ¬(¬P ∧ ¬Q)
    THEOREM INFER   ¬(¬P ∧ ¬Q) ⊢ P ∨ Q
    THEOREM INFER   P ∧ Q ⊢ ¬(¬P ∨ ¬Q)
    THEOREM INFER   ¬(¬P ∨ ¬Q) ⊢ P ∧ Q
    THEOREM INFER   ¬(P ∨ Q) ⊢ ¬P ∧ ¬Q
    THEOREM INFER   ¬P ∧ ¬Q ⊢ ¬(P ∨ Q)
    THEOREM INFER   ¬(P ∧ Q) ⊢ ¬P ∨ ¬Q
    THEOREM INFER   ¬P ∨ ¬Q ⊢ ¬(P ∧ Q)
    THEOREM INFER   ⊢ ¬(P ∧ ¬P)
    THEOREM INFER   Q → P, P → R ⊢ Q → R
    THEOREM INFER   (P → Q) ∨ (Q → P)
    THEOREM INFER   P ∧ ¬P ⊢ Q
    THEOREM INFER   ((P → Q) → P) → P
  
    THEOREM INFER   var c, P(c), ∀x.(P(x) → Q(x)) ⊢ Q(c)
    THEOREM INFER   ∀x.(P(x) → Q(x)) ⊢ ∀x.P(x) → ∀x.Q(x)
    THEOREM INFER   ∀x.(P(x) → Q(x)), ∀x.(Q(x) → R(x)) ⊢ ∀x.(P(x) → R(x))
    THEOREM INFER   ∀x.P(x) ∧ ∀x.Q(x) ⊢ ∀x.(P(x) ∧ Q(x))
    THEOREM INFER   ∀x.(P(x) ∧ Q(x)) ⊢ ∀x.P(x) ∧ ∀x.Q(x)
    THEOREM INFER   ∀x.(P(x) → Q(x)), ∃x.P(x) ⊢ ∃x.Q(x)
    THEOREM INFER   ∃x.(P(x) ∧ Q(x)) ⊢ ∃x.P(x) ∧ ∃x.Q(x)
    THEOREM INFER   ∃x.P(x) ∨ ∃x.Q(x) ⊢ ∃x.(P(x) ∨ Q(x))
    THEOREM INFER   ∃x.(P(x) ∨ Q(x)) ⊢ ∃x.P(x) ∨ ∃x.Q(x)
    THEOREM INFER   var c, ∀x.P(x) ⊢ ∃x.P(x)
    THEOREM INFER   ∀x.P(x) ⊢ ¬(∃x. ¬P(x))
    THEOREM INFER   ¬(∃x. ¬P(x)) ⊢ ∀x.P(x)
    THEOREM INFER   ∃x.P(x) ⊢ ¬(∀x. ¬P(x))
    THEOREM INFER   ¬(∀x. ¬P(x)) ⊢ ∃x.P(x)
    THEOREM INFER   ¬(∀x.P(x)) ⊢ ∃x. ¬P(x)
    THEOREM INFER   ∃x. ¬P(x) ⊢ ¬(∀x.P(x))
    THEOREM INFER   ¬(∃x.P(x)) ⊢ ∀x. ¬P(x)
    THEOREM INFER   ∀x. ¬P(x) ⊢ ¬(∃x.P(x))
  
    THEOREM "∀x.P(x) ⊢ ∃x.P(x) NOT" IS ∀x.P(x) ⊢ ∃x.P(x)
    THEOREM "P(c), ∀x.(P(x) → Q(x)) ⊢ Q(c) NOT" IS P(c), ∀x.(P(x) → Q(x)) ⊢ Q(c)
    THEOREM "var c, Q(c) ⊢ ∀x.(P(x) → Q(x)) NOT" IS var c, Q(c) ⊢ ∀x.(P(x) → Q(x))
    THEOREM "(∀x.P(x)) → (∀x.Q(x)) ⊢ ∀x.(P(x) → Q(x)) NOT" IS ∀x.P(x) → ∀x.Q(x) ⊢ ∀x.(P(x) → Q(x))
    THEOREM "(∃x.P(x)) ∧ (∃x.Q(x)) ⊢ ∃x.(P(x) ∧ Q(x)) NOT" IS ∃x.P(x) ∧ ∃x.Q(x) ⊢ ∃x.(P(x) ∧ Q(x))
  
    BUTTON Apply IS apply TheoremForwardOrBackward COMMAND
END
