INFIX 8000R * ∎
CONSTANT f

CONJECTUREPANEL TwoVars 

THEOREM IS fresh i, ∀x.(P(x,x)→P(x,j)), ¬P(i,j) ⊢ ¬∀x.P(x,x) /* Doesn't reload */

THEOREM IS fresh i, ∀x.(P(f(x,x))→P(f(x,j))), ¬P(f(i,j)) ⊢ ¬∀x.P(f(x,x))

THEOREM IS fresh i, ∀x.(P(x*x)→P(x*j)), ¬P(i*j) ⊢ ¬∀x.P(x*x) /* Does reload */

THEOREM IS fresh i, ∀x.((x∎x)→(x∎j)), ¬(i∎j) ⊢ ¬∀x.(x∎x) /* Does reload */

END


