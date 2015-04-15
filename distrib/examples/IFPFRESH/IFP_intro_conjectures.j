CONJECTUREPANEL "Introductory Conjectures"
    THEOREM IS  ¬¬E ⊢ E

    THEOREM IS E, ¬E ⊢ F
    THEOREM IS E, ¬E, G ⊢ F
    THEOREM IS E→F→G→H ⊢ G→F→E→H

    THEOREM IS  E∨(F∨G) ⊢ (E∨F)∨G
    THEOREM IS  (E∨F)∨G ⊢ E∨(F∨G)
    
    THEOREM IS  ¬(E∨F) ⊢ ¬E∧¬F
    THEOREM IS  ⊢ E∨¬E

    THEOREM IS  ¬F→¬E ⊢ E→F
    
    THEOREM IS  ¬E ∨ ¬F ⊢ ¬(E∧F)
    THEOREM IS  ¬E ∧ ¬F ⊢ ¬(E∨F)    
    THEOREM IS  E ∧ F ⊢ ¬(¬E∨¬F)    
    THEOREM IS  E ∨ F ⊢ ¬(¬E∧¬F)    
    THEOREM IS  ¬(¬E∧¬F) ⊢ E∨F
    THEOREM IS  ¬(¬E∨¬F) ⊢ E∧F
    THEOREM IS  ¬(E∨F) ⊢ ¬E∧¬F
    THEOREM IS  ¬(E∧F) ⊢ ¬E∨¬F
    THEOREM IS  (E→F)∨(F→E)

    THEOREM IS  ∀x.R(x) ⊢ ∃y.R(y)
    THEOREM IS  ∃ x . ∀y . R(x,y) ⊢ ∀ y . ∃x . R(x,y)
    
    THEOREM IS  ¬∀x.R(x) ⊢ ∃y.¬R(y)
    THEOREM IS  ¬∃x.R(x) ⊢ ∀y.¬R(y)
    THEOREM IS  ¬∀x.¬R(x) ⊢ ∃y.R(y)
    THEOREM IS  ¬∃x.¬R(x) ⊢ ∀y.R(y)
    THEOREM IS  ∀x.R(x) ⊢ ¬∃y.¬R(y)
    THEOREM IS  ∃x.R(x) ⊢ ¬∀y.¬R(y)
    THEOREM IS  ∀x.¬R(x) ⊢ ¬∃y.R(y)
    THEOREM IS  ∃x.¬R(x) ⊢ ¬∀y.R(y)

    THEOREM IS  R(k) ⊢ ∃x.(R(x)→R(j)∧R(k))
    

    BUTTON Apply    IS apply ResolveOrTheorem COMMAND
    
END




