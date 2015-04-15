CONJECTUREPANEL Conjectures
    THEOREM IS  E, F ⊢ E∧F
    THEOREM IS  E∧F ⊢ E
    THEOREM IS  E∧F ⊢ F
    THEOREM IS  E∧(F∧G) ⊢ (E∧F)∧G
    THEOREM IS  (E∧F)∧G ⊢ E∧(F∧G)

    THEOREM IS  E, E→F ⊢ F
    THEOREM IS  E→F, F→G, E ⊢ G
    THEOREM IS  E→(F→G), E→F, E ⊢ G
    THEOREM IS  E→F, F→G ⊢ E→G
    THEOREM IS  E→F→G ⊢ F→E→G
    THEOREM IS  E→F→G ⊢ (E→F)→(E→G)
    THEOREM IS  E ⊢ F→E
    THEOREM IS  ⊢ E→F→E
    THEOREM IS  E→F ⊢ (F→G)→E→G
    THEOREM IS  E→F→G→H ⊢ G→F→E→H
    THEOREM IS  ⊢ (E→F→G)→(E→F)→E→G
    THEOREM IS  (E→F)→G ⊢ E→F→G

    THEOREM IS  E∧F ⊢ E→F
    THEOREM IS  (E→F)∧(E→G) ⊢ E→(F∧G)
    THEOREM IS  E→(F∧G) ⊢ (E→F)∧(E→G)
    THEOREM IS  E→F→G ⊢ (E∧F)→G
    THEOREM IS  (E∧F)→G ⊢ E→F→G
    THEOREM IS  (E→F)→G ⊢ (E∧F)→G
    THEOREM IS  E∧(F→G) ⊢ (E→F)→G

    THEOREM IS  E ⊢ E∨F
    THEOREM IS  F ⊢ E∨F
    THEOREM IS  E∨F ⊢ F∨E

    THEOREM IS  F→G ⊢ (E∨F)→(E∨G)
    THEOREM IS  E∨E ⊢ E
    THEOREM IS  E ⊢ E∨E
    THEOREM IS  ⊢ ((E→F)→E)→E

    THEOREM IS  E∧(F∨G) ⊢ (E∧F)∨(E∧G)
    THEOREM IS  E∧F∨E∧G ⊢ E∧(F∨G)
    THEOREM IS  E∨F∧G ⊢ (E∨F)∧(E∨G)
    THEOREM IS  (E∨F)∧(E∨G) ⊢ E∨F∧G

    THEOREM IS  (E→G)∧(F→G) ⊢ E∨F→G
    THEOREM IS  E∨F→G ⊢ (E→G)∧(F→G)

    THEOREM IS  E ⊢ E∧⊤ 
    THEOREM IS  E∧⊤ ⊢E 
    THEOREM IS  E ⊢ E∨⊤ 

    THEOREM IS  E ⊢ E∨⊥ 
    THEOREM IS  E∨⊥ ⊢E 
    THEOREM IS  E∧⊥ ⊢ E 

    THEOREM IS  E ⊢ ¬¬E
    THEOREM IS  ¬E ⊢ E→F
    THEOREM IS  E→F ⊢ ¬F→¬E

    THEOREM IS  E∨F, ¬F ⊢ E
    THEOREM IS  E∨F, ¬E ⊢ F

    THEOREM IS  E∨F ⊢ ¬(¬E∧¬F)
    THEOREM IS  E∧F ⊢ ¬(¬E∨¬F)
    
    THEOREM IS  ¬(E∨F) ⊢ ¬E∧¬F
    THEOREM IS  ¬E∧¬F ⊢ ¬(E∨F)
    THEOREM IS  ¬E∨¬F ⊢ ¬(E∧F)
    THEOREM IS   ⊢ ¬(E∧¬E)

    THEOREM IS  E∧¬E ⊢ F

    THEOREM IS  R(j), ∀x.(R(x)→S(x)) ⊢ S(j)
    THEOREM IS  ∀x.(R(x)→S(x)) ⊢ ∀y.R(y)→∀z.S(z)
    THEOREM IS  ∀x.(R(x)→S(x)), ∀y.(S(y)→T(y)) ⊢ ∀z.(R(z)→T(z))
    THEOREM IS  ∀x.R(x)∧∀y.S(y) ⊢ ∀z.(R(z)∧S(z))
    THEOREM IS  ∀x.(R(x)∧S(x)) ⊢ ∀y.R(y)∧∀z.S(z)
    THEOREM IS  ∀x.(R(x)→S(x)), ∃y.R(y) ⊢ ∃z.S(z)
    THEOREM IS  ∃x.(R(x)∧S(x)) ⊢ ∃y.R(y)∧∃z.S(z)
    THEOREM IS  ∃x.R(x)∨∃y.S(y) ⊢ ∃z.(R(z)∨S(z))
    THEOREM IS  ∃x.(R(x)∨S(x)) ⊢ ∃y.R(y)∨∃z.S(z)

    THEOREM IS ∀x.(x=0∨x=1), ∃x.R(x), ¬R(1) ⊢ R(0)

    BUTTON Apply    IS apply ResolveOrTheorem COMMAND
    
END



