/* $Id$ */

HYPHIT	P     ⊢ P   IS hyp	 
HYPHIT	P∧Q ⊢ R IS ALT	(SEQ "∧ elim(L)" (WITHHYPSEL hyp))
					    (SEQ "∧ elim(R)" (WITHHYPSEL hyp))
					    (SEQ (ForwardCut 0 "∧ elim(L)") (ForwardCut 0 "∧ elim(R)"))
HYPHIT	P→Q  ⊢ R    IS ForwardCut 0 "→ elim"
HYPHIT	P∨Q  ⊢ R    IS ForwardUncut 0 "∨ elim"
HYPHIT	¬¬P   ⊢ Q   IS ForwardCut 0 "¬ elim"
HYPHIT	∀x.P ⊢ Q    IS ForwardCut 0 "∀ elim with side condition hidden" 
HYPHIT	∃x.P ⊢ Q    IS ForwardUncut 0 "∃ elim"

CONCHIT Q∧R IS "∧ intro"
CONCHIT Q∨R IS ALT (SEQ "∨ intro(L)" hyp) (SEQ "∨ intro(R)" hyp)
CONCHIT Q→R IS "→ intro"      
CONCHIT ¬Q  IS "¬ intro"       
CONCHIT ∀x.Q	IS "∀ intro"  
CONCHIT ∃x.Q	IS "∃ intro with side condition hidden"
