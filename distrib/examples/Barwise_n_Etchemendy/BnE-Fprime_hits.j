/* $Id$ */

HYPHIT A→B ⊢ C IS "→-E tac"
HYPHIT A∧B ⊢ C IS 
    ALT (SEQ "∧-E(L)" (WITHHYPSEL hyp))
	    (SEQ "∧-E(R)" (WITHHYPSEL hyp))
	    (SEQ (ForwardCut 0 "∧-E(L)") (ForwardCut 0 "∧-E(R)"))
HYPHIT A∨B  ⊢ C IS "∨-E tac"
HYPHIT ∀x.P ⊢ C IS "∀-E tac"
HYPHIT ∀(x,y).P ⊢ C IS "∀-E tac"
HYPHIT ∀(x,y,z).P ⊢ C IS "∀-E tac"
HYPHIT ∀(w,x,y,z).P ⊢ C IS "∀-E tac"
HYPHIT ∃x.P ⊢ C IS "∃-E tac"
HYPHIT ∃(x,y).P ⊢ C IS "∃-E tac"
HYPHIT ∃(x,y,z).P ⊢ C IS "∃-E tac"
HYPHIT ∃(w,x,y,z).P ⊢ C IS "∃-E tac"

CONCHIT A→B IS "→-I"	  
CONCHIT A↔B IS "↔-I"
CONCHIT A∧B IS "∧-I"
CONCHIT A∨B IS ALT (SEQ "∨-I(L)" hyp) (SEQ "∨-I(R)" hyp)
CONCHIT ¬A IS "¬-I"  
CONCHIT ⊥ IS "⊥-I"     
CONCHIT ∀x.P IS "∀-I"  
CONCHIT ∀(x,y).P IS "∀-I"  
CONCHIT ∀(x,y,z).P IS "∀-I"  
CONCHIT ∀(w,x,y,z).P IS "∀-I"  
CONCHIT ∃x.P IS "∃-I tac"  
CONCHIT ∃(x,y).P IS "∃-I tac"  
CONCHIT ∃(x,y,z).P IS "∃-I tac"	 
CONCHIT ∃(w,x,y,z).P IS "∃-I tac"  
CONCHIT A=A IS "A=A"
