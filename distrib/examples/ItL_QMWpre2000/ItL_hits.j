/* $Id$ */

HYPHIT	P     ⊢ P	IS hyp       
HYPHIT	P∧Q ⊢ R	IS ALT	(SEQ "∧-E(L)" (WITHHYPSEL hyp))
                                           	(SEQ "∧-E(R)" (WITHHYPSEL hyp))
                                           	(SEQ (ForwardCut 0 "∧-E(L)") (ForwardCut 0 "∧-E(R)"))
HYPHIT	P→Q  ⊢ R	IS ForwardCut 1 "→-E"
HYPHIT	P∨Q  ⊢ R	IS ForwardUncut 0 "∨-E"
HYPHIT	¬¬P   ⊢ Q	IS ForwardCut 0 "¬-E"
HYPHIT	∀x.P ⊢ Q	IS ForwardCut 0 "∀-E with side condition hidden"	
HYPHIT	∃x.P ⊢ Q	IS ForwardUncut 0 "∃-E"

CONCHIT	Q∧R	IS "∧-I"
CONCHIT	Q∨R	IS ALT (SEQ "∨-I(L)" hyp) (SEQ "∨-I(R)" hyp)
CONCHIT	Q→R	IS "→-I"      
CONCHIT	¬Q	IS "¬-I"       
CONCHIT	∀x.Q	IS "∀-I"  
CONCHIT	∃x.Q	IS "∃-I with side condition hidden"
