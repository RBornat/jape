/* $Id$ */

/* rules to use an explicit contradiction symbol */
 
CONSTANT ⊥

RULE "¬-E"				IS FROM ¬¬A INFER A
RULE "¬-I"				IS FROM A ⊢ ⊥ INFER ¬A
RULE "⊥-I"(B)		IS FROM B ∧ ¬B INFER ⊥

MENU Rules IS 
	ENTRY "⊥-I"
END
