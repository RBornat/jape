/* $Id$ */

/* rules to use an explicit contradiction symbol */
 
CONSTANT Ù

RULE "Â-E"				IS FROM ÂÂA INFER A
RULE "Â-I"				IS FROM A æ Ù INFER ÂA
RULE "Ù-I"(B)		IS FROM B ¦ ÂB INFER Ù

MENU Rules IS 
	ENTRY "Ù-I"
END
