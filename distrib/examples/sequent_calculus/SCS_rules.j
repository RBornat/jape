/* $Id$ */

/*
	An all-introduction variant of intuitionistic predicate calculus
	Bernard Sufrin & Richard Bornat, Oxford 1991
	updated Richard Bornat, Hornsey April 1994 (how time flies!)
	updated again July 1996, with proper sequent syntax
*/

FONTS "Konstanz"
INITIALISE displaystyle tree

INFIX   1000    1000    é
INFIX   1101    1100    ç
INFIX   1500    1500    ¦
INFIX   1600    1600    ë
PREFIX Â

LEFTFIX è .
LEFTFIX ä .

CLASS BAG ‚
CLASS FORMULA A, B, C, D, P, Q, R, S
CLASS VARIABLE x, y, z, m, n, u, v
CONSTANT Ù

BIND x SCOPE P IN äx . P
BIND x SCOPE P IN èx . P


SEQUENT IS BAG æ FORMULA

INITIALISE interpretpredicates true

RULE	hyp(A)							INFER ‚,A æ A
RULE	"æ¦"		FROM ‚ æ A AND  ‚ æ B		INFER ‚ æ A¦B
RULE	"¦æ"		FROM ‚, A, B æ C			INFER ‚, A¦B æ C
RULE	"æë(L)"	FROM  ‚ æ A 				INFER ‚ æ AëB
RULE	"æë(R)"	FROM  ‚ æ B 				INFER ‚ æ AëB
RULE	"ëæ"		FROM ‚, A æ C AND ‚, B æ C	INFER ‚, AëB æ C
RULE	"æÂ"		FROM ‚ æ Aç Ù 			INFER ‚ æ ÂA
RULE	"Âæ"		FROM ‚, Aç Ù æ B 			INFER ‚, ÂA æ B
RULE	"æç"		FROM ‚, A æ B 				INFER ‚ æ AçB
RULE	"çæ"		FROM ‚ æ A AND ‚, B æ C		INFER ‚, AçB æ C
RULE	"æé"		FROM ‚ æ AçB AND ‚ æ BçA	INFER ‚ æ AéB
RULE	"éæ"		FROM ‚, AçB,  BçA æ C		INFER ‚, AéB æ C
RULE	"Ùæ"							INFER ‚, Ù æ A
RULE	"æè"(OBJECT m) WHERE FRESH m
			FROM ‚ æ P(m) 				INFER ‚ æ èx.P(x)
RULE	"èæ"(B)	FROM ‚, P(B) æ C 			INFER ‚, èx.P(x) æ C
RULE	"æä"(B)	FROM ‚ æ P(B)				INFER ‚ æ äx.P(x)
RULE	"äæ"(OBJECT m) WHERE FRESH m
			FROM  ‚, P(m) æ C 			INFER ‚, äx.P(x) æ C
RULE	cut(A)	FROM ‚ æ A AND ‚, A æ C 		INFER ‚ æ C
RULE	thin(A)	FROM ‚ æ B 				INFER ‚, A æ B
RULE	dup(A)	FROM ‚, A, A æ B 			INFER ‚, A æ B

/* It would be nice to be able to prove this derived rule ...
DERIVED	"æÂ'"(A)		FROM A æ Ù INFER ÂA
*/
        
/* and this one, but it's really just contradiction ...
DERIVED	"Âæ'"(A,B)	FROM A INFER  ÂA æ B
*/
                                
MENU Rules IS
	ENTRY hyp
	ENTRY cut
	SEPARATOR
	ENTRY "¦æ"
	ENTRY "ëæ"
	ENTRY "çæ"
	ENTRY "Âæ"
	ENTRY "éæ"
	ENTRY "Ùæ"
	ENTRY "èæ"
	ENTRY "äæ"
	SEPARATOR
	ENTRY "æ¦"
	ENTRY "æë(L)"
	ENTRY "æë(R)"
	ENTRY "æç"
	ENTRY "æÂ"
	ENTRY "æé"
	ENTRY "æè"
	ENTRY "æä"
END

TACTIC  "æë"	IS ALT	(SEQ "æë(L)" hyp)
					(SEQ "æë(R)" hyp)
					(JAPE (fail ("æë" does not lead to an immediate conclusion)))

MENU Auto
	TACTIC "Prove this propositional goal"		IS (PROVE Propositional)
	TACTIC "Prove remaining propositional goals"	IS Propositional
	TACTIC Propositional IS
		DO	(ALT	(PROVE hyp)
		      		"æë"
		      		"æç" "æÂ" "æ¦" "¦æ" "Âæ" 
		      		"ëæ"
		      		"çæ"
		      		(SEQ "æë(L)" (PROVE Propositional) "æë(R)" (PROVE Propositional))
		      		"æë(L)"
		      		"æë(R)"
		      		"Ùæ"
			)
END
	
HYPHIT	A æ A	IS hyp       
HYPHIT	A¦B æ C	IS "¦æ"    
HYPHIT	AëB æ C	IS "ëæ"    
HYPHIT	AçB æ C	IS "çæ"    
HYPHIT	ÂA æ B	IS "Âæ"     
HYPHIT	AéB æ C	IS "éæ"   
HYPHIT	Ù æ A	IS "Ùæ" 
HYPHIT	èx.A æ B	IS "èæ"
HYPHIT	äx.A æ B	IS "äæ"

CONCHIT	B¦C	IS "æ¦"
CONCHIT	BëC	IS "æë"      
CONCHIT	BçC	IS "æç"      
CONCHIT	ÂB	IS "æÂ"       
CONCHIT	BéC	IS "æé"     
CONCHIT	èx.B	IS "æè"  
CONCHIT	äx.B	IS "æä"  

AUTOMATCH hyp

STRUCTURERULE IDENTITY    hyp
STRUCTURERULE CUT            cut
STRUCTURERULE WEAKEN     thin
