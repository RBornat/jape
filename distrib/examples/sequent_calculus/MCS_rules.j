/* $Id$ */

/*
	The multi-conclusion sequent calculus
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

CLASS FORMULA A, B, C, D, P, Q, R, S
CLASS VARIABLE x, y, z
CLASS CONSTANT F, G, H, m, n

BIND    x SCOPE P IN äx . P
BIND    x SCOPE P IN èx . P

SEQUENT IS BAG æ BAG

RULE	axiom(A)		INFER A æ A
RULE	"æ¦"		FROM æ A AND æ B INFER æ A¦B
RULE	"¦æ"		FROM A, B æ  INFER   A¦B æ 
RULE	"æë"		FROM æ A,B INFER æ AëB
RULE	"ëæ"		FROM A æ  AND B æ  INFER AëB æ 
RULE	"æÂ"		FROM A æ INFER æ ÂA
RULE	"Âæ"		FROM æ A INFER ÂA æ 
RULE	"æç"		FROM A æ B INFER æ AçB
RULE	"çæ"		FROM AçB æ A AND B æ  INFER AçB æ
RULE	"æé"		FROM æ AçB AND æ BçA INFER æ AéB
RULE	"éæ"		FROM AçB, BçA æ  INFER AéB æ 
RULE	"æè"(OBJECT m) WHERE FRESH m
			FROM æ A[x\m] INFER æ èx . A
RULE	"èæ"(B)		FROM èx.A, A[x\B] æ INFER èx.A æ
RULE	"æä"(B)		FROM æ A[x\B] INFER æ äx.A
RULE	"äæ"(OBJECT m) WHERE FRESH m
			FROM  A[x\m] æ INFER äx.A æ
RULE	cut(A)		FROM æ A AND A æ INFER æ
RULE	"weakenæ"(A)	FROM æ INFER A æ 
RULE	"æweaken"(A)	FROM æ INFER æ A
RULE	"contractæ"(A)	FROM A, A æ INFER A æ 
RULE	"æcontract"(A)	FROM æ A, A INFER æ A 
                                
MENU Rules IS
	ENTRY axiom	IS  axiom
	SEPARATOR
	ENTRY "¦æ"	IS "¦æ"
	ENTRY "ëæ"	IS "ëæ"
	ENTRY "çæ"	IS "çæ"
	ENTRY "Âæ"	IS "Âæ"
	ENTRY "éæ"	IS "éæ"
	ENTRY "èæ"	IS "èæ"
	ENTRY "äæ"	IS "äæ"
	SEPARATOR
	ENTRY "æ¦"	IS "æ¦"
	ENTRY "æë"	IS "æë"
	ENTRY "æç"	IS "æç"
	ENTRY "æÂ"	IS "æÂ"
	ENTRY "æé"	IS "æé"
	ENTRY "æè"	IS "æè"
	ENTRY "æä"	IS "æä"
	SEPARATOR
	ENTRY cut		IS cut
	ENTRY "weakenæ"	IS "weakenæ"
	ENTRY "æweaken"	IS "æweaken"
	ENTRY "contractæ"	IS "contractæ"
	ENTRY "æcontract"	IS "æcontract"
END

HYPHIT	A æ A	IS axiom       
HYPHIT	AçB æ	IS "çæ"        
HYPHIT	AëB æ	IS "ëæ"
HYPHIT	A¦B æ 	IS "¦æ"    
HYPHIT	ÂA æ		IS "Âæ"    
HYPHIT	AéB æ	IS "éæ"    
HYPHIT	èx.A æ	IS "èæ"
HYPHIT	äx.A æ	IS "äæ"

CONCHIT	B¦C	IS "æ¦"
CONCHIT	BëC	IS "æë"      
CONCHIT	BçC	IS "æç"
CONCHIT	ÂB	IS "æÂ"       
CONCHIT	BéC	IS "æé"     
CONCHIT	èx.B	IS "æè"  
CONCHIT	äx.B	IS "æä"  

AUTOMATCH axiom

STRUCTURERULE CUT            		cut
STRUCTURERULE LEFTWEAKEN     	"weakenæ"
STRUCTURERULE RIGHTWEAKEN   	"æweaken"
STRUCTURERULE LEFTCONTRACT   	"contractæ"
STRUCTURERULE RIGHTCONTRACT 	"æcontract"
