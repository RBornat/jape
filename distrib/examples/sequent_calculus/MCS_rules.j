/* $Id$ */

/*
	The multi-conclusion sequent calculus. Dyckhoffery removed.
*/

USE "sequent_syntax.j"

SEQUENT IS BAG æ BAG

RULE	axiom(A)								INFER ‚,A æ A,Æ
RULE	"æ¦"		FROM ‚ æ A,Æ AND ‚ æ B,Æ 		INFER ‚ æ A¦B,Æ
RULE	"¦æ"		FROM ‚,A, B æ Æ 				INFER ‚,A¦B æ Æ
RULE	"æë"		FROM ‚ æ A,B,Æ 				INFER ‚ æ AëB,Æ
RULE	"ëæ"		FROM ‚,A æ Æ AND ‚,B æ Æ		INFER ‚,AëB æ Æ
RULE	"æÂ"		FROM ‚,A æ Æ					INFER ‚ æ ÂA,Æ
RULE	"Âæ"		FROM ‚ æ A,Æ 					INFER ‚,ÂA æ Æ
RULE	"æç"		FROM ‚,A æ B,Æ 				INFER ‚ æ AçB,Æ
RULE	"çæ"		FROM ‚ æ A,Æ AND ‚,B æ Æ		INFER ‚,AçB æ Æ
RULE	"æé"		FROM ‚ æ AçB,Æ AND ‚ æ BçA,Æ	INFER ‚ æ AéB,Æ
RULE	"éæ"		FROM ‚, AçB, BçA æ Æ			INFER ‚,AéB æ Æ
RULE	"æè"(OBJECT m) WHERE FRESH m
			FROM ‚ æ A(m),Æ				INFER ‚ æ èx.A(x),Æ
RULE	"èæ"(B)	FROM ‚, A(B) æ Æ				INFER ‚,èx.A(x) æ Æ
RULE	"æä"(B)	FROM ‚ æ A(B),Æ				INFER ‚ æ äx.A(x),Æ
RULE	"äæ"(OBJECT m) WHERE FRESH m
			FROM  ‚,A(m) æ Æ				INFER ‚, äx.A(x) æ Æ
RULE	cut(A)	FROM ‚ æ A,Æ AND ‚,A æ Æ		INFER ‚ æ Æ
RULE	"weakenæ"(A)	FROM ‚ æ Æ				INFER ‚,A æ Æ
RULE	"æweaken"(A)	FROM ‚ æ Æ				INFER ‚ æ A,Æ
RULE	"contractæ"(A)	FROM ‚, A, A æ Æ			INFER ‚, A æ Æ
RULE	"æcontract"(A)	FROM ‚ æ A,A,Æ			INFER ‚ æ A,Æ
                                
MENU Rules IS
	ENTRY axiom
	SEPARATOR
	ENTRY "¦æ"
	ENTRY "ëæ"
	ENTRY "çæ"
	ENTRY "Âæ"
	ENTRY "éæ"
	ENTRY "èæ"
	ENTRY "äæ"
	SEPARATOR
	ENTRY "æ¦"
	ENTRY "æë"
	ENTRY "æç"
	ENTRY "æÂ"
	ENTRY "æé"
	ENTRY "æè"
	ENTRY "æä"
	SEPARATOR
	ENTRY cut	
	ENTRY "weakenæ"
	ENTRY "æweaken"
	ENTRY "contractæ"
	ENTRY "æcontract"
END

HYPHIT	A æ A	IS axiom       
HYPHIT	AçB æ	IS "çæ"        
HYPHIT	AëB æ	IS "ëæ"
HYPHIT	A¦B æ 	IS "¦æ"    
HYPHIT	ÂA æ		IS "Âæ"    
HYPHIT	AéB æ	IS "éæ"    
HYPHIT	èx.A æ	IS "èæ"
HYPHIT	äx.A æ	IS "äæ"

CONCHIT	æ B¦C	IS "æ¦"
CONCHIT	æ BëC	IS "æë"      
CONCHIT	æ BçC	IS "æç"
CONCHIT	æ ÂB		IS "æÂ"       
CONCHIT	æ BéC	IS "æé"     
CONCHIT	æ èx.B	IS "æè"  
CONCHIT	æ äx.B	IS "æä"  

AUTOMATCH axiom

STRUCTURERULE CUT            		cut
STRUCTURERULE LEFTWEAKEN     	"weakenæ"
STRUCTURERULE RIGHTWEAKEN   	"æweaken"
