/* $Id$ */

/*
	The multi-conclusion sequent calculus
*/

FONTS "Konstanz 12 9 Detroit"
DISPLAY TREE

INFIX   1000    1000    é
INFIX   1100    1100    ç
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
RULE	"¦-R"		FROM æ A AND æ B INFER æ A¦B
RULE	"¦-L"		FROM A, B æ  INFER   A¦B æ 
RULE	"ë-R"		FROM æ A,B INFER æ AëB
RULE	"ë-L"		FROM A æ  AND B æ  INFER AëB æ 
RULE	"Â-R"		FROM A æ INFER æ ÂA
RULE	"Â-L"		FROM æ A INFER ÂA æ 
RULE	"ç-R"		FROM A æ B INFER æ AçB
RULE	"ç-L"		FROM AçB æ A AND B æ  INFER AçB æ
RULE	"é-R"		FROM æ AçB AND æ BçA INFER æ AéB
RULE	"é-L"		FROM AçB, BçA æ  INFER AéB æ 
RULE	"è-R"(OBJECT y) WHERE FRESH y
			FROM æ A[x\y] INFER æ èx . A
RULE	"è-L"(B)		FROM èx.A, A[x\B] æ INFER èx.A æ
RULE	"ä-R"(B)		FROM æ A[x\B] INFER æ äx.A
RULE	"ä-L"(OBJECT y) WHERE FRESH y
			FROM  A[x\y] æ INFER äx.A æ
RULE	cut(A)		FROM æ A AND A æ INFER æ
RULE	leftweaken(A)	FROM æ INFER A æ 
RULE	rightweaken(A)	FROM æ INFER æ A
RULE	leftcontract(A)	FROM A, A æ INFER A æ 
RULE	rightcontract(A)	FROM æ A, A INFER æ A 
                                
 
THEOREM	modusponens	IS A, AçB æ B
THEOREM contradiction	IS A, ÂA æ

MENU Rules IS
	ENTRY axiom	IS  axiom
	SEPARATOR
	ENTRY "¦-L"	IS "¦-L"
	ENTRY "ë-L"	IS "ë-L"
	ENTRY "ç-L"	IS "ç-L"
	ENTRY "Â-L"	IS "Â-L"
	ENTRY "é-L"	IS "é-L"
	ENTRY "è-L"	IS "è-L"
	ENTRY "ä-L"	IS "ä-L"
	SEPARATOR
	ENTRY "¦-R"	IS "¦-R"
	ENTRY "ë-R"	IS "ë-R"
	ENTRY "ç-R"	IS "ç-R"
	ENTRY "Â-R"	IS "Â-R"
	ENTRY "é-R"	IS "é-R"
	ENTRY "è-R"	IS "è-R"
	ENTRY "ä-R"	IS "ä-R"
	SEPARATOR
	ENTRY cut		IS cut
	ENTRY leftweaken	IS leftweaken
	ENTRY rightweaken	IS rightweaken
	ENTRY leftcontract	IS leftcontract
	ENTRY rightcontract	IS rightcontract
END

HYPHIT	A æ A	IS axiom       
HYPHIT	AçB æ	IS "ç-L"        
HYPHIT	AëB æ	IS "ë-L"
HYPHIT	A¦B æ 	IS "¦-L"    
HYPHIT	ÂA æ	IS "Â-L"    
HYPHIT	AéB æ	IS "é-L"    
HYPHIT	èx.A æ	IS "è-L"
HYPHIT	äx.A æ	IS "ä-L"

CONCHIT	B¦C	IS "¦-R"
CONCHIT	BëC	IS "ë-R"      
CONCHIT	BçC	IS "ç-R"
CONCHIT	ÂB	IS "Â-R"       
CONCHIT	BéC	IS "é-R"     
CONCHIT	èx.B	IS "è-R"  
CONCHIT	äx.B	IS "ä-R"  

AUTOMATCH axiom

STRUCTURERULE IDENTITY    axiom
STRUCTURERULE CUT            cut
STRUCTURERULE LEFTWEAKEN     leftweaken
STRUCTURERULE RIGHTWEAKEN   rightweaken
STRUCTURERULE LEFTCONTRACT   leftcontract
STRUCTURERULE RIGHTCONTRACT rightcontract
