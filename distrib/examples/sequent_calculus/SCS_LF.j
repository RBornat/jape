/* $Id$ */

/* single-conclusion sequent calculus with LF-like treatment of variables */

USE "SCS.jt"
USE "sequent_scoping.j"

RULE	"жи"(OBJECT m) WHERE FRESH m
			FROM В , constant m ж P(m) 				INFER В ж иx.P(x)
RULE	"иж"(B)	FROM В, P(B) ж C AND В ж B inscope			INFER В, иx.P(x) ж C
RULE	"жд"(B)	FROM В ж P(B) AND В ж B inscope			INFER В ж дx.P(x)
RULE	"дж"(OBJECT m) WHERE FRESH m
			FROM  В, constant m, P(m) ж C 				INFER В, дx.P(x) ж C

TACTIC "иж with side condition hidden" IS LAYOUT "иж" (0) (WITHSELECTIONS "иж")
TACTIC "жд with side condition hidden" IS LAYOUT "жд" (0) (WITHSELECTIONS "жд")

MENU Rules IS
	ENTRY "иж" IS "иж with side condition hidden"
	ENTRY "жд" IS "жд with side condition hidden"
END

HYPHIT	иx.A ж B	IS "иж with side condition hidden"
CONCHIT	дx.B		IS "жд with side condition hidden"
