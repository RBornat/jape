/* $Id$ */

USE "sequent_scoping.j"

RULE	"жи"(OBJECT m) WHERE FRESH m
			FROM ‚, var m ж A(m),Ж				INFER ‚ ж иx.A(x),Ж
RULE	"иж"(B)	FROM ‚, A(B) ж Ж AND ‚ ж B inscope		INFER ‚,иx.A(x) ж Ж
RULE	"жд"(B)	FROM ‚ ж A(B),Ж AND ‚ ж B inscope		INFER ‚ ж дx.A(x),Ж
RULE	"дж"(OBJECT m) WHERE FRESH m
			FROM  ‚, var m, A(m) ж Ж				INFER ‚, дx.A(x) ж Ж

TACTIC "иж with side condition hidden" IS LAYOUT "иж" (0) (WITHSELECTIONS "иж")
TACTIC "жд with side condition hidden" IS LAYOUT "жд" (0) (WITHSELECTIONS "жд")

MENU Rules IS
	ENTRY "иж" IS "иж with side condition hidden"
	ENTRY "жд" IS "жд with side condition hidden"
END

HYPHIT	иx.A ж	IS "иж with side condition hidden"
CONCHIT	ж дx.B	IS "жд with side condition hidden"
