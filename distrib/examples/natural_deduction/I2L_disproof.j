/* $Id$ */

/* forcing semantics */

SEMANTICTURNSTILE � IS �

FORCEDEF A�B IS BOTH (FORCE A) (FORCE B)
FORCEDEF A�B IS EITHER (FORCE A) (FORCE B)
FORCEDEF A�B IS EVERYWHERE (IF (FORCE A) (FORCE B))
FORCEDEF �A IS NOWHERE (FORCE A)

FORCEDEF �x.P(x) IS EVERYWHERE (ALL (actual i) (FORCE (P(i))))
FORCEDEF �x.P(x) IS SOME (actual i) ( FORCE (P(i)))

