/* $Id$ */

SEQUENT IS BAG æ FORMULA

CLASS VARIABLE x y z c d
CLASS FORMULA A B C D P Q R S
CONSTANT Ù

SUBSTFIX	2000
JUXTFIX	1900

PREFIX	500		Â
INFIX		400L		= ­
INFIX		300L		¦
INFIX		200L		ë
INFIX		100R		ç ê
INFIX		50L		÷

LEFTFIX	10	è .
LEFTFIX	10	ä .
LEFTFIX	10	ä! .

BIND x SCOPE P IN èx . P
BIND x SCOPE P IN äx . P
BIND x SCOPE P IN ä!x . P

INITIALISE autoAdditiveLeft true /* avoid explicit statement of left context */
