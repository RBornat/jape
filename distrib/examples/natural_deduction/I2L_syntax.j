/* $Id$ */

CLASS VARIABLE x y z   i j k
CLASS FORMULA A B C   E F G  P  R S T
CLASS BAG FORMULA ‚

CONSTANT Ù

PREFIX	10		actual
/* POSTFIX	10		inscope   no longer used. I don't think that proof files will crash without it */

INFIX		100R		ç
INFIX		120L		ë
INFIX		140L		¦

LEFTFIX	180		è .
LEFTFIX	180		ä .

PREFIX	200		Â

JUXTFIX		300
SUBSTFIX	400 

BIND x SCOPE A IN èx . A
BIND x SCOPE B IN äx . B

SEQUENT IS BAG æ FORMULA

INITIALISE autoAdditiveLeft		true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates	true /* allow predicate syntax ... */
