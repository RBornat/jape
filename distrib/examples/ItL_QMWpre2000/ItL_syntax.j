/* $Id$ */

CLASS VARIABLE x y z c d
CLASS FORMULA A B C P Q R S
CLASS BAG FORMULA ‚

PREFIX	10		var
POSTFIX	10		inscope

INFIX		100R	ç
INFIX		120L		ë
INFIX		140L		¦

LEFTFIX	180		è .
LEFTFIX	180		ä .

PREFIX	200		Â
JUXTFIX	300
SUBSTFIX	400 

BIND x SCOPE P IN èx . P
BIND x SCOPE P IN äx . P

SEQUENT IS BAG æ FORMULA

INITIALISE autoAdditiveLeft	true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates	true /* allow predicate syntax ... */
