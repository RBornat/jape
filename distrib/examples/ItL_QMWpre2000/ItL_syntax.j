/* $Id$ */

CLASS VARIABLE x y z c d
CLASS FORMULA A B C P Q R S

PREFIX	10		var
POSTFIX	10		inscope

LEFTFIX	20		è .
LEFTFIX	20		ä .

INFIX		100R		ç
INFIX		120L		¦
INFIX		140L		ë
PREFIX	200		Â
JUXTFIX	300
SUBSTFIX	400 

BIND x SCOPE P IN èx . P
BIND x SCOPE P IN äx . P

INITIALISE autoAdditiveLeft	true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates	true /* allow predicate syntax ... */
