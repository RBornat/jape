/* $Id$ */

INITIALISE interpretpredicates true

FONTS "Konstanz"
INITIALISE displaystyle tree

CLASS BAG ‚ Æ
CLASS FORMULA A B C D P Q R S
CLASS VARIABLE u v w x y z m n

/* first two operators are for LF treatment of variables */
PREFIX	10		var
POSTFIX	10		inscope

LEFTFIX	20		è .
LEFTFIX	20		ä .

INFIX		100L		é
INFIX		110R		ç
INFIX		150L		¦
INFIX		160L		ë
PREFIX	200		Â
JUXTFIX	300
SUBSTFIX	400 

BIND    x SCOPE P IN äx . P
BIND    x SCOPE P IN èx . P
