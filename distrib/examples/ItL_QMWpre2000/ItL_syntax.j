/* $Id$ */

INFIX 300 300 ¦
INFIX 200 200 ë
INFIX 101 100 ç	/* ç is right-associative, which means a higher _leftwards_ precedence!! */
PREFIX Â

LEFTFIX è .
LEFTFIX ä .

CLASS VARIABLE x, y, z
CLASS FORMULA A, B, C, P, Q, R, S
CLASS CONSTANT c

BIND x SCOPE P IN èx . P
BIND x SCOPE P IN äx . P

INITIALISE autoAdditiveLeft	true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates	true /* allow predicate syntax ... */
