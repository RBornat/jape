/* $Id$ */

INFIX 300L ¦
INFIX 200L ë
INFIX 100R ç
PREFIX Â

LEFTFIX è .
LEFTFIX ä .

PREFIX wellformed, constant

CLASS VARIABLE x, y, z, c, d
CLASS FORMULA A, B, C, P, Q, R, S
/* CLASS CONSTANT c, d */

BIND x SCOPE P IN èx . P
BIND x SCOPE P IN äx . P

INITIALISE autoAdditiveLeft	true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates	true /* allow predicate syntax ... */
