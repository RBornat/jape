/* $Id$ */

SEQUENT IS BAG æ FORMULA

INFIX 400L =, ­
INFIX 300L ¦
INFIX 200L ë
INFIX 100R ç, ê
INFIX 50L ÷

PREFIX Â

LEFTFIX è .
LEFTFIX ä .
LEFTFIX ä! .

CLASS VARIABLE x, y, z
CLASS FORMULA A, B, C, D, P, Q, R, S
CLASS CONSTANT c, d
CONSTANT Ù

BIND x SCOPE P IN èx . P
BIND x SCOPE P IN äx . P
BIND x SCOPE P IN ä!x . P

INITIALISE autoAdditiveLeft true /* avoid explicit statement of left context */
