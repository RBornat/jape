/* $Id$ */

SEQUENT IS BAG æ FORMULA

INFIX 400 400 =, ­
INFIX 300 300 ¦
INFIX 200 200 ë
INFIX 101 100 ç, ê
INFIX 50 50 ÷

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
