/* $Id$ */

PREFIX #
POSTFIX ø

INFIX 300 300 ê
INFIX 300 300 Ø
INFIX 300 300 Ÿ

INFIX 201 200 •

INFIX 151 150 š

INFIX 101 100 Š /* rassoc: left priority higher */

INFIX 50 50 ‘

INFIX 10 10 +

OUTFIX {  }
OUTFIX <  >

LEFTFIX è .

CLASS VARIABLE x, k
CLASS FORMULA X, Y, Z
CLASS CONSTANT P, Q, R, K, N
CONSTANT A, B, S

BIND x SCOPE P IN èx . P

SEQUENT IS BAG æ FORMULA

INITIALISE autoAdditiveLeft true /* allow rules to be stated without an explicit left context */
