/* $Id$ */

/* syntax of very simple set theory, for QMW IDS course 1997 */

CLASS VARIABLE u v w
CONSTANT ¯ Ù U EQ

OUTFIX < >
PREFIX	1000		Pow
PREFIX	800		ïï ßß
POSTFIX	800		ø
INFIX		700L		ï ß -
INFIX		720L		¥
INFIX		740L		ô
INFIX		600L		§
INFIX		500L		Ú ÂÚ
/* highest priority PREFIX 450 Â in BnE-Fprime_syntax.j */

OUTFIX { | }

BIND y SCOPE P IN { y | P }
BIND x y SCOPE P IN { <x,y> | P }
