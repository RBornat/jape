/* $Id$ */

/* syntax of very simple set theory, for QMW IDS course 1997 */

CLASS VARIABLE u v w
CONSTANT ¯ Ù U

OUTFIX < >
PREFIX	800		ïï ßß
POSTFIX	800		ø
INFIX		700L		ï ß -
INFIX		600L		§
INFIX		500L		Ú ÂÚ
/* 400 is = in BnE-Fprime_syntax.j */

OUTFIX { | }

BIND y SCOPE P IN { y | P }
BIND x y SCOPE P IN { <x,y> | P }
