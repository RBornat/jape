/* $Id$ */

/* syntax of very simple set theory, for QMW IDS course 1996 */

CLASS VARIABLE u, v, w
CONSTANT ¯, Ù, U

INFIX 700 700 ï, ß, -
INFIX 600 600 §
INFIX 500 500 Ú, ÂÚ
/* 400 is = in BnE-Fprime_syntax.j */

OUTFIX { | }
PREFIX ïï, ßß
POSTFIX ø

BIND y SCOPE P IN { y | P }
