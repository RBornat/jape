/* $Id$ */

/* stuff to add LF vars to the naive set theory encoding */

USE "BnE-Fprime_LF.j" 

RULE "§-I(c)"(OBJECT c) WHERE FRESH c IS FROM new c, cÚA æ cÚB INFER A§B
RULE "§-I(<c,d>)"(OBJECT c,OBJECT d) WHERE FRESH c,d IS FROM new c, new d, <c,d>ÚA æ <c,d>ÚB INFER A§B
