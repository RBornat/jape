/* $Id$ */

/* stuff to add LF vars to the naive set theory encoding */

USE "BnE-Fprime_LF.j" 

RULE "⊆-I(c)"(OBJECT c) WHERE FRESH c IS FROM new c, c∈A ⊢ c∈B INFER A⊆B
RULE "⊆-I(<c,d>)"(OBJECT c,OBJECT d) WHERE FRESH c,d IS FROM new c, new d, <c,d>∈A ⊢ <c,d>∈B INFER A⊆B
