/*
        $Id: IFP_rules.j 671 2015-04-16 10:29:30Z sufrin $
*/

/*
        Rules
*/


   RULE hyp(A)   IS INFER A ⊢ A
   RULE "⊥ elim" IS FROM ⊥ INFER B
   RULE "⊤ intro" IS INFER ⊤
   RULE "⊤ elim"  IS FROM C INFER ⊤ ⊢ C


   RULE "→ intro"          IS FROM A ⊢ B INFER A→B
   RULE "∧ intro"          IS FROM A AND B INFER A ∧ B
   RULE "∨ intro(L)"(B)    IS FROM A INFER A ∨ B
   RULE "∨ intro(R)"(B)    IS FROM A INFER B ∨ A
   RULE "↔ intro"          IS FROM A→B AND B→A INFER A ↔ B


   RULE "→ elim"    IS FROM A→B AND A INFER B
   RULE "∧ elim(L)" IS FROM A ∧ B INFER A
   RULE "∧ elim(R)" IS FROM A ∧ B INFER B
   RULE "∨ elim"    IS FROM A ∨ B AND A ⊢ C AND B ⊢ C INFER C
   RULE "↔ elim(L)"   IS FROM A ↔ B INFER A→B
   RULE "↔ elim(R)"   IS FROM A ↔ B INFER B→A

   
   RULE "¬ intro"     IS FROM A ⊢ ⊥ INFER ¬A
   RULE "¬ elim"(B)   IS FROM ¬B AND B INFER ⊥
   RULE "¬¬ elim"     IS FROM ¬¬B INFER B

   RULE cut(B)  IS FROM B AND B ⊢ C INFER C
   RULE thin(A) IS FROM C INFER A ⊢ C

   RULE "∀ intro"(OBJECT i) 
       WHERE FRESH i IS 
       FROM fresh i ⊢ P(i) 
       INFER ∀x .P(x)
       
   RULE "∃ intro"(T) IS FROM P(T) INFER ∃x.P(x)
   
                          
   RULE "∀ elim"(T)  IS FROM ∀x. P(x) INFER P(T)
                          
   RULE "∃ elim"(OBJECT i) 
         WHERE FRESH i  /* AND i NOTIN ∃x.P(x) */
         IS    FROM fresh i, P(i) ⊢ C INFER ∃x.P(x) ⊢ C
         
   RULE "= intro" IS T=T
   
   RULE "= elim"(T1,T2,ABSTRACTION P) IS FROM T1=T2 AND P(T1) INFER P(T2)
   
     
/* 
        Declare the names of the standard structural rules
*/
IDENTITY        hyp
CUT             cut
WEAKEN          thin
AUTOMATCH       hyp

/*      and the ''formula'' used in the presentation of FRESH */
SCOPEHYP        x IN fresh x

/* ---------------- */








