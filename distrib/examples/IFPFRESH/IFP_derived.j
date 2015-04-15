/* $Id$
   Opportunity to do proofs of derived rules, that we have introduced in the
   regular rules menu as honest-to-god rules.
*/

CONJECTUREPANEL "Derived Rules"
    DERIVED RULE "(RAA)"     IS FROM ¬A ⊢ ⊥ INFER A
    DERIVED RULE "(∧⊢)"      IS FROM A , B ⊢ C INFER A∧B ⊢ C
    DERIVED RULE "(¬¬⊢)"     IS FROM B ⊢ C INFER ¬¬B⊢C
    DERIVED RULE "(∀⊢)"      IS FROM P(T)  ⊢ C INFER ∀x . P(x) ⊢ C
    DERIVED RULE "(= sym)"    IS FROM T1=T2 INFER T2=T1   
    DERIVED RULE "(= trans)"  IS FROM T1=T2 AND T2=T3 INFER T1=T3   
    BUTTON Apply IS apply ForbidDerived COMMAND 
END


TACTIC ForbidDerived(rulename) 
      (Fail ("(All you can do with %s here is to start proving it)\
              \\nThe derived rules are already available (as if proven) on the Rules menu.", rulename))


/** Proofs left unloaded

﻿CONJECTUREPANEL "Derived Rules"
PROOF "(∧⊢)"
FROM A,
     B 
     ⊢ C 
INFER A∧B 
     ⊢ C 
FORMULAE
0 A∧B,
1 A,
2 B,
3 C 
IS
SEQ (cut[B,C\1,3]) ("∧ elim(L)"[A,B\1,2]) (hyp[A\0]) (cut[B,C\2,3]) ("∧ elim(R)"[A,B\1,2]) (hyp[A\0]) (GIVEN 0)
END
CONJECTUREPANEL "Derived Rules"
PROOF "(¬¬⊢)"
FROM B 
     ⊢ C 
INFER ¬¬B 
     ⊢ C 
FORMULAE
0 ¬¬B,
1 B,
2 C 
IS
SEQ (cut[B,C\1,2]) ("¬¬ elim"[B\1]) (hyp[A\0]) (GIVEN 0)
END
CONJECTUREPANEL "Derived Rules"
PROOF "(∀⊢)"
FROM P(T)
     ⊢ C 
INFER ∀x.P(x)
     ⊢ C 
FORMULAE
0 ∀x.P(x),
1 x,
2 T,
3 P(x),
4 P(T),
5 C 
IS
SEQ (cut[B,C\4,5]) ("∀ elim"[x,T,P\1,2,3]) (hyp[A\0]) (GIVEN 0)
END
CONJECTUREPANEL "Derived Rules"
PROOF "(RAA)"
FROM ¬A 
     ⊢ ⊥ 
INFER A 
FORMULAE
0 ¬A,
1 A 
IS
SEQ ("¬¬ elim"[B\1]) ("¬ intro"[A\0]) (GIVEN 0)
END
**/


