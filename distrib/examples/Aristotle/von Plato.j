/*
    Copyright (C) 2020 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the Aristotleian deductive logic encoding, distributed with jape.

    Jape is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Jape is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with jape; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
    (or look at http://www.gnu.org).

*/

/* Aristotle's deductive logic according to von Plato.
   Encoded by RB, 07-10/2020
 */

USE "Syllogisms syntax.j"
USE "Syllogisms rules.j"

INITIALISE autoAdditiveLeft true
INITIALISE applyconjectures none

INITIALISE displaystyle box
INITIALISE outermostbox false
INITIALISE innerboxes false

INITIALISE outerassumptionword "assumption"
INITIALISE innerassumptionword "assumption"

INITIALISE multiassumptionlines false

INITIALISE hidecut true
INITIALISE hidehyp true
INITIALISE priorAntes true
INITIALISE hidewhy true

INITIALISE multihypsel true

AUTOMATCH see

RULE vp1 IS FROM ∏⁺(P,S) INFER ∑⁺(S,P)
RULE vp2 IS FROM ∏⁻(P,S) INFER ∏⁻(S,P)

DERIVED RULE vp3 IS FROM ∏⁻(P,S) INFER ∑⁻(S,P)
DERIVED RULE vp4 IS FROM ∑⁺(P,S) INFER ∑⁺(S,P)

PATCHALERT "vp3 is unproved"
           "The subaltern rule ∏⁻(P,S) ⇒ ∑⁻(S,P), which you need for the step \
            \you chose, is unproved. It's vp3 in the Derived Rules panel. Prove it \
            \before you use it. (It's easier to prove in sequent style.)"
            ("OK")

PATCHALERT "vp4 is unproved"
           "The reversal rule ∑⁺(P,S) ⇒ ∑⁺(S,P), which you need for the step \
            \you chose, is unproved. It's vp4 in the Derived Rules panel. Prove it \
            \before you use it. (It's easier to prove in sequent style.)"
            ("OK")

CONJECTUREPANEL "Derived Rules" IS
  ENTRY "(vp3, subaltern) FROM ∏⁻(P,S) INFER ∑⁻(S,P)" IS vp3
  ENTRY "(vp4, reversal)  FROM ∑⁺(P,S) INFER ∑⁺(S,P)" IS vp4
END

RULE Barbara(M) IS FROM ∏⁺(M,P) AND ∏⁺(S,M) INFER ∏⁺(S,P)
RULE Celarent(M) IS FROM ∏⁻(M,P) AND ∏⁺(S,M) INFER ∏⁻(S,P)

RULES contra ARE
      FROM ∏⁺(S,P) INFER ∑⁻(S,P)*
  AND FROM ∏⁻(S,P) INFER ∑⁺(S,P)*      
  AND FROM ∑⁺(S,P) INFER ∏⁻(S,P)*
  AND FROM ∑⁻(S,P) INFER ∏⁺(S,P)*
END

RULES IP(B) ARE
     WHERE SINGLEDISCHARGE ∏⁺(S,P) FROM ∏⁺(S,P) ⊢ B AND B* INFER ∑⁻(S,P)  
 AND WHERE SINGLEDISCHARGE ∏⁻(S,P) FROM ∏⁻(S,P) ⊢ B AND B* INFER ∑⁺(S,P)  
 AND WHERE SINGLEDISCHARGE ∑⁺(S,P) FROM ∑⁺(S,P) ⊢ B AND B* INFER ∏⁻(S,P)  
 AND WHERE SINGLEDISCHARGE ∑⁻(S,P) FROM ∑⁻(S,P) ⊢ B AND B* INFER ∏⁺(S,P) 
END

TACTIC "contra-tac" IS LAYOUT HIDEROOT contra

AUTOMATCH "contra-tac"

RULE dcontra IS FROM A INFER A**

TACTIC remstar IS 
  WHEN (LETGOAL (_A*) (LAYOUT HIDEROOT dcontra))
       SKIP
       
TACTIC dogiven(i) IS SEQ remstar (GIVEN i)

INITIALISE givenMenuTactic dogiven

MENU Rules IS
  ENTRY "subaltern (∏⁺⇒∑⁺,∏⁻⇒∑⁻)" IS WHEN (LETHYP (∏⁺(_P,_S)) (ForwardOrBackward ForwardCut 0 vp1))
                                          (LETHYP (∏⁻(_S,_P)) (ForwardOrBackward ForwardCut 0 vp3))
                                          (LETHYP _A (Fail ("subaltern is not applicable to antecedent %t", _A)))
                                          (LETGOAL (∑⁺(_S,_P)) (furdle vp1))
                                          (LETGOAL (∑⁻(_S,_P)) (furdle vp3))
                                          (LETGOAL _A (Fail ("subaltern is not applicable to consequent %t", _A)))

  ENTRY "reversal (∏⁻⇒∏⁻,∑⁺⇒∑⁺)" IS WHEN (LETHYP (∏⁻(_P,_S)) (ForwardOrBackward ForwardCut 0 vp2))
                                         (LETHYP (∑⁺(_P,_S)) (ForwardOrBackward ForwardCut 0 vp4))
                                         (LETHYP _A (Fail ("conversion is not applicable to antecedent %t", _A)))
                                         (LETGOAL (∏⁻(_S,_P)) (furdle vp2))
                                         (LETGOAL (∑⁺(_S,_P)) (furdle vp4))
                                         (LETGOAL _A (Fail ("conversion is not applicable to consequent %t", _A)))
  
  ENTRY "(Barbara) ∏⁺(M,P), ∏⁺(S,M) ⇒ ∏⁺(S,P)"  IS "Syllogism-tac" Barbara (∏⁺(_M,_P)) (∏⁺(_S,_M)) _M (∏⁺(_S,_P))
  ENTRY "(Celarent) ∏⁻(M,P), ∏⁺(S,M) ⇒ ∏⁻(S,P)" IS "Syllogism-tac" Celarent (∏⁻(_M,_P)) (∏⁺(_S,_M)) _M (∏⁻(_S,_P))

  SEPARATOR
    
  ENTRY IP IS WHEN (LETGOAL _A (LAYOUT "IP" ALL IP (LAYOUT ASSUMPTION ("contradicting %t",_A))))
                   (ALERT "Please select a consequent to be proved by IP")
                   
  SEPARATOR
  
  ENTRY contra IS "contra-tac"

  ENTRY same IS SEQ remstar (WHEN (LETHYP _A (see _A))
                                 see)
END

MENU Edit
  RADIOBUTTON displaystyle
       "sequent style"          IS tree
  AND  "Aristotle style"        IS box
  INITIALLY box
  END
  RADIOBUTTON innerboxes
       "with boxes"          IS true
  AND  "linear"              IS false
  INITIALLY false
  END
  RADIOBUTTON hidewhy
       "with step names"    IS false
  AND  "just numbers"       IS true
  INITIALLY true
  END
END
