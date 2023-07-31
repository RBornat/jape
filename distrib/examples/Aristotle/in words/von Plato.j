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
   Encoded by RB, 07-10/2020 and 05/2022
 */

USE "Syllogisms syntax.j"
USE "../Syllogisms rules.j"
USE "../Syllogisms appearance.j"

RULE "subaltern every⇒some" IS FROM every X is Y INFER some Y is X
RULE "reversal no⇒no" IS FROM no X is Y INFER no Y is X

DERIVED RULE "subaltern no⇒some-non" IS FROM no X is Y INFER some Y is non X
DERIVED RULE "reversal some⇒some"  IS FROM some X is Y INFER some Y is X

PATCHALERT "\"subaltern no⇒some-non\" is unproved"
           "The subaltern rule no⇒some-non, which you need for the step \
            \you chose, is unproved. It's in the Derived Rules panel. Prove it \
            \before you use it. (It's easier to prove in sequent style.)"
            ("OK")

PATCHALERT "\"reversal some⇒some\" is unproved"
           "The reversal rule some⇒some, which you need for the step \
            \you chose, is unproved. It's in the Derived Rules panel. Prove it \
            \before you use it. (It's easier to prove in sequent style.)"
            ("OK")

CONJECTUREPANEL "Derived Rules" IS
  ENTRY "no X is Y ⇒ some Y is non X" IS "subaltern no⇒some-non"
  ENTRY "some X is Y ⇒ some Y is X" IS "reversal some⇒some"
END

RULE Barbara(M) IS FROM every M is P AND every S is M INFER every S is P
RULE Celarent(M) IS FROM no M is P AND every S is M INFER no S is P

RULES contra ARE
      FROM every S is P INFER (some S is non P)*
  AND FROM no S is P INFER (some S is P)*      
  AND FROM some S is P INFER (no S is P)*
  AND FROM some S is non P INFER (every S is P)*
END

RULES IP(B) ARE
     WHERE NTDISCHARGE every S is P FROM every S is P ⊢ B AND B* INFER some S is non P  
 AND WHERE NTDISCHARGE no S is P FROM no S is P ⊢ B AND B* INFER some S is P  
 AND WHERE NTDISCHARGE some S is P FROM some S is P ⊢ B AND B* INFER no S is P  
 AND WHERE NTDISCHARGE some S is non P FROM some S is non P ⊢ B AND B* INFER every S is P 
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
  ENTRY "subaltern (every⇒some,no⇒some-non)" IS WHEN (LETHYP (every _P is _S) (ForwardOrBackward ForwardCut 0 "subaltern every⇒some"))
                                          (LETHYP (no _S is _P) (ForwardOrBackward ForwardCut 0 "subaltern no⇒some-non"))
                                          (LETHYP _A (Fail ("subaltern is not applicable to antecedent %t", _A)))
                                          (LETGOAL (some _S is _P) (furdle "subaltern every⇒some"))
                                          (LETGOAL (some _S is non _P) (furdle "subaltern no⇒some-non"))
                                          (LETGOAL _A (Fail ("subaltern is not applicable to consequent %t", _A)))

  ENTRY "reversal (no⇔no,some⇔some)" IS WHEN (LETHYP (no _P is _S) (ForwardOrBackward ForwardCut 0 "reversal no⇒no"))
                                         (LETHYP (some _P is _S) (ForwardOrBackward ForwardCut 0 "reversal some⇒some"))
                                         (LETHYP _A (Fail ("conversion is not applicable to antecedent %t", _A)))
                                         (LETGOAL (no _S is _P) (furdle "reversal no⇒no"))
                                         (LETGOAL (some _S is _P) (furdle "reversal some⇒some"))
                                         (LETGOAL _A (Fail ("conversion is not applicable to consequent %t", _A)))
  
  ENTRY "(Barbara) every M is P, every S is M ⇒ every S is P"  IS "Syllogism-tac" Barbara (every _M is _P) (every _S is _M) _M (every _S is _P)
  ENTRY "(Celarent) no M is P, every S is M ⇒ no S is P" IS "Syllogism-tac" Celarent (no _M is _P) (every _S is _M) _M (no _S is _P)

  SEPARATOR
    
  ENTRY IP IS WHEN (LETGOAL _A (LAYOUT "IP" ALL IP (LAYOUT ASSUMPTION ("contradicting %t",_A))))
                   (ALERT "Please select a consequent to be proved by IP")
                   
  SEPARATOR
  
  /* ENTRY contra IS "contra-tac" */

  ENTRY "previously" IS SEQ remstar (WHEN (LETHYP _A (see _A))
                                            see
                                    )
END
