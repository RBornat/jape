/*
    Copyright (C) 2000-8 Richard Bornat
     
        richard@bornat.me.uk

    This file is part of the I2L logic encoding, distributed with jape.

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

/* patch the I2L quantifier mechanisms to suit a theory that only quantifies over integers */

PREFIX  10  integer

RULE "∀ elim"          IS FROM ∀x. P(x) INFER P(A)
RULE "∃ elim"(OBJECT i) WHERE FRESH i AND i NOTIN ∃x.P(x)
                        IS FROM ∃x.P(x) AND integer i, P(i) ⊢ C INFER C
RULE "∀ intro"(OBJECT i) WHERE FRESH i
                    IS FROM integer i ⊢ P(i) INFER ∀x .P(x)
RULE "∃ intro"          IS FROM P(A) INFER ∃x.P(x)

MENU Forward IS
    RENAMEENTRY "∀ elim (needs variable)" "∀ elim (needs formula)"                                                                                                                            
END

TACTIC "∀ elim forward" IS
  WHEN    
    (LETHYP2 (integer _i) (∀_x._A) ("∀ elim forward step" (∀_x._A) _i))
    (LETHYP (∀_x._A) 
        (WHEN (LETARGSEL _B ("∀ elim forward step" (∀_x._A) _B))
              ("∀ elim forward moan" ("You only selected %t.", ∀_x._A))))
    (LETHYP (integer _i) ("∀ elim forward moan" ("You only selected %t.", integer _i)))
    (LETHYPS _As ("∀ elim forward moan" ("You selected %l.", (_As, ", ", " and "))))
    (LETGOAL (∀_x._A)
        (ALERT  ("To make a ∀ elim step forward, you must select a hypothesis of the form ∀x.A.\
                \\n\n\
                \You didn't select any hypotheses, but the \
                \current conclusion is %t, which could be used to make a ∀ intro step backwards.\
                \\nDid you perhaps mean to make a backward step with ∀ intro?", ∀_x._A)
                ("OK",STOP) ("Huh?",SEQ Explainhypothesisandconclusionwords STOP)
        )
    )
    ("∀ elim forward moan" "You didn't select any hypotheses")

TACTIC "∀ elim forward step" (P, _A) IS
    CUTIN "∀ elim"[A\_A] (WITHHYPSEL (hyp P))

TACTIC "∀ elim forward moan" (extra) IS
    ALERT   ("To make a ∀ elim step forward, you must select a hypothesis \
             \of the form ∀x.A, and also either select a pseudo-assumption \
             \of the form integer i, or subformula-select a \
             \formula to instantiate x. %s",extra)
             ("OK", STOP) /* ("Huh?", SEQ (SHOWHOWTO "TextSelect") STOP) */


MENU Backward IS
    RENAMEENTRY "∃ intro (needs variable)" "∃ intro (needs formula)"
END

TACTIC "∃ intro backward unselected goal" (A) IS
    ALERT ("To make an ∃ intro step backwards, you have to select a conclusion of the form \
           \∃x.A. There is a conclusion of that form (it's %t), but you didn't select it.\n\n\
           \Select the conclusion you want to work backwards from, and try again.",A)
           ("OK", STOP)

TACTIC "∃ intro backward hypcheck" (action, gmess) IS
    WHEN    
        (LETHYP (integer _i) (action _i)) /* the right hypothesis - hoorah! */
        (LETHYP _A /* the wrong hypothesis */
            ("∃ intro backward selection moan" ("%s you selected the hypothesis %t instead.", gmess, _A)))
        (LETHYPS _As /* more than one */
            ("∃ intro backward selection moan" ("%s you selected more than one hypothesis – %l.", gmess, (_As, ", ", " and "))))
        (LETARGSEL _A (action _A))
        ("∃ intro backward selection moan" ("%s you didn't select a hypothesis or subformula-select a formula.", gmess))

TACTIC "∃ intro backward selection moan" (stuff) IS
    ALERT   ("To make an ∃ intro step backwards, you have to use a conclusion of the form \
             \∃x.A, and also either select a pseudo-assumption of the form integer i, \
             \or subformula-select a formula to instantiate x. %s", stuff)
             ("OK", STOP) /* ("Huh?", SEQ (SHOWHOWTO "TextSelect") STOP) */

TACTIC "∃ intro backward step" (_A) IS
    SEQ "∃ intro"[A\_A] fstep
