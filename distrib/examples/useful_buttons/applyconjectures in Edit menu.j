/* $Id$ */

MENU Edit
    RADIOBUTTON applyconjectures IS
        "Forbid the use of unproven theorems/rules in other proofs"  IS none
    AND "Permit the use of unproven theorems, but not rules ..."     IS theorems
    AND "Permit the use of unproven rules, but not theorems ..."     IS rules
    AND "Permit the use of unproven theorems and rules ..."          IS all
    INITIALLY none
    END
END

