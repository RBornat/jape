/* $Id$ */

CONJECTUREPANEL "Set Conjectures" IS
  THEOREMS "Russell" ARE
        {x|x∉x}∈{x|x∉x}↔{x|x∉x}∉{x|x∉x}
    AND A∨¬A
    AND ¬(A∨B) ⊢ ¬A∧¬B
    AND ⊥
  END
END
