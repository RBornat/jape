/*
        This is the theory of mappings described
        in the appendix to the note
        ``Animating Operational Semantics with Jape''
        by Bernard Sufrin and Richard Bornat.

        $Id$
*/
        CLASS FORMULA  map
        CONSTANT   ▪ ⊥
        INFIX   5L ==
        INFIX   3L ⊕
        RULES Map WHERE  y NOTIN x
        ARE   ⊢  ▪ x = ⊥
        AND   ⊢  (x==V)  x = V
        AND   ⊢  (x==V)  y = ⊥
        END
        RULES Select  WHERE W NOTIN ⊥
        ARE INFER⊢ V ⊕ ⊥ = V
        AND INFER⊢ V ⊕ W = W
        END
        RULE "⊕"
        FROM  ⊢  map' x = V
        AND   ⊢  map  x = W
        AND   ⊢  W ⊕ V  = X
        INFER ⊢ (map⊕map') x = X
    TACTIC Lookup IS
    (ALT Map (SEQ "⊕" Lookup Lookup Select))
    RULE  Update0  ⊢ (▪⊕(x==V)) = (▪⊕(x==V))
    RULE  Update1  ⊢ (map⊕(x==W)⊕(x==V)) = (map⊕(x==V))
    RULE  Update2
    FROM  ⊢ (map⊕(x==V)) = map'
    INFER ⊢ (map⊕(y==W)⊕(x==V)) = (map'⊕(y==W))

    TACTIC Update IS (ALT  Update0 Update1 (SEQ Update2 (PROVE Update)))
/*
CONJECTUREPANEL Environments
 BUTTON "Lookup"  IS apply Lookup
 BUTTON "⊕" IS apply "⊕"
 BUTTON "Select"  IS apply Select
 BUTTON "Update"  IS apply Update
 THEOREMS environments ARE
      ⊢ ▪  x = _T
 AND  ⊢ (x==3 ⊕ ▪)   x = _T
 AND  ⊢ (x==3 ⊕ y==4) x = _T
 AND  ⊢ (y==3 ⊕ x==4) x = _T
 AND  ⊢ ((y==3 ⊕ x==4) ⊕ (a==3 ⊕ b==4)) a = _T
 AND  ⊢ ((y==3 ⊕ x==4) ⊕ (a==3 ⊕ b==4)) b = _T
 AND  ⊢ ((y==3 ⊕ x==4) ⊕ (a==3 ⊕ b==4)) y = _T
 AND  ⊢ ((w==3 ⊕ x==4) ⊕ (a==3 ⊕ b==4) ⊕ (y==3 ⊕ x==4) ⊕ (a==3 ⊕ b==4)) w = _T
 AND  ⊢ (w==3 ⊕ x==4 ⊕ a==3 ⊕ b==4 ⊕ y==3 ⊕ x==4 ⊕ a==3 ⊕ b==4) w = _T
 AND  ⊢ (x==4 ⊕  w==(3 ⊥) ⊕ x==4 ⊕ a==3 ⊕ b==4 ⊕ y==3 ⊕ x==4 ⊕ a==3 ⊕ b==4) w = _T
 AND  ⊢ (▪ ⊕ (x==3) ⊕ (y==4) ⊕ (z==5) ⊕ (x==6)) = _T
 AND  ⊢ (▪ ⊕ (x==3) ⊕ (y==4) ⊕ (z==5) ⊕ (p==6)) = _T
 END
END
*/
