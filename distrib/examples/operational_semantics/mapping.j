/*
        This is the theory of mappings described
        in the appendix to the note
        ``Animating Operational Semantics with Jape''
        by Bernard Sufrin and Richard Bornat.

        $Id$
*/
        CLASS FORMULA  map
        CONSTANT   â Ù
        INFIX   5L ==
        INFIX   3L ¼
        RULES Map WHERE  y NOTIN x
        ARE   æ  â x = Ù
        AND   æ  (x==V)  x = V
        AND   æ  (x==V)  y = Ù
        END
        RULES Select  WHERE W NOTIN Ù
        ARE INFERæ V ¼ Ù = V
        AND INFERæ V ¼ W = W
        END
        RULE "¼"
        FROM  æ  map' x = V
        AND   æ  map  x = W
        AND   æ  W ¼ V  = X
        INFER æ (map¼map') x = X
    TACTIC Lookup IS
    (ALT Map (SEQ "¼" Lookup Lookup Select))
    RULE  Update0  æ (â¼(x==V)) = (â¼(x==V))
    RULE  Update1  æ (map¼(x==W)¼(x==V)) = (map¼(x==V))
    RULE  Update2
    FROM  æ (map¼(x==V)) = map'
    INFER æ (map¼(y==W)¼(x==V)) = (map'¼(y==W))

    TACTIC Update IS (ALT  Update0 Update1 (SEQ Update2 (PROVE Update)))
/*
CONJECTUREPANEL Environments
 BUTTON "Lookup"  IS apply Lookup
 BUTTON "¼" IS apply "¼"
 BUTTON "Select"  IS apply Select
 BUTTON "Update"  IS apply Update
 THEOREMS environments ARE
      æ â  x = _T
 AND  æ (x==3 ¼ â)   x = _T
 AND  æ (x==3 ¼ y==4) x = _T
 AND  æ (y==3 ¼ x==4) x = _T
 AND  æ ((y==3 ¼ x==4) ¼ (a==3 ¼ b==4)) a = _T
 AND  æ ((y==3 ¼ x==4) ¼ (a==3 ¼ b==4)) b = _T
 AND  æ ((y==3 ¼ x==4) ¼ (a==3 ¼ b==4)) y = _T
 AND  æ ((w==3 ¼ x==4) ¼ (a==3 ¼ b==4) ¼ (y==3 ¼ x==4) ¼ (a==3 ¼ b==4)) w = _T
 AND  æ (w==3 ¼ x==4 ¼ a==3 ¼ b==4 ¼ y==3 ¼ x==4 ¼ a==3 ¼ b==4) w = _T
 AND  æ (x==4 ¼  w==(3 Ù) ¼ x==4 ¼ a==3 ¼ b==4 ¼ y==3 ¼ x==4 ¼ a==3 ¼ b==4) w = _T
 AND  æ (â ¼ (x==3) ¼ (y==4) ¼ (z==5) ¼ (x==6)) = _T
 AND  æ (â ¼ (x==3) ¼ (y==4) ¼ (z==5) ¼ (p==6)) = _T
 END
END
*/
