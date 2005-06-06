/*
    Equality
        $Id$
    
    Includes syntax of arithmetic and comparison operators other than equality, but not used.
    
*/

CLASS VARIABLE x y
CLASS FORMULA A B C F G X Y Z
CONSTANT ⊥
 
/* this to allow functions stuff to use square brackets for lists */
SUBSTFIX    2000 { x \ A }
JUXTFIX 1000
INFIX       200L    = ≥ ≤ ≠ < >
INFIX       250L    + -
INFIX       260L    * /
INFIX       270L    ^

 TACTIC Fail(x)     IS (SEQ (ALERT x) STOP)

/***************************** rules *****************************/
 
RULE hyp IS A ⊢ A
IDENTITY hyp

RULE "= reflexive"          IS                                  INFER X = X
RULE "= transitive"(Y)      IS FROM X = Y AND Y = Z             INFER X = Z
RULE "= symmetric"          IS FROM X = Y                       INFER Y = X
RULE "(,)="                 IS FROM X0=X1 AND Y0=Y1 INFER (X0, Y0) = (X1, Y1)

/* The rules of extensionality incorporate a generalization step: hence the FRESH provisos. */
RULE ext (OBJECT x) WHERE FRESH x                   IS FROM  F x = G x              INFER F = G
RULE ext2(OBJECT x, OBJECT y) WHERE FRESH x, y      IS FROM  F (x, y) = G (x,y)     INFER F = G

 /* use of AA, rather than A, is to help some other rule somewhere, which uses OBJECT A */
RULE   rewrite (X,ABSTRACTION AA)           IS FROM X=Y AND AA(Y) INFER AA(X)
RULE   rewritebackwards (Y,ABSTRACTION AA)  IS FROM X=Y AND AA(X) INFER AA(Y)

/* smallstep rewrite rules */
RULE rewriteLR (OBJECT x)           IS FROM X=Y INFER AA{x\X}=AA{x\Y}
RULE rewriteRL (OBJECT x)           IS FROM X=Y INFER AA{x\Y}=AA{x\X} /* derivable */

/*
    Infrastructure for rewriting in an equational theory
*/

TACTIC Flatten IS
    LAYOUT "Associativity" (0)
        (WHEN   (LETARGSEL _A (FLATTEN _A))
                (LETGOAL (_X = _Y) (IF(FLATTEN(_X))) (IF(FLATTEN(_Y)))) 
                (LETGOAL _X (Fail (Cannot Flatten _X)))
        )

/* Now obsolete ...
  TACTIC Find IS 
    WHEN    (LETARGSEL _A (ALT (FIND _A) (Fail (Cannot find _A)))) 
            (Fail (Please select something to find))
*/

TACTIC Unfold(x) IS LAYOUT "Fold %h" (1) (UNFOLD rewrite x)

TACTIC UnfoldOneSel(x) IS
    WHEN    (LETSUBSTSEL _A (LAYOUT "Fold %h" (1) (WITHSUBSTSEL rewrite)) x)
            (LETARGSEL _A (Fail (The formula you selected (_A) is not a proper subformula)))
            (Fail (Please text-select an expression))
        
TACTIC Fold(x) IS LAYOUT "Unfold %h" (1) (FOLD rewritebackwards x)

TACTIC FoldOneSel(x) IS
    WHEN    (LETSUBSTSEL    _A (LAYOUT "Unfold %h" (1) (WITHSUBSTSEL rewritebackwards)) x)
            (LETARGSEL _A (Fail (The formula you selected (_A) is not a proper subformula)))
            (Fail (Please text-select an expression))

TACTIC "Unfold/Fold with hypothesis"  IS
    WHEN 
        (LETHYP _A
            (WHEN   
                (LETCONCSUBSTSEL (_B{_x\_C})
                    (ALT (SEQ (WITHSUBSTSEL rewrite) (WITHHYPSEL hyp))
                         (SEQ (WITHSUBSTSEL rewritebackwards) (WITHHYPSEL hyp))
                         (Fail (hypothesis _A doesn't fit sub-formula _C))
                    )
                )
                (ALT (SEQ rewrite (WITHHYPSEL hyp))
                     (SEQ rewritebackwards (WITHHYPSEL hyp))
                     (LETGOAL _B (Fail (hypothesis _A doesn't rewrite conclusion _B)))
                )
            )
        )
        (Fail (please select a hypothesis))

TACTIC withsubstrewrite(t)
    WHEN 
        (LETCONCSUBSTSEL _A (WITHSUBSTSEL t))
        (Fail (please text-select a sub-formula, or sub-formulae, in a conclusion))
        
RULE "Fold with hypothesis" (X, ABSTRACTION AA)     IS FROM X=Y ⊢ AA(Y) INFER X=Y ⊢ AA(X)
RULE "Unfold with hypothesis" (Y, ABSTRACTION AA)   IS FROM X=Y ⊢ AA(X) INFER X=Y ⊢ AA(Y)

TACTIC HypFoldUnfold(t) IS 
    WHEN 
        (LETHYP _A 
            (WHEN 
                (LETCONCSUBSTSEL (_B{_x\_C})
                    (ALT    (WITHSUBSTSEL t)
                        (Fail (hypothesis _A doesn't fit sub-formula _C))
                    )
                )
                (Fail (please text-select a sub-formula, or sub-formulae, in a conclusion))
            )
        )
        (Fail (please select a hypothesis))

TACTIC "UnfoldHyp"  IS HypFoldUnfold "Fold with hypothesis"
TACTIC "FoldHyp"    IS HypFoldUnfold "Unfold with hypothesis"
