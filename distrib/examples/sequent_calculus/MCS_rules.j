/* $Id$ */

/*
    The multi-conclusion sequent calculus. Dyckhoffery removed.
*/

USE "sequent_syntax.j"

SEQUENT IS BAG ⊢ BAG

RULE    axiom(A)                                        INFER Γ,A ⊢ A,∆
RULE    "⊢∧"        FROM Γ ⊢ A,∆ AND Γ ⊢ B,∆    INFER Γ ⊢ A∧B,∆
RULE    "∧⊢"        FROM Γ,A, B ⊢ ∆                INFER Γ,A∧B ⊢ ∆
RULE    "⊢∨"        FROM Γ ⊢ A,B,∆                 INFER Γ ⊢ A∨B,∆
RULE    "∨⊢"        FROM Γ,A ⊢ ∆ AND Γ,B ⊢ ∆    INFER Γ,A∨B ⊢ ∆
RULE    "⊢¬"        FROM Γ,A ⊢ ∆                   INFER Γ ⊢ ¬A,∆
RULE    "¬⊢"        FROM Γ ⊢ A,∆                   INFER Γ,¬A ⊢ ∆
RULE    "⊢→"        FROM Γ,A ⊢ B,∆                 INFER Γ ⊢ A→B,∆
RULE    "→⊢"        FROM Γ ⊢ A,∆ AND Γ,B ⊢ ∆    INFER Γ,A→B ⊢ ∆
RULE    "⊢≡"        FROM Γ ⊢ A→B,∆ AND Γ ⊢ B→A,∆    INFER Γ ⊢ A≡B,∆
RULE    "≡⊢"        FROM Γ, A→B, B→A ⊢ ∆         INFER Γ,A≡B ⊢ ∆
RULE    "⊢∀"(OBJECT m) WHERE FRESH m
                      FROM Γ ⊢ A(m),∆                INFER Γ ⊢ ∀x.A(x),∆
RULE    "∀⊢"(B)     FROM Γ, A(B) ⊢ ∆               INFER Γ,∀x.A(x) ⊢ ∆
RULE    "⊢∃"(B)     FROM Γ ⊢ A(B),∆                INFER Γ ⊢ ∃x.A(x),∆
RULE    "∃⊢"(OBJECT m) WHERE FRESH m
                      FROM  Γ,A(m) ⊢ ∆               INFER Γ, ∃x.A(x) ⊢ ∆
RULE    cut(A)  FROM Γ ⊢ A,∆ AND Γ,A ⊢ ∆          INFER Γ ⊢ ∆
RULE    "weaken⊢"(A)    FROM Γ ⊢ ∆                  INFER Γ,A ⊢ ∆
RULE    "⊢weaken"(A)    FROM Γ ⊢ ∆                  INFER Γ ⊢ A,∆
RULE    "contract⊢"(A)  FROM Γ, A, A ⊢ ∆            INFER Γ, A ⊢ ∆
RULE    "⊢contract"(A)  FROM Γ ⊢ A,A,∆              INFER Γ ⊢ A,∆
                                
MENU Rules IS
    ENTRY axiom
    SEPARATOR
    ENTRY "∧⊢"
    ENTRY "∨⊢"
    ENTRY "→⊢"
    ENTRY "¬⊢"
    ENTRY "≡⊢"
    ENTRY "∀⊢"
    ENTRY "∃⊢"
    SEPARATOR
    ENTRY "⊢∧"
    ENTRY "⊢∨"
    ENTRY "⊢→"
    ENTRY "⊢¬"
    ENTRY "⊢≡"
    ENTRY "⊢∀"
    ENTRY "⊢∃"
    SEPARATOR
    ENTRY cut   
    ENTRY "weaken⊢"
    ENTRY "⊢weaken"
    ENTRY "contract⊢"
    ENTRY "⊢contract"
END

HYPHIT  A ⊢ A   IS axiom       
HYPHIT  A→B ⊢   IS "→⊢"        
HYPHIT  A∨B ⊢   IS "∨⊢"
HYPHIT  A∧B ⊢   IS "∧⊢"    
HYPHIT  ¬A ⊢        IS "¬⊢"    
HYPHIT  A≡B ⊢   IS "≡⊢"    
HYPHIT  ∀x.A ⊢  IS "∀⊢"
HYPHIT  ∃x.A ⊢  IS "∃⊢"

CONCHIT ⊢ B∧C   IS "⊢∧"
CONCHIT ⊢ B∨C   IS "⊢∨"      
CONCHIT ⊢ B→C   IS "⊢→"
CONCHIT ⊢ ¬B        IS "⊢¬"       
CONCHIT ⊢ B≡C   IS "⊢≡"     
CONCHIT ⊢ ∀x.B  IS "⊢∀"  
CONCHIT ⊢ ∃x.B  IS "⊢∃"  

AUTOMATCH axiom

STRUCTURERULE CUT                   cut
STRUCTURERULE LEFTWEAKEN        "weaken⊢"
STRUCTURERULE RIGHTWEAKEN       "⊢weaken"
