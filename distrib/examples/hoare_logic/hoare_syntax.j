/* $Id$ */

CLASS VARIABLE x y z a b c d v i j k m n
CLASS FORMULA A B C D E F I J K M N  P Q R S T U V 
CLASS BAG FORMULA Γ

CONSTANT ⊥

PREFIX	10	actual

INFIX 10 L ;
INFIX 12 L :=

INFIX 50 L ⊕
INFIX 60 L ↦

INFIX	100R	→
INFIX	120L	∨
INFIX	140L	∧

LEFTFIX	180	∀ .
LEFTFIX	180	∃ .

INFIX	300L	<   >   ≤   ≥   ≠   =   ≡   ¬≡   

INFIX 	400 L	+ -
INFIX 	410 L	* div

PREFIX	1200	¬

JUXTFIX	9000

SUBSTFIX	10000 	« E / x  » /* so that { }, [ ] are available for other uses */

BIND x SCOPE P IN ∀x . P
BIND x SCOPE P IN ∃x . P

SEQUENT IS BAG ⊢ FORMULA

INITIALISE autoAdditiveLeft true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates true /* allow predicate syntax ... */

OUTFIX { } /* for assertions */
OUTFIX [ ] /* for indexing */

OUTFIX if then else fi
OUTFIX while do od

CONSTANT skip

INITIALISE hidetransitivity true
INITIALISE hidereflexivity true

RULES "assign" (OBJECT xx) ARE 
	FROM Q→R«E/x» INFER {Q} (x:=E) {R}
  AND	FROM Q→R«a⊕I↦E/a» INFER {Q} (a[I]:=E) {R}
END 

TACTIC ":=" IS
  LETGOALPATH G	
	(ALT assign (Fail "not an assignment statement")) 
	(LAYOUT HIDEROOT "→ intro")
	(LETGOALPATH G1
		(ALT (SEQ hyp (GOALPATH G) (LAYOUT ":=" ()) (GOALPATH G1) NEXTGOAL)
			(SEQ (GOALPATH G )(LAYOUT ":=") (GOALPATH G1))
		)
	)

RULE semicolon(Q) IS FROM { P } P1 { Q } AND  { Q } P2 { R } INFER  { P } (P1; P2) { R }

RULE "***assert***" IS INFER P /* to help with arithmetic */

TACTICPANEL Instructions
	ENTRY "X:=E" IS ":="
	RULE "while-do-od"(P, OBJECT x, OBJECT vt) WHERE FRESH vt IS
		FROM  Q→ P AND {P∧B} S {P} AND P∧¬B→R AND P∧B→(∃x.(T>x)) AND {P∧B∧vt=T} S {T<vt}
		INFER { Q } while B do S od { R}
	RULE "if-then-else-fi" IS FROM {Q∧B} S1 {R} AND {Q∧¬B} S2  {R} INFER { Q } if B then S1 else S2 fi { R }
	RULE "skip" IS INFER   { Q } skip { Q }
	ENTRY "S1;S2" IS 
		(LETGOALPATH G (LAYOUT COMPRESS "sequence") semicolon (GOALPATH (SUBGOAL G 1)))
	/* ENTRY "strengthen postcondition" IS 
		(LETGOALPATH G "strengthen postcondition" (GOALPATH (SUBGOAL G 1)) (LAYOUT HIDEROOT "→-I")) */
	
	ENTRY "***assert***"
	
/*	RULE "(A;B);C≜A;(B;C)" IS	A;B;C ≜ A;(B;C)
	ENTRY "flatten ;" IS 
		iterateR2L "rewrite≜"  "symmetric≜" (QUOTE (_A;(_B;_C))) "(A;B);C≜A;(B;C)" (Fail "no semicolons to flatten")
	
	BUTTON	"A≜…"	IS apply rewriteL2R "rewrite≜"  "symmetric≜"  COMMAND
	BUTTON	"…≜B"	IS apply rewriteR2L "rewrite≜"  "symmetric≜"  COMMAND */
	BUTTON	Apply	IS apply COMMAND
END
