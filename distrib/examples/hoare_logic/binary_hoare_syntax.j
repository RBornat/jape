/* $Id$ */

CLASS VARIABLE x y z a b c d v i j k m n
CLASS FORMULA A B C D E F I J K M N  P Q R S T U V 
CLASS BAG FORMULA ‚

CONSTANT Ù

PREFIX	10		actual

INFIX 10 L ;
INFIX 12 L :=

INFIX 50 L ¼
INFIX 60 L Ø

INFIX		100R		ç
INFIX		120L		ë
INFIX		140L		¦

LEFTFIX	180		è .
LEFTFIX	180		ä .

INFIX 300L <   >   ²   ³   ­   =   é   Âé   

INFIX 400 L + -
INFIX 410 L * div

PREFIX	1200		Â

JUXTFIX	  9000

SUBSTFIX	10000 Ç E / x  È /* so that { }, [ ] are available for other uses */

BIND x SCOPE P IN èx . P
BIND x SCOPE P IN äx . P

SEQUENT IS BAG æ FORMULA

INITIALISE autoAdditiveLeft	true /* allow rules to be stated without an explicit left context */
INITIALISE interpretpredicates	true /* allow predicate syntax ... */



OUTFIX { } /* for assertions */
OUTFIX [ ] /* for indexing */

OUTFIX if then else fi
OUTFIX while do od

CONSTANT skip

INITIALISE hidetransitivity true
INITIALISE hidereflexivity true

RULES "assign" (OBJECT xx) ARE 
		FROM QçRÇE/xÈ INFER {Q} (x:=E) {R}
AND	FROM QçRÇa¼IØE/aÈ INFER {Q} (a[I]:=E) {R}
END 

TACTIC ":=" IS
	LETGOALPATH G
		(ALT assign (Fail "not an assignment statement")) 
		(LAYOUT HIDEROOT "ç intro")
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
		FROM  Qç P AND {P¦B} S {P} AND P¦ÂBçR AND P¦Bç(äx.(T>x)) AND {P¦B¦vt=T} S {T<vt}
		INFER { Q } while B do S od { R}
	RULE "if-then-else-fi" IS FROM {Q¦B} S1 {R} AND {Q¦ÂB} S2  {R} INFER { Q } if B then S1 else S2 fi { R }
	RULE "skip" IS INFER   { Q } skip { Q }
	ENTRY "S1;S2" IS 
		(LETGOALPATH G (LAYOUT COMPRESS "sequence") semicolon (GOALPATH (SUBGOAL G 1)))
	/* ENTRY "strengthen postcondition" IS 
		(LETGOALPATH G "strengthen postcondition" (GOALPATH (SUBGOAL G 1)) (LAYOUT HIDEROOT "ç-I")) */
	
	ENTRY "***assert***"
	
/*	RULE "(A;B);C÷A;(B;C)" IS	A;B;C ÷ A;(B;C)
	ENTRY "flatten ;" IS 
		iterateR2L "rewrite÷"  "symmetric÷" (QUOTE (_A;(_B;_C))) "(A;B);C÷A;(B;C)" (Fail "no semicolons to flatten")
	
	BUTTON	"A÷É"	IS apply rewriteL2R "rewrite÷"  "symmetric÷"  COMMAND
	BUTTON	"É÷B"	IS apply rewriteR2L "rewrite÷"  "symmetric÷"  COMMAND */
	BUTTON	Apply	IS apply COMMAND
END
