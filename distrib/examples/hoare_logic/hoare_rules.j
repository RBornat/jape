RULE "skip" IS {A} skip {A}

RULE "tilt" IS {false} tilt {A}

RULE sequence(C) IS FROM { A } F { C } AND  { C } G { B } INFER  { A } (F; G) { B }

RULE "choice" IS FROM {A∧E} F1 {B} AND {A∧ï뾽E} F2  {B} INFER { A } if E then F1 else F2 fi { B }

RULE "variable-assignment" IS INFER {Rï뾽E/xï뾽} (x:=E) {R}

RULE "array-element-assignment" (OBJECT a) IS INFER {Rï뾽a⋯뾽I⇯뾽E/aï뾽} (a[I]:=E) {R}

RULE "while"(I, M, OBJECT Km) WHERE FRESH Km IS
	FROM  {I∧E} F {I} AND I∧E⇯뾽M>0 AND {I∧E∧M=Km} F {M<Km}
	INFER { I } while E do F od {I∧ï뾽E}

RULE "consequence(L)" IS FROM A⇯뾽C AND {C} F {B} INFER {A} F {B}

RULE "consequence(R)" IS FROM {A} F {C} AND C⇯뾽B INFER {A} F {B}

RULE "obviously..." IS INFER A /* to help with arithmetic */

