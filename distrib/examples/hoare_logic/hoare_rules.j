RULE "skip" IS {A} skip {A}

RULE "tilt" IS {false} tilt {A}

RULE sequence(C) IS FROM { A } F { C } AND  { C } G { B } INFER	 { A } (F; G) { B }

RULE "choice" IS FROM {A∧E} F1 {B} AND {A∧¬E} F2  {B} INFER { A } if E then F1 else F2 fi { B }

RULE "variable-assignment" IS INFER {R«E/x»} (x:=E) {R}

RULE "array-element-assignment" (OBJECT a) IS INFER {R«a⊕I↦E/a»} (a[I]:=E) {R}

RULE "while"(I, M, OBJECT Km) WHERE FRESH Km IS
    FROM  {I∧E} F {I} AND I∧E→M>0 AND {I∧E∧M=Km} F {M<Km}
    INFER { I } while E do F od {I∧¬E}

RULE "consequence(L)" IS FROM A→C AND {C} F {B} INFER {A} F {B}

RULE "consequence(R)" IS FROM {A} F {C} AND C→B INFER {A} F {B}

RULE "obviously" IS INFER A /* to help with arithmetic */

