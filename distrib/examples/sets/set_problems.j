/* $Id$ */

TACTIC TheoremForwardOrBackward(X) IS
  WHEN (LETHYP _P cut (WITHSELECTIONS X)) X
  
CONJECTUREPANEL "Set Conjectures" IS
	THEOREMS "Set Conjectures" ARE
		(∃x.P(x) ∧ (∀y. P(y) → y=x)) ↔ (∃u. P(u)) ∧ (∀(v,w). P(v) ∧ P(w) → v=w)
	AND 	A⊆U
	AND	A=B ↔ A⊆B ∧ B⊆A
	AND	A⊆A
	AND	A⊆B, B⊆C ⊢ A⊆C
	AND	Ø⊆A
	AND C∈A∪B↔C∈A∨C∈B
	AND C∈A∩B↔C∈A∧C∈B
	AND C∈A-B↔C∈A∧C¬∈B
	AND	A∪B=B∪A
	AND A∩B=B∩A
	AND (A∪B)∪C=A∪(B∪C)
	AND (A∩B)∩C=A∩(B∩C)
	AND A∪(B∩C)=(A∪B)∩(A∪C)
	AND A∩(B∪C)=(A∩B)∪(A∩C)
	AND A-(B∪C)=(A-B)∩(A-C)
	AND A-(B∩C)=(A-B)∪(A-C)
	AND A∪A=A
	AND A∩A=A
	AND A∪Ø=A
	AND A∩Ø=Ø
	AND A-Ø=A
	AND A-B⊆A
	AND A⊆B, C⊆D ⊢ (A∪C)⊆(B∪D)
	AND A⊆B, C⊆D ⊢ (A∩C)⊆(B∩D)
	AND A⊆A∪B
	AND A∩B⊆A
	AND A⊆B ⊢ A∪B=B
	AND A⊆B ⊢ A∩B=A
	AND A∩(B-A)=Ø
	AND A∪(B-A)=A∪B
	AND A∪A⫠=U
	AND A∩A⫠=Ø
	AND A⫠=B ↔ (A∪B=U)∧(A∩B=Ø)
	AND A⫠⫠=A
	AND (A∪B)⫠=A⫠∩B⫠
	AND (A∩B)⫠=A⫠∪B⫠
	AND (OBJECT y) INFER <C,D>∈A•B ↔ (∃y.<C,y>∈A∧<y,D>∈B)
	AND (A•B)•C=A•(B•C)

	END

	BUTTON Apply IS apply TheoremForwardOrBackward COMMAND
	BUTTON "A↔…" IS apply ForwardSubstHiding "rewrite ↔ «" "rewrite ↔ »"  COMMAND
	BUTTON "…↔B" IS apply ForwardSubstHiding "rewrite ↔ »" "rewrite ↔ «"  COMMAND
	BUTTON "A=…" IS apply ForwardSubstHiding "rewrite = «" "rewrite = »"  COMMAND
	BUTTON "…=B" IS apply ForwardSubstHiding "rewrite = »" "rewrite = «"  COMMAND
END
