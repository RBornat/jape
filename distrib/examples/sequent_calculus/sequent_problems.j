/*
        Some problems, originally cribbed from MacLogic

       $Id$
       
*/

/* 14/ix/92 - RB tidied up some of the provisos in later problems, and can't prove
        17, 21, 24, 26, 41 (or dnegh, dnegh2) using aipc rules.
        
        37 as originally stated was false, RB believes; he also believes that it 
        was probably intended to be stated as 37a.  Or maybe 37b
        
        This activity showed up some problems with proviso simplification/interpretation, 
        q.v. elsewhere.
 */

CONJECTUREPANEL "Conjectures"
  THEOREM INFER	P→(Q→R)				⊢ (P→Q)→(P→R)
  THEOREM INFER	P→(Q→R), Q 			⊢ P→R
  THEOREM INFER	R→S						⊢ (P→R) → (P→S)
  THEOREM INFER	P→(P→Q)				⊢ P→Q
  THEOREM INFER	P							⊢ Q→(P ∧ Q)
  THEOREM INFER	P∧(Q∧R)				⊢ (P∧Q)∧R
  THEOREM INFER	P→Q,P→R				⊢ P→Q∧R
  THEOREM INFER	P→(Q→R)				⊢ (P∧Q) → R
  THEOREM INFER	P∧(Q∨R)				⊢ (P∧Q) ∨ (P∧R)
  THEOREM INFER	P∨(Q∧R)				⊢ (P∨Q) ∧ (P∨R)
  THEOREM INFER	P→S,Q→ ¬S			⊢ ¬(P∧Q)
  THEOREM INFER	P→Q, Q→ ¬P			⊢ ¬P
  THEOREM INFER	P→ ( Q∧R )				⊢ ( P→Q ) ∧ ( P→R )
  THEOREM INFER	(P→Q) ∧ (Q→R), ¬R	⊢ ¬P
  THEOREM INFER	P→Q,¬Q					⊢ (¬¬¬P)∨Q
  THEOREM INFER	P∧Q						⊢ ¬(P ≡ ¬Q)
  THEOREM INFER	P≡Q,Q≡ ¬R				⊢ R ≡ ¬P
  THEOREM INFER	P∨Q,¬P					⊢ Q
  THEOREM INFER	¬(P∨Q)					⊢ ¬P ∧ ¬Q
  THEOREM INFER	¬P ∧ ¬Q					⊢ ¬(P∨Q)
  THEOREM INFER	¬(P∧Q)					⊢ ¬P ∨ ¬Q
  THEOREM INFER	¬P ∨ ¬Q					⊢ ¬(P∧Q)
  THEOREM INFER								⊢ ¬(P ∧ ¬P)
  THEOREM INFER								⊢ ((P → Q) → P) → P
  THEOREM INFER								⊢ (P ∧ ¬P) → Q
  THEOREM INFER	P→ ¬(Q→R)			⊢ (P→Q) ∧ (P→ ¬R)
  THEOREM INFER	¬(P→(Q∨R)) 			⊢ (Q∨R)→P
  
  THEOREM INFER	∀x.¬Q(x),  P→(∀x.Q(x))	⊢ ¬P
  THEOREM WHERE x NOTIN P INFER P∨¬P, ∀x.P→Q(x), ∀x. ¬P→Q(x) 	⊢ ∀x.Q(x)
  THEOREM INFER	R∨¬R, ∀x.R→S(x), ∀x. ¬R→S(x)	⊢ ∀x.S(x)
  THEOREM INFER	∀x.P(x)→Q(x), ∀x.Q(x)→R(x) 		⊢ ∀x.P(x)→R(x)
  THEOREM INFER	∀x.P(x)→R(x), ∀x.Q(x)→ ¬R(x)   	⊢ ∀x.(P(x)→¬Q(x)) ∧ (Q(x)→¬P(x))
  THEOREM INFER	S(m,n), ∀x.P(x) → ¬S(x,n) 			⊢ ¬P(m)
  THEOREM INFER	∀x.P(x)→Q(x), ∀x.R(x)→¬Q(x)   	⊢ ∀x.R(x)→¬P(x)
  THEOREM INFER	∃x.P(x)∧Q(x)			  					⊢ ∃x.P(x)
  THEOREM INFER	∃x.P(x)∧Q(x)			  					⊢ ∃x.Q(x)∧P(x)  
  THEOREM INFER	∃x.P(x)∧¬Q(x), ∀x.P(x)→R(x) 	 	⊢ ∃x.R(x)∧¬Q(x)
  THEOREM INFER	(∀x.Q(x)) → (∀y.¬Q(y)) 				⊢ ¬(∃z.Q(z))
  THEOREM INFER	(∀x.Q(x)) → (∀y.¬Q(y)) 				⊢ ∃z.¬Q(z)
  THEOREM INFER	(∃x.P(x)) → (∃y.¬P(y)) 				⊢ ¬(∀z.P(z))
  THEOREM INFER	∃x.¬P(x) 									⊢ ¬(∀x.P(x))
  THEOREM INFER	∀x.P(x)→Q 								⊢ (∃x.P(x))→Q
  THEOREM INFER	∀x.S(x) → ((¬P(x)∧¬Q(x)) → R(x)) ⊢ ∀x.(S(x)∧¬R(x))→(P(x)∨Q(x))
  
  THEOREM INFER	¬¬P ⊢ P
  THEOREM INFER	P ⊢ ¬¬P
  THEOREM	WHERE x NOTIN y INFER ∃x.∀y.P ⊢ ∀y.∃x.P
  THEOREM INFER	∃x.∀y.Q 		⊢ ∀y.∃x.Q
  THEOREM INFER	∃x.∀y.P(x,y) 	⊢ ∀y.∃x.P(x,y)
  THEOREM INFER	∃x.∀y.P(x,y) 	⊢ ∀v.∃u.P(u,v)
  THEOREM INFER	∀x.P(x)			⊢ ∀y.P(y) 
  THEOREM INFER	∃x.P(x) 			⊢ ∃y.P(y)
  THEOREM INFER	∃y.P(y) 			⊢ ∀x.P(x)
END
